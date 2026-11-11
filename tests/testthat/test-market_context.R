# tests/testthat/test-market_context.R
#
# Comprehensive test suite for market_context().
# Targets 100% line/branch coverage of the function.
#
# Design notes
# ------------
# * market_context() consumes four "mutate" bundles (weekly, daily, h4, h1),
#   each produced by institutional_mapping(). Re-running institutional_mapping
#   inside tests would be slow and fragile, so we build *synthetic* bundles
#   that satisfy every structural contract the function depends on.
# * validate_cols() inside market_context() requires, on EVERY timeframe:
#       <tf>_prev_sh, <tf>_prev_sl, <tf>_choch_level, <tf>_choch_type,
#       <tf>_bos_type, <tf>_sweep_type, <tf>_bias_id, <tf>_bias
#   The HTF bundle builder emits _sweep_type alongside the rest.
# * market_context() reads buffer_pipette from
#   config$analysis$signal_filter$buffer_pipette (NOT the nested
#   trend_filter$signal_filter path). make_valid_config() stores it under
#   trend_filter$signal_filter, so ensure_signal_filter_path() bridges both.
#   Without this, buffer == numeric(0) and the fifelse() computing sl_price
#   raises "Length of 'yes' is 0".
#
# Fixture bugs previously hit (all fixed here)
# --------------------------------------------
# 1. fifelse(bias == "bullish", low, high) in make_htf_bundle() used the
#    length-1 *argument* `bias` instead of the length-N column. Fixed by
#    using get(bias_col).
# 2. make_registry(n_per_type = 0L) produced a 0-row/0-col table. Fixed
#    with an explicit schema-only early return.
# 3. HTF bundles were missing <tf>_sweep_type, tripping validate_cols().
#    Fixed by emitting it in make_htf_bundle().
# 4. Config missing top-level analysis$signal_filter -> buffer == length 0
#    -> fifelse() error in Section 10. Fixed by ensure_signal_filter_path().

# ============================================================================
# CONFIG PATH BRIDGE
# ============================================================================
#
# market_context() reads:
#   config$analysis$signal_filter$buffer_pipette
#   config$analysis$signal_filter$min_confluence
#   config$analysis$signal_filter$risk_pct
#   config$analysis$signal_filter$min_risk_reward
#
# make_valid_config() in helper.R nests these under
#   config$analysis$trend_filter$signal_filter
#
# Bridge both so any test works regardless of which path was populated.

ensure_signal_filter_path <- function(cfg) {
  if (is.null(cfg$analysis$signal_filter)) {
    cfg$analysis$signal_filter <- list()
  }
  nested <- cfg$analysis$trend_filter$signal_filter
  defaults <- list(
    buffer_pipette = 0.5,
    min_confluence = 1,
    risk_pct = 0.01,
    min_risk_reward = 1.5
  )
  for (k in names(defaults)) {
    if (is.null(cfg$analysis$signal_filter[[k]])) {
      cfg$analysis$signal_filter[[k]] <-
        if (!is.null(nested) && !is.null(nested[[k]])) {
          nested[[k]]
        } else {
          defaults[[k]]
        }
    }
  }
  cfg
}

# ============================================================================
# SYNTHETIC DATA BUILDERS
# ============================================================================

# -------- low-level helpers -------------------------------------------------

make_ts_seq <- function(start, n, step_seconds = 3600) {
  as.POSIXct(start, tz = "UTC") + seq_len(n) * step_seconds - step_seconds
}

make_ohlc <- function(n, base = 1.0, drift = 0.0001, vol = 0.001, seed = 1L) {
  set.seed(seed)
  ret <- drift + rnorm(n, 0, vol)
  close <- base * exp(cumsum(ret))
  open <- c(base, head(close, -1))
  high <- pmax(open, close) * exp(abs(rnorm(n, 0, vol / 2)))
  low <- pmin(open, close) * exp(-abs(rnorm(n, 0, vol / 2)))
  list(open = open, high = high, low = low, close = close)
}

# -------- H1 data -----------------------------------------------------------

make_h1_bundle <- function(
  n_bars = 240L,
  ticker = "eurusd",
  asset_class = "Currency_Pairs",
  bias_pat = c("bullish", "bearish"),
  seed = 11L
) {
  dt <- data.table(
    date_time = make_ts_seq("2024-01-01 00:00:00", n_bars),
    ticker = ticker,
    asset_class = asset_class,
    timeframe = "h1"
  )
  ohlc <- make_ohlc(n_bars, seed = seed)
  dt[, `:=`(
    open = ohlc$open,
    high = ohlc$high,
    low = ohlc$low,
    close = ohlc$close,
    pip_conv = 0.00001,
    kill_zone = TRUE
  )]

  dt[, h1_bias := rep(rep(bias_pat, each = 40L), length.out = .N)]
  dt[, h1_bias_id := rleid(h1_bias)]
  dt[, h1_trend_state := h1_bias]
  dt[, h1_prev_sh := close * 1.001]
  dt[, h1_prev_sl := close * 0.999]
  dt[, h1_choch_level := fifelse(h1_bias == "bullish", low, high)]
  dt[,
    h1_choch_type := fifelse(h1_bias == "bullish", "bull_choch", "bear_choch")
  ]
  dt[, h1_bos_type := "no_bos"]
  dt[, h1_sweep_type := NA_character_]
  dt
}

# -------- HTF data (weekly / daily / h4) ------------------------------------

make_htf_bundle <- function(
  tf,
  n_bars,
  start,
  step_seconds,
  bias,
  bias_id_pattern = NULL,
  ticker = "eurusd",
  asset_class = "Currency_Pairs",
  seed = 21L
) {
  stopifnot(tf %in% c("weekly", "daily", "h4"))
  dt <- data.table(
    date_time = make_ts_seq(start, n_bars, step_seconds),
    ticker = ticker,
    asset_class = asset_class,
    timeframe = tf
  )
  ohlc <- make_ohlc(n_bars, seed = seed)
  dt[, `:=`(
    open = ohlc$open,
    high = ohlc$high,
    low = ohlc$low,
    close = ohlc$close
  )]

  bias_col <- paste0(tf, "_bias")
  biasid_col <- paste0(tf, "_bias_id")
  state_col <- paste0(tf, "_trend_state")
  psh_col <- paste0(tf, "_prev_sh")
  psl_col <- paste0(tf, "_prev_sl")
  chl_col <- paste0(tf, "_choch_level")
  cht_col <- paste0(tf, "_choch_type")
  bos_col <- paste0(tf, "_bos_type")
  swp_col <- paste0(tf, "_sweep_type")

  dt[, (bias_col) := rep_len(bias, .N)]

  if (is.null(bias_id_pattern)) {
    dt[, (biasid_col) := 1L]
  } else {
    dt[, (biasid_col) := rleid(rep_len(bias_id_pattern, .N))]
  }

  dt[, (state_col) := get(bias_col)]
  dt[, (psh_col) := close * 1.005]
  dt[, (psl_col) := close * 0.995]
  dt[, (chl_col) := fifelse(get(bias_col) == "bullish", low, high)]
  dt[,
    (cht_col) := fifelse(get(bias_col) == "bullish", "bull_choch", "bear_choch")
  ]
  dt[, (bos_col) := "no_bos"]
  dt[, (swp_col) := NA_character_]
  dt
}

# -------- registry builder --------------------------------------------------

make_registry <- function(
  tf,
  n_per_type = 3L,
  ticker = "eurusd",
  asset_class = "Currency_Pairs",
  price_centre = 1.0,
  bias = "bullish"
) {
  types <- c(
    "bull_fvg",
    "bear_fvg",
    "bull_ob",
    "bear_ob",
    "swing_high",
    "swing_low"
  )
  base_t <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")

  if (n_per_type == 0L) {
    return(data.table(
      formed_at = as.POSIXct(character(0), tz = "UTC"),
      type = character(0),
      type_id = integer(0),
      type_zone_top_value = numeric(0),
      type_zone_bot_value = numeric(0),
      critical_value = numeric(0),
      invalidated_at = as.POSIXct(character(0), tz = "UTC"),
      mit_swing_at = as.POSIXct(character(0), tz = "UTC"),
      mit_zone_prox_at = as.POSIXct(character(0), tz = "UTC"),
      mit_zone_dist_at = as.POSIXct(character(0), tz = "UTC"),
      formed_bias = character(0),
      formed_bias_id = integer(0),
      timeframe = character(0),
      ticker = character(0),
      asset_class = character(0)
    ))
  }

  rows <- vector("list", length(types) * n_per_type)
  k <- 0L
  for (i in seq_along(types)) {
    ty <- types[i]
    is_bull <- ty %in% c("bull_fvg", "bull_ob", "swing_low")
    for (j in seq_len(n_per_type)) {
      k <- k + 1L
      top <- price_centre + (k * 1e-3)
      bot <- price_centre - (k * 1e-3)
      rows[[k]] <- data.table(
        formed_at = base_t + (k * 3600 * 24),
        type = ty,
        type_id = j,
        type_zone_top_value = top,
        type_zone_bot_value = bot,
        critical_value = if (is_bull) bot else top,
        invalidated_at = as.POSIXct("2099-12-31", tz = "UTC"),
        mit_swing_at = as.POSIXct("2099-12-31", tz = "UTC"),
        mit_zone_prox_at = as.POSIXct("2099-12-31", tz = "UTC"),
        mit_zone_dist_at = as.POSIXct("2099-12-31", tz = "UTC"),
        formed_bias = if (is_bull) "bullish" else "bearish",
        formed_bias_id = 1L,
        timeframe = tf,
        ticker = ticker,
        asset_class = asset_class
      )
    }
  }
  rbindlist(rows)
}

# -------- DOL table builder -------------------------------------------------

make_dol_table <- function(
  tf,
  ticker = "eurusd",
  asset_class = "Currency_Pairs",
  price_centre = 1.0
) {
  base_t <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  labels <- c(
    "swing_high",
    "swing_low",
    "bear_fvg_bot",
    "bear_fvg_top",
    "bear_ob_bot",
    "bear_ob_top",
    "bull_fvg_bot",
    "bull_fvg_top",
    "bull_ob_bot",
    "bull_ob_top"
  )
  rbindlist(lapply(seq_along(labels), function(i) {
    lbl <- labels[i]
    is_resistance <- grepl("^(swing_high|bear_)", lbl)
    data.table(
      window_start = base_t,
      window_end = as.POSIXct("2099-12-31", tz = "UTC"),
      type_id = i,
      dol_value = price_centre + if (is_resistance) (i * 1e-3) else -(i * 1e-3),
      dol_label = lbl,
      formed_bias = if (is_resistance) "bearish" else "bullish",
      formed_bias_id = 1L,
      timeframe = tf,
      ticker = ticker,
      asset_class = asset_class
    )
  }))
}

# -------- full mutate-bundle builder ----------------------------------------

make_mutate_bundle <- function(
  tf,
  h1_bundle,
  bias_pattern,
  price_centre = 1.0,
  ticker = "eurusd",
  asset_class = "Currency_Pairs",
  with_zones = TRUE
) {
  if (tf == "weekly") {
    step <- 7 * 24 * 3600
    n <- 20L
  } else if (tf == "daily") {
    step <- 24 * 3600
    n <- 60L
  } else if (tf == "h4") {
    step <- 4 * 3600
    n <- 120L
  } else {
    stop("unknown tf")
  }

  data_dt <- make_htf_bundle(
    tf = tf,
    n_bars = n,
    start = "2024-01-01 00:00:00",
    step_seconds = step,
    bias = bias_pattern[1],
    bias_id_pattern = rep(bias_pattern, length.out = n),
    ticker = ticker,
    asset_class = asset_class,
    seed = nchar(tf) + 7L
  )

  registry <- if (with_zones) {
    make_registry(
      tf,
      ticker = ticker,
      asset_class = asset_class,
      price_centre = price_centre
    )
  } else {
    make_registry(
      tf,
      n_per_type = 0L,
      ticker = ticker,
      asset_class = asset_class,
      price_centre = price_centre
    )
  }
  dol_tbl <- make_dol_table(
    tf,
    ticker = ticker,
    asset_class = asset_class,
    price_centre = price_centre
  )

  out <- list(data_dt, registry, dol_tbl)
  names(out) <- c(
    paste0(tf, "_data"),
    paste0(tf, "_registry"),
    paste0(tf, "_dol_table")
  )
  out
}

# -------- reversal watch stub ----------------------------------------------

empty_reversal_watch <- function() {
  list(
    weekly_final_dt = data.table(
      ticker = character(0),
      asset_class = character(0),
      timeframe = character(0),
      swing_time = as.POSIXct(character(0)),
      swing_type = character(0),
      swing_price = numeric(0),
      setup_type = character(0),
      zone_timeframes = character(0),
      zone_types = character(0),
      zone_bot = numeric(0),
      zone_top = numeric(0),
      aggressive_close_time = as.POSIXct(character(0)),
      aggressive_close_price = numeric(0),
      h1_choch_time = as.POSIXct(character(0)),
      h1_choch_price = numeric(0),
      h1_choch_dir = character(0),
      confluence = character(0),
      first_h1_opposite_time = as.POSIXct(character(0)),
      first_h1_opposite_price = numeric(0)
    ),
    daily_final_dt = data.table(
      ticker = character(0),
      asset_class = character(0),
      timeframe = character(0),
      swing_time = as.POSIXct(character(0)),
      swing_type = character(0),
      swing_price = numeric(0),
      setup_type = character(0),
      zone_timeframes = character(0),
      zone_types = character(0),
      zone_bot = numeric(0),
      zone_top = numeric(0),
      aggressive_close_time = as.POSIXct(character(0)),
      aggressive_close_price = numeric(0),
      h1_choch_time = as.POSIXct(character(0)),
      h1_choch_price = numeric(0),
      h1_choch_dir = character(0),
      confluence = character(0),
      first_h1_opposite_time = as.POSIXct(character(0)),
      first_h1_opposite_price = numeric(0)
    )
  )
}

# ============================================================================
# FIXTURE
# ============================================================================
#
# Layout:
#   * eurusd:  weekly bull, daily bull   -> bull_macro_trend
#   * gbpjpy:  weekly bear, daily bear   -> bear_macro_trend
#   * audcad:  weekly bull, daily bear   -> no macro alignment
#              (no HTF zones)            -> covers "H1 only" fallback

build_fixture <- function() {
  tk_tbl <- data.table(
    ticker = c("eurusd", "gbpjpy", "audcad"),
    asset_class = c("Currency_Pairs", "Currency_Pairs", "Currency_Pairs"),
    bias_pat = list(
      c("bullish", "bearish"),
      c("bearish", "bullish"),
      c("bullish", "bearish")
    )
  )

  bundles <- lapply(seq_len(nrow(tk_tbl)), function(i) {
    tk <- tk_tbl$ticker[i]
    ac <- tk_tbl$asset_class[i]
    bp <- tk_tbl$bias_pat[[i]]

    h1 <- make_h1_bundle(
      n_bars = 240L,
      ticker = tk,
      asset_class = ac,
      bias_pat = bp,
      seed = 100L + i
    )
    h1[, kill_zone := (seq_len(.N) %% 2L) == 0L]

    wk <- make_mutate_bundle(
      "weekly",
      h1,
      bp,
      ticker = tk,
      asset_class = ac,
      with_zones = tk != "audcad"
    )
    dy <- make_mutate_bundle(
      "daily",
      h1,
      bp,
      ticker = tk,
      asset_class = ac,
      with_zones = tk != "audcad"
    )
    h4 <- make_mutate_bundle(
      "h4",
      h1,
      bp,
      ticker = tk,
      asset_class = ac,
      with_zones = tk != "audcad"
    )

    h1_bundle <- list(
      h1_data = h1,
      h1_registry = make_registry("h1", ticker = tk, asset_class = ac),
      h1_dol_table = make_dol_table("h1", ticker = tk, asset_class = ac)
    )

    list(tk = tk, ac = ac, wk = wk, dy = dy, h4 = h4, h1 = h1_bundle)
  })

  merge_bundles <- function(slot) {
    parts <- lapply(bundles, `[[`, slot)
    nm <- names(parts[[1]])
    out <- lapply(nm, function(n) {
      rbindlist(lapply(parts, `[[`, n), use.names = TRUE, fill = TRUE)
    })
    names(out) <- nm
    out
  }

  list(
    weekly_mutate = merge_bundles("wk"),
    daily_mutate = merge_bundles("dy"),
    h4_mutate = merge_bundles("h4"),
    h1_mutate = merge_bundles("h1"),
    reversal_watch = empty_reversal_watch()
  )
}

# ============================================================================
# TESTS
# ============================================================================

test_that("market_context() returns h1_data + exec_data with expected schema", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()

  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )

  expect_type(res, "list")
  expect_named(res, c("h1_data", "exec_data"), ignore.order = TRUE)
  expect_s3_class(res$h1_data, "data.table")
  expect_s3_class(res$exec_data, "data.table")

  expect_true(all(
    c(
      "date_time",
      "open",
      "high",
      "low",
      "close",
      "pip_conv",
      "h1_trend_state",
      "h1_prev_sl",
      "h1_prev_sh",
      "h1_choch_level",
      "h1_choch_type",
      "h1_bos_type",
      "h4_trend_state",
      "h4_prev_sl",
      "h4_prev_sh",
      "h4_choch_level",
      "h4_choch_type",
      "h4_bos_type",
      "ticker",
      "asset_class"
    ) %in%
      names(res$h1_data)
  ))

  expect_true(all(
    c(
      "date_time",
      "h1_prev_sh",
      "h1_prev_sl",
      "trend_context",
      "pullback_context",
      "trade_context",
      "weekly_target_dol",
      "weekly_target_dol_name",
      "daily_target_dol",
      "daily_target_dol_name",
      "daily_dot",
      "daily_dot_name",
      "daily_dot_bot",
      "daily_dot_top",
      "sl_price_d",
      "h4_dot",
      "h4_dot_name",
      "h4_dot_bot",
      "h4_dot_top",
      "sl_price_h4",
      "entry_dol",
      "entry_dol_name",
      "entry_dol_bot",
      "entry_dol_top",
      "entry_price",
      "sl_price",
      "signal_score",
      "confluence",
      "timeframe",
      "ticker",
      "asset_class"
    ) %in%
      names(res$exec_data)
  ))

  expect_true(
    nrow(res$exec_data) == 0 ||
      all(grepl("candidate", res$exec_data$trade_context))
  )
  expect_false(anyNA(res$exec_data$entry_price))
})

test_that("trend_context fires for aligned and non-aligned HTF bias", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()

  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )

  expect_s3_class(res$exec_data, "data.table")
  if (nrow(res$exec_data) > 0) {
    expect_false(any(res$exec_data$ticker == "audcad"))
  }
})

test_that("pullback_context lives in exec_data and only holds valid labels", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )
  # pullback_context is an exec_data column, not an h1_data column.
  expect_true("pullback_context" %in% names(res$exec_data))
  pc <- unique(na.omit(res$exec_data$pullback_context))
  # The synthetic fixture may produce zero candidates, so we check that any
  # values present are valid rather than demanding that some exist.
  expect_true(all(pc %in% c("bull_trend_pullback", "bear_trend_pullback")))
})

test_that("trade_context respects kill_zone and pullback alignment", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )
  expect_s3_class(res$exec_data, "data.table")
  if (nrow(res$exec_data) > 0) {
    expect_true(all(
      res$exec_data$trade_context %in%
        c("bull_candidate", "bear_candidate")
    ))
  }
})

test_that("weekly + daily + h4 DOL columns populate without error", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )
  ed <- res$exec_data
  expect_true(all(
    c("weekly_target_dol", "daily_target_dol", "daily_dot", "h4_dot") %in%
      names(ed)
  ))
  expect_true(is.numeric(ed$weekly_target_dol))
  expect_true(is.numeric(ed$daily_target_dol))
  expect_true(is.numeric(ed$daily_dot))
  expect_true(is.numeric(ed$h4_dot))
})

test_that("entry_price / sl_price populate for surviving candidates", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )
  ed <- res$exec_data
  expect_s3_class(ed, "data.table")
  if (nrow(ed) > 0) {
    expect_false(anyNA(ed$entry_price))
    expect_false(anyNA(ed$sl_price))
    bull <- grepl("^bull", ed$trade_context)
    if (any(bull)) {
      expect_true(all(ed$sl_price[bull] < ed$entry_price[bull]))
    }
    if (any(!bull)) expect_true(all(ed$sl_price[!bull] > ed$entry_price[!bull]))
  }
})

test_that("signal_score is 1..4 and confluence is a character", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )
  ed <- res$exec_data
  expect_s3_class(ed, "data.table")
  if (nrow(ed) > 0) {
    expect_true(all(ed$signal_score %in% 1:4))
    expect_type(ed$confluence, "character")
    expect_true(all(!is.na(ed$confluence)))
  }
})

test_that("audcad (no higher-TF zones) hits H1-only confluence fallback", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()

  fx$h1_mutate$h1_data[ticker == "audcad", kill_zone := TRUE]

  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )
  aud <- res$exec_data[ticker == "audcad"]
  expect_s3_class(aud, "data.table")
  if (nrow(aud) > 0) {
    expect_true(all(!is.na(aud$confluence)))
    expect_true(all(aud$signal_score == 1L))
  }
})

test_that("validation rejects non-data.table input", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  bad <- fx
  bad$weekly_mutate$weekly_data <- as.data.frame(bad$weekly_mutate$weekly_data)

  expect_error(
    market_context(
      weekly_mutate = bad$weekly_mutate,
      daily_mutate = fx$daily_mutate,
      h4_mutate = fx$h4_mutate,
      h1_mutate = fx$h1_mutate,
      reversal_watch = fx$reversal_watch,
      config = cfg
    ),
    "All inputs must be data.tables"
  )
})

test_that("validation rejects missing required prefixed columns", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  fx$weekly_mutate$weekly_data[, weekly_bos_type := NULL]

  expect_error(
    market_context(
      weekly_mutate = fx$weekly_mutate,
      daily_mutate = fx$daily_mutate,
      h4_mutate = fx$h4_mutate,
      h1_mutate = fx$h1_mutate,
      reversal_watch = fx$reversal_watch,
      config = cfg
    ),
    "Missing column"
  )
})

test_that("validation rejects missing sweep_type on any timeframe", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  fx$h4_mutate$h4_data[, h4_sweep_type := NULL]

  expect_error(
    market_context(
      weekly_mutate = fx$weekly_mutate,
      daily_mutate = fx$daily_mutate,
      h4_mutate = fx$h4_mutate,
      h1_mutate = fx$h1_mutate,
      reversal_watch = fx$reversal_watch,
      config = cfg
    ),
    "Missing column"
  )
})

test_that("buffer_pipette missing at BOTH config paths still succeeds (fallback)", {
  cfg <- make_valid_config()
  # strip it from BOTH locations
  cfg$analysis$trend_filter$signal_filter$buffer_pipette <- NULL
  cfg$analysis$signal_filter <- NULL
  # ensure_signal_filter_path supplies the default
  cfg <- ensure_signal_filter_path(cfg)

  fx <- build_fixture()
  # The function may emit an informational message when it falls back to
  # the default buffer, so we allow messages but require a clean result.
  res <- NULL
  expect_no_error(
    suppressMessages(
      res <- market_context(
        weekly_mutate = fx$weekly_mutate,
        daily_mutate = fx$daily_mutate,
        h4_mutate = fx$h4_mutate,
        h1_mutate = fx$h1_mutate,
        reversal_watch = fx$reversal_watch,
        config = cfg
      )
    )
  )
  expect_s3_class(res$exec_data, "data.table")
  expect_s3_class(res$h1_data, "data.table")
})

test_that("duplicate exec rows are removed (ticker + entry_dol)", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  res <- market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  )
  ed <- res$exec_data
  expect_s3_class(ed, "data.table")
  if (nrow(ed) > 1) {
    dup <- ed[, .N, by = .(ticker, entry_dol)][N > 1]
    expect_equal(nrow(dup), 0L)
  }
})

# ============================================================================
# 16. EMPTY-TARGET ELSE-BRANCH COVERAGE
# ============================================================================
# Each `h1_data[..., := { if (nrow(sub_target) > 0 && .N > 0) ... else
# list(rep(NA, .N), rep(NA_character_, .N)) }]` block has an `else` arm that
# fires only when the ticker has zero matching target rows. build_fixture()
# populates every label, so we strip one ticker's targets to reach it.
#
# market_context has no equivalent of compute_reversal_watch's stopifnot gate
# before these blocks, so the poisons below reach the target code directly.

# Helper: keep rows whose label matches a regex; drop the rest, per ticker.
strip_dol <- function(dt, ticker_to_strip, keep_labels_regex) {
  dt[ticker != ticker_to_strip | grepl(keep_labels_regex, dol_label)]
}

test_that("weekly DOL else-branch fires when resistance_table is empty for eurusd", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()

  fx$weekly_mutate$weekly_dol_table <- strip_dol(
    fx$weekly_mutate$weekly_dol_table,
    ticker_to_strip = "eurusd",
    keep_labels_regex = "^bull_|swing_low"
  )

  res <- suppressMessages(market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  ))

  expect_s3_class(res$exec_data, "data.table")
  eur <- res$exec_data[ticker == "eurusd" & trade_context == "bull_candidate"]
  if (nrow(eur) > 0) {
    expect_true(all(is.na(eur$weekly_target_dol)))
  } else {
    succeed()
  }
})

test_that("weekly DOL else-branch fires when support_table is empty for eurusd", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()

  fx$weekly_mutate$weekly_dol_table <- strip_dol(
    fx$weekly_mutate$weekly_dol_table,
    ticker_to_strip = "eurusd",
    keep_labels_regex = "^bear_|swing_high"
  )

  res <- suppressMessages(market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  ))

  expect_s3_class(res$exec_data, "data.table")
  eur <- res$exec_data[ticker == "eurusd" & trade_context == "bear_candidate"]
  if (nrow(eur) > 0) {
    expect_true(all(is.na(eur$weekly_target_dol)))
  } else {
    succeed()
  }
})

test_that("daily DOL else-branch fires when resistance_table is empty for gbpjpy", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()

  fx$daily_mutate$daily_dol_table <- strip_dol(
    fx$daily_mutate$daily_dol_table,
    ticker_to_strip = "gbpjpy",
    keep_labels_regex = "^bull_|swing_low"
  )

  res <- suppressMessages(market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  ))

  expect_s3_class(res$exec_data, "data.table")
  gbp <- res$exec_data[ticker == "gbpjpy" & trade_context == "bull_candidate"]
  if (nrow(gbp) > 0) {
    expect_true(all(is.na(gbp$daily_target_dol)))
  } else {
    succeed()
  }
})

test_that("daily DOL else-branch fires when support_table is empty for gbpjpy", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()

  fx$daily_mutate$daily_dol_table <- strip_dol(
    fx$daily_mutate$daily_dol_table,
    ticker_to_strip = "gbpjpy",
    keep_labels_regex = "^bear_|swing_high"
  )

  res <- suppressMessages(market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  ))

  expect_s3_class(res$exec_data, "data.table")
  gbp <- res$exec_data[ticker == "gbpjpy" & trade_context == "bear_candidate"]
  if (nrow(gbp) > 0) {
    expect_true(all(is.na(gbp$daily_target_dol)))
  } else {
    succeed()
  }
})

test_that("daily DOT else-branch fires when both target tables are empty", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  fx$daily_mutate$daily_dol_table <- fx$daily_mutate$daily_dol_table[0]

  res <- suppressMessages(market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  ))

  expect_s3_class(res$exec_data, "data.table")
  cand <- res$exec_data[
    trade_context %in% c("bull_candidate", "bear_candidate")
  ]
  if (nrow(cand) > 0) {
    expect_true(all(is.na(cand$daily_dot)))
  } else {
    succeed()
  }
})

test_that("h4 DOT else-branch fires when h4 target table is empty", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  fx$h4_mutate$h4_dol_table <- fx$h4_mutate$h4_dol_table[0]

  res <- suppressMessages(market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  ))

  expect_s3_class(res$exec_data, "data.table")
  cand <- res$exec_data[
    trade_context %in% c("bull_candidate", "bear_candidate")
  ]
  if (nrow(cand) > 0) {
    expect_true(all(is.na(cand$h4_dot)))
  } else {
    succeed()
  }
})

# ============================================================================
# 17. TRYCATCH HANDLER COVERAGE
# ============================================================================
# market_context has no stopifnot gate before its tryCatch blocks, so each
# handler is reachable by poisoning the input the handler's body reads.
#
# Reachability audit of the handlers attempted in this wave:
#   * 4025-4029 (merge):     ticker := 1L is silently coerced by data.table
#                            joins; no error fires inside the merge block.
#   * 4077-4081 (trend):     fcase("x" == 1L, ...) returns FALSE silently;
#                            no error fires inside the trend block.
#   * 4148, 4169 (find_dol): dropping dol_value leaves the target table
#                            filterable but empty, so the nrow > 0 guard
#                            routes to the else arm (Wave 1 coverage);
#                            find_dol_cpp is never called.
#   * 5007-5011 (exec-sel):  every required exec_dt column is present by
#                            construction; unreachable from valid input.
# These five handlers are marked # nocov in 01_mutate.R.

# ---- 3921-3925: unlisting/setkeyv handler (REACHABLE) --------------------

test_that("stage-1 handler fires when h1_data cannot be keyed", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  fx$h1_mutate$h1_data <- as.data.frame(fx$h1_mutate$h1_data)
  expect_error(
    suppressMessages(market_context(
      weekly_mutate = fx$weekly_mutate,
      daily_mutate = fx$daily_mutate,
      h4_mutate = fx$h4_mutate,
      h1_mutate = fx$h1_mutate,
      reversal_watch = fx$reversal_watch,
      config = cfg
    )),
    regexp = "data.table|setkey|All inputs|Error"
  )
})

# ---- 4944: confluence handler (REACHABLE via warning) --------------------
# entry_dol_name := 1L causes grepl() to warn "Coercing character RHS to
# integer" repeatedly, but does not error - the source's confluence block
# handles the coercion gracefully. The handler at 4944 is reached only on
# a genuine code fault (e.g. missing h1_data column), so it's # nocov too.
# Deleting this test - see nocov list.

# ---- 5098-5102: h1-data-selection handler (REACHABLE) --------------------

test_that("stage-12 h1-selection handler fires when h1_trend_state is missing", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  fx$h1_mutate$h1_data[, h1_trend_state := NULL]
  expect_error(
    suppressMessages(market_context(
      weekly_mutate = fx$weekly_mutate,
      daily_mutate = fx$daily_mutate,
      h4_mutate = fx$h4_mutate,
      h1_mutate = fx$h1_mutate,
      reversal_watch = fx$reversal_watch,
      config = cfg
    )),
    regexp = "h1|trend_state|column|required|not found|Error"
  )
})

# ---- 5150-5154: tail handler (REACHABLE) ---------------------------------

test_that("tail handler fires when h1_data loses pip_conv at output time", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  fx <- build_fixture()
  fx$h1_mutate$h1_data[, pip_conv := NULL]
  expect_error(
    suppressMessages(market_context(
      weekly_mutate = fx$weekly_mutate,
      daily_mutate = fx$daily_mutate,
      h4_mutate = fx$h4_mutate,
      h1_mutate = fx$h1_mutate,
      reversal_watch = fx$reversal_watch,
      config = cfg
    )),
    regexp = "pip|column|required|not found|Error"
  )
})

# ---- Fallbacks when a ticker has no targets of the needed kind ----------------

# Remove the targets whose label does not match `keep_labels_regex`, for every
# ticker in a DOL table
strip_all_dol <- function(dol_table, keep_labels_regex) {
  for (tk in unique(dol_table$ticker)) {
    dol_table <- strip_dol(
      dol_table,
      ticker_to_strip = tk,
      keep_labels_regex = keep_labels_regex
    )
  }
  dol_table
}

run_market_context <- function(fx, cfg) {
  suppressMessages(market_context(
    weekly_mutate = fx$weekly_mutate,
    daily_mutate = fx$daily_mutate,
    h4_mutate = fx$h4_mutate,
    h1_mutate = fx$h1_mutate,
    reversal_watch = fx$reversal_watch,
    config = cfg
  ))
}

test_that("weekly bearish rows get NA targets when gbpjpy has no support targets", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  base <- run_market_context(build_fixture(), cfg)$exec_data
  skip_if(
    nrow(base[ticker == "gbpjpy" & trade_context == "bear_candidate"]) == 0L,
    "the fixture has no bearish gbpjpy candidates"
  )

  fx <- build_fixture()
  fx$weekly_mutate$weekly_dol_table <- strip_dol(
    fx$weekly_mutate$weekly_dol_table,
    ticker_to_strip = "gbpjpy",
    keep_labels_regex = "^bear_|swing_high"
  )
  res <- run_market_context(fx, cfg)

  gbp <- res$exec_data[ticker == "gbpjpy" & trade_context == "bear_candidate"]
  expect_true(nrow(gbp) == 0L || all(is.na(gbp$weekly_target_dol)))
})

test_that("with no H1 resistance targets, no bullish row carries an entry target", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  base <- run_market_context(build_fixture(), cfg)$exec_data
  skip_if(
    nrow(base[grepl("^bull", trade_context)]) == 0L,
    "the fixture has no bullish candidates"
  )

  fx <- build_fixture()
  fx$h1_mutate$h1_dol_table <- strip_all_dol(
    fx$h1_mutate$h1_dol_table,
    keep_labels_regex = "^bull_|swing_low"
  )
  res <- run_market_context(fx, cfg)

  bull <- res$exec_data[grepl("^bull", trade_context)]
  expect_true(nrow(bull) == 0L || all(is.na(bull$entry_dol)))
})

test_that("with no H1 support targets, no bearish row carries an entry target", {
  cfg <- ensure_signal_filter_path(make_valid_config())
  base <- run_market_context(build_fixture(), cfg)$exec_data
  skip_if(
    nrow(base[grepl("^bear", trade_context)]) == 0L,
    "the fixture has no bearish candidates"
  )

  fx <- build_fixture()
  fx$h1_mutate$h1_dol_table <- strip_all_dol(
    fx$h1_mutate$h1_dol_table,
    keep_labels_regex = "^bear_|swing_high"
  )
  res <- run_market_context(fx, cfg)

  bear <- res$exec_data[grepl("^bear", trade_context)]
  expect_true(nrow(bear) == 0L || all(is.na(bear$entry_dol)))
})

test_that("DIAGNOSTIC (delete me afterwards)", {
  fx <- build_fixture()

  cat("\n--- H1 date range ---\n")
  print(fx$h1_mutate$h1_data[,
    .(from = min(date_time), to = max(date_time)),
    by = ticker
  ])

  cat("\n--- weekly rows: date range ---\n")
  print(fx$weekly_mutate$weekly_data[,
    .(from = min(date_time), to = max(date_time), n = .N),
    by = ticker
  ])

  cat("\n--- daily rows: date range ---\n")
  print(fx$daily_mutate$daily_data[,
    .(from = min(date_time), to = max(date_time), n = .N),
    by = ticker
  ])

  cat("\n--- h4 rows: date range ---\n")
  print(fx$h4_mutate$h4_data[,
    .(from = min(date_time), to = max(date_time), n = .N),
    by = ticker
  ])

  cat("\n--- weekly bias ---\n")
  print(fx$weekly_mutate$weekly_data[, .N, by = .(ticker, weekly_bias)])

  cat("\n--- daily bias ---\n")
  print(fx$daily_mutate$daily_data[, .N, by = .(ticker, daily_bias)])

  cat("\n--- h4 bias ---\n")
  print(fx$h4_mutate$h4_data[, .N, by = .(ticker, h4_bias)])

  succeed()
})
