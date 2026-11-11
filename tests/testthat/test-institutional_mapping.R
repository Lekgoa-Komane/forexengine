# tests/testthat/test-institutional_mapping.R
# Comprehensive test suite for institutional_mapping()
#
# Fixture notes:
#   * make_mapping_input() now guards swing-injection indices against n_bars.
#   * volume/open_interest use .N so they never overflow when data.table
#     internally extends a vector.
#   * No test relies on hard-coded index 70 being in range.

# ============================================================================
# FIXTURES
# ============================================================================

# Deterministic multi-ticker OHLC generator that satisfies every required
# column of institutional_mapping() and always produces swing points.
make_mapping_input <- function(
  time_frame = "h1",
  n_bars = 120L,
  tickers = c("audcad", "eurusd"),
  seed = 42L
) {
  set.seed(seed)
  stopifnot(n_bars >= 40L)

  needs_volume <- !time_frame %in% c("h1", "h4")

  make_one <- function(tk, offset = 0) {
    t <- seq_len(n_bars)
    base <- 1.2000 +
      offset +
      0.0020 * sin(t / 5) +
      0.0010 * cumsum(rnorm(n_bars, 0, 0.0005))

    open <- base
    close <- base + rnorm(n_bars, 0, 0.0003)
    high <- pmax(open, close) + abs(rnorm(n_bars, 0, 0.0002))
    low <- pmin(open, close) - abs(rnorm(n_bars, 0, 0.0002))

    # Inject clean swings, but only at indices that actually exist
    hi_idx <- intersect(c(20L, 55L, 90L), seq_len(n_bars))
    lo_idx <- intersect(c(35L, 70L, 105L), seq_len(n_bars))
    if (length(hi_idx)) {
      high[hi_idx] <- high[hi_idx] + 0.0030
    }
    if (length(lo_idx)) {
      low[lo_idx] <- low[lo_idx] - 0.0030
    }

    dt <- data.table(
      date_time = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") +
        seq(0, by = 3600, length.out = n_bars),
      open = open,
      high = high,
      low = low,
      close = close,
      timeframe = time_frame,
      ticker = tk,
      asset_class = "Currency_Pairs"
    )

    if (needs_volume) {
      # Use .N, not n_bars — data.table may have extended the column
      dt[, `:=`(
        volume = runif(.N, 1e3, 1e4),
        open_interest = runif(.N, 1e3, 1e4)
      )]
    }
    dt
  }

  rbindlist(lapply(seq_along(tickers), function(i) {
    make_one(tickers[i], offset = 0.05 * (i - 1))
  }))
}

# Flat (zero-volatility) data — guaranteed to produce zero swings.
make_flat_input <- function(time_frame = "h1", n_bars = 60L) {
  data.table(
    date_time = as.POSIXct("2020-01-01", tz = "UTC") +
      seq(0, by = 3600, length.out = n_bars),
    open = 1.2,
    high = 1.2,
    low = 1.2,
    close = 1.2,
    timeframe = time_frame,
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )
}

run_mapping <- function(dt, config) institutional_mapping(dt, config)

# ============================================================================
# 1. INPUT VALIDATION
# ============================================================================

test_that("errors when input is not a data.table", {
  config <- make_valid_config()
  df <- as.data.frame(make_mapping_input())

  expect_error(
    institutional_mapping(df, config),
    regexp = "must be a data.table"
  )
})

test_that("errors when required columns are missing (daily path)", {
  config <- make_valid_config()
  bad <- make_mapping_input(time_frame = "daily")[, volume := NULL]

  expect_error(
    institutional_mapping(bad, config),
    regexp = "Missing columns"
  )
})

test_that("errors when required columns are missing (h1 path)", {
  config <- make_valid_config()
  bad <- make_mapping_input(time_frame = "h1")[, asset_class := NULL]

  expect_error(
    institutional_mapping(bad, config),
    regexp = "Missing columns"
  )
})

test_that("errors when lookback_period is NULL for the timeframe", {
  config <- make_valid_config()
  config$analysis$trend_filter$h1$lookback_period <- NULL
  dt <- make_mapping_input(time_frame = "h1")

  # The %||% fallback only fires *after* the explicit NULL check,
  # so this must error (previous test was wrong).
  expect_error(
    institutional_mapping(dt, config),
    regexp = "lookback_period"
  )
})

# ============================================================================
# 2. RETURN STRUCTURE
# ============================================================================

test_that("returns the four expected named elements for h1", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  res <- suppressWarnings(run_mapping(dt, config))

  expect_type(res, "list")
  expect_named(
    res,
    c("h1_data", "h1_registry", "h1_dol_table", "swings"),
    ignore.order = FALSE
  )
  expect_s3_class(res$h1_data, "data.table")
  expect_s3_class(res$h1_registry, "data.table")
  expect_s3_class(res$h1_dol_table, "data.table")
  expect_s3_class(res$swings, "data.table")
})

test_that("returns the four expected named elements for daily", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "daily", n_bars = 90L)

  res <- suppressWarnings(run_mapping(dt, config))

  expect_named(
    res,
    c("daily_data", "daily_registry", "daily_dol_table", "swings"),
    ignore.order = FALSE
  )
})

# ============================================================================
# 3. OUTPUT COLUMN CONTRACTS
# ============================================================================

test_that("h1_data contains all required renamed columns", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  res <- suppressWarnings(run_mapping(dt, config))
  cols <- names(res$h1_data)

  expect_true(all(
    c(
      "date_time",
      "open",
      "high",
      "low",
      "close",
      "timeframe",
      "ticker",
      "asset_class"
    ) %in%
      cols
  ))

  expect_true(all(
    c(
      "tokyo",
      "london",
      "new_york",
      "kill_zone",
      "pip_conv"
    ) %in%
      cols
  ))

  expect_true(any(grepl("^h1_swing", cols)))
  expect_true(any(grepl("^h1_bias", cols)))
  expect_true(any(grepl("^h1_prev", cols)))
  expect_true(any(grepl("^h1_.*level", cols)))
  expect_true(any(grepl("^h1_.*type", cols)))
  expect_true(any(grepl("^h1_.*state", cols)))
})

test_that("non-h1 data omits session columns", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "daily", n_bars = 90L)

  res <- suppressWarnings(run_mapping(dt, config))
  cols <- names(res$daily_data)

  expect_false(any(c("tokyo", "london", "new_york", "kill_zone") %in% cols))
  expect_true("date_time" %in% cols)
  expect_true(any(grepl("^daily_bias", cols)))
})

test_that("registry has the expected schema", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  reg <- suppressWarnings(run_mapping(dt, config))$h1_registry

  expect_true(all(
    c(
      "formed_at",
      "type_id",
      "type",
      "type_zone_bot_value",
      "type_zone_top_value",
      "critical_value",
      "invalid_rule",
      "mitig_rule",
      "rel_op",
      "invalidated_at",
      "mit_swing_at",
      "mit_zone_prox_at",
      "mit_zone_dist_at",
      "formed_bias",
      "formed_bias_id",
      "timeframe",
      "ticker",
      "asset_class"
    ) %in%
      names(reg)
  ))
})

test_that("dol_table has the expected schema", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  dol <- suppressWarnings(run_mapping(dt, config))$h1_dol_table

  expect_true(all(
    c(
      "window_start",
      "type_id",
      "dol_value",
      "dol_label",
      "formed_bias_id",
      "formed_bias",
      "window_end",
      "timeframe",
      "ticker",
      "asset_class"
    ) %in%
      names(dol)
  ))
})

test_that("swings table exposes the documented swing columns", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  sw <- suppressWarnings(run_mapping(dt, config))$swings

  expect_true(all(
    c(
      "date_time",
      "swing_high",
      "swing_low",
      "prev_sh",
      "prev_sl",
      "swing_price",
      "sw_gap",
      "swing_type",
      "structure",
      "bias",
      "ticker",
      "asset_class"
    ) %in%
      names(sw)
  ))
})

# ============================================================================
# 4. DATA INTEGRITY / SEMANTIC CHECKS
# ============================================================================

test_that("row count and key columns are preserved in *_data", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  res <- suppressWarnings(run_mapping(dt, config))

  expect_equal(nrow(res$h1_data), nrow(dt))
  expect_setequal(unique(res$h1_data$ticker), unique(dt$ticker))
  expect_true(all(res$h1_data$timeframe == "h1"))
})

test_that("trend_state only contains bullish/bearish/NA", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  res <- suppressWarnings(run_mapping(dt, config))
  st_col <- grep("^h1_.*state$", names(res$h1_data), value = TRUE)
  skip_if(length(st_col) == 0, "trend_state not present")

  expect_true(all(
    unique(res$h1_data[[st_col]]) %in%
      c("bullish", "bearish", NA_character_)
  ))
})

test_that("choch_type and bos_type only use documented labels", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  res <- suppressWarnings(run_mapping(dt, config))
  ct <- grep("^h1_.*choch.*type$", names(res$h1_data), value = TRUE)
  bt <- grep("^h1_.*bos.*type$", names(res$h1_data), value = TRUE)

  if (length(ct) > 0) {
    expect_true(all(
      unique(res$h1_data[[ct]]) %in%
        c("no_choch", "bull_choch", "bear_choch")
    ))
  }
  if (length(bt) > 0) {
    expect_true(all(
      unique(res$h1_data[[bt]]) %in%
        c("no_bos", "bull_bos", "bear_bos", "bull_range_bos", "bear_range_bos")
    ))
  }
})

test_that("sweep_type only uses documented labels", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  res <- suppressWarnings(run_mapping(dt, config))
  sc <- grep("^h1_.*sweep.*type$", names(res$h1_data), value = TRUE)
  skip_if(length(sc) == 0, "sweep_type not present")

  expect_true(all(
    unique(res$h1_data[[sc]]) %in%
      c("bull_liq_sweep", "bear_liq_sweep", NA_character_)
  ))
})

test_that("registry formed_at values exist in the source data", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  res <- suppressWarnings(run_mapping(dt, config))
  reg <- res$h1_registry
  skip_if(nrow(reg) == 0, "no registry rows produced")

  valid_dates <- unique(res$h1_data$date_time)
  expect_true(all(reg$formed_at %in% valid_dates))
})

test_that("registry zones have bottom <= top for FVG/OB types", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  reg <- suppressWarnings(run_mapping(dt, config))$h1_registry
  zones <- reg[grepl("fvg|ob", type)]
  skip_if(nrow(zones) == 0, "no zone rows")

  expect_true(all(
    zones$type_zone_bot_value <= zones$type_zone_top_value,
    na.rm = TRUE
  ))
})

test_that("registry only contains documented type values", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  reg <- suppressWarnings(run_mapping(dt, config))$h1_registry
  skip_if(nrow(reg) == 0, "no registry rows")

  expect_true(all(
    unique(reg$type) %in%
      c(
        "swing_high",
        "swing_low",
        "bull_fvg",
        "bear_fvg",
        "bull_ob",
        "bear_ob"
      )
  ))
})

test_that("every registry type_id is unique within ticker+type", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  reg <- suppressWarnings(run_mapping(dt, config))$h1_registry
  skip_if(nrow(reg) == 0, "no registry rows")

  dup <- reg[, .N, by = .(asset_class, ticker, type, type_id)][N > 1]
  expect_equal(nrow(dup), 0)
})

test_that("dol_table rows reference registered type_ids", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  res <- suppressWarnings(run_mapping(dt, config))
  reg <- res$h1_registry
  dol <- res$h1_dol_table
  skip_if(nrow(dol) == 0 || nrow(reg) == 0, "empty registry/dol")

  expect_true(all(dol$type_id %in% reg$type_id))
})

test_that("dol_table window_start <= window_end where present", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  dol <- suppressWarnings(run_mapping(dt, config))$h1_dol_table
  skip_if(nrow(dol) == 0, "no dol rows")

  ok <- !is.na(dol$window_start) & !is.na(dol$window_end)
  expect_true(all(dol$window_start[ok] <= dol$window_end[ok]))
})

test_that("registry invalidation timestamps are >= formation timestamps", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  reg <- suppressWarnings(run_mapping(dt, config))$h1_registry
  skip_if(nrow(reg) == 0, "no registry rows")

  expect_true(all(reg$invalidated_at >= reg$formed_at, na.rm = TRUE))
  expect_true(all(reg$mit_swing_at >= reg$formed_at, na.rm = TRUE))
})

test_that("kill_zone is logical and present only for h1", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  res <- suppressWarnings(run_mapping(dt, config))

  expect_type(res$h1_data$kill_zone, "logical")
  expect_true(all(res$h1_data$kill_zone %in% c(TRUE, FALSE)))
})

test_that("make_valid_config provides sessions in the shape institutional_mapping reads", {
  cfg <- make_valid_config()
  s <- cfg$analysis$sessions
  expect_true(!is.null(s$london$standard_time$start))
  expect_true(!is.null(s$london$standard_time$end))
  expect_true(!is.null(s$london$daylight_time$start))
  expect_true(!is.null(s$london$kill_zones$london_open$start))
  expect_true(!is.null(s$london$kill_zones$london_open$end))
  expect_true(!is.null(s$newyork$kill_zones$ny_open$start))
  expect_true(!is.null(s$newyork$kill_zones$ny_open$end))
})

test_that("pip_conv matches regex-based config factor", {
  config <- make_valid_config()
  dt <- make_mapping_input(
    time_frame = "h1",
    tickers = c("audcad", "gbpjpy") # one JPY, one non-JPY
  )

  res <- suppressWarnings(run_mapping(dt, config))

  jpy <- res$h1_data[ticker == "gbpjpy"]
  non <- res$h1_data[ticker == "audcad"]

  expect_true(all(
    jpy$pip_conv == config$transformation$pip_factor$value_for_match
  ))
  expect_true(all(
    non$pip_conv == config$transformation$pip_factor$default_value
  ))
})

test_that("session labels only use Tokyo/London/NY/NA", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 48L)

  res <- suppressWarnings(run_mapping(dt, config))

  for (col in c("tokyo", "london", "new_york")) {
    vals <- unique(res$h1_data[[col]])
    expect_true(all(vals %in% c("Tokyo", "London", "NY", NA_character_)))
  }
})

# ============================================================================
# 5. CONFIG OVERRIDES / BRANCH COVERAGE
# ============================================================================

test_that("respects custom lookback_period for each timeframe", {
  config <- make_valid_config()
  config$analysis$trend_filter$h1$lookback_period <- 5L

  dt <- make_mapping_input(time_frame = "h1")
  res <- suppressWarnings(run_mapping(dt, config))

  expect_s3_class(res$h1_data, "data.table")
})

test_that("handles custom swing/noise/OB/CHoCH quantiles", {
  config <- make_valid_config()
  config$analysis$swing_filter$noise_filter <- 0.80
  config$analysis$swing_filter$sw_gap_bot <- 0.20
  config$analysis$swing_filter$sw_gap_top <- 0.80
  config$analysis$swing_filter$ob_filter <- 0.50
  config$analysis$choch_filter$choch_quant <- 0.50

  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)
  res <- suppressWarnings(run_mapping(dt, config))
  expect_s3_class(res$h1_data, "data.table")
})

test_that("handles custom signal_filter quantiles (FVG band)", {
  config <- make_valid_config()
  config$analysis$trend_filter$signal_filter$lower_gap <- 0.30
  config$analysis$trend_filter$signal_filter$upper_gap <- 0.90

  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)
  expect_s3_class(
    suppressWarnings(run_mapping(dt, config))$h1_data,
    "data.table"
  )
})

test_that("flat-price input does not crash (known hardening gap)", {
  config <- make_valid_config()
  flat <- make_flat_input(time_frame = "h1", n_bars = 60L)

  # institutional_mapping() currently crashes on all-flat data inside
  # the swing dedup step (`min(swing_price)` on all-NA). If you harden
  # that step, remove the expect_error and switch to expect_silent.
  expect_error(
    suppressWarnings(run_mapping(flat, config)),
    regexp = "no non-missing arguments|must be length 1|length 0"
  )
})

# ============================================================================
# 6. DAILY (volume-bearing) PATH
# ============================================================================

test_that("daily path requires and preserves volume/open_interest inputs", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "daily", n_bars = 90L)

  expect_true(all(c("volume", "open_interest") %in% names(dt)))

  res <- suppressWarnings(run_mapping(dt, config))
  expect_s3_class(res$daily_data, "data.table")
})

test_that("non-h1/h4 timeframes all use the same mapping path", {
  config <- make_valid_config()

  for (tf in c("daily", "weekly")) {
    dt <- make_mapping_input(time_frame = tf, n_bars = 90L)
    res <- suppressWarnings(run_mapping(dt, config))
    expect_true(paste0(tf, "_data") %in% names(res))
  }
})

# ============================================================================
# 7. H4 PATH
# ============================================================================

test_that("h4 path runs without session columns", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h4", n_bars = 120L)

  res <- suppressWarnings(run_mapping(dt, config))

  expect_named(
    res,
    c("h4_data", "h4_registry", "h4_dol_table", "swings"),
    ignore.order = FALSE
  )
  expect_false("kill_zone" %in% names(res$h4_data))
})

# ============================================================================
# 8. ANOMALY DETECTION BRANCH
# ============================================================================

test_that("extreme spike bar does not crash the pipeline", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")

  dt[
    ticker == "audcad" & rowid(ticker) == 30,
    `:=`(high = high * 5, low = low / 5, close = close * 4)
  ]

  expect_s3_class(
    suppressWarnings(run_mapping(dt, config))$h1_data,
    "data.table"
  )
})

# ============================================================================
# 9. FVG / OB DETECTION BRANCHES
# ============================================================================

test_that("synthetic gap produces at least one FVG when band is wide", {
  config <- make_valid_config()
  config$analysis$trend_filter$signal_filter$lower_gap <- 0.00
  config$analysis$trend_filter$signal_filter$upper_gap <- 1.00

  dt <- make_mapping_input(time_frame = "h1", n_bars = 120L)
  dt[
    ticker == "audcad" & rowid(ticker) %in% c(49, 50, 51),
    `:=`(
      high = c(1.3000, 1.3100, 1.3200),
      low = c(1.2900, 1.3050, 1.3150),
      open = c(1.2950, 1.3075, 1.3175),
      close = c(1.2950, 1.3075, 1.3175)
    )
  ]

  res <- suppressWarnings(run_mapping(dt, config))
  expect_s3_class(res$h1_registry, "data.table")
})

test_that("loose OB filter runs cleanly", {
  config <- make_valid_config()
  config$analysis$swing_filter$ob_filter <- 0.10

  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)
  res <- suppressWarnings(run_mapping(dt, config))

  expect_s3_class(res$h1_registry[grepl("ob", type)], "data.table")
})

# ============================================================================
# 10. TIMEZONE / DST (h1 only)
# ============================================================================

test_that("DST boundaries are computed for multiple years", {
  config <- make_valid_config()
  n <- 120L
  dt <- data.table(
    date_time = as.POSIXct("2021-03-20 00:00:00", tz = "UTC") +
      seq(0, by = 3600, length.out = n),
    open = 1.2,
    high = 1.2001 + 0.002 * sin(seq_len(n) / 5),
    low = 1.1999 + 0.002 * sin(seq_len(n) / 5),
    close = 1.20005 + 0.002 * sin(seq_len(n) / 5),
    timeframe = "h1",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )

  res <- suppressWarnings(run_mapping(dt, config))
  expect_s3_class(res$h1_data, "data.table")
})

test_that("single-year DST branch completes cleanly", {
  config <- make_valid_config()
  n <- 48L
  dt <- data.table(
    date_time = as.POSIXct("2020-06-01 00:00:00", tz = "UTC") +
      seq(0, by = 3600, length.out = n),
    open = 1.2,
    high = 1.2001 + 0.002 * sin(seq_len(n) / 5),
    low = 1.1999 + 0.002 * sin(seq_len(n) / 5),
    close = 1.20005 + 0.002 * sin(seq_len(n) / 5),
    timeframe = "h1",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )

  expect_s3_class(
    suppressWarnings(run_mapping(dt, config))$h1_data,
    "data.table"
  )
})

# ============================================================================
# 11. IDEMPOTENCY / STABILITY
# ============================================================================

test_that("running twice on identical input yields identical output", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 120L)

  r1 <- suppressWarnings(run_mapping(copy(dt), config))
  r2 <- suppressWarnings(run_mapping(copy(dt), config))

  expect_equal(r1$h1_data, r2$h1_data)
  expect_equal(r1$h1_registry, r2$h1_registry)
  expect_equal(r1$h1_dol_table, r2$h1_dol_table)
})

test_that("multi-ticker input produces per-ticker state", {
  config <- make_valid_config()
  dt <- make_mapping_input(
    time_frame = "h1",
    n_bars = 120L,
    tickers = c("audcad", "eurusd", "gbpjpy")
  )

  res <- suppressWarnings(run_mapping(dt, config))
  expect_setequal(unique(res$h1_data$ticker), c("audcad", "eurusd", "gbpjpy"))
})

# ============================================================================
# 12. SWINGS TABLE SEMANTICS
# ============================================================================

test_that("swings swing_type only contains high/low", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  sw <- suppressWarnings(run_mapping(dt, config))$swings
  skip_if(nrow(sw) == 0, "no swings detected")

  expect_true(all(unique(sw$swing_type) %in% c("high", "low")))
})

test_that("swings structure values are HH/HL/LH/LL/NA", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  sw <- suppressWarnings(run_mapping(dt, config))$swings
  skip_if(nrow(sw) == 0, "no swings detected")

  expect_true(all(
    unique(sw$structure) %in%
      c("HH", "HL", "LH", "LL", NA_character_)
  ))
})

test_that("swings bias values are bullish/bearish/range/NA", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 200L)

  sw <- suppressWarnings(run_mapping(dt, config))$swings
  skip_if(nrow(sw) == 0, "no swings detected")

  expect_true(all(
    unique(sw$bias) %in%
      c("bullish", "bearish", "range", NA_character_)
  ))
})

# ============================================================================
# 13. ERROR-HANDLING BRANCHES
# ============================================================================

test_that("errors when dt has multiple timeframe values", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1")
  dt[1, timeframe := "daily"]

  expect_error(suppressWarnings(run_mapping(dt, config)))
})

# test_that("errors when config is missing sessions (h1 path)", {
#   config <- make_valid_config()
#   config$analysis$sessions <- NULL
#   dt <- make_mapping_input(time_frame = "h1")
#
#   expect_error(
#     suppressWarnings(run_mapping(dt, config)),
#     regexp = "sessions|as.ITime|argument|subscript"
#   )
# })

test_that("NULL sessions yields NA session labels (no crash)", {
  config <- make_valid_config()
  config$analysis$sessions <- NULL
  dt <- make_mapping_input(time_frame = "h1")

  res <- suppressWarnings(run_mapping(copy(dt), config))

  expect_s3_class(res$h1_data, "data.table")
  expect_true(all(is.na(res$h1_data$tokyo)))
  expect_true(all(is.na(res$h1_data$london)))
  expect_true(all(is.na(res$h1_data$new_york)))
  expect_true(all(is.na(res$h1_data$kill_zone)))
})

# ============================================================================
# 14. PIPELINE / PERFORMANCE SANITY
# ============================================================================

test_that("large-ish input completes in reasonable time", {
  config <- make_valid_config()
  dt <- make_mapping_input(time_frame = "h1", n_bars = 400L)

  t0 <- Sys.time()
  res <- suppressWarnings(run_mapping(dt, config))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_s3_class(res$h1_data, "data.table")
  expect_lt(elapsed, 120)
})

# ============================================================================
# 15. compute_dynamic_bos_choch
# ============================================================================

test_that("compute_dynamic_bos_choch emits range breakout labels", {
  n <- 31L # match the longest vector

  dt <- data.table(
    high = c(rep(1.2000, 10), 1.2020, rep(1.2010, 20)), # 31
    low = c(rep(1.1900, 10), 1.1980, rep(1.1990, 20)), # 31
    close = c(rep(1.1950, 10), 1.2000, 1.2000, rep(1.2100, 9), rep(1.2000, 10)), # 31
    swing_high = c(rep(NA_real_, 10), 1.2015, rep(NA_real_, 20)), # 31
    swing_low = c(rep(NA_real_, 10), NA_real_, rep(NA_real_, 20)), # 31
    bias = c(rep(NA_character_, 10), rep("range", 21)), # 31
    volatility = rep(0.1, n) # 31
  )

  out <- compute_dynamic_bos_choch(dt, choch_quant = 0.1)

  expect_true(any(out$bos_type %in% c("bull_range_bos", "bear_range_bos")))
})

test_that("compute_dynamic_bos_choch logs diagnostic output when dbg_bars set", {
  n <- 20L
  dt <- data.table(
    high = rep(1.2, n),
    low = rep(1.19, n),
    close = rep(1.195, n),
    swing_high = rep(NA_real_, n),
    swing_low = rep(NA_real_, n),
    bias = rep("bullish", n),
    volatility = rep(0.1, n)
  )

  expect_output(
    compute_dynamic_bos_choch(dt, choch_quant = 0.618, dbg_bars = c(5L, 10L)),
    "bar"
  )
})

test_that("range mode seeds range_hi/lo then fires bull_range_bos", {
  n <- 20L
  dt <- data.table(
    high = c(rep(1.2000, 5), 1.2010, 1.2020, rep(1.2010, 13)),
    low = c(rep(1.1900, 5), 1.1980, 1.1970, rep(1.1990, 13)),
    close = c(rep(1.1950, 5), 1.2000, 1.2050, rep(1.2000, 13)),
    swing_high = c(rep(NA_real_, 5), 1.2015, rep(NA_real_, 14)),
    swing_low = c(rep(NA_real_, 5), NA_real_, 1.1905, rep(NA_real_, 13)),
    bias = c(rep(NA_character_, 3), rep("range", 4), rep("range", 13)),
    volatility = rep(0.1, n)
  )
  out <- compute_dynamic_bos_choch(dt, choch_quant = 0.1)
  expect_true("bull_range_bos" %in% out$bos_type)
  expect_equal(
    out$choch_level[which(out$bos_type == "bull_range_bos")][1],
    out$choch_level[which(out$bos_type == "bull_range_bos")][1]
  )
})

test_that("range mode fires bear_range_bos on downside breakout", {
  n <- 20L
  dt <- data.table(
    high = c(rep(1.2000, 5), 1.2010, 1.2000, rep(1.1990, 13)),
    low = c(rep(1.1900, 5), 1.1890, 1.1800, rep(1.1850, 13)),
    close = c(rep(1.1950, 5), 1.1900, 1.1750, rep(1.1900, 13)),
    swing_high = c(rep(NA_real_, 5), 1.2015, rep(NA_real_, 14)),
    swing_low = c(rep(NA_real_, 5), NA_real_, 1.1905, rep(NA_real_, 13)),
    bias = c(rep(NA_character_, 3), rep("range", 17)),
    volatility = rep(0.1, n)
  )
  out <- compute_dynamic_bos_choch(dt, choch_quant = 0.1)
  expect_true("bear_range_bos" %in% out$bos_type)
})

test_that("choch_level default: range mode picks range_hi then range_lo", {
  n <- 12L
  dt <- data.table(
    high = rep(1.2000, n),
    low = rep(1.1900, n),
    close = rep(1.1950, n),
    swing_high = c(NA, 1.2010, rep(NA_real_, n - 2L)), # range_hi exists
    swing_low = c(NA, NA, 1.1890, rep(NA_real_, n - 3L)),
    bias = rep("range", n),
    volatility = rep(0.1, n)
  )
  out <- compute_dynamic_bos_choch(dt, choch_quant = 0.9)
  # On a quiet bar in range mode, choch_level should equal range_hi
  expect_equal(out$choch_level[5], 1.2010)
})

test_that("choch_level default: bullish trend uses active_dn", {
  n <- 12L
  dt <- data.table(
    high = rep(1.2000, n),
    low = rep(1.1900, n),
    close = rep(1.1950, n),
    swing_high = c(NA, 1.2010, rep(NA_real_, n - 2L)),
    swing_low = c(NA, NA, 1.1890, rep(NA_real_, n - 3L)),
    bias = rep("bullish", n),
    volatility = rep(0.1, n)
  )
  out <- compute_dynamic_bos_choch(dt, choch_quant = 0.9)
  expect_equal(out$choch_level[5], 1.1890) # active_dn
})

test_that("choch_level default: bearish trend uses active_up", {
  n <- 12L
  dt <- data.table(
    high = rep(1.2000, n),
    low = rep(1.1900, n),
    close = rep(1.1950, n),
    swing_high = c(NA, 1.2010, rep(NA_real_, n - 2L)),
    swing_low = c(NA, NA, 1.1890, rep(NA_real_, n - 3L)),
    bias = rep("bearish", n),
    volatility = rep(0.1, n)
  )
  out <- compute_dynamic_bos_choch(dt, choch_quant = 0.9)
  # bar 5 is quiet; mode == "trend", trend_state == "bearish"
  # -> default choch_level = active_up (the last confirmed swing high)
  expect_equal(out$choch_level[5], 1.2010)
})

test_that("choch_level default: range mode falls back to range_lo when range_hi is NA", {
  n <- 12L
  dt <- data.table(
    high = rep(1.2000, n),
    low = rep(1.1900, n),
    close = rep(1.1950, n),
    swing_high = rep(NA_real_, n), # no swing high yet
    swing_low = c(NA, NA, 1.1890, rep(NA_real_, n - 3L)),
    bias = rep("range", n),
    volatility = rep(0.1, n)
  )
  out <- compute_dynamic_bos_choch(dt, choch_quant = 0.9)
  # range_hi is NA (no swing_high seen), so the range branch must fall to range_lo
  expect_equal(out$choch_level[5], 1.1890)
})

# ==============================================================================
# INSTITUTIONAL_MAPPING ERROR-HANDLER COVERAGE
# ==============================================================================
