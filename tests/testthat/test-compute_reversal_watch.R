# tests/testthat/test-compute_reversal_watch.R

# ============================================================================
# Fixture builders
# ============================================================================

make_h1_data <- function(
  tickers = "eurusd",
  asset_class = "Currency_Pairs",
  start = as.POSIXct("2023-11-01 00:00:00", tz = "UTC"),
  n = 3000
) {
  rbindlist(lapply(tickers, function(tk) {
    data.table(
      date_time = start + hours(seq_len(n) - 1),
      open = 1.10,
      high = 1.11,
      low = 1.09,
      close = 1.10,
      ticker = tk,
      asset_class = asset_class,
      h1_swing_high = NA_real_,
      h1_swing_low = NA_real_,
      h1_choch_level = NA_real_,
      h1_choch_type = "no_choch",
      h1_bias = NA_character_
    )
  }))
}

make_weekly_data <- function(
  tickers = "eurusd",
  asset_class = "Currency_Pairs",
  start = as.POSIXct("2023-12-01 00:00:00", tz = "UTC"),
  n = 10
) {
  rbindlist(lapply(tickers, function(tk) {
    data.table(
      date_time = start + weeks(seq_len(n) - 1),
      open = 1.10,
      high = 1.15,
      low = 1.05,
      close = 1.10,
      ticker = tk,
      asset_class = asset_class,
      weekly_swing_high = NA_real_,
      weekly_swing_low = NA_real_
    )
  }))
}

make_daily_data <- function(
  tickers = "eurusd",
  asset_class = "Currency_Pairs",
  start = as.POSIXct("2023-12-04 00:00:00", tz = "UTC"),
  n = 40
) {
  rbindlist(lapply(tickers, function(tk) {
    data.table(
      date_time = start + days(seq_len(n) - 1),
      open = 1.10,
      high = 1.15,
      low = 1.05,
      close = 1.10,
      ticker = tk,
      asset_class = asset_class,
      daily_swing_high = NA_real_,
      daily_swing_low = NA_real_
    )
  }))
}

make_zone <- function(
  tickers = "eurusd",
  asset_class = "Currency_Pairs",
  formed_at = as.POSIXct("2023-11-01 00:00:00", tz = "UTC"),
  invalidated_at = as.POSIXct("2099-01-01 00:00:00", tz = "UTC"),
  zone_type = "bear_fvg",
  bot = 1.10,
  top = 1.16
) {
  rbindlist(lapply(tickers, function(tk) {
    data.table(
      formed_at = formed_at,
      type_id = 1L,
      type = zone_type,
      type_zone_bot_value = bot,
      type_zone_top_value = top,
      critical_value = bot,
      invalid_rule = "close",
      mitig_rule = "high",
      rel_op = " > ",
      invalidated_at = invalidated_at,
      mit_swing_at = invalidated_at,
      mit_zone_prox_at = invalidated_at,
      mit_zone_dist_at = invalidated_at,
      formed_bias = "bullish",
      formed_bias_id = 1L,
      time_frame = "weekly",
      ticker = tk,
      asset_class = asset_class
    )
  }))
}

make_mutated_list <- function(tf, data, registry) {
  setNames(
    list(data, registry),
    c(paste0(tf, "_data"), paste0(tf, "_registry"))
  )
}

h1_index_on_or_after <- function(h1, iso_date) {
  target <- as.POSIXct(iso_date, tz = "UTC")
  idx <- which(h1$date_time >= target)[1]
  stopifnot(!is.na(idx))
  idx
}

make_inputs <- function(
  tickers = "eurusd",
  weekly_swings = TRUE,
  daily_swings = TRUE,
  weekly_zone = "bear",
  daily_zone = "bear",
  h1_choch = "bear"
) {
  asset_class <- "Currency_Pairs"

  h1 <- make_h1_data(tickers, asset_class)

  wk <- make_weekly_data(tickers, asset_class, n = 10)
  if (weekly_swings) {
    wk[1, weekly_swing_high := 1.12]
    wk[4, weekly_swing_low := 1.06]
  }

  dy <- make_daily_data(tickers, asset_class, n = 40)
  if (daily_swings) {
    dy[2, daily_swing_high := 1.12]
    dy[5, daily_swing_low := 1.06]
  }

  wk_reg <- switch(
    weekly_zone,
    "bear" = rbind(
      make_zone(
        tickers,
        asset_class,
        zone_type = "bear_fvg",
        bot = 1.10,
        top = 1.16
      ),
      make_zone(
        tickers,
        asset_class,
        zone_type = "bull_fvg",
        bot = 1.04,
        top = 1.08
      )
    ),
    "bull" = make_zone(
      tickers,
      asset_class,
      zone_type = "bull_fvg",
      bot = 1.04,
      top = 1.16
    ),
    "none" = make_zone(tickers, asset_class)[0]
  )

  dy_reg <- switch(
    daily_zone,
    "bear" = rbind(
      make_zone(
        tickers,
        asset_class,
        zone_type = "bear_fvg",
        bot = 1.10,
        top = 1.16
      ),
      make_zone(
        tickers,
        asset_class,
        zone_type = "bull_fvg",
        bot = 1.04,
        top = 1.08
      )
    ),
    "bull" = make_zone(
      tickers,
      asset_class,
      zone_type = "bull_fvg",
      bot = 1.04,
      top = 1.16
    ),
    "none" = make_zone(tickers, asset_class)[0]
  )
  if (nrow(dy_reg) > 0) {
    dy_reg[, time_frame := "daily"]
  }

  low_ix1 <- h1_index_on_or_after(h1, "2023-12-26 12:00:00")
  high_ix1 <- h1_index_on_or_after(h1, "2023-12-27 12:00:00")
  low_ix2 <- h1_index_on_or_after(h1, "2023-12-28 12:00:00")
  high_ix2 <- h1_index_on_or_after(h1, "2023-12-29 12:00:00")

  h1[low_ix1, `:=`(h1_swing_low = 1.08, h1_bias = "bullish")]
  h1[high_ix1, `:=`(h1_swing_high = 1.12, h1_bias = "bearish")]
  h1[low_ix2, `:=`(h1_swing_low = 1.09, h1_bias = "bullish")]
  h1[high_ix2, `:=`(h1_swing_high = 1.11, h1_bias = "bearish")]

  agg_idx <- h1_index_on_or_after(h1, "2023-12-20 00:00:00")
  h1[(agg_idx):(agg_idx + 10), close := 1.09]

  choch_idx <- h1_index_on_or_after(h1, "2023-12-30 00:00:00")
  if (h1_choch == "bear") {
    h1[choch_idx, `:=`(h1_choch_level = 1.10, h1_choch_type = "bear_choch")]
  } else if (h1_choch == "bull") {
    h1[choch_idx, `:=`(h1_choch_level = 1.10, h1_choch_type = "bull_choch")]
  }

  list(
    weekly_mutated = make_mutated_list("weekly", wk, wk_reg),
    daily_mutated = make_mutated_list("daily", dy, dy_reg),
    h4_mutated = list(),
    h1_mutated = make_mutated_list("h1", h1, data.table())
  )
}

run <- function(inputs) {
  compute_reversal_watch(
    inputs$weekly_mutated,
    inputs$daily_mutated,
    inputs$h4_mutated,
    inputs$h1_mutated
  )
}

# ============================================================================
# 1. Structure & schema
# ============================================================================

test_that("returns the expected list structure", {
  res <- run(make_inputs())
  expect_type(res, "list")
  expect_named(res, c("weekly_final_dt", "daily_final_dt"))
  expect_true(data.table::is.data.table(res$weekly_final_dt))
  expect_true(data.table::is.data.table(res$daily_final_dt))
})

test_that("a matched scenario yields the expected schema", {
  res <- run(make_inputs())

  expect_gt(
    nrow(res$weekly_final_dt),
    0,
    label = "weekly_final_dt should have rows in default fixture"
  )
  expect_gt(
    nrow(res$daily_final_dt),
    0,
    label = "daily_final_dt should have rows in default fixture"
  )

  expected <- c(
    "ticker",
    "asset_class",
    "timeframe",
    "swing_time",
    "swing_type",
    "swing_price",
    "setup_type",
    "zone_timeframes",
    "zone_types",
    "zone_bot",
    "zone_top",
    "aggressive_close_time",
    "aggressive_close_price",
    "h1_choch_time",
    "h1_choch_price",
    "h1_choch_dir",
    "confluence",
    "first_h1_opposite_time",
    "first_h1_opposite_price"
  )
  expect_true(all(expected %in% names(res$weekly_final_dt)))
  expect_true(all(expected %in% names(res$daily_final_dt)))
})

test_that("output is sorted by ticker, swing_time, swing_type", {
  tickers <- c("audcad", "eurusd", "gbpjpy")
  res <- run(make_inputs(tickers = tickers))
  for (tf in c("weekly_final_dt", "daily_final_dt")) {
    dt <- res[[tf]]
    if (nrow(dt) > 1) {
      ord <- order(dt$ticker, dt$swing_time, dt$swing_type)
      expect_equal(dt$ticker, dt$ticker[ord])
      expect_equal(dt$swing_time, dt$swing_time[ord])
      expect_equal(dt$swing_type, dt$swing_type[ord])
    }
  }
})

# ============================================================================
# 2. Zone / naked classification
# ============================================================================

test_that("weekly swing high inside bear zone is classified zone_swing", {
  res <- run(make_inputs())
  hi <- res$weekly_final_dt[
    swing_type == "high" & setup_type == "zone_swing"
  ]
  expect_gt(nrow(hi), 0)
  expect_true(all(hi$zone_bot <= hi$swing_price))
  expect_true(all(hi$swing_price <= hi$zone_top))
})

test_that("swing outside every zone is naked_swing", {
  inputs <- make_inputs(weekly_zone = "none", daily_zone = "none")
  h1 <- inputs$h1_mutated$h1_data
  win_lo <- h1_index_on_or_after(h1, "2023-12-02 00:00:00")
  h1[(win_lo):(win_lo + 10), close := 1.05]
  inputs$h1_mutated$h1_data <- h1
  res <- run(inputs)
  naked <- res$weekly_final_dt[setup_type == "naked_swing"]
  expect_gt(nrow(naked), 0)
  expect_true(all(naked$confluence == "naked"))
})

test_that("bull swing-high never matches bull zone (direction filter)", {
  inputs <- make_inputs(weekly_zone = "bull", daily_zone = "none")
  res <- run(inputs)
  hi_zone <- res$weekly_final_dt[
    swing_type == "high" & setup_type == "zone_swing"
  ]
  expect_equal(nrow(hi_zone), 0)
})

test_that("bear swing-low never matches bear zone (direction filter)", {
  inputs <- make_inputs(weekly_zone = "bear", daily_zone = "none")
  inputs$weekly_mutated$weekly_registry <-
    make_zone(zone_type = "bear_fvg", bot = 1.04, top = 1.16)
  res <- run(inputs)
  lo_zone <- res$weekly_final_dt[
    swing_type == "low" & setup_type == "zone_swing"
  ]
  expect_equal(nrow(lo_zone), 0)
})

test_that("zones inactive at swing_time are treated as naked", {
  inputs <- make_inputs()
  inputs$weekly_mutated$weekly_registry[,
    invalidated_at := as.POSIXct("2023-11-30 00:00:00", tz = "UTC")
  ]
  inputs$daily_mutated$daily_registry[,
    invalidated_at := as.POSIXct("2023-11-30 00:00:00", tz = "UTC")
  ]
  res <- run(inputs)
  expect_equal(nrow(res$weekly_final_dt[setup_type == "zone_swing"]), 0)
  if (nrow(res$weekly_final_dt) > 0) {
    expect_true(all(res$weekly_final_dt$setup_type == "naked_swing"))
  }
})

test_that("NA invalidated_at is treated as still-active zone", {
  inputs <- make_inputs()
  na_time <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  inputs$weekly_mutated$weekly_registry[, invalidated_at := na_time]
  inputs$daily_mutated$daily_registry[, invalidated_at := na_time]
  expect_silent(res <- run(inputs))
  expect_true(data.table::is.data.table(res$weekly_final_dt))
})

# ============================================================================
# 3. Aggressive close & CHoCH matching
# ============================================================================

test_that("bearish CHoCH after bear aggressive close matches swing high", {
  res <- run(make_inputs(h1_choch = "bear"))
  hi <- res$weekly_final_dt[swing_type == "high"]
  expect_gt(nrow(hi), 0)
  expect_true(all(hi$h1_choch_dir == "bearish"))
  expect_true(all(hi$h1_choch_time >= hi$aggressive_close_time))
})

test_that("bullish CHoCH after bull aggressive close matches swing low", {
  inputs <- make_inputs(
    weekly_zone = "bear",
    daily_zone = "bear",
    h1_choch = "bull"
  )
  h1 <- inputs$h1_mutated$h1_data
  win_lo <- h1_index_on_or_after(h1, "2023-12-23 00:00:00")
  h1[(win_lo):(win_lo + 10), close := 1.09]
  inputs$h1_mutated$h1_data <- h1
  res <- run(inputs)
  lo <- res$weekly_final_dt[swing_type == "low"]
  expect_gt(nrow(lo), 0)
  expect_true(all(lo$h1_choch_dir == "bullish"))
  expect_true(all(lo$h1_choch_time >= lo$aggressive_close_time))
})

test_that("no CHoCH event -> no rows survive", {
  inputs <- make_inputs(h1_choch = "none")
  res <- run(inputs)
  expect_equal(nrow(res$weekly_final_dt), 0)
  expect_equal(nrow(res$daily_final_dt), 0)
})

test_that("bull CHoCH never matches bear aggressive close (direction filter)", {
  inputs <- make_inputs(h1_choch = "bull")
  res <- run(inputs)
  hi <- res$weekly_final_dt[swing_type == "high"]
  expect_equal(nrow(hi), 0)
})

test_that("range CHoCH types are recognized", {
  inputs <- make_inputs(h1_choch = "none")
  choch_idx <- h1_index_on_or_after(
    inputs$h1_mutated$h1_data,
    "2023-12-30 00:00:00"
  )
  inputs$h1_mutated$h1_data[
    choch_idx,
    `:=`(
      h1_choch_level = 1.10,
      h1_choch_type = "bear_range_choch"
    )
  ]
  res <- run(inputs)
  hi <- res$weekly_final_dt[swing_type == "high"]
  expect_gt(nrow(hi), 0)
  expect_true(all(hi$h1_choch_dir == "bearish"))
})

test_that("aggressive_close_time strictly after swing_time for every row", {
  res <- run(make_inputs())
  for (tf in c("weekly_final_dt", "daily_final_dt")) {
    dt <- res[[tf]]
    if (nrow(dt) > 0) {
      expect_true(all(dt$aggressive_close_time > dt$swing_time))
    }
  }
})

test_that("h1_choch_time >= aggressive_close_time for every row", {
  res <- run(make_inputs())
  for (tf in c("weekly_final_dt", "daily_final_dt")) {
    dt <- res[[tf]]
    if (nrow(dt) > 0) {
      expect_true(all(dt$h1_choch_time >= dt$aggressive_close_time))
    }
  }
})

# ============================================================================
# 4. First-opposite H1 swing augmentation
# ============================================================================

test_that("first_h1_opposite fields are attached for swing highs", {
  res <- run(make_inputs(h1_choch = "bear"))
  hi <- res$weekly_final_dt[swing_type == "high"]
  expect_gt(nrow(hi), 0)
  expect_true(any(!is.na(hi$first_h1_opposite_time)))
  expect_true(all(
    hi$first_h1_opposite_time > hi$swing_time,
    na.rm = TRUE
  ))
})

test_that("first_h1_opposite fields are attached for swing lows", {
  inputs <- make_inputs(h1_choch = "bull")
  h1 <- inputs$h1_mutated$h1_data
  win_lo <- h1_index_on_or_after(h1, "2023-12-23 00:00:00")
  h1[(win_lo):(win_lo + 10), close := 1.09]
  inputs$h1_mutated$h1_data <- h1
  res <- run(inputs)
  lo <- res$weekly_final_dt[swing_type == "low"]
  expect_gt(nrow(lo), 0)
  expect_true(any(!is.na(lo$first_h1_opposite_time)))
  expect_true(all(
    lo$first_h1_opposite_time > lo$swing_time,
    na.rm = TRUE
  ))
})

# ============================================================================
# 5. Weekly + daily both produce rows
# ============================================================================

test_that("daily swing points also produce output rows", {
  res <- run(make_inputs())
  expect_gt(nrow(res$daily_final_dt), 0)
  expect_true(all(res$daily_final_dt$timeframe == "daily"))
})

test_that("weekly rows are labelled timeframe == 'weekly'", {
  res <- run(make_inputs())
  expect_true(all(res$weekly_final_dt$timeframe == "weekly"))
})

# ============================================================================
# 6. Empty-input paths
# ============================================================================

test_that("no weekly swings -> empty weekly output", {
  res <- run(make_inputs(weekly_swings = FALSE))
  expect_equal(nrow(res$weekly_final_dt), 0)
})

test_that("no daily swings -> empty daily output", {
  res <- run(make_inputs(daily_swings = FALSE))
  expect_equal(nrow(res$daily_final_dt), 0)
})

test_that("empty registries yield empty final outputs", {
  inputs <- make_inputs()
  inputs$weekly_mutated$weekly_registry <- make_zone()[0]
  inputs$daily_mutated$daily_registry <- make_zone()[0]
  res <- run(inputs)
  expect_true(data.table::is.data.table(res$weekly_final_dt))
  expect_true(data.table::is.data.table(res$daily_final_dt))
})

test_that("h4_mutated is accepted but unused", {
  inputs <- make_inputs()
  inputs$h4_mutated <- list(h4_data = data.table(), h4_registry = data.table())
  expect_silent(res <- run(inputs))
  expect_type(res, "list")
})

# ============================================================================
# 7. Overlapping-zone confluence aggregation
# ============================================================================

test_that("overlapping zones aggregate confluence string with commas", {
  inputs <- make_inputs()
  extra <- make_zone(zone_type = "bear_ob", bot = 1.10, top = 1.16)
  inputs$weekly_mutated$weekly_registry <-
    rbind(inputs$weekly_mutated$weekly_registry, extra)
  res <- run(inputs)
  zs <- res$weekly_final_dt[setup_type == "zone_swing"]
  expect_gt(nrow(zs), 0)
  expect_true(any(grepl(",", zs$confluence)))
})

# ============================================================================
# 8. Schema invariants
# ============================================================================

test_that("zone_bot <= zone_top for every row", {
  res <- run(make_inputs())
  for (tf in c("weekly_final_dt", "daily_final_dt")) {
    dt <- res[[tf]]
    if (nrow(dt) > 0) {
      expect_true(all(dt$zone_bot <= dt$zone_top, na.rm = TRUE))
    }
  }
})

test_that("setup_type is zone_swing or naked_swing", {
  res <- run(make_inputs())
  for (tf in c("weekly_final_dt", "daily_final_dt")) {
    dt <- res[[tf]]
    if (nrow(dt) > 0) {
      expect_true(all(dt$setup_type %in% c("zone_swing", "naked_swing")))
    }
  }
})

test_that("swing_type is high or low", {
  res <- run(make_inputs())
  for (tf in c("weekly_final_dt", "daily_final_dt")) {
    dt <- res[[tf]]
    if (nrow(dt) > 0) {
      expect_true(all(dt$swing_type %in% c("high", "low")))
    }
  }
})

test_that("confluence is never NA on zone_swing rows", {
  res <- run(make_inputs())
  zs <- res$weekly_final_dt[setup_type == "zone_swing"]
  expect_gt(nrow(zs), 0)
  expect_false(any(is.na(zs$confluence)))
})

# ============================================================================
# 9. Error paths
# ============================================================================

test_that("missing h1_data raises an error", {
  inputs <- make_inputs()
  inputs$h1_mutated <- list(h1_registry = inputs$h1_mutated$h1_registry)
  expect_error(run(inputs))
})

test_that("missing h1_registry does NOT error (currently unused)", {
  inputs <- make_inputs()
  inputs$h1_mutated <- list(h1_data = inputs$h1_mutated$h1_data)
  res <- expect_silent(run(inputs))
  expect_named(res, c("weekly_final_dt", "daily_final_dt"))
  expect_true(data.table::is.data.table(res$weekly_final_dt))
  expect_true(data.table::is.data.table(res$daily_final_dt))
})

test_that("missing daily_data raises an error", {
  inputs <- make_inputs()
  inputs$daily_mutated <- list(
    daily_registry = inputs$daily_mutated$daily_registry
  )
  expect_error(run(inputs))
})

test_that("missing weekly_data raises an error", {
  inputs <- make_inputs()
  inputs$weekly_mutated <- list(
    weekly_registry = inputs$weekly_mutated$weekly_registry
  )
  expect_error(run(inputs))
})

test_that("missing weekly_swing_high column raises an error", {
  inputs <- make_inputs()
  inputs$weekly_mutated$weekly_data[, weekly_swing_high := NULL]
  expect_error(run(inputs))
})

test_that("missing daily_swing_low column raises an error", {
  inputs <- make_inputs()
  inputs$daily_mutated$daily_data[, daily_swing_low := NULL]
  expect_error(run(inputs))
})

test_that("missing h1_choch_level column raises an error", {
  inputs <- make_inputs()
  inputs$h1_mutated$h1_data[, h1_choch_level := NULL]
  expect_error(run(inputs))
})

test_that("missing h1_close column raises an error", {
  inputs <- make_inputs()
  inputs$h1_mutated$h1_data[, close := NULL]
  expect_error(run(inputs))
})

test_that("missing 'type' column on weekly registry raises an error", {
  inputs <- make_inputs()
  inputs$weekly_mutated$weekly_registry[, type := NULL]
  expect_error(run(inputs))
})

test_that("missing 'invalidated_at' on daily registry raises an error", {
  inputs <- make_inputs()
  inputs$daily_mutated$daily_registry[, invalidated_at := NULL]
  expect_error(run(inputs))
})

test_that("NULL weekly_mutated raises an error", {
  inputs <- make_inputs()
  inputs$weekly_mutated <- NULL
  expect_error(run(inputs))
})

# ============================================================================
# 10. EXTRACTION-BLOCK ERROR HANDLERS
# ============================================================================
# Only the FIRST slot in each pair is reachable in isolation: the function
# aborts on h1_data before ever touching h1_registry, and likewise for the
# daily/weekly pairs. The `*_registry` handlers (2511-2512, 2573-2574, and
# their weekly equivalents) are only reachable if h1_data succeeds AND the
# registry slot throws - not possible without a custom `$` method. They are
# marked # nocov in 01_mutate.R.

test_that("h1_data extraction handler fires when h1_mutated is not a list", {
  inputs <- make_inputs()
  inputs$h1_mutated <- 42L
  expect_error(run(inputs), regexp = "h1_data|atomic|\\$")
})

test_that("daily_data extraction handler fires when daily_mutated is not a list", {
  inputs <- make_inputs()
  inputs$daily_mutated <- 42L
  expect_error(run(inputs), regexp = "daily_data|atomic|\\$")
})

test_that("weekly_data extraction handler fires when weekly_mutated is not a list", {
  inputs <- make_inputs()
  inputs$weekly_mutated <- 42L
  expect_error(run(inputs), regexp = "weekly_data|atomic|\\$")
})

# ============================================================================
# 11. PREPARATION-BLOCK ERROR HANDLERS
# ============================================================================
# Reachability audit for compute_reversal_watch's prep-block handlers:
#
# Section 2 runs stopifnot(all(required_* %in% names(...))) BEFORE any prep
# block, so removing/renaming a validated column dies at Section 2.
#
# Type-poisoning a validated column (as.character, as.numeric, all-NA window)
# is silently absorbed because:
#   * as.POSIXct(character|numeric, tz="UTC") coerces in R 4.3+ without error;
#   * data.table's non-equi join coerces mismatched types without error;
#   * which.max(NA_real_) returns integer(0), and a 0-length .EACHI result
#     becomes a silent 0-row group, not an error.
#
# Every prep-block handler is therefore unreachable from a valid public API
# without a code fault. They are marked # nocov in 01_mutate.R:
#   * weekly swing-prep handler   (~01_mutate.R:2573-2574)
#   * daily swing-prep handler    (~01_mutate.R:2868-2869)
#   * zone-prep handler           (~01_mutate.R:2917-2918)
#   * h1_close-prep handler       (~01_mutate.R:2933-2934)
#   * CHoCH-event-prep handler    (~01_mutate.R:2962-2963)
#   * matches-overlap handler     (~01_mutate.R:3054-3055)
#
# The reachable test here documents the Section-2 validation gate itself,
# which is what a caller will actually hit when a required column is missing.

test_that("zone-prep validation gate rejects a registry missing a zone column", {
  inputs <- make_inputs()
  inputs$weekly_mutated$weekly_registry[, type_zone_bot_value := NULL]
  expect_error(
    run(inputs),
    regexp = "required_registry|is not TRUE|zone|not found|object"
  )
})

# ============================================================================
# 12. H1 SWING-EVENT PREP + FALLBACK-BRANCH COVERAGE
# ============================================================================

test_that("h1_swing_events prep handler fires when h1_swing_high is missing", {
  inputs <- make_inputs()
  inputs$h1_mutated$h1_data[, h1_swing_high := NULL]
  # Section 2's required_h1 does NOT include h1_swing_high, so the prep
  # block is reached and its handler fires on the missing column.
  expect_error(run(inputs), regexp = "H1 swing|swing_high|Failed|object")
})

test_that("weekly swing join hits the .N == 0 fallback", {
  inputs <- make_inputs()
  inputs$weekly_mutated$weekly_data[
    !is.na(weekly_swing_high),
    date_time := as.POSIXct("2100-01-01", tz = "UTC")
  ]
  res <- expect_silent(run(inputs))
  expect_true(data.table::is.data.table(res$weekly_final_dt))
  expect_true(
    all(is.na(res$weekly_final_dt[swing_type == "high"]$swing_price)) ||
      nrow(res$weekly_final_dt[swing_type == "high"]) == 0
  )
})

test_that("daily swing join hits the .N == 0 fallback", {
  inputs <- make_inputs()
  inputs$daily_mutated$daily_data[
    !is.na(daily_swing_low),
    date_time := as.POSIXct("2100-01-01", tz = "UTC")
  ]
  res <- expect_silent(run(inputs))
  expect_true(data.table::is.data.table(res$daily_final_dt))
})

# ============================================================================
# 13. AGGRESSIVE-CLOSE HANDLER
# ============================================================================
# Kept as-is: this handler IS reachable because close is only validated for
# presence, and a character-typed close flows through extraction, swing prep,
# zone prep, and h1_close prep cleanly, then breaks the numeric comparison
# inside the aggressive-close block.

test_that("aggressive-close handler fires when h1 close is non-numeric", {
  inputs <- make_inputs()
  inputs$h1_mutated$h1_data[, close := as.character(close)]
  expect_error(run(inputs), regexp = "Aggressive close|close|Failed")
})
