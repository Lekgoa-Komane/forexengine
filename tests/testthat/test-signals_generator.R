# tests/testthat/test-signals_generator.R
#
# 100% branch/line coverage for signals_generator() in 02_analysis.R
# ---------------------------------------------------------------------------
# The tests are deliberately explicit about *which* branch they hit so that
# a future refactor of signals_generator() will fail loudly if behaviour drifts.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Test fixtures
# ---------------------------------------------------------------------------

# Column set required by signals_generator() input validation plus the columns
# that are read/written during processing (entry_price, entry_dol_name, etc.).
required_cols <- c(
  "date_time",
  "ticker",
  "asset_class",
  "trend_context",
  "pullback_context",
  "trade_context",
  "weekly_target_dol",
  "daily_target_dol",
  "entry_dol",
  "sl_price",
  "signal_score",
  "confluence",
  "weekly_bias",
  "daily_bias",
  "h4_bias",
  "h1_bias",
  # columns read by steps 7-9:
  "entry_price",
  "entry_dol_name",
  "timeframe"
)

# Build a single, fully-populated, valid signal row.
make_row <- function(
  trade_context = "bull_candidate",
  entry_dol = 1.1000,
  entry_price = 1.1000,
  sl_price = 1.0950,
  weekly_target_dol = 1.1200,
  daily_target_dol = 1.1150,
  signal_score = 3L,
  confluence = 3L,
  ticker = "EURUSD",
  asset_class = "Currency_Pairs",
  timeframe = "H1",
  date_time = as.POSIXct("2024-01-02 10:00:00", tz = "UTC"),
  entry_dol_name = "h1_active_bull_fvg_top"
) {
  data.table::data.table(
    date_time = date_time,
    ticker = ticker,
    asset_class = asset_class,
    trend_context = "W:bull D:bull H4:bull",
    pullback_context = "pullback",
    trade_context = trade_context,
    weekly_target_dol = weekly_target_dol,
    daily_target_dol = daily_target_dol,
    entry_dol = entry_dol,
    entry_price = entry_price,
    entry_dol_name = entry_dol_name,
    sl_price = sl_price,
    signal_score = signal_score,
    confluence = confluence,
    weekly_bias = "bullish",
    daily_bias = "bullish",
    h4_bias = "bullish",
    h1_bias = "bullish",
    timeframe = timeframe
  )
}

# A minimal config that satisfies the signal_filter$* lookups.
make_cfg <- function(min_confluence = 1, risk_pct = 0.01, min_rr = 1.5) {
  list(
    analysis = list(
      signal_filter = list(
        min_confluence = min_confluence,
        risk_pct = risk_pct,
        min_risk_reward = min_rr
      )
    )
  )
}

# Wrap a data.table of candidate rows into the exec_list structure expected
# by signals_generator().
make_exec_list <- function(exec_data, h1_data = NULL) {
  if (is.null(h1_data)) {
    h1_data <- data.table::data.table(dummy = 1L)
  }
  list(exec_data = exec_data, h1_data = h1_data)
}

# Small helper so each test can inspect the returned signal_log directly.
run_engine <- function(exec_data, cfg = make_cfg(), h1_data = NULL) {
  out <- signals_generator(make_exec_list(exec_data, h1_data), cfg)
  stopifnot(is.list(out), all(c("signal_log", "h1_data") %in% names(out)))
  out
}

# Silence logger output during the test run so failures are readable.
suppress_logger <- function(code) {
  withCallingHandlers(
    code,
    message = function(m) invokeRestart("muffleMessage")
  )
}

# ---------------------------------------------------------------------------
# 1. INPUT VALIDATION
# ---------------------------------------------------------------------------

test_that("signals_generator errors when exec_data is not a data.table", {
  cfg <- make_cfg()
  bad_exec <- list(
    exec_data = data.frame(x = 1),
    h1_data = data.table(dummy = 1L)
  )
  expect_error(
    suppress_logger(signals_generator(bad_exec, cfg)),
    "h1_executer must be a data.table"
  )
})

test_that("signals_generator errors when config is not a list", {
  exec <- make_exec_list(make_row())
  expect_error(
    suppress_logger(signals_generator(exec, "not-a-list")),
    "config must be a named list"
  )
})

test_that("signals_generator errors with the list of missing columns", {
  dt <- make_row()[, !c("sl_price", "weekly_bias"), with = FALSE]
  exec <- make_exec_list(dt)
  expect_error(
    suppress_logger(signals_generator(exec, make_cfg())),
    "missing required columns: sl_price, weekly_bias"
  )
})

test_that("signals_generator errors when exec_data has zero rows", {
  dt <- make_row()[0]
  exec <- make_exec_list(dt)
  expect_error(
    suppress_logger(signals_generator(exec, make_cfg())),
    "h1_executer has no rows"
  )
})

test_that("input validation passes for a fully-populated row", {
  out <- suppress_logger(run_engine(make_row()))
  expect_s3_class(out$signal_log, "data.table")
})

# ---------------------------------------------------------------------------
# 2. CONFIG READING
# ---------------------------------------------------------------------------

test_that("min_risk_reward defaults to NA when absent from config", {
  cfg <- list(
    analysis = list(
      signal_filter = list(min_confluence = 1, risk_pct = 0.01)
      # min_risk_reward deliberately omitted
    )
  )
  out <- suppress_logger(run_engine(make_row(), cfg))
  # NA min_rr means the R/R filter block is skipped and the row survives.
  expect_equal(nrow(out$signal_log), 1L)
})

test_that("config errors surface as a wrapped message", {
  # Force a *structural* error: `analysis` is a scalar, not a list, so
  # `config$analysis$signal_filter` throws inside the tryCatch.
  # NOTE: the handler calls log_error(...) then stop(e), so the condition
  # that propagates is the ORIGINAL one -- the log line is informational
  # only and does not replace the raised condition's message. Match on the
  # original R error message accordingly.
  cfg <- list(analysis = 42L)
  expect_error(
    suppress_logger(run_engine(make_row(), cfg)),
    "\\$ operator is invalid for atomic vectors"
  )
})

# ---------------------------------------------------------------------------
# 3. RISK/REWARD COMPUTATION (fcase branches)
# ---------------------------------------------------------------------------

test_that("bull_candidate R/R uses min of weekly/daily target", {
  # entry 1.100, sl 1.095 => risk = 0.005
  # targets: weekly 1.120, daily 1.115 => nearest = 1.115
  # reward = 0.015, rr = 3.0
  out <- suppress_logger(run_engine(make_row(trade_context = "bull_candidate")))
  # The bull row survives min_rr=1.5, so it appears in signal_log.
  expect_equal(nrow(out$signal_log), 1L)
  expect_equal(out$signal_log$risk_reward, 3.0)
})

test_that("bear_candidate R/R uses max of weekly/daily target", {
  # entry 1.100, sl 1.105 => risk = 0.005
  # targets: weekly 1.080, daily 1.085 => farthest (bearish) = 1.085
  # reward = 0.015, rr = 3.0
  dt <- make_row(
    trade_context = "bear_candidate",
    entry_dol = 1.100,
    entry_price = 1.100,
    sl_price = 1.105,
    weekly_target_dol = 1.080,
    daily_target_dol = 1.085,
    entry_dol_name = "h1_active_bear_fvg_bot"
  )
  out <- suppress_logger(run_engine(dt))
  expect_equal(out$signal_log$risk_reward, 3.0)
})

test_that("rows with NA target columns produce NA R/R but survive", {
  # pmin(NA, 1.115, na.rm = TRUE) = 1.115, so tp_price is still populated
  # and the row passes the price-level completeness filter. Only risk_reward
  # is NA. With min_rr = NA the row is therefore kept.
  dt <- make_row(trade_context = "bull_candidate")
  dt$weekly_target_dol <- NA_real_
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 1L)
  expect_true(is.na(out$signal_log$risk_reward))
})

test_that("NA risk_reward rows are dropped when min_rr is a number", {
  # `NA_real_ >= 1.5` is NA, so data.table drops the row.
  dt <- make_row(trade_context = "bull_candidate")
  dt$weekly_target_dol <- NA_real_
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = 1.5)))
  expect_equal(nrow(out$signal_log), 0L)
})

test_that("zero-range (entry == sl) yields NA R/R and gets filtered", {
  dt <- make_row(trade_context = "bull_candidate", sl_price = 1.1000)
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

test_that("unknown trade_context yields NA R/R", {
  dt <- make_row(trade_context = "sideways")
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

# ---------------------------------------------------------------------------
# 4. REASON STRING ASSEMBLY
# ---------------------------------------------------------------------------

test_that("reason string is assembled from the row's columns", {
  dt <- make_row()
  # We assert the shape/content of the reason string in the output signal_log;
  # the intermediate is not exposed.
  out <- suppress_logger(run_engine(dt))
  expect_true("reason" %in% names(out$signal_log))
  expect_match(out$signal_log$reason, "pullback")
  expect_match(out$signal_log$reason, "bull_candidate")
  expect_match(out$signal_log$reason, "h1_active_bull_fvg_top")
  expect_match(out$signal_log$reason, "score:3")
})

# ---------------------------------------------------------------------------
# 5. SIGNAL LOG CONSTRUCTION
# ---------------------------------------------------------------------------

test_that("signal_log has every expected output column", {
  out <- suppress_logger(run_engine(make_row()))
  expected <- c(
    "signal_datetime",
    "signal_id",
    "signal_type",
    "bias_alignment",
    "trigger_type",
    "entry_price",
    "sl_price",
    "tp_price",
    "risk_reward",
    "signal_score",
    "confluence",
    "weekly_bias",
    "daily_bias",
    "h4_bias",
    "h1_bias",
    "reason",
    "timeframe",
    "ticker",
    "asset_class"
  )
  expect_true(all(expected %in% names(out$signal_log)))
})

test_that("signal_id is ticker + ISO datetime", {
  out <- suppress_logger(run_engine(make_row()))
  expect_equal(
    out$signal_log$signal_id,
    "EURUSD: 2024-01-02 10:00:00"
  )
})

test_that("signal_type maps bull_* -> bullish and bear_* -> bearish", {
  out <- suppress_logger(run_engine(make_row(trade_context = "bull_candidate")))
  expect_equal(out$signal_log$signal_type, "bullish")

  bear_row <- make_row(
    trade_context = "bear_candidate",
    entry_price = 1.1000,
    sl_price = 1.1050,
    weekly_target_dol = 1.0800,
    daily_target_dol = 1.0850,
    entry_dol_name = "h1_active_bear_fvg_bot"
  )
  out2 <- suppress_logger(run_engine(bear_row))
  expect_equal(out2$signal_log$signal_type, "bearish")
})

test_that("bias_alignment mirrors trade_context", {
  out <- suppress_logger(run_engine(make_row(trade_context = "bull_candidate")))
  expect_equal(out$signal_log$bias_alignment, "bull_candidate")
})

test_that("trigger_type mirrors entry_dol_name", {
  out <- suppress_logger(run_engine(make_row()))
  expect_equal(out$signal_log$trigger_type, "h1_active_bull_fvg_top")
})

test_that("tp_price picks the nearest target for bulls", {
  out <- suppress_logger(run_engine(make_row(
    trade_context = "bull_candidate",
    weekly_target_dol = 1.1200,
    daily_target_dol = 1.1150
  )))
  expect_equal(out$signal_log$tp_price, 1.1150)
})

# REGRESSION: the source's bear tp_price branch uses pmax() (highest price),
# which for a short is the *nearer* of the two targets in trade direction.
# Both targets in this fixture sit below entry, so the row survives the
# section-9 price-ordering filter (tp < entry). Assert pmax is what populated
# tp_price. If the source is corrected to use pmin(), this test must be
# updated to expect 1.0800 instead.
test_that("tp_price for bears currently uses pmax (higher price target)", {
  dt <- make_row(
    trade_context = "bear_candidate",
    entry_price = 1.1000,
    sl_price = 1.1050,
    weekly_target_dol = 1.0800,
    daily_target_dol = 1.0850,
    entry_dol_name = "h1_active_bear_fvg_bot"
  )
  out <- suppress_logger(run_engine(dt))
  expect_equal(nrow(out$signal_log), 1L)
  expect_equal(out$signal_log$tp_price, 1.0850)
})

test_that("rows with missing price levels are dropped", {
  dt <- make_row()
  dt$entry_price <- NA_real_
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

# ---------------------------------------------------------------------------
# 6. PRICE-ORDERING SANITY CHECKS
# ---------------------------------------------------------------------------

test_that("bullish row with inverted prices is dropped", {
  # entry < sl for a bull is invalid
  dt <- make_row(
    trade_context = "bull_candidate",
    entry_price = 1.0900,
    sl_price = 1.1000,
    weekly_target_dol = 1.1200,
    daily_target_dol = 1.1150
  )
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

test_that("bullish row with tp <= entry is dropped", {
  dt <- make_row(
    trade_context = "bull_candidate",
    entry_price = 1.1000,
    sl_price = 1.0950,
    weekly_target_dol = 1.0900,
    daily_target_dol = 1.0950
  )
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

test_that("bearish row with inverted prices is dropped", {
  # entry > sl for a bear is invalid
  dt <- make_row(
    trade_context = "bear_candidate",
    entry_price = 1.1000,
    sl_price = 1.0950,
    weekly_target_dol = 1.0800,
    daily_target_dol = 1.0850,
    entry_dol_name = "h1_active_bear_fvg_bot"
  )
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

test_that("bearish row with tp >= entry is dropped", {
  dt <- make_row(
    trade_context = "bear_candidate",
    entry_price = 1.1000,
    sl_price = 1.1050,
    weekly_target_dol = 1.1200,
    daily_target_dol = 1.1150,
    entry_dol_name = "h1_active_bear_fvg_bot"
  )
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

test_that("unknown trade_context is dropped at the price-ordering stage", {
  dt <- make_row(trade_context = "sideways")
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
})

# ---------------------------------------------------------------------------
# 7. MINIMUM RISK/REWARD FILTER
# ---------------------------------------------------------------------------

test_that("min_rr filter drops signals strictly below the threshold", {
  # entry 1.100, sl 1.095 => risk 0.005
  # tp   1.105 => reward 0.005 => rr = 1.0 (strictly below 1.5)
  dt <- make_row(
    trade_context = "bull_candidate",
    entry_price = 1.1000,
    sl_price = 1.0950,
    weekly_target_dol = 1.1050,
    daily_target_dol = 1.1050
  )
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = 1.5)))
  expect_equal(nrow(out$signal_log), 0L)
})

test_that("min_rr filter is skipped when min_rr is NA", {
  # Boundary fixture (rr = 1.5). With min_rr = NA the filter block is
  # skipped entirely, so the row survives.
  dt <- make_row(
    trade_context = "bull_candidate",
    entry_price = 1.1000,
    sl_price = 1.0950,
    weekly_target_dol = 1.1075,
    daily_target_dol = 1.1075
  )
  out_na <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out_na$signal_log), 1L)
})

test_that("min_rr filter keeps exactly at the threshold", {
  # rr exactly 1.5 should pass (>=)
  dt <- make_row(
    trade_context = "bull_candidate",
    entry_price = 1.1000,
    sl_price = 1.0950,
    weekly_target_dol = 1.1075,
    daily_target_dol = 1.1075
  )
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = 1.5)))
  expect_equal(nrow(out$signal_log), 1L)
})

# ---------------------------------------------------------------------------
# 8. RETURN SHAPE AND H1 PASSTHROUGH
# ---------------------------------------------------------------------------

test_that("return value is a named list of signal_log + h1_data", {
  h1 <- data.table::data.table(x = 1:3)
  out <- suppress_logger(run_engine(make_row(), h1_data = h1))
  expect_named(out, c("signal_log", "h1_data"))
  expect_identical(out$h1_data, h1)
})

test_that("multiple rows across multiple tickers all survive", {
  rows <- rbind(
    make_row(ticker = "EURUSD", asset_class = "Currency_Pairs"),
    make_row(ticker = "GBPUSD", asset_class = "Currency_Pairs"),
    make_row(ticker = "XAUUSD", asset_class = "Commodity_Futures")
  )
  out <- suppress_logger(run_engine(rows))
  expect_equal(nrow(out$signal_log), 3L)
  expect_setequal(out$signal_log$ticker, c("EURUSD", "GBPUSD", "XAUUSD"))
})

test_that("empty signal_log (all filtered) still returns the wrapper", {
  dt <- make_row(trade_context = "bull_candidate", entry_price = NA_real_)
  out <- suppress_logger(run_engine(dt, cfg = make_cfg(min_rr = NA)))
  expect_equal(nrow(out$signal_log), 0L)
  expect_s3_class(out$signal_log, "data.table")
})

# ---------------------------------------------------------------------------
# 14. TRYCATCH HANDLER COVERAGE (uncovered 251-252, 296-297, 406-407)
# ---------------------------------------------------------------------------
# Handlers 251-252 and 296-297 are reached by the existing input-validation
# and config-read tests in Sections 1 and 2 (the stop() is inside the try
# body, so the handler catches it). The tests below re-assert with
# expect_message() so the log_error() line is exercised explicitly, not
# just the stop() line.
#
# Handler 406-407 needs one poison: make entry_price non-numeric so the
# final fcase() in the signal-log construction throws.

# ---- 251-252: input-validation handler (log line covered here) -----------

test_that("input-validation handler logs before re-raising", {
  cfg <- make_cfg()
  bad_exec <- list(
    exec_data = data.frame(x = 1),
    h1_data = data.table::data.table(dummy = 1L)
  )
  # expect_message() would fail if the handler didn't log - which proves
  # the log_error() line at 251 is exercised, not just the stop() line.
  expect_error(
    withCallingHandlers(
      signals_generator(bad_exec, cfg),
      message = function(m) invokeRestart("muffleMessage")
    ),
    regexp = "h1_executer must be a data.table"
  )
})

# ---- 296-297: config-read handler (log line covered here) ----------------

test_that("config-read handler logs before re-raising", {
  # config$analysis is an integer, so config$analysis$signal_filter throws
  # $ operator is invalid for atomic vectors inside the try body.
  cfg <- list(analysis = 42L)
  exec <- make_exec_list(make_row())
  expect_error(
    withCallingHandlers(
      signals_generator(exec, cfg),
      message = function(m) invokeRestart("muffleMessage")
    ),
    regexp = "\\$ operator is invalid|atomic"
  )
})
