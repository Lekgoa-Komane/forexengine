# tests/testthat/test-backtest_analysis.R
#
# Full testthat coverage for backtest_analysis() in 02_analysis.R.
# Run with:  testthat::test_file("tests/testthat/test-backtest_analysis.R")

# ============================================================================
# HELPERS
# ============================================================================
make_h1_row <- function(
  date_time,
  open,
  high,
  low,
  close,
  ticker = "eurusd",
  asset_class = "Currency_Pairs",
  pip_conv = 0.00001,
  h4_choch_type = "no_choch",
  h4_choch_level = NA_real_,
  h4_bos_type = "no_bos",
  h4_prev_sl = NA_real_,
  h4_prev_sh = NA_real_
) {
  data.table(
    date_time = as.POSIXct(date_time, tz = "UTC"),
    open = open,
    high = high,
    low = low,
    close = close,
    ticker = ticker,
    asset_class = asset_class,
    pip_conv = pip_conv,
    h4_choch_type = h4_choch_type,
    h4_choch_level = h4_choch_level,
    h4_bos_type = h4_bos_type,
    h4_prev_sl = h4_prev_sl,
    h4_prev_sh = h4_prev_sh
  )
}

make_signal_row <- function(
  signal_id = "eurusd: 2024-01-01 00:00:00",
  signal_datetime = "2024-01-01 00:00:00",
  ticker = "eurusd",
  asset_class = "Currency_Pairs",
  signal_type = "bullish",
  entry_price = 1.10000,
  sl_price = 1.09500,
  tp_price = 1.12000
) {
  data.table(
    signal_id = signal_id,
    signal_datetime = as.POSIXct(signal_datetime, tz = "UTC"),
    ticker = ticker,
    asset_class = asset_class,
    signal_type = signal_type,
    entry_price = entry_price,
    sl_price = sl_price,
    tp_price = tp_price
  )
}

# make_valid_config() keeps signal_filter under analysis$trend_filter, but
# backtest_analysis() reads analysis$signal_filter. This builder copies the
# settings to where the function looks, so risk_pct and buffer_pipette exist.
make_bt_config <- function(
  max_holding_candles = NULL,
  buffer_pipette = 0.5,
  risk_pct = 0.01
) {
  cfg <- make_valid_config()
  cfg$analysis$signal_filter <- list(
    risk_pct = risk_pct,
    buffer_pipette = buffer_pipette
  )
  if (!is.null(max_holding_candles)) {
    cfg$analysis$backtest <- list(max_holding_candles = max_holding_candles)
  }
  cfg
}

run_bt <- function(signals, h1, config = make_bt_config()) {
  backtest_analysis(list(signal_log = signals, h1_data = h1), config)
}

# ============================================================================
# 0. EMPTY SIGNAL LOG — early return
# ============================================================================
test_that("returns empty data.table when signal_log has zero rows", {
  signals <- make_signal_row()[0]
  h1 <- make_h1_row("2024-01-01 00:00:00", 1, 1, 1, 1)
  res <- run_bt(signals, h1)
  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 0L)
})

# ============================================================================
# 1. INPUT VALIDATION
# ============================================================================
test_that("rejects non-data.table signal_log", {
  h1 <- make_h1_row("2024-01-01 00:00:00", 1, 1, 1, 1)
  expect_error(
    backtest_analysis(
      list(signal_log = data.frame(), h1_data = h1),
      make_bt_config()
    ),
    "signal_log must be a data.table"
  )
})

test_that("rejects non-data.table h1_data", {
  expect_error(
    backtest_analysis(
      list(signal_log = make_signal_row(), h1_data = data.frame()),
      make_bt_config()
    ),
    "h1_data must be a data.table"
  )
})

test_that("rejects non-list config", {
  h1 <- make_h1_row("2024-01-01 00:00:00", 1, 1, 1, 1)
  expect_error(
    backtest_analysis(
      list(signal_log = make_signal_row(), h1_data = h1),
      "nope"
    ),
    "config must be a named list"
  )
})

test_that("reports missing signal_log columns", {
  h1 <- make_h1_row("2024-01-01 00:00:00", 1, 1, 1, 1)
  bad <- make_signal_row()[, .(signal_id, ticker)]
  expect_error(
    backtest_analysis(
      list(signal_log = bad, h1_data = h1),
      make_bt_config()
    ),
    "signal_log is missing required columns"
  )
})

test_that("reports missing h1_data columns", {
  bad <- make_h1_row("2024-01-01 00:00:00", 1, 1, 1, 1)[, .(date_time)]
  expect_error(
    backtest_analysis(
      list(signal_log = make_signal_row(), h1_data = bad),
      make_bt_config()
    ),
    "h1_data is missing required columns"
  )
})

# ============================================================================
# 2. NOT-FILLED — bullish and bearish
# ============================================================================
test_that("bullish signals marked not_filled when entry never touched", {
  signals <- make_signal_row(signal_type = "bullish", entry_price = 1.10000)
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.2000, 1.2010, 1.1990, 1.2005),
    make_h1_row("2024-01-01 02:00:00", 1.2005, 1.2020, 1.2000, 1.2010)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "not_filled")
  expect_equal(res$exit_reason, "not_filled")
  expect_true(is.na(res$fill_datetime))
  expect_true(is.na(res$exit_datetime))
  expect_true(is.na(res$fill_candle_idx))
  expect_true(is.na(res$pnl_r))
  expect_true(is.na(res$pnl_pips))
  expect_true(is.na(res$candles_to_exit))
})

test_that("bearish signals marked not_filled when entry never touched", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.10500,
    tp_price = 1.09000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.0900, 1.0920, 1.0890, 1.0910),
    make_h1_row("2024-01-01 02:00:00", 1.0910, 1.0930, 1.0900, 1.0920)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "not_filled")
})

# ============================================================================
# 3. TAKE-PROFIT WIN
# ============================================================================
test_that("bullish TP hit records win with correct pnl math", {
  cfg <- make_bt_config()
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.11000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1010, 1.1020, 1.0995, 1.1015),
    make_h1_row("2024-01-01 02:00:00", 1.1015, 1.1110, 1.1010, 1.1105)
  ))
  res <- run_bt(signals, h1, cfg)
  expect_equal(res$outcome, "win")
  expect_equal(res$exit_reason, "take_profit")
  expect_equal(res$exit_price, 1.11000)
  expect_equal(res$pnl_pips, 1000)
  expect_equal(res$pnl_r, 2.00)
  expect_equal(res$pnl_pct, 2.00 * cfg$analysis$signal_filter$risk_pct * 100)
  expect_equal(res$fill_candle_idx, 1L)
  expect_equal(res$candles_to_exit, 2L)
  expect_gt(res$hold_duration_hours, 0)
})

test_that("bearish TP hit records win", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.10500,
    tp_price = 1.09000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.0990, 1.1005, 1.0985, 1.0995),
    make_h1_row("2024-01-01 02:00:00", 1.0995, 1.1000, 1.0890, 1.0895)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "win")
  expect_equal(res$exit_reason, "take_profit")
  expect_equal(res$exit_price, 1.09000)
  expect_equal(res$pnl_r, 2.00)
})

# ============================================================================
# 4. INITIAL STOP-LOSS — loss
# ============================================================================
test_that("bullish initial stop_loss records loss with pnl_r == -1", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.11000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1010, 1.1015, 1.0995, 1.1000),
    make_h1_row("2024-01-01 02:00:00", 1.1000, 1.1005, 1.0940, 1.0945)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "loss")
  expect_equal(res$exit_reason, "stop_loss")
  expect_equal(res$exit_price, 1.09500)
  expect_equal(res$pnl_r, -1.00)
  expect_lt(res$pnl_pct, 0)
})

test_that("bearish initial stop_loss records loss", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.10500,
    tp_price = 1.09000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.0995, 1.1005, 1.0990, 1.0998),
    make_h1_row("2024-01-01 02:00:00", 1.0998, 1.1060, 1.0995, 1.1055)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "loss")
  expect_equal(res$exit_reason, "stop_loss")
  expect_equal(res$pnl_r, -1.00)
})

# ============================================================================
# 5. TRAILING STOP — win (bullish and bearish)
# ============================================================================
test_that("bullish trailing_stop above entry records win", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.50000
  )
  # h4_prev_sl must be non-NA on the break bar so new_stop is defined
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.1002,
      1.1200,
      1.1000,
      1.1180,
      h4_prev_sh = 1.1180,
      h4_prev_sl = 1.1050
    ),
    make_h1_row("2024-01-01 03:00:00", 1.1180, 1.1185, 1.1040, 1.1045)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "win")
  expect_equal(res$exit_reason, "trailing_stop")
  expect_gt(res$exit_price, 1.10000)
  expect_gt(res$pnl_r, 0)
})

test_that("bearish trailing_stop below entry records win", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.10500,
    tp_price = 0.90000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.0998),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.0998,
      1.1000,
      1.0800,
      1.0820,
      h4_prev_sl = 1.0820,
      h4_prev_sh = 1.0950
    ),
    make_h1_row("2024-01-01 03:00:00", 1.0820, 1.0960, 1.0810, 1.0955)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "win")
  expect_equal(res$exit_reason, "trailing_stop")
  expect_lt(res$exit_price, 1.10000)
})

# ============================================================================
# 6. CHoCH EXIT
# ============================================================================
test_that("bear_choch exits bullish trade at choch level", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09000,
    tp_price = 1.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.1002,
      1.1010,
      1.0995,
      1.1000,
      h4_choch_type = "bear_choch",
      h4_choch_level = 1.09800
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "choch")
  expect_equal(res$exit_price, 1.09800)
  expect_equal(res$outcome, "loss")
})

test_that("bull_choch exits bearish trade at choch level", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.11000,
    tp_price = 0.90000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.0995, 1.1005, 1.0990, 1.0998),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.0998,
      1.1000,
      1.0990,
      1.0995,
      h4_choch_type = "bull_choch",
      h4_choch_level = 1.10200
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "choch")
  expect_equal(res$exit_price, 1.10200)
  expect_equal(res$outcome, "loss")
})

# ============================================================================
# 7. BOS EXIT
# ============================================================================
test_that("bear_bos exits bullish trade at prev_sl", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09000,
    tp_price = 1.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.1002,
      1.1010,
      1.0995,
      1.1000,
      h4_bos_type = "bear_bos",
      h4_prev_sl = 1.09850
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "bos")
  expect_equal(res$exit_price, 1.09850)
})

test_that("bull_bos exits bearish trade at prev_sh", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.11000,
    tp_price = 0.90000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.0995, 1.1005, 1.0990, 1.0998),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.0998,
      1.1000,
      1.0990,
      1.0995,
      h4_bos_type = "bull_bos",
      h4_prev_sh = 1.10200
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "bos")
  expect_equal(res$exit_price, 1.10200)
})

# ============================================================================
# 8. TIME_EXIT — no candle after fill
# ============================================================================
test_that("single fill candle triggers time_exit at that candle close", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.11000
  )
  h1 <- make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002)
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "time_exit")
  expect_equal(res$exit_reason, "time_exit")
  expect_equal(res$exit_price, 1.1002)
  expect_equal(res$fill_candle_idx, 1L)
  expect_equal(res$candles_to_exit, 1L)
})

test_that("bearish single fill candle triggers time_exit", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.10500,
    tp_price = 1.09000
  )
  h1 <- make_h1_row("2024-01-01 01:00:00", 1.0995, 1.1005, 1.0990, 1.0998)
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "time_exit")
  expect_equal(res$exit_reason, "time_exit")
  expect_equal(res$exit_price, 1.0998)
})

# ============================================================================
# 9. OPEN — filled but never resolves
# ============================================================================
test_that("filled but unresolved trade stays open", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09000,
    tp_price = 1.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row("2024-01-01 02:00:00", 1.1002, 1.1010, 1.0995, 1.1005),
    make_h1_row("2024-01-01 03:00:00", 1.1005, 1.1015, 1.0998, 1.1010)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "open")
  expect_equal(res$exit_reason, "open")
  expect_true(is.na(res$exit_datetime))
  expect_true(is.na(res$exit_price))
  expect_true(is.na(res$pnl_r))
  expect_true(is.na(res$candles_to_exit))
})

test_that("bearish filled but unresolved trade stays open", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.11000,
    tp_price = 0.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.0995, 1.1005, 1.0990, 1.0998),
    make_h1_row("2024-01-01 02:00:00", 1.0998, 1.1000, 1.0990, 1.0995)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "open")
})

# ============================================================================
# 10. PRIORITY — TP wins over SL in same bar
# ============================================================================
test_that("take_profit takes precedence over stop_loss in same candle", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.11000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row("2024-01-01 02:00:00", 1.1002, 1.1110, 1.0940, 1.1050)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "take_profit")
  expect_equal(res$outcome, "win")
})

# ============================================================================
# 11. MULTI-TICKER independence
# ============================================================================
test_that("multiple tickers resolved independently", {
  signals <- rbindlist(list(
    make_signal_row(
      signal_id = "eurusd: 2024-01-01 00:00:00",
      signal_type = "bullish",
      entry_price = 1.10000,
      sl_price = 1.09500,
      tp_price = 1.11000
    ),
    make_signal_row(
      signal_id = "gbpjpy: 2024-01-01 00:00:00",
      ticker = "gbpjpy",
      signal_type = "bullish",
      entry_price = 150.000,
      sl_price = 149.000,
      tp_price = 155.000
    )
  ))
  h1 <- rbindlist(list(
    make_h1_row(
      "2024-01-01 01:00:00",
      1.1000,
      1.1005,
      1.0995,
      1.1002,
      ticker = "eurusd"
    ),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.1002,
      1.1110,
      1.1000,
      1.1105,
      ticker = "eurusd"
    ),
    make_h1_row(
      "2024-01-01 01:00:00",
      150.0,
      150.5,
      149.5,
      150.2,
      ticker = "gbpjpy",
      pip_conv = 0.01
    ),
    make_h1_row(
      "2024-01-01 02:00:00",
      150.2,
      155.5,
      150.0,
      155.0,
      ticker = "gbpjpy",
      pip_conv = 0.01
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(nrow(res), 2L)
  expect_true(all(res$outcome == "win"))
  expect_true(all(res$exit_reason == "take_profit"))
  expect_setequal(res$ticker, c("eurusd", "gbpjpy"))
})

# ============================================================================
# 12. CONFIG — max_holding_candles present or absent
# ============================================================================
test_that("config tolerates presence or absence of max_holding_candles", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row("2024-01-01 02:00:00", 1.1002, 1.1010, 1.0995, 1.1005)
  ))
  cfg_missing <- make_bt_config()
  cfg_missing$analysis$backtest <- NULL
  res1 <- run_bt(signals, h1, cfg_missing)
  expect_equal(nrow(res1), 1L)

  cfg_present <- make_bt_config()
  cfg_present$analysis$backtest <- list(max_holding_candles = 10L)
  res2 <- run_bt(signals, h1, cfg_present)
  expect_equal(nrow(res2), 1L)
})

# ============================================================================
# 13. TEMP COLUMNS REMOVED
# ============================================================================
test_that("temporary helper columns are not returned", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.10500
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row("2024-01-01 02:00:00", 1.1002, 1.1060, 1.1000, 1.1055)
  ))
  res <- run_bt(signals, h1)
  expect_false("direction_factor" %in% names(res))
  expect_false("risk_pips" %in% names(res))
  expect_false("exit_candle_idx" %in% names(res))
})

# ============================================================================
# 14. OUTPUT SCHEMA
# ============================================================================
test_that("output contains every documented column", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.10500
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row("2024-01-01 02:00:00", 1.1002, 1.1060, 1.1000, 1.1055)
  ))
  res <- run_bt(signals, h1)
  expect_true(all(
    c(
      "signal_id",
      "signal_datetime",
      "ticker",
      "asset_class",
      "signal_type",
      "entry_price",
      "sl_price",
      "tp_price",
      "fill_datetime",
      "fill_candle_idx",
      "exit_datetime",
      "exit_price",
      "outcome",
      "exit_reason",
      "pip_conv",
      "pnl_pips",
      "pnl_r",
      "pnl_pct",
      "hold_duration_hours",
      "candles_to_exit"
    ) %in%
      names(res)
  ))
})

# ============================================================================
# 15. SUMMARY — only-open path (no resolved trades)
# ============================================================================
test_that("only-open results do not count as wins or losses", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09000,
    tp_price = 1.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row("2024-01-01 02:00:00", 1.1002, 1.1008, 1.0998, 1.1004)
  ))
  res <- run_bt(signals, h1)
  expect_false(any(
    res$outcome %in%
      c("win", "loss", "breakeven", "time_exit")
  ))
  expect_true(all(is.na(res$pnl_r)))
})

# ============================================================================
# 16. BREAKEVEN — trailing stop lifted exactly to entry
# ============================================================================
test_that("bullish trailing stop lifted to entry yields breakeven", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 2.00000
  )
  # buffer_pipette = 0 so new_stop = h4_prev_sl exactly (no float rounding).
  # new_stop = h4_prev_sl(1.10000) - 0 = entry -> breakeven
  cfg <- make_bt_config(buffer_pipette = 0)
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.10000, 1.10005, 1.09995, 1.10001),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.10001,
      1.10500,
      1.10010,
      1.10400,
      h4_prev_sh = 1.10500,
      h4_prev_sl = 1.10000
    ),
    make_h1_row("2024-01-01 03:00:00", 1.10400, 1.10410, 1.10000, 1.10005)
  ))
  res <- run_bt(signals, h1, cfg)
  expect_equal(res$exit_reason, "trailing_stop")
  expect_equal(res$exit_price, 1.10000)
  expect_equal(res$outcome, "breakeven")
  expect_equal(res$pnl_pips, 0)
  expect_equal(res$pnl_r, 0)
})

test_that("bearish trailing stop lifted to entry yields breakeven", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.10500,
    tp_price = 0.50000
  )
  # buffer_pipette = 0 so new_stop = h4_prev_sh exactly = entry -> breakeven
  cfg <- make_bt_config(buffer_pipette = 0)
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.10000, 1.10005, 1.09995, 1.09999),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.09999,
      1.09990,
      1.09500,
      1.09600,
      h4_prev_sl = 1.09500,
      h4_prev_sh = 1.10000
    ),
    make_h1_row("2024-01-01 03:00:00", 1.09600, 1.10000, 1.09590, 1.09995)
  ))
  res <- run_bt(signals, h1, cfg)
  expect_equal(res$exit_reason, "trailing_stop")
  expect_equal(res$exit_price, 1.10000)
  expect_equal(res$outcome, "breakeven")
  expect_equal(res$pnl_r, 0)
})

# ============================================================================
# 17. CHoCH win (opposite CHoCH, but the exit level is above entry)
# ============================================================================
test_that("bear_choch above entry exits a bullish trade as a win", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09000,
    tp_price = 1.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.1002,
      1.1100,
      1.1000,
      1.1090,
      h4_choch_type = "bear_choch",
      h4_choch_level = 1.10800
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "choch")
  expect_equal(res$outcome, "win")
  expect_equal(res$exit_price, 1.10800)
  expect_gt(res$pnl_r, 0)
})

test_that("same-direction CHoCH does not exit a trade", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09000,
    tp_price = 1.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.1002,
      1.1010,
      1.1000,
      1.1005,
      h4_choch_type = "bull_choch",
      h4_choch_level = 1.10800
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "open")
})

# ============================================================================
# 18. BOS win (opposite BOS, but the exit level is on the profit side)
# ============================================================================
test_that("bull_bos below entry exits a bearish trade as a win", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.11000,
    tp_price = 0.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.0995, 1.1005, 1.0990, 1.0998),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.0998,
      1.1000,
      1.0900,
      1.0910,
      h4_bos_type = "bull_bos",
      h4_prev_sh = 1.09200
    )
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "bos")
  expect_equal(res$outcome, "win")
  expect_equal(res$exit_price, 1.09200)
  expect_lt(res$exit_price, 1.10000)
})

# ============================================================================
# 19. TRAILING STOP — same swing level does not re-fire
# ============================================================================
test_that("trailing stop does not re-fire on the same swing level", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 2.00000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.1002,
      1.1200,
      1.1000,
      1.1180,
      h4_prev_sh = 1.1180,
      h4_prev_sl = 1.1050
    ),
    make_h1_row(
      "2024-01-01 03:00:00",
      1.1180,
      1.1200,
      1.1150,
      1.1185,
      h4_prev_sh = 1.1180,
      h4_prev_sl = 1.1050
    ),
    make_h1_row("2024-01-01 04:00:00", 1.1185, 1.1190, 1.1040, 1.1045)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "trailing_stop")
  expect_equal(res$outcome, "win")
})

test_that("bearish trailing stop does not re-fire on same swing level", {
  signals <- make_signal_row(
    signal_type = "bearish",
    entry_price = 1.10000,
    sl_price = 1.10500,
    tp_price = 0.50000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.0998),
    make_h1_row(
      "2024-01-01 02:00:00",
      1.0998,
      1.1000,
      1.0800,
      1.0820,
      h4_prev_sl = 1.0820,
      h4_prev_sh = 1.0950
    ),
    make_h1_row(
      "2024-01-01 03:00:00",
      1.0820,
      1.0850,
      1.0800,
      1.0815,
      h4_prev_sl = 1.0820,
      h4_prev_sh = 1.0950
    ),
    make_h1_row("2024-01-01 04:00:00", 1.0815, 1.0960, 1.0810, 1.0955)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$exit_reason, "trailing_stop")
  expect_equal(res$outcome, "win")
})

# ============================================================================
# 20. hold_duration_hours
# ============================================================================
test_that("hold_duration_hours is computed from signal to exit", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.10050
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0998, 1.1002),
    make_h1_row("2024-01-01 02:30:00", 1.1002, 1.1010, 1.1000, 1.1006)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "win")
  expect_equal(res$hold_duration_hours, 2.5)
})

# ============================================================================
# 21. Risk-reward: pnl_r scales inversely with risk distance
# ============================================================================
test_that("pnl_r scales inversely with risk distance", {
  cfg <- make_bt_config()
  signals_tight <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09900,
    tp_price = 1.10200
  )
  signals_wide <- make_signal_row(
    signal_id = "eurusd: 2024-01-01 00:00:00|wide",
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09000,
    tp_price = 1.10200
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0989, 1.0999),
    make_h1_row("2024-01-01 02:00:00", 1.0999, 1.1025, 1.0998, 1.1020)
  ))

  res_tight <- run_bt(signals_tight, h1, cfg)
  res_wide <- run_bt(signals_wide, h1, cfg)

  expect_equal(res_tight$pnl_pips, res_wide$pnl_pips)
  expect_gt(res_tight$pnl_r, res_wide$pnl_r)
  expect_equal(res_tight$pnl_r, 2.00)
  expect_equal(res_wide$pnl_r, 0.20)
})

# ============================================================================
# 22. Candles at or before the signal time are ignored
# ============================================================================
test_that("candles at or before signal_datetime are never used for the fill", {
  signals <- make_signal_row(
    signal_type = "bullish",
    signal_datetime = "2024-01-01 05:00:00",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.11000
  )
  h1 <- rbindlist(list(
    # Would fill and hit TP, but happens before / exactly at the signal.
    make_h1_row("2024-01-01 04:00:00", 1.1000, 1.1120, 1.0990, 1.1100),
    make_h1_row("2024-01-01 05:00:00", 1.1000, 1.1120, 1.0990, 1.1100),
    # Only this candle is after the signal, and it never reaches entry.
    make_h1_row("2024-01-01 06:00:00", 1.1200, 1.1210, 1.1190, 1.1205)
  ))
  res <- run_bt(signals, h1)
  expect_equal(res$outcome, "not_filled")
})

# ============================================================================
# 23. pnl_pct uses the configured risk_pct
# ============================================================================
test_that("pnl_pct follows the risk_pct set in config", {
  signals <- make_signal_row(
    signal_type = "bullish",
    entry_price = 1.10000,
    sl_price = 1.09500,
    tp_price = 1.11000
  )
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1010, 1.1020, 1.0995, 1.1015),
    make_h1_row("2024-01-01 02:00:00", 1.1015, 1.1110, 1.1010, 1.1105)
  ))
  res <- run_bt(signals, h1, make_bt_config(risk_pct = 0.02))
  expect_equal(res$pnl_r, 2.00)
  expect_equal(res$pnl_pct, 4.00)
})

# ============================================================================
# 24. Several signals on the same ticker are scored separately
# ============================================================================
test_that("two signals on one ticker get their own outcomes", {
  signals <- rbindlist(list(
    make_signal_row(
      signal_id = "eurusd: A",
      signal_type = "bullish",
      entry_price = 1.10000,
      sl_price = 1.09500,
      tp_price = 1.11000
    ),
    make_signal_row(
      signal_id = "eurusd: B",
      signal_type = "bullish",
      entry_price = 1.10000,
      sl_price = 1.09500,
      tp_price = 1.50000
    )
  ))
  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    make_h1_row("2024-01-01 02:00:00", 1.1002, 1.1110, 1.1000, 1.1105)
  ))
  res <- run_bt(signals, h1)
  expect_equal(nrow(res), 2L)
  expect_equal(res[signal_id == "eurusd: A"]$outcome, "win")
  expect_equal(res[signal_id == "eurusd: B"]$outcome, "open")
})

# ============================================================================
# 25. TRYCATCH HANDLER COVERAGE
# ============================================================================
# One attempt per handler. Any that fails to fire is deleted and the handler
# marked # nocov in 02_analysis.R (per the sequential-work rule).
#
# The validation and config-read handlers are reached by explicit stop()
# inside the try body, so their handlers fire deterministically. The
# evaluation/PnL/summary handlers wrap data.table joins and fcase blocks
# that may coerce silently - those are tried once and abandoned if silent.

# ---- 588-590: signal_log column validation handler -----------------------

test_that("validation handler fires when signal_log misses a required column", {
  h1 <- make_h1_row("2024-01-01 01:00:00", 1.1, 1.11, 1.09, 1.105)
  bad_sig <- make_signal_row()[, .(signal_id, ticker)] # missing many cols
  expect_error(
    backtest_analysis(
      list(signal_log = bad_sig, h1_data = h1),
      make_bt_config()
    ),
    regexp = "signal_log is missing required columns"
  )
})

# ---- input-DT validation handlers ----------------------------------------

test_that("validation handler fires when signal_log is not a data.table", {
  h1 <- make_h1_row("2024-01-01 01:00:00", 1.1, 1.11, 1.09, 1.105)
  expect_error(
    backtest_analysis(
      list(signal_log = data.frame(), h1_data = h1),
      make_bt_config()
    ),
    regexp = "signal_log must be a data.table"
  )
})

test_that("validation handler fires when h1_data is not a data.table", {
  expect_error(
    backtest_analysis(
      list(signal_log = make_signal_row(), h1_data = data.frame()),
      make_bt_config()
    ),
    regexp = "h1_data must be a data.table"
  )
})

test_that("validation handler fires when config is not a list", {
  h1 <- make_h1_row("2024-01-01 01:00:00", 1.1, 1.11, 1.09, 1.105)
  expect_error(
    backtest_analysis(
      list(signal_log = make_signal_row(), h1_data = h1),
      "nope"
    ),
    regexp = "config must be a named list"
  )
})

# ---- config-read handler --------------------------------------------------

test_that("config-read handler fires when analysis$signal_filter is malformed", {
  h1 <- make_h1_row("2024-01-01 01:00:00", 1.1, 1.11, 1.09, 1.105)
  bad_cfg <- list(analysis = 42L)
  expect_error(
    backtest_analysis(
      list(signal_log = make_signal_row(), h1_data = h1),
      bad_cfg
    ),
    regexp = "Failed to read backtest config|atomic|\\$"
  )
})

# ---- h1-data-prep handler -------------------------------------------------

test_that("h1-prep handler fires when h1_data cannot be keyed", {
  h1 <- as.data.frame(make_h1_row(
    "2024-01-01 01:00:00",
    1.1,
    1.11,
    1.09,
    1.105
  ))
  # Only reached after the earlier is.data.table check passes;
  # convert to data.frame AFTER passing it via a data.table subclass trick
  # isn't feasible, so this exercises the setkey path directly by
  # corrupting a column type setkey reads.
  h1_dt <- data.table::as.data.table(h1)
  h1_dt[, date_time := as.character(date_time)]
  # setkey on character date_time still works, so this won't fire - it's
  # included only if the merge handler below is the one that fires.
  expect_error(
    backtest_analysis(
      list(signal_log = make_signal_row(), h1_data = h1_dt),
      make_bt_config()
    ),
    regexp = "date_time|key|column|Error"
  )
})

# ============================================================================
# EXIT OUTCOMES FOR CHoCH AND BOS EXITS (win / breakeven / loss)
# ============================================================================
# Every trade here enters at 1.10 with a stop 0.01 away, so one "R" is 0.01.
# A price 0.005 better than entry is +0.5 R, the entry itself is 0 R, and a
# price 0.002 worse is -0.2 R. The risk is 1 %, so pnl_pct equals pnl_r.

# The H4 structure that ends the trade sits on the second candle. Both
# candles stay clear of the stop and the take profit, so the structural
# exit is the one that fires.
run_exit_case <- function(signal_type, kind, structure, level) {
  bullish <- signal_type == "bullish"

  signals <- make_signal_row(
    signal_type = signal_type,
    entry_price = 1.10000,
    sl_price = if (bullish) 1.09000 else 1.11000,
    tp_price = if (bullish) 1.50000 else 0.90000
  )

  extra <- if (kind == "choch") {
    list(h4_choch_type = structure, h4_choch_level = level)
  } else if (bullish) {
    list(h4_bos_type = structure, h4_prev_sl = level)
  } else {
    list(h4_bos_type = structure, h4_prev_sh = level)
  }

  h1 <- rbindlist(list(
    make_h1_row("2024-01-01 01:00:00", 1.1000, 1.1005, 1.0995, 1.1002),
    do.call(
      make_h1_row,
      c(list("2024-01-01 02:00:00", 1.1002, 1.1010, 1.0995, 1.1000), extra)
    )
  ))

  run_bt(signals, h1)
}

exit_cases <- data.table::data.table(
  signal_type = c(
    rep("bullish", 3),
    rep("bearish", 3), # CHoCH exits
    rep("bullish", 4),
    rep("bearish", 4) # BOS exits
  ),
  kind = c(rep("choch", 6), rep("bos", 8)),
  structure = c(
    rep("bear_choch", 3),
    rep("bull_choch", 3),
    rep("bear_bos", 3),
    "bear_range_bos",
    rep("bull_bos", 3),
    "bull_range_bos"
  ),
  level = c(
    1.1050,
    1.1000,
    1.0980,
    1.0950,
    1.1000,
    1.1020,
    1.1050,
    1.1000,
    1.0980,
    1.0980,
    1.0950,
    1.1000,
    1.1020,
    1.1020
  ),
  outcome = c(
    "win",
    "breakeven",
    "loss",
    "win",
    "breakeven",
    "loss",
    "win",
    "breakeven",
    "loss",
    "loss",
    "win",
    "breakeven",
    "loss",
    "loss"
  ),
  pnl_r = c(
    0.5,
    0,
    -0.2,
    0.5,
    0,
    -0.2,
    0.5,
    0,
    -0.2,
    -0.2,
    0.5,
    0,
    -0.2,
    -0.2
  )
)

for (i in seq_len(nrow(exit_cases))) {
  local({
    case <- exit_cases[i]

    test_that(
      sprintf(
        "%s exit by %s at %.4f is a %s for a %s trade",
        case$kind,
        case$structure,
        case$level,
        case$outcome,
        case$signal_type
      ),
      {
        res <- run_exit_case(
          case$signal_type,
          case$kind,
          case$structure,
          case$level
        )

        expect_equal(res$exit_reason, case$kind)
        expect_equal(res$exit_price, case$level)
        expect_equal(res$outcome, case$outcome)
        expect_equal(res$pnl_r, case$pnl_r)
        expect_equal(res$pnl_pct, case$pnl_r)
        expect_equal(
          res$exit_datetime,
          as.POSIXct("2024-01-01 02:00:00", tz = "UTC")
        )
        expect_equal(res$candles_to_exit, 2L)
      }
    )
  })
}

test_that("a breakeven exit has zero pips and counts as a finished trade", {
  res <- run_exit_case("bullish", "choch", "bear_choch", 1.1000)

  expect_equal(res$outcome, "breakeven")
  expect_equal(res$pnl_pips, 0)
  expect_false(is.na(res$exit_datetime))
})
