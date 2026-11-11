# tests/testthat/helper-results.R

# A small fake registry: 3 tickers (audcad 3 rows, eurusd 2 rows,
# xauusd 1 row), 2 asset classes, one timeframe.
# The 2027-02-05 date plays the "never happened" placeholder.
make_fake_registry <- function(timeframe = "weekly") {
  data.table::data.table(
    formed_at = as.POSIXct(
      c(
        "2025-01-06",
        "2025-01-13",
        "2025-01-20",
        "2025-01-06",
        "2025-01-13",
        "2025-01-06"
      ),
      tz = "UTC"
    ),
    type_id = c(1L, 1L, 1L, 1L, 2L, 1L),
    type = c(
      "swing_high",
      "bear_fvg",
      "bear_ob",
      "swing_low",
      "bull_fvg",
      "swing_high"
    ),
    type_zone_bot_value = c(NA, 1.05, 1.06, NA, 1.01, NA),
    type_zone_top_value = c(NA, 1.07, 1.08, NA, 1.03, NA),
    critical_value = c(1.09, 1.07, 1.08, 0.99, 1.01, 2650.5),
    invalidated_at = as.POSIXct(
      c(
        "2025-02-03 23:59:00",
        "2027-02-05 23:00:00",
        NA,
        NA,
        "2025-03-03 23:59:00",
        "2027-02-05 23:00:00"
      ),
      tz = "UTC"
    ),
    timeframe = timeframe,
    ticker = c("audcad", "audcad", "audcad", "eurusd", "eurusd", "xauusd"),
    asset_class = c(rep("Currency_Pairs", 5L), "Metals")
  )
}

# All files left under the engine output folder (parquet and .tmp)
engine_files <- function(cfg, pattern = "\\.(parquet|tmp)$") {
  list.files(
    cfg$paths$forexengine$output_base,
    pattern = pattern,
    recursive = TRUE
  )
}

# Six fake journal rows, saved in a mixed-up order on purpose.
# Signal times: sig-1 09-01 08:00, sig-2 09-10 12:00, sig-3 09-15 08:00,
#               sig-4 10-01 14:00, sig-5 10-05 00:00, sig-6 10-31 23:30
make_fake_trades <- function() {
  x <- data.frame(
    ticker = c("eurusd", "eurusd", "audcad", "xauusd", "xauusd", "audcad"),
    asset_class = c(
      "Currency_Pairs",
      "Currency_Pairs",
      "Currency_Pairs",
      "Metals",
      "Metals",
      "Currency_Pairs"
    ),
    timeframe = c("h1", "h1", "h1", "h1", "h4", "h1"),
    signal_id = paste0("sig-", 1:6),
    signal_type = c(
      "bullish",
      "bearish",
      "bullish",
      "bullish",
      "bearish",
      "bearish"
    ),
    entry_price = c(1.10, 1.11, 0.95, 2650, 2700, 0.96),
    stop_loss = c(1.09, 1.12, 0.94, 2640, 2710, 0.97),
    take_profit = c(1.12, 1.09, 0.98, 2680, 2670, 0.93),
    signal_occurred_at = c(
      "2026-09-01 08:00:00",
      "2026-09-10 12:00:00",
      "2026-09-15 08:00:00",
      "2026-10-01 14:00:00",
      "2026-10-05 00:00:00",
      "2026-10-31 23:30:00"
    ),
    opened_at = c(
      "2026-09-01 09:00:00",
      "2026-09-10 13:00:00",
      NA,
      "2026-10-01 15:00:00",
      "2026-10-05 01:00:00",
      "2026-11-01 01:00:00"
    ),
    closed_at = c(
      "2026-09-02 10:00:00",
      "2026-09-12 07:30:00",
      NA,
      NA,
      "2026-10-06 04:00:00",
      "2026-11-02 09:00:00"
    ),
    status = c("closed", "closed", "cancelled", "open", "breakeven", "closed"),
    outcome = c("win", "loss", "not_filled", "open", "breakeven", "win"),
    pnl_r = c(2, -1, NA, NA, 0, 1.5),
    stringsAsFactors = FALSE
  )
  x <- x[c(6, 2, 4, 1, 5, 3), ]
  rownames(x) <- NULL
  x
}

# Open a journal database in a temporary folder, optionally with the fake
# trades inside, and close it again when the test ends.
open_journal <- function(cfg, trades = NULL, env = parent.frame()) {
  con <- connect_journal_db(cfg)
  withr::defer(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con),
    envir = env
  )
  if (!is.null(trades)) {
    DBI::dbAppendTable(con, "trades", trades)
  }
  con
}

# Open a paper trading database in a temporary folder and close it again
# when the test ends.
open_paper <- function(cfg, env = parent.frame()) {
  con <- init_paper_trading_db(cfg)
  withr::defer(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con),
    envir = env
  )
  con
}

# One paper trade, with only the columns that are required
make_paper_trade <- function(signal_id, signal_datetime, ticker = "eurusd") {
  list(
    signal_id = signal_id,
    ticker = ticker,
    asset_class = "Currency_Pairs",
    signal_datetime = signal_datetime,
    direction = "long",
    entry_price = 1.10,
    stop_loss = 1.09,
    take_profit = 1.12,
    status = "taken"
  )
}

# A fake backtest table with the same columns as the real one (5 rows):
#   s-1 win, s-2 loss, s-3 breakeven, s-4 not filled, s-5 still open.
# Rows 4 and 5 carry exit and profit numbers ON PURPOSE: the sync should clear
# them, and the tests check that it does.
make_fake_bt <- function() {
  utc <- function(x) as.POSIXct(x, tz = "UTC")

  data.table::data.table(
    signal_datetime = utc(c(
      "2026-09-01 08:00:00",
      "2026-09-10 12:00:00",
      "2026-09-15 08:00:00",
      "2026-10-01 14:00:00",
      "2026-10-05 00:00:00"
    )),
    signal_id = paste0("s-", 1:5),
    signal_type = c("bullish", "bearish", "bullish", "bullish", "bearish"),
    bias_alignment = c(
      "bull_candidate",
      "bear_candidate",
      "bull_candidate",
      "bull_candidate",
      "bear_candidate"
    ),
    trigger_type = "choch",
    entry_price = c(1.10, 1.11, 0.95, 2650, 2700),
    sl_price = c(1.09, 1.12, 0.94, 2640, 2710),
    tp_price = c(1.12, 1.09, 0.98, 2680, 2670),
    risk_reward = c(2, 2, 3, 3, 3),
    signal_score = c(3L, 2L, 2L, 4L, 3L),
    confluence = c(3L, 2L, 2L, 4L, 3L),
    weekly_bias = "bullish",
    daily_bias = "bullish",
    h4_bias = "bullish",
    h1_bias = "bullish",
    reason = "test signal",
    timeframe = "h1",
    ticker = c("eurusd", "eurusd", "audcad", "xauusd", "xauusd"),
    asset_class = c(
      "Currency_Pairs",
      "Currency_Pairs",
      "Currency_Pairs",
      "Metals",
      "Metals"
    ),
    fill_datetime = utc(c(
      "2026-09-01 09:00:00",
      "2026-09-10 13:00:00",
      "2026-09-15 09:00:00",
      "2026-10-01 15:00:00",
      "2026-10-05 01:00:00"
    )),
    fill_candle_idx = c(10L, 20L, 30L, 40L, 50L),
    exit_datetime = utc(c(
      "2026-09-02 10:00:00",
      "2026-09-12 07:30:00",
      "2026-09-16 09:00:00",
      "2026-10-02 10:00:00",
      "2026-10-06 10:00:00"
    )),
    exit_price = c(1.12, 1.12, 0.95, 2660, 2690),
    outcome = c("win", "loss", "breakeven", "not_filled", "open"),
    exit_reason = c("tp", "sl", "trail", "none", "none"),
    pip_conv = 1e-4,
    pnl_pips = c(200, -100, 0, 10, 10),
    pnl_r = c(2, -1, 0, 0.5, 0.5),
    pnl_pct = c(0.04, -0.02, 0, 0.01, 0.01),
    hold_duration_hours = c(25, 42.5, 24, 1, 1),
    candles_to_exit = c(25L, 43L, 24L, 1L, 1L)
  )
}

# The raw journal table, exactly as stored (times stay as text)
read_trades <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM trades ORDER BY signal_id")
}

# One row of that table
trade_row <- function(trades, id) {
  trades[trades$signal_id == id, ]
}

# Write review notes for one signal, straight into the table
add_review <- function(
  con,
  signal_id,
  emotion_rating = NA_integer_,
  setup_quality = NA_integer_,
  mistakes = NA_character_,
  notes = NA_character_,
  tags = NA_character_
) {
  DBI::dbExecute(
    con,
    "UPDATE trades
     SET emotion_rating = ?, setup_quality = ?, mistakes = ?, notes = ?, tags = ?
     WHERE signal_id = ?",
    params = list(
      emotion_rating,
      setup_quality,
      mistakes,
      notes,
      tags,
      signal_id
    )
  )
}

# Set one text column to the same value on every row
set_all <- function(con, column, value) {
  DBI::dbExecute(
    con,
    sprintf("UPDATE trades SET %s = ?", column),
    params = list(value)
  )
}

# The raw paper trades table, exactly as stored
read_paper <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM paper_trades ORDER BY id")
}

# One row of it, found by signal_id
paper_row <- function(trades, signal_id) {
  trades[trades$signal_id == signal_id, ]
}
