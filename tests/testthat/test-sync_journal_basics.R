# tests/testthat/test-sync_journal_basics.R

# ---- Writing the rows --------------------------------------------------------

test_that("writes every backtest row and returns the total without printing it", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  res <- withVisible(sync_journal_from_bt(con, make_fake_bt()))

  expect_false(res$visible)
  expect_equal(res$value$total, 5L)
  expect_equal(nrow(read_trades(con)), 5L)
  expect_equal(read_trades(con)$signal_id, paste0("s-", 1:5))
})

test_that("accepts a plain data.frame as well as a data.table", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  res <- sync_journal_from_bt(con, as.data.frame(make_fake_bt()))

  expect_equal(res$total, 5L)
  expect_equal(nrow(read_trades(con)), 5L)
})

test_that("the backtest table you pass in is not changed", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  before <- data.table::copy(bt)

  sync_journal_from_bt(con, bt)

  expect_equal(names(bt), names(before))
  expect_s3_class(bt$signal_datetime, "POSIXct")
  expect_equal(as.data.frame(bt), as.data.frame(before))
})

test_that("extra backtest columns are ignored", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  bt[, junk_col := "x"]

  expect_error(sync_journal_from_bt(con, bt), NA)
  expect_false("junk_col" %in% DBI::dbListFields(con, "trades"))
  expect_false("fill_candle_idx" %in% DBI::dbListFields(con, "trades"))
  expect_false("pip_conv" %in% DBI::dbListFields(con, "trades"))
})

test_that("a signal_id that is a number is saved as text", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  bt[, signal_id := 1:5]

  sync_journal_from_bt(con, bt)

  expect_equal(read_trades(con)$signal_id, as.character(1:5))
})

# ---- Column names and times -------------------------------------------------

test_that("backtest columns are renamed to the journal's names", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  row <- trade_row(read_trades(con), "s-1")

  expect_equal(row$stop_loss, 1.09)
  expect_equal(row$take_profit, 1.12)
  expect_equal(row$entry_price, 1.10)
  expect_equal(row$signal_occurred_at, "2026-09-01 08:00:00")
  expect_equal(row$opened_at, "2026-09-01 09:00:00")
  expect_equal(row$closed_at, "2026-09-02 10:00:00")
})

test_that("times given in another time zone are saved as UTC text", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()[1]
  # 10:00 in Johannesburg is 08:00 UTC
  bt[,
    signal_datetime := as.POSIXct(
      "2026-09-01 10:00:00",
      tz = "Africa/Johannesburg"
    )
  ]

  sync_journal_from_bt(con, bt)

  expect_equal(read_trades(con)$signal_occurred_at, "2026-09-01 08:00:00")
})

test_that("times that already arrive as text are kept as they are", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()[1]
  bt[, signal_datetime := "2026-09-01 08:00:00"]

  sync_journal_from_bt(con, bt)

  expect_equal(read_trades(con)$signal_occurred_at, "2026-09-01 08:00:00")
})

# ---- Status from the outcome ------------------------------------------------

test_that("status is worked out from the outcome", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  trades <- read_trades(con)

  expect_equal(
    trades$status,
    c("closed", "closed", "closed", "cancelled", "open")
  )
})

test_that("a breakeven trade is closed, and its outcome stays breakeven", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  row <- trade_row(read_trades(con), "s-3")

  expect_equal(row$status, "closed")
  expect_equal(row$outcome, "breakeven")
})

test_that("the journal never uses the status 'breakeven'", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())

  expect_false("breakeven" %in% read_trades(con)$status)
})

# ---- Cleaning rows by outcome ------------------------------------------------

test_that("a signal that was never filled loses its exit and result fields", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  row <- trade_row(read_trades(con), "s-4")

  for (col in c(
    "opened_at",
    "closed_at",
    "exit_price",
    "pnl_pips",
    "pnl_r",
    "pnl_pct",
    "hold_duration_hours",
    "candles_to_exit"
  )) {
    expect_true(is.na(row[[col]]), info = col)
  }
  # the signal itself is kept
  expect_equal(row$entry_price, 2650)
  expect_equal(row$stop_loss, 2640)
  expect_equal(row$outcome, "not_filled")
})

test_that("a trade that is still open keeps its opening time but loses its result", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  row <- trade_row(read_trades(con), "s-5")

  expect_equal(row$opened_at, "2026-10-05 01:00:00")
  for (col in c(
    "closed_at",
    "exit_price",
    "pnl_pips",
    "pnl_r",
    "pnl_pct",
    "hold_duration_hours",
    "candles_to_exit"
  )) {
    expect_true(is.na(row[[col]]), info = col)
  }
})

test_that("closed trades keep their results", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  win <- trade_row(read_trades(con), "s-1")
  loss <- trade_row(read_trades(con), "s-2")

  expect_equal(win$exit_price, 1.12)
  expect_equal(win$exit_reason, "tp")
  expect_equal(win$pnl_pips, 200)
  expect_equal(win$pnl_r, 2)
  expect_equal(win$pnl_pct, 0.04)
  expect_equal(win$hold_duration_hours, 25)
  expect_equal(win$candles_to_exit, 25L)
  expect_equal(loss$pnl_r, -1)
  expect_equal(loss$exit_reason, "sl")
})

# ---- Columns the backtest does not have -------------------------------------

test_that("review fields start empty and the lot size starts at 1", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  trades <- read_trades(con)

  expect_true(all(trades$lot_size == 1))
  for (col in c(
    "risk_percent",
    "account_balance",
    "emotion_rating",
    "setup_quality",
    "mistakes",
    "notes",
    "tags"
  )) {
    expect_true(all(is.na(trades[[col]])), info = col)
  }
})

test_that("created_at and updated_at are filled with the time of the sync", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  trades <- read_trades(con)

  pattern <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  expect_true(all(grepl(pattern, trades$created_at)))
  expect_true(all(grepl(pattern, trades$updated_at)))
  expect_equal(trades$created_at, trades$updated_at)
})

test_that("the timeframe from the backtest is kept", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  bt[2, timeframe := "h4"]

  sync_journal_from_bt(con, bt)

  expect_equal(read_trades(con)$timeframe, c("h1", "h4", "h1", "h1", "h1"))
})

test_that("with no timeframe column the timeframe is h1", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  bt[, timeframe := NULL]

  sync_journal_from_bt(con, bt)

  expect_equal(read_trades(con)$timeframe, rep("h1", 5L))
})
