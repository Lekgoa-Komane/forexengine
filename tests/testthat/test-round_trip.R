# tests/testthat/test-round_trip.R

# ---- Backtest -> journal -> reader ------------------------------------------

test_that("a backtest goes into the journal and comes back out the same", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()

  sync_journal_from_bt(con, bt)
  x <- get_journal_trades(con)

  expect_equal(nrow(x), nrow(bt))
  expect_setequal(x$signal_id, bt$signal_id)

  i <- match(x$signal_id, bt$signal_id)
  expect_equal(x$ticker, bt$ticker[i])
  expect_equal(x$asset_class, bt$asset_class[i])
  expect_equal(x$entry_price, bt$entry_price[i])
  expect_equal(x$stop_loss, bt$sl_price[i])
  expect_equal(x$take_profit, bt$tp_price[i])
  expect_equal(x$signal_occurred_at, bt$signal_datetime[i])
})

test_that("the results of closed trades come back unchanged, times included", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()

  sync_journal_from_bt(con, bt)
  closed <- get_journal_trades(con, status = "closed")
  j <- match(closed$signal_id, bt$signal_id)

  expect_equal(closed$pnl_r, bt$pnl_r[j])
  expect_equal(closed$pnl_pips, bt$pnl_pips[j])
  expect_equal(closed$exit_price, bt$exit_price[j])
  expect_equal(closed$opened_at, bt$fill_datetime[j])
  expect_equal(closed$closed_at, bt$exit_datetime[j])
})

test_that("trades that never filled or are still open carry no result", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  unfinished <- get_journal_trades(con, status = c("open", "cancelled"))

  expect_equal(nrow(unfinished), 2L)
  expect_true(all(is.na(unfinished$pnl_r)))
  expect_true(all(is.na(unfinished$closed_at)))
})

# ---- The dashboard's numbers -------------------------------------------------

test_that("wins, losses, breakevens and total R come out right", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  sync_journal_from_bt(con, make_fake_bt())

  finished <- get_journal_trades(con, status = "closed")

  # three finished trades: one win, one loss, one breakeven
  expect_equal(nrow(finished), 3L)
  expect_equal(sum(finished$outcome == "win"), 1L)
  expect_equal(sum(finished$outcome == "loss"), 1L)
  expect_equal(sum(finished$outcome == "breakeven"), 1L)
  expect_equal(sum(finished$pnl_r), 1) # +2, -1 and 0
})

test_that("the trade count leaves out open trades and never-filled signals", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  sync_journal_from_bt(con, make_fake_bt())

  all_rows <- get_journal_trades(con)
  finished <- get_journal_trades(con, status = "closed")

  expect_equal(nrow(all_rows), 5L)
  expect_equal(nrow(finished), 3L)
})

# ---- Notes through the whole loop -------------------------------------------

test_that("a review written by update_journal_review survives a sync and is read back", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt)
  update_journal_review(
    con,
    "s-1",
    list(notes = "good entry", emotion_rating = 4L, tags = "london")
  )

  sync_journal_from_bt(con, bt)
  x <- get_journal_trades(con, ticker = "eurusd")
  row <- x[signal_id == "s-1"]

  expect_equal(row$notes, "good entry")
  expect_equal(row$emotion_rating, 4L)
  expect_equal(row$tags, "london")
  expect_s3_class(row$created_at, "POSIXct")
})

# ---- Journal and paper trades -----------------------------------------------

test_that("a paper trade stays linked to its journal trade through a sync", {
  cfg <- make_valid_config()
  journal <- open_journal(cfg)
  paper <- open_paper(cfg)
  sync_journal_from_bt(journal, make_fake_bt())

  trade <- make_paper_trade("s-5", "2026-10-05 00:00:00", ticker = "xauusd")
  trade$direction <- "short"
  trade$entry_price <- 2700
  trade$stop_loss <- 2710
  trade$take_profit <- 2670
  insert_paper_trade(paper, trade)

  # the engine runs again and the journal is rewritten
  sync_journal_from_bt(journal, make_fake_bt())

  both <- merge(
    get_journal_trades(journal),
    get_paper_trades(paper),
    by = "signal_id",
    suffixes = c("_journal", "_paper")
  )

  expect_equal(nrow(both), 1L)
  expect_equal(both$signal_id, "s-5")
  expect_equal(both$ticker_journal, both$ticker_paper)
})

test_that("a paper trade keeps the numbers you saw, even if the engine changes them", {
  cfg <- make_valid_config()
  journal <- open_journal(cfg)
  paper <- open_paper(cfg)
  sync_journal_from_bt(journal, make_fake_bt())

  trade <- make_paper_trade("s-5", "2026-10-05 00:00:00", ticker = "xauusd")
  trade$direction <- "short"
  trade$entry_price <- 2700
  insert_paper_trade(paper, trade)

  # the engine now gives a different entry for the same signal
  later <- make_fake_bt()
  later[signal_id == "s-5", entry_price := 2705]
  sync_journal_from_bt(journal, later)

  both <- merge(
    get_journal_trades(journal),
    get_paper_trades(paper),
    by = "signal_id",
    suffixes = c("_journal", "_paper")
  )

  expect_equal(both$entry_price_journal, 2705)
  expect_equal(both$entry_price_paper, 2700)
})

# ---- Parquet results and the journal ----------------------------------------

test_that("the parquet results and the journal share the same key columns", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  sync_journal_from_bt(con, make_fake_bt())
  save_result(make_fake_registry("h1"), "registry", cfg)

  zones <- get_result("registry", "h1", ticker = "eurusd", config = cfg)
  trades <- get_journal_trades(con, ticker = "eurusd")

  keys <- c("ticker", "asset_class", "timeframe")
  expect_true(all(keys %in% names(zones)))
  expect_true(all(keys %in% names(trades)))
  for (k in keys) {
    expect_equal(class(zones[[k]]), class(trades[[k]]), info = k)
  }
  expect_equal(unique(zones$timeframe), unique(trades$timeframe))
})

test_that("a trade can be joined to the zones of its ticker and timeframe", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  sync_journal_from_bt(con, make_fake_bt())
  save_result(make_fake_registry("h1"), "registry", cfg)

  zones <- get_result("registry", "h1", ticker = "eurusd", config = cfg)
  trades <- get_journal_trades(con, ticker = "eurusd")

  joined <- merge(
    trades,
    zones,
    by = c("ticker", "timeframe"),
    suffixes = c("_trade", "_zone"),
    allow.cartesian = TRUE
  )

  expect_equal(nrow(joined), nrow(trades) * nrow(zones))
})
