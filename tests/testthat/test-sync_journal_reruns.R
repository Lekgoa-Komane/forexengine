# tests/testthat/test-sync_journal_reruns.R

review_cols <- c("emotion_rating", "setup_quality", "mistakes", "notes", "tags")
time_pattern <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"

# ---- The rows ----------------------------------------------------------------

test_that("running the sync again replaces the rows and does not duplicate them", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  sync_journal_from_bt(con, make_fake_bt())
  trades <- read_trades(con)

  expect_equal(nrow(trades), 5L)
  expect_false(anyDuplicated(trades$signal_id) > 0L)
})

test_that("a signal that is no longer in the backtest is removed", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt)
  add_review(con, "s-3", notes = "this note goes with the signal")

  sync_journal_from_bt(con, bt[signal_id != "s-3"])

  expect_equal(read_trades(con)$signal_id, c("s-1", "s-2", "s-4", "s-5"))
})

test_that("a new signal in the backtest is added", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt[1:3])
  add_review(con, "s-1", notes = "kept")

  sync_journal_from_bt(con, bt)
  trades <- read_trades(con)

  expect_equal(trades$signal_id, paste0("s-", 1:5))
  expect_equal(trade_row(trades, "s-1")$notes, "kept")
})

test_that("a trade whose result changed is updated, and keeps its notes", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  sync_journal_from_bt(con, make_fake_bt()) # s-5 is still open
  add_review(con, "s-5", notes = "watching this one")

  later <- make_fake_bt()
  later[
    signal_id == "s-5",
    `:=`(
      outcome = "win",
      exit_price = 2670,
      pnl_pips = 300,
      pnl_r = 3,
      pnl_pct = 0.06,
      exit_reason = "tp"
    )
  ]
  sync_journal_from_bt(con, later)
  row <- trade_row(read_trades(con), "s-5")

  expect_equal(row$status, "closed")
  expect_equal(row$outcome, "win")
  expect_equal(row$pnl_r, 3)
  expect_equal(row$closed_at, "2026-10-06 10:00:00")
  expect_equal(row$notes, "watching this one")
})

# ---- Your manual notes -------------------------------------------------------

test_that("all five review fields come back after a sync", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt)
  add_review(
    con,
    "s-1",
    emotion_rating = 4L,
    setup_quality = 5L,
    mistakes = "moved stop",
    notes = "good entry",
    tags = "london,choch"
  )
  add_review(con, "s-2", notes = "only a note")
  before <- read_trades(con)[, review_cols]

  sync_journal_from_bt(con, bt)
  after <- read_trades(con)

  expect_equal(after[, review_cols], before)
  s1 <- trade_row(after, "s-1")
  expect_equal(s1$emotion_rating, 4L)
  expect_equal(s1$setup_quality, 5L)
  expect_equal(s1$mistakes, "moved stop")
  expect_equal(s1$notes, "good entry")
  expect_equal(s1$tags, "london,choch")
})

test_that("a trade without notes stays without notes", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt)
  add_review(con, "s-1", notes = "a note")

  sync_journal_from_bt(con, bt)
  row <- trade_row(read_trades(con), "s-3")

  for (col in review_cols) {
    expect_true(is.na(row[[col]]), info = col)
  }
})

test_that("notes follow the signal, not the order of the backtest rows", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt)
  add_review(con, "s-1", notes = "good entry")
  add_review(con, "s-4", notes = "never filled")

  sync_journal_from_bt(con, bt[5:1])
  trades <- read_trades(con)

  expect_equal(trade_row(trades, "s-1")$notes, "good entry")
  expect_equal(trade_row(trades, "s-4")$notes, "never filled")
  expect_true(is.na(trade_row(trades, "s-2")$notes))
})

# ---- Dates the journal keeps -------------------------------------------------

test_that("created_at stays for old signals and is new for new signals", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt[1:3])
  set_all(con, "created_at", "2020-01-01 00:00:00")

  sync_journal_from_bt(con, bt)
  trades <- read_trades(con)
  old <- trades[trades$signal_id %in% c("s-1", "s-2", "s-3"), ]
  new <- trades[trades$signal_id %in% c("s-4", "s-5"), ]

  expect_true(all(old$created_at == "2020-01-01 00:00:00"))
  expect_false(any(new$created_at == "2020-01-01 00:00:00"))
  expect_match(new$created_at, time_pattern)
})

test_that("updated_at moves to the time of the latest sync", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  sync_journal_from_bt(con, bt)
  set_all(con, "updated_at", "2020-01-01 00:00:00")

  sync_journal_from_bt(con, bt)
  trades <- read_trades(con)

  expect_false(any(trades$updated_at == "2020-01-01 00:00:00"))
  expect_match(trades$updated_at, time_pattern)
})

# ---- Clean-up ----------------------------------------------------------------

test_that("the temporary table is gone after a sync", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  sync_journal_from_bt(con, make_fake_bt())
  sync_journal_from_bt(con, make_fake_bt())

  leftovers <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_temp_master WHERE name = 'manual_review_tmp'"
  )
  expect_equal(nrow(leftovers), 0L)
})
