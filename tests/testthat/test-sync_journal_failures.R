# tests/testthat/test-sync_journal_failures.R

# A journal that already holds the five fake trades and one review note
journal_with_note <- function(cfg, env = parent.frame()) {
  con <- open_journal(cfg, env = env)
  sync_journal_from_bt(con, make_fake_bt())
  add_review(con, "s-1", notes = "keep me", emotion_rating = 4L)
  con
}

# Is the sync's temporary table still there?
temp_table_left <- function(con) {
  nrow(DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_temp_master WHERE name = 'manual_review_tmp'"
  )) >
    0L
}

# ---- Before anything is touched ---------------------------------------------

test_that("stops when the connection is closed", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  DBI::dbDisconnect(con)

  expect_error(
    sync_journal_from_bt(con, make_fake_bt()),
    "Invalid database connection.",
    fixed = TRUE
  )
})

test_that("names the columns that are missing, and changes nothing", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  before <- read_trades(con)
  bad <- make_fake_bt()
  bad[, c("ticker", "weekly_bias") := NULL]

  expect_error(
    sync_journal_from_bt(con, bad),
    "Missing columns in bt after preparation: ticker, weekly_bias",
    fixed = TRUE
  )
  expect_equal(read_trades(con), before)
  expect_false(temp_table_left(con))
})

test_that("a backtest without signal_id or outcome stops, and changes nothing", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  before <- read_trades(con)

  for (col in c("signal_id", "outcome")) {
    bad <- make_fake_bt()
    bad[, (col) := NULL]

    expect_error(sync_journal_from_bt(con, bad), info = col)
    expect_equal(read_trades(con), before, info = col)
  }
})

# ---- Failures in the middle: everything is rolled back ----------------------

test_that("a repeated signal_id stops the sync and rolls everything back", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  before <- read_trades(con)
  dup <- make_fake_bt()
  dup[signal_id == "s-2", signal_id := "s-1"]

  expect_error(sync_journal_from_bt(con, dup), "UNIQUE")
  after <- read_trades(con)

  expect_equal(after, before)
  expect_equal(nrow(after), 5L)
  expect_equal(trade_row(after, "s-1")$notes, "keep me")
  expect_false(temp_table_left(con))
})

test_that("a row with no entry price stops the sync and rolls everything back", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  before <- read_trades(con)
  bad <- make_fake_bt()
  bad[1, entry_price := NA_real_]

  expect_error(sync_journal_from_bt(con, bad), "NOT NULL")

  expect_equal(read_trades(con), before)
  expect_false(temp_table_left(con))
})

test_that("a failed sync does not block the next one", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  dup <- make_fake_bt()
  dup[signal_id == "s-2", signal_id := "s-1"]
  expect_error(sync_journal_from_bt(con, dup), "UNIQUE")

  expect_error(sync_journal_from_bt(con, make_fake_bt()), NA)

  trades <- read_trades(con)
  expect_equal(nrow(trades), 5L)
  expect_equal(trade_row(trades, "s-1")$notes, "keep me")
})

test_that("a failed first sync leaves the journal empty", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  dup <- make_fake_bt()
  dup[signal_id == "s-2", signal_id := "s-1"]

  expect_error(sync_journal_from_bt(con, dup), "UNIQUE")

  expect_equal(nrow(read_trades(con)), 0L)
  expect_false(temp_table_left(con))
})

# ---- An empty backtest -------------------------------------------------------

test_that("an empty backtest is refused and the journal is left alone", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  before <- read_trades(con)

  expect_error(
    sync_journal_from_bt(con, make_fake_bt()[0L]),
    "`bt` has no rows.",
    fixed = TRUE
  )

  after <- read_trades(con)
  expect_equal(after, before)
  expect_equal(nrow(after), 5L)
  expect_equal(trade_row(after, "s-1")$notes, "keep me")
  expect_false(temp_table_left(con))
})

test_that("an empty data.frame and NULL are refused as well", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  before <- read_trades(con)

  expect_error(
    sync_journal_from_bt(con, as.data.frame(make_fake_bt())[0, ]),
    "`bt` has no rows.",
    fixed = TRUE
  )
  expect_error(
    sync_journal_from_bt(con, NULL),
    "`bt` has no rows.",
    fixed = TRUE
  )
  expect_equal(read_trades(con), before)
})

test_that("an empty backtest does not block the next sync", {
  cfg <- make_valid_config()
  con <- journal_with_note(cfg)
  expect_error(sync_journal_from_bt(con, make_fake_bt()[0L]), "no rows")

  expect_error(sync_journal_from_bt(con, make_fake_bt()), NA)

  trades <- read_trades(con)
  expect_equal(nrow(trades), 5L)
  expect_equal(trade_row(trades, "s-1")$notes, "keep me")
})
