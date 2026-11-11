# tests/testthat/test-update_journal_review.R

time_pattern <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
review_cols <- c("emotion_rating", "setup_quality", "mistakes", "notes", "tags")

# A journal with the five fake trades already saved
journal_with_trades <- function(cfg, env = parent.frame()) {
  con <- open_journal(cfg, env = env)
  sync_journal_from_bt(con, make_fake_bt())
  con
}

# ---- What it changes ---------------------------------------------------------

test_that("saves a note for the trade with that signal_id", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  update_journal_review(con, "s-1", list(notes = "good entry"))
  trades <- read_trades(con)

  expect_equal(trade_row(trades, "s-1")$notes, "good entry")
})

test_that("only the trade asked for is changed", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  update_journal_review(con, "s-1", list(notes = "good entry"))
  trades <- read_trades(con)

  for (id in c("s-2", "s-3", "s-4", "s-5")) {
    expect_true(is.na(trade_row(trades, id)$notes), info = id)
  }
})

test_that("can save all five review fields at once", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  update_journal_review(
    con,
    "s-2",
    list(
      emotion_rating = 3L,
      setup_quality = 5L,
      mistakes = "late entry",
      notes = "stopped out",
      tags = "ny,fvg"
    )
  )
  row <- trade_row(read_trades(con), "s-2")

  expect_equal(row$emotion_rating, 3L)
  expect_equal(row$setup_quality, 5L)
  expect_equal(row$mistakes, "late entry")
  expect_equal(row$notes, "stopped out")
  expect_equal(row$tags, "ny,fvg")
})

test_that("fields that are not given are left as they were", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  update_journal_review(con, "s-1", list(emotion_rating = 4L, notes = "first"))

  update_journal_review(con, "s-1", list(notes = "second"))
  row <- trade_row(read_trades(con), "s-1")

  expect_equal(row$emotion_rating, 4L)
  expect_equal(row$notes, "second")
})

test_that("a field can be cleared with NA", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  update_journal_review(con, "s-1", list(notes = "to be cleared"))

  update_journal_review(con, "s-1", list(notes = NA_character_))

  expect_true(is.na(trade_row(read_trades(con), "s-1")$notes))
})

test_that("the trade's own data is not touched", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  before <- read_trades(con)

  update_journal_review(con, "s-1", list(notes = "x", emotion_rating = 2L))
  after <- read_trades(con)

  untouched <- setdiff(names(before), c(review_cols, "updated_at"))
  expect_equal(after[, untouched], before[, untouched])
})

test_that("updated_at moves for that trade only", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  set_all(con, "updated_at", "2020-01-01 00:00:00")

  update_journal_review(con, "s-1", list(notes = "x"))
  trades <- read_trades(con)

  expect_false(trade_row(trades, "s-1")$updated_at == "2020-01-01 00:00:00")
  expect_match(trade_row(trades, "s-1")$updated_at, time_pattern)
  expect_equal(trade_row(trades, "s-2")$updated_at, "2020-01-01 00:00:00")
})

test_that("returns TRUE without printing it", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  res <- withVisible(update_journal_review(con, "s-1", list(notes = "x")))

  expect_false(res$visible)
  expect_true(res$value)
})

test_that("a signal_id that is a number finds the trade", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  bt <- make_fake_bt()
  bt[, signal_id := 1:5]
  sync_journal_from_bt(con, bt)

  update_journal_review(con, 1, list(notes = "numeric id"))

  expect_equal(trade_row(read_trades(con), "1")$notes, "numeric id")
})

test_that("quotes and SQL words in a note are saved as plain text", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  nasty <- "it's a \"test\"; DROP TABLE trades; --"

  update_journal_review(con, "s-1", list(notes = nasty))

  expect_true(DBI::dbExistsTable(con, "trades"))
  expect_equal(trade_row(read_trades(con), "s-1")$notes, nasty)
  expect_equal(nrow(read_trades(con)), 5L)
})

test_that("a review survives the next sync", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  update_journal_review(con, "s-1", list(notes = "kept", setup_quality = 4L))

  sync_journal_from_bt(con, make_fake_bt())
  row <- trade_row(read_trades(con), "s-1")

  expect_equal(row$notes, "kept")
  expect_equal(row$setup_quality, 4L)
})

# ---- What it refuses ---------------------------------------------------------

test_that("stops when no trade has that signal_id, and changes nothing", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  expect_error(
    update_journal_review(con, "nope", list(notes = "x")),
    "No trade has signal_id 'nope'.",
    fixed = TRUE
  )
  expect_true(all(is.na(read_trades(con)$notes)))
})

test_that("stops when review_data is empty", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  expect_error(
    update_journal_review(con, "s-1", list()),
    "`review_data` is empty.",
    fixed = TRUE
  )
  expect_error(
    update_journal_review(con, "s-1", NULL),
    "`review_data` is empty.",
    fixed = TRUE
  )
})

test_that("stops when review_data has no field names", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  expect_error(
    update_journal_review(con, "s-1", list("a note")),
    "`review_data` must be a named list.",
    fixed = TRUE
  )
  expect_error(
    update_journal_review(con, "s-1", list(notes = "x", "y")),
    "`review_data` must be a named list.",
    fixed = TRUE
  )
})

test_that("stops on a field that is not a review field, and changes nothing", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  expect_error(
    update_journal_review(con, "s-1", list(notes = "x", pnl_r = 99)),
    "Invalid review fields: pnl_r",
    fixed = TRUE
  )
  expect_true(all(is.na(read_trades(con)$notes)))
  expect_false(any(read_trades(con)$pnl_r == 99, na.rm = TRUE))
})

test_that("stops when signal_id is not one non-empty value", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  msg <- "`signal_id` must be one non-empty text value."

  expect_error(
    update_journal_review(con, NULL, list(notes = "x")),
    msg,
    fixed = TRUE
  )
  expect_error(
    update_journal_review(con, NA, list(notes = "x")),
    msg,
    fixed = TRUE
  )
  expect_error(
    update_journal_review(con, "", list(notes = "x")),
    msg,
    fixed = TRUE
  )
  expect_error(
    update_journal_review(con, c("s-1", "s-2"), list(notes = "x")),
    msg,
    fixed = TRUE
  )
})

test_that("the table's own rules still apply to a rating", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)

  expect_error(
    update_journal_review(con, "s-1", list(emotion_rating = 6L)),
    "CHECK"
  )
  expect_error(
    update_journal_review(con, "s-1", list(setup_quality = 0L)),
    "CHECK"
  )
  expect_true(is.na(trade_row(read_trades(con), "s-1")$emotion_rating))
})

test_that("stops when the connection is closed", {
  cfg <- make_valid_config()
  con <- journal_with_trades(cfg)
  DBI::dbDisconnect(con)

  expect_error(update_journal_review(con, "s-1", list(notes = "x")))
})
