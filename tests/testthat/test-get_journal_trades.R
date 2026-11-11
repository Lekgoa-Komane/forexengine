# tests/testthat/test-get_journal_trades.R

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

ids <- function(x) x$signal_id

# ---- Empty and full ---------------------------------------------------------

test_that("an empty journal gives an empty table that keeps its columns", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)

  x <- get_journal_trades(con)

  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 0L)
  expect_setequal(names(x), DBI::dbListFields(con, "trades"))
  expect_s3_class(x$signal_occurred_at, "POSIXct")
})

test_that("returns every row and every column of the table", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con)

  expect_equal(nrow(x), 6L)
  expect_setequal(names(x), DBI::dbListFields(con, "trades"))
})

test_that("sorts by the time the signal appeared", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con)

  expect_equal(ids(x), paste0("sig-", 1:6))
})

test_that("reading does not change the database", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  get_journal_trades(con, status = "closed")
  get_journal_trades(con)

  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM trades")$n, 6L)
})

# ---- Timestamps -------------------------------------------------------------

test_that("the text timestamps come back as real UTC date-times", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con)

  for (col in c(
    "signal_occurred_at",
    "opened_at",
    "closed_at",
    "created_at",
    "updated_at"
  )) {
    expect_s3_class(x[[col]], "POSIXct")
    expect_equal(attr(x[[col]], "tzone"), "UTC", info = col)
  }
  expect_equal(
    x[signal_id == "sig-1"]$signal_occurred_at,
    as.POSIXct("2026-09-01 08:00:00", tz = "UTC")
  )
})

test_that("a missing time stays NA", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con)

  expect_true(is.na(x[signal_id == "sig-3"]$opened_at))
  expect_true(is.na(x[signal_id == "sig-3"]$closed_at))
  expect_true(is.na(x[signal_id == "sig-4"]$closed_at))
  expect_false(is.na(x[signal_id == "sig-4"]$opened_at))
})

# ---- Filters (each alone) ---------------------------------------------------

test_that("filters by ticker", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_equal(
    ids(get_journal_trades(con, ticker = "eurusd")),
    c("sig-1", "sig-2")
  )
  expect_equal(
    nrow(get_journal_trades(con, ticker = c("eurusd", "audcad"))),
    4L
  )
})

test_that("filters by asset class", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_equal(
    ids(get_journal_trades(con, asset_class = "Metals")),
    c("sig-4", "sig-5")
  )
})

test_that("filters by timeframe", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_equal(ids(get_journal_trades(con, timeframe = "h4")), "sig-5")
  expect_equal(nrow(get_journal_trades(con, timeframe = "h1")), 5L)
})

test_that("filters by status", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_equal(nrow(get_journal_trades(con, status = "closed")), 3L)
  expect_equal(
    nrow(get_journal_trades(con, status = c("closed", "breakeven"))),
    4L
  )
  expect_equal(ids(get_journal_trades(con, status = "cancelled")), "sig-3")
  expect_equal(ids(get_journal_trades(con, status = "open")), "sig-4")
})

test_that("filters work together", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con, ticker = "xauusd", status = "open")

  expect_equal(ids(x), "sig-4")
})

test_that("a filter that matches nothing gives an empty table with columns", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con, ticker = "eurusd", status = "open")

  expect_equal(nrow(x), 0L)
  expect_setequal(names(x), DBI::dbListFields(con, "trades"))
})

test_that("a ticker written like an attack is treated as a plain name", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con, ticker = "x'; DROP TABLE trades; --")

  expect_equal(nrow(x), 0L)
  expect_true(DBI::dbExistsTable(con, "trades"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM trades")$n, 6L)
})

# ---- Date limits ------------------------------------------------------------

test_that("`from` includes its own moment", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  # sig-2 appeared at exactly 2026-09-10 12:00:00
  at <- get_journal_trades(con, from = "2026-09-10 12:00:00")
  after <- get_journal_trades(con, from = "2026-09-10 12:00:01")

  expect_equal(ids(at), paste0("sig-", 2:6))
  expect_equal(ids(after), paste0("sig-", 3:6))
})

test_that("a plain date for `from` means the start of that day", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con, from = "2026-09-10")

  expect_equal(ids(x), paste0("sig-", 2:6))
})

test_that("a plain date for `to` includes the whole of that day", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  # sig-6 appeared at 23:30 on 2026-10-31
  as_text <- get_journal_trades(con, to = "2026-10-31")
  as_date <- get_journal_trades(con, to = as.Date("2026-10-31"))

  expect_equal(nrow(as_text), 6L)
  expect_equal(nrow(as_date), 6L)
})

test_that("a date-time for `to` is used exactly, and includes its own moment", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  midnight <- get_journal_trades(con, to = "2026-10-31 00:00:00")
  exact <- get_journal_trades(con, to = "2026-10-05 00:00:00")

  expect_equal(ids(midnight), paste0("sig-", 1:5))
  expect_equal(ids(exact), paste0("sig-", 1:5))
})

test_that("a POSIXct limit is used as the exact moment", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(
    con,
    from = as.POSIXct("2026-10-01 14:00:00", tz = "UTC")
  )

  expect_equal(ids(x), c("sig-4", "sig-5", "sig-6"))
})

test_that("`from` and `to` work together", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(con, from = "2026-09-10", to = "2026-09-15")

  expect_equal(ids(x), c("sig-2", "sig-3"))
})

test_that("date_by = 'closed' uses the closing time and drops trades not closed", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  after <- get_journal_trades(con, from = "2026-10-01", date_by = "closed")
  before <- get_journal_trades(con, to = "2026-09-30", date_by = "closed")

  # sig-3 (cancelled) and sig-4 (still open) have no closing time
  expect_equal(ids(after), c("sig-5", "sig-6"))
  expect_equal(ids(before), c("sig-1", "sig-2"))
})

test_that("date limits can be mixed with other filters", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  x <- get_journal_trades(
    con,
    ticker = "audcad",
    status = "closed",
    from = "2026-10-01"
  )

  expect_equal(ids(x), "sig-6")
})

# ---- Argument and connection checks ----------------------------------------

test_that("stops when `from` is later than `to`", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_error(
    get_journal_trades(con, from = "2026-10-01", to = "2026-09-01"),
    "`from` is later than `to`.",
    fixed = TRUE
  )
})

test_that("stops when a date limit cannot be read", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_error(
    get_journal_trades(con, from = "not a date"),
    "`from` must be one date or date-time",
    fixed = TRUE
  )
  expect_error(
    get_journal_trades(con, to = c("2026-01-01", "2026-02-01")),
    "`to` must be one date or date-time",
    fixed = TRUE
  )
  expect_error(
    get_journal_trades(con, from = NA),
    "`from` must be one date or date-time",
    fixed = TRUE
  )
})

test_that("stops when `status` has a value the table does not allow", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_error(
    get_journal_trades(con, status = "winning"),
    "`status` must be one or more of: open, closed, breakeven, cancelled. Got: winning",
    fixed = TRUE
  )
})

test_that("stops when `ticker`, `asset_class`, `timeframe` or `status` is not text", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_error(
    get_journal_trades(con, ticker = 5),
    "`ticker` must be text (one or more non-empty values).",
    fixed = TRUE
  )
  expect_error(
    get_journal_trades(con, asset_class = ""),
    "`asset_class` must be text (one or more non-empty values).",
    fixed = TRUE
  )
  expect_error(
    get_journal_trades(con, timeframe = NA_character_),
    "`timeframe` must be text (one or more non-empty values).",
    fixed = TRUE
  )
  expect_error(
    get_journal_trades(con, status = 1),
    "`status` must be text (one or more non-empty values).",
    fixed = TRUE
  )
})

test_that("stops when `date_by` is not one of the two choices", {
  cfg <- make_valid_config()
  con <- open_journal(cfg, make_fake_trades())

  expect_error(
    get_journal_trades(con, date_by = "opened"),
    "should be one of",
    fixed = TRUE
  )
})

test_that("stops when the connection is closed", {
  cfg <- make_valid_config()
  con <- open_journal(cfg)
  DBI::dbDisconnect(con)

  expect_error(
    get_journal_trades(con),
    "`con` is not an open database connection.",
    fixed = TRUE
  )
})

test_that("stops when the database has no `trades` table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    get_journal_trades(con),
    "The journal has no `trades` table.",
    fixed = TRUE
  )
})

# ---- The small date helper --------------------------------------------------

test_that("as_utc_text turns dates and times into UTC text", {
  expect_null(as_utc_text(NULL, "from"))

  expect_equal(as_utc_text("2026-10-31", "to"), "2026-10-31 00:00:00")
  expect_equal(as_utc_text(as.Date("2026-10-31"), "to"), "2026-10-31 00:00:00")
  expect_equal(
    as_utc_text("2026-10-31 14:05:09", "to"),
    "2026-10-31 14:05:09"
  )
})

test_that("as_utc_text only stretches a plain date to the end of the day", {
  expect_equal(
    as_utc_text("2026-10-31", "to", end_of_day = TRUE),
    "2026-10-31 23:59:59"
  )
  expect_equal(
    as_utc_text(as.Date("2026-10-31"), "to", end_of_day = TRUE),
    "2026-10-31 23:59:59"
  )
  # a date-time is used exactly as given
  expect_equal(
    as_utc_text("2026-10-31 10:00:00", "to", end_of_day = TRUE),
    "2026-10-31 10:00:00"
  )
})

test_that("as_utc_text converts a time from another time zone to UTC", {
  local_time <- as.POSIXct("2026-10-31 10:00:00", tz = "Africa/Johannesburg")

  expect_equal(as_utc_text(local_time, "from"), "2026-10-31 08:00:00")
})

test_that("a journal table in the wrong shape stops with an error", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))

  # a `trades` table that has no `signal_occurred_at` column
  DBI::dbExecute(con, "CREATE TABLE trades (id INTEGER, ticker TEXT)")

  expect_error(get_journal_trades(con), "signal_occurred_at")
})
