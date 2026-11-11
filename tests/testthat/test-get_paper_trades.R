# tests/testthat/test-get_paper_trades.R

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

# ---- No trades yet ----------------------------------------------------------

test_that("no trades gives an empty table that keeps its columns", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)

  x <- get_paper_trades(con)

  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 0L)
  expect_setequal(names(x), DBI::dbListFields(con, "paper_trades"))
})

test_that("a database with no table yet also gives an empty table, not an error", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  expect_false(DBI::dbExistsTable(con, "paper_trades"))

  x <- get_paper_trades(con)

  expect_equal(nrow(x), 0L)
  expect_true(DBI::dbExistsTable(con, "paper_trades"))
  expect_setequal(names(x), DBI::dbListFields(con, "paper_trades"))
})

test_that("an empty result still works in the dashboard's calculations", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)

  x <- get_paper_trades(con)

  # these would break on a table with no columns at all
  expect_equal(nrow(x[status == "closed"]), 0L)
  expect_equal(sum(x$pnl_r, na.rm = TRUE), 0)
})

# ---- With trades ------------------------------------------------------------

test_that("returns the saved trades, sorted by the time of the signal", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  insert_paper_trade(con, make_paper_trade("p-3", "2026-10-15 08:00:00"))
  insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))
  insert_paper_trade(con, make_paper_trade("p-2", "2026-09-20 08:00:00"))

  x <- get_paper_trades(con)

  expect_equal(nrow(x), 3L)
  expect_equal(x$signal_id, c("p-1", "p-2", "p-3"))
})

test_that("returns every column of the table", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))

  x <- get_paper_trades(con)

  expect_setequal(names(x), DBI::dbListFields(con, "paper_trades"))
  expect_equal(x$direction, "long")
  expect_equal(x$stop_loss, 1.09)
})

test_that("reading does not change anything and can be repeated", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))

  first <- get_paper_trades(con)
  second <- get_paper_trades(con)

  expect_equal(first, second)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM paper_trades")$n,
    1L
  )
})

test_that("shows a trade after it has been closed", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))
  id <- DBI::dbGetQuery(con, "SELECT id FROM paper_trades")$id

  update_paper_trade_close(con, id, list(outcome = "win", pnl_r = 2))
  x <- get_paper_trades(con)

  expect_equal(x$status, "closed")
  expect_equal(x$outcome, "win")
  expect_equal(x$pnl_r, 2)
})

# ---- A real problem must still stop ----------------------------------------

test_that("a closed connection stops with an error instead of looking empty", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  DBI::dbDisconnect(con)

  expect_error(get_paper_trades(con))
})

test_that("a table in the wrong shape stops with an error", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))

  # a `paper_trades` table that has no `signal_datetime` column
  DBI::dbExecute(con, "CREATE TABLE paper_trades (id INTEGER)")
  DBI::dbExecute(con, "INSERT INTO paper_trades (id) VALUES (1)")

  expect_error(get_paper_trades(con))
})

test_that("a failing query is reported as an error, not as an empty table", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)

  testthat::local_mocked_bindings(
    dbGetQuery = function(...) stop("query failed"),
    .package = "DBI"
  )

  expect_error(get_paper_trades(con), "query failed", fixed = TRUE)
})
