# tests/testthat/test-paper_trading_connect.R

paper_path <- function(cfg) {
  file.path(
    cfg$paths$forexengine$db_dir,
    paste0(cfg$database$table_prefixes$paper_trade, cfg$database$db_suffix)
  )
}

# Close a connection when the test ends
close_later <- function(con, env = parent.frame()) {
  withr::defer(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), envir = env)
  con
}

test_that("connecting creates the database file named from the config", {
  cfg <- make_valid_config()

  con <- close_later(connect_paper_trading_db(cfg))

  expect_true(DBI::dbIsValid(con))
  expect_true(file.exists(paper_path(cfg)))
  expect_equal(basename(paper_path(cfg)), "paper_trading.sqlite")
})

test_that("the file name comes from the config", {
  cfg <- make_valid_config()
  cfg$database$table_prefixes$paper_trade <- "my_paper"

  con <- close_later(connect_paper_trading_db(cfg))

  expect_true(
    file.exists(file.path(cfg$paths$forexengine$db_dir, "my_paper.sqlite"))
  )
})

test_that("creates the folder when it is missing, even several levels deep", {
  cfg <- make_valid_config()
  cfg$paths$forexengine$db_dir <- file.path(
    tempfile("missing"),
    "deeper",
    "Databases"
  )

  con <- close_later(connect_paper_trading_db(cfg))

  expect_true(dir.exists(cfg$paths$forexengine$db_dir))
  expect_true(file.exists(paper_path(cfg)))
})

test_that("connecting alone does not create the table", {
  cfg <- make_valid_config()

  con <- close_later(connect_paper_trading_db(cfg))

  expect_false(DBI::dbExistsTable(con, "paper_trades"))
})

test_that("init_paper_trading_db returns an open connection with the table", {
  cfg <- make_valid_config()

  con <- close_later(init_paper_trading_db(cfg))

  expect_true(DBI::dbIsValid(con))
  expect_true(DBI::dbExistsTable(con, "paper_trades"))
})

test_that("the table has every column the app and readers use", {
  cfg <- make_valid_config()
  con <- close_later(init_paper_trading_db(cfg))

  needed <- c(
    "id",
    "signal_id",
    "ticker",
    "asset_class",
    "signal_datetime",
    "direction",
    "entry_price",
    "stop_loss",
    "take_profit",
    "risk_reward",
    "confluence",
    "signal_score",
    "status",
    "skip_reason",
    "actual_entry",
    "actual_sl",
    "actual_tp",
    "risk_pct",
    "position_size",
    "account_balance_before",
    "account_balance_after",
    "exit_price",
    "exit_datetime",
    "outcome",
    "exit_reason",
    "pnl_r",
    "pnl_pct",
    "emotion_rating",
    "notes",
    "created_at",
    "updated_at"
  )

  expect_equal(
    setdiff(needed, DBI::dbListFields(con, "paper_trades")),
    character(0)
  )
})

test_that("creating the table twice is safe and keeps the data", {
  cfg <- make_valid_config()
  con <- close_later(init_paper_trading_db(cfg))
  insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))

  res <- withVisible(create_paper_trades_table(con))
  create_paper_trades_table(con)

  expect_false(res$visible)
  expect_true(res$value)
  expect_equal(nrow(read_paper(con)), 1L)
})

test_that("starting again keeps the saved trades", {
  cfg <- make_valid_config()
  first <- init_paper_trading_db(cfg)
  insert_paper_trade(first, make_paper_trade("p-1", "2026-09-01 08:00:00"))
  DBI::dbDisconnect(first)

  second <- close_later(init_paper_trading_db(cfg))

  expect_equal(read_paper(second)$signal_id, "p-1")
})

test_that("a new trade gets the table's defaults", {
  cfg <- make_valid_config()
  con <- close_later(init_paper_trading_db(cfg))

  DBI::dbExecute(
    con,
    "INSERT INTO paper_trades
       (signal_id, ticker, signal_datetime, direction, entry_price, stop_loss, take_profit)
     VALUES ('p-1', 'eurusd', '2026-09-01 08:00:00', 'long', 1.10, 1.09, 1.12)"
  )
  row <- read_paper(con)

  expect_equal(row$id, 1L)
  expect_equal(row$status, "pending")
  expect_match(row$created_at, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
})

test_that("stops with an error when the database cannot be opened", {
  cfg <- make_valid_config()
  blocker <- tempfile("blocker")
  writeLines("not a folder", blocker)
  cfg$paths$forexengine$db_dir <- file.path(blocker, "Databases")

  expect_error(suppressWarnings(init_paper_trading_db(cfg)))
})

test_that("stops when the config has no database folder", {
  cfg <- make_valid_config()
  cfg$paths$forexengine$db_dir <- NULL

  expect_error(init_paper_trading_db(cfg))
})
