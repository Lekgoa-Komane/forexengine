# tests/testthat/test-connect_journal_db.R

# Where the journal database should be, worked out from the config
journal_path <- function(cfg) {
  file.path(
    cfg$paths$forexengine$db_dir,
    paste0(cfg$database$table_prefixes$journal, cfg$database$db_suffix)
  )
}

# Open the journal and close it again when the test ends
connect_for_test <- function(cfg, env = parent.frame()) {
  con <- connect_journal_db(cfg)
  withr::defer(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con),
    envir = env
  )
  con
}

# The smallest row the table accepts
add_minimal_trade <- function(con, signal_id = "s1") {
  DBI::dbExecute(
    con,
    "INSERT INTO trades (ticker, signal_id, entry_price, stop_loss, take_profit)
     VALUES (?, ?, 1.10, 1.09, 1.12)",
    params = list("eurusd", signal_id)
  )
}

count_trades <- function(con) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM trades")$n
}

# ---- The database file and folder -------------------------------------------

test_that("returns an open connection and creates the database file", {
  cfg <- make_valid_config()

  con <- connect_for_test(cfg)

  expect_true(DBI::dbIsValid(con))
  expect_true(file.exists(journal_path(cfg)))
  expect_equal(basename(journal_path(cfg)), "trading_journal.sqlite")
})

test_that("the file name comes from the config", {
  cfg <- make_valid_config()
  cfg$database$table_prefixes$journal <- "my_journal"

  con <- connect_for_test(cfg)

  expect_true(
    file.exists(file.path(cfg$paths$forexengine$db_dir, "my_journal.sqlite"))
  )
})

test_that("creates the folder when it is missing, even several levels deep", {
  cfg <- make_valid_config()
  cfg$paths$forexengine$db_dir <- file.path(
    tempfile("missing"),
    "deeper",
    "Databases"
  )
  expect_false(dir.exists(cfg$paths$forexengine$db_dir))

  con <- connect_for_test(cfg)

  expect_true(dir.exists(cfg$paths$forexengine$db_dir))
  expect_true(file.exists(journal_path(cfg)))
})

# ---- The table --------------------------------------------------------------

test_that("creates the trades table with every column the sync and readers use", {
  cfg <- make_valid_config()
  con <- connect_for_test(cfg)

  needed <- c(
    "id",
    "ticker",
    "asset_class",
    "timeframe",
    "signal_id",
    "signal_type",
    "bias_alignment",
    "trigger_type",
    "confluence",
    "entry_price",
    "stop_loss",
    "take_profit",
    "exit_price",
    "exit_reason",
    "risk_reward",
    "signal_score",
    "lot_size",
    "signal_occurred_at",
    "opened_at",
    "closed_at",
    "status",
    "outcome",
    "pnl_pips",
    "pnl_r",
    "pnl_pct",
    "hold_duration_hours",
    "candles_to_exit",
    "emotion_rating",
    "setup_quality",
    "mistakes",
    "notes",
    "tags",
    "created_at",
    "updated_at"
  )

  expect_true(DBI::dbExistsTable(con, "trades"))
  expect_equal(setdiff(needed, DBI::dbListFields(con, "trades")), character(0))
})

test_that("running it again is safe and keeps the data", {
  cfg <- make_valid_config()
  first <- connect_journal_db(cfg)
  add_minimal_trade(first)
  DBI::dbDisconnect(first)

  second <- connect_for_test(cfg)

  expect_equal(count_trades(second), 1L)
})

test_that("a new row gets the table's default values", {
  cfg <- make_valid_config()
  con <- connect_for_test(cfg)

  add_minimal_trade(con)
  row <- DBI::dbGetQuery(con, "SELECT * FROM trades")

  expect_equal(row$id, 1L)
  expect_equal(row$status, "open")
  expect_equal(row$timeframe, "h1")
  expect_equal(row$lot_size, 1)
  expect_match(row$created_at, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
})

# ---- The table's own rules --------------------------------------------------

test_that("the table refuses a repeated signal_id", {
  cfg <- make_valid_config()
  con <- connect_for_test(cfg)
  add_minimal_trade(con, "s1")

  expect_error(add_minimal_trade(con, "s1"), "UNIQUE")
  expect_equal(count_trades(con), 1L)
})

test_that("the table refuses a status that is not allowed", {
  cfg <- make_valid_config()
  con <- connect_for_test(cfg)

  expect_error(
    DBI::dbExecute(
      con,
      "INSERT INTO trades (ticker, signal_id, entry_price, stop_loss, take_profit, status)
       VALUES ('a', 'b', 1, 1, 1, 'weird')"
    ),
    "CHECK"
  )
})

test_that("the table accepts each of its four statuses", {
  cfg <- make_valid_config()
  con <- connect_for_test(cfg)

  for (st in c("open", "closed", "breakeven", "cancelled")) {
    expect_error(
      DBI::dbExecute(
        con,
        "INSERT INTO trades (ticker, signal_id, entry_price, stop_loss, take_profit, status)
         VALUES ('a', ?, 1, 1, 1, ?)",
        params = list(paste0("id-", st), st)
      ),
      NA,
      info = st
    )
  }
  expect_equal(count_trades(con), 4L)
})

test_that("the table refuses a rating outside 1 to 5", {
  cfg <- make_valid_config()
  con <- connect_for_test(cfg)

  expect_error(
    DBI::dbExecute(
      con,
      "INSERT INTO trades (ticker, signal_id, entry_price, stop_loss, take_profit, emotion_rating)
       VALUES ('a', 'b', 1, 1, 1, 6)"
    ),
    "CHECK"
  )
})

test_that("the table refuses a row with no entry price", {
  cfg <- make_valid_config()
  con <- connect_for_test(cfg)

  expect_error(
    DBI::dbExecute(
      con,
      "INSERT INTO trades (ticker, signal_id, stop_loss, take_profit)
       VALUES ('a', 'b', 1, 1)"
    ),
    "NOT NULL"
  )
})

# ---- An older database ------------------------------------------------------

test_that("an older table without exit_reason is upgraded and keeps its rows", {
  cfg <- make_valid_config()
  dir.create(
    cfg$paths$forexengine$db_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  old <- DBI::dbConnect(RSQLite::SQLite(), journal_path(cfg))
  DBI::dbExecute(
    old,
    "CREATE TABLE trades (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       ticker TEXT NOT NULL,
       signal_id TEXT NOT NULL UNIQUE,
       entry_price REAL NOT NULL,
       stop_loss REAL NOT NULL,
       take_profit REAL NOT NULL
     )"
  )
  DBI::dbExecute(
    old,
    "INSERT INTO trades (ticker, signal_id, entry_price, stop_loss, take_profit)
     VALUES ('eurusd', 's1', 1.1, 1.09, 1.12)"
  )
  DBI::dbDisconnect(old)

  con <- connect_for_test(cfg)

  expect_true("exit_reason" %in% DBI::dbListFields(con, "trades"))
  expect_equal(count_trades(con), 1L)
})

# ---- Failures ---------------------------------------------------------------

test_that("stops with an error when the database cannot be opened", {
  cfg <- make_valid_config()
  # a plain FILE where the folder should be, so nothing can be created under it
  blocker <- tempfile("blocker")
  writeLines("not a folder", blocker)
  cfg$paths$forexengine$db_dir <- file.path(blocker, "Databases")

  expect_error(suppressWarnings(connect_journal_db(cfg)))
})

test_that("stops when the config has no database folder", {
  cfg <- make_valid_config()
  cfg$paths$forexengine$db_dir <- NULL

  expect_error(connect_journal_db(cfg))
})

test_that("a file that is not a database stops with an error", {
  cfg <- make_valid_config()
  dir.create(
    cfg$paths$forexengine$db_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines("this is not a database", journal_path(cfg))

  expect_error(suppressWarnings(connect_journal_db(cfg)))
})
