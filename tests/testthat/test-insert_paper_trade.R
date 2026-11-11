# tests/testthat/test-insert_paper_trade.R

count_paper <- function(con) nrow(read_paper(con))

test_that("saves a trade with only the required fields, and defaults the rest", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$status <- NULL

  insert_paper_trade(con, trade)
  row <- read_paper(con)

  expect_equal(nrow(row), 1L)
  expect_equal(row$signal_id, "p-1")
  expect_equal(row$direction, "long")
  expect_equal(row$status, "pending")
  expect_true(is.na(row$actual_entry))
})

test_that("saves every field it is given", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- c(
    make_paper_trade("p-1", "2026-09-01 08:00:00"),
    list(
      risk_reward = 2,
      confluence = "3",
      signal_score = 3L,
      actual_entry = 1.101,
      actual_sl = 1.089,
      actual_tp = 1.121,
      risk_pct = 1.5,
      position_size = 0.02,
      account_balance_before = 20,
      notes = "placed by hand"
    )
  )

  insert_paper_trade(con, trade)
  row <- read_paper(con)

  expect_equal(row$status, "taken")
  expect_equal(row$actual_entry, 1.101)
  expect_equal(row$risk_pct, 1.5)
  expect_equal(row$account_balance_before, 20)
  expect_equal(row$notes, "placed by hand")
})

test_that("a skipped signal can be saved with its reason", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$status <- "skipped"
  trade$skip_reason <- "news at 13:30"

  insert_paper_trade(con, trade)
  row <- read_paper(con)

  expect_equal(row$status, "skipped")
  expect_equal(row$skip_reason, "news at 13:30")
})

test_that("accepts a one-row data.table or data.frame as well as a list", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)

  insert_paper_trade(
    con,
    data.table::as.data.table(make_paper_trade("p-1", "2026-09-01 08:00:00"))
  )
  insert_paper_trade(
    con,
    as.data.frame(make_paper_trade("p-2", "2026-09-02 08:00:00"))
  )

  expect_equal(read_paper(con)$signal_id, c("p-1", "p-2"))
})

test_that("returns TRUE without printing it", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)

  res <- withVisible(
    insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))
  )

  expect_false(res$visible)
  expect_true(res$value)
})

test_that("quotes and SQL words in a note are saved as plain text", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  nasty <- "it's a \"test\"; DROP TABLE paper_trades; --"
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$notes <- nasty

  insert_paper_trade(con, trade)

  expect_true(DBI::dbExistsTable(con, "paper_trades"))
  expect_equal(read_paper(con)$notes, nasty)
})

# ---- What it refuses ---------------------------------------------------------

test_that("names the required columns that are missing", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$ticker <- NULL
  trade$direction <- NULL

  expect_error(
    insert_paper_trade(con, trade),
    "Missing required columns: ticker, direction",
    fixed = TRUE
  )
  expect_equal(count_paper(con), 0L)
})

test_that("a trade without a signal_id is refused", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$signal_id <- NULL

  expect_error(insert_paper_trade(con, trade))
  expect_equal(count_paper(con), 0L)
})

test_that("a repeated signal_id is refused and the first trade stays", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))

  expect_error(
    insert_paper_trade(con, make_paper_trade("p-1", "2026-09-05 08:00:00")),
    "UNIQUE"
  )
  expect_equal(count_paper(con), 1L)
  expect_equal(read_paper(con)$signal_datetime, "2026-09-01 08:00:00")
})

test_that("a direction other than long or short is refused", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$direction <- "bullish"

  expect_error(insert_paper_trade(con, trade), "CHECK")
  expect_equal(count_paper(con), 0L)
})

test_that("a status that is not allowed is refused", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$status <- "weird"

  expect_error(insert_paper_trade(con, trade), "CHECK")
})

test_that("risk_pct must be from 1 to 2", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)

  with_risk <- function(id, risk) {
    trade <- make_paper_trade(id, "2026-09-01 08:00:00")
    trade$risk_pct <- risk
    trade
  }

  expect_error(insert_paper_trade(con, with_risk("p-1", 1)), NA)
  expect_error(insert_paper_trade(con, with_risk("p-2", 2)), NA)
  expect_error(insert_paper_trade(con, with_risk("p-3", 0.5)), "CHECK")
  expect_error(insert_paper_trade(con, with_risk("p-4", 3)), "CHECK")
  expect_equal(count_paper(con), 2L)
})

test_that("an emotion rating outside 1 to 5 is refused", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$emotion_rating <- 6L

  expect_error(insert_paper_trade(con, trade), "CHECK")
})

test_that("a missing entry price is refused", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$entry_price <- NA_real_

  expect_error(insert_paper_trade(con, trade), "NOT NULL")
  expect_equal(count_paper(con), 0L)
})

test_that("a field that is not in the table is refused", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$colour <- "red"

  expect_error(insert_paper_trade(con, trade))
  expect_equal(count_paper(con), 0L)
})

test_that("stops when the connection is closed", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  DBI::dbDisconnect(con)

  expect_error(
    insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))
  )
})
