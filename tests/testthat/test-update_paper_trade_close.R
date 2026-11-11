# tests/testthat/test-update_paper_trade_close.R

# A paper trading database with two taken trades (ids 1 and 2)
paper_with_two <- function(cfg, env = parent.frame()) {
  con <- open_paper(cfg, env = env)
  insert_paper_trade(con, make_paper_trade("p-1", "2026-09-01 08:00:00"))
  insert_paper_trade(con, make_paper_trade("p-2", "2026-09-10 08:00:00"))
  con
}

full_close <- function() {
  list(
    exit_price = 1.12,
    exit_datetime = "2026-09-02 10:00:00",
    outcome = "win",
    exit_reason = "tp",
    pnl_r = 2,
    pnl_pct = 0.04,
    account_balance_after = 20.8,
    emotion_rating = 4L,
    notes = "done"
  )
}

# ---- What it changes ---------------------------------------------------------

test_that("saves the exit fields and sets the status to closed", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)

  update_paper_trade_close(con, 1, full_close())
  row <- paper_row(read_paper(con), "p-1")

  expect_equal(row$status, "closed")
  expect_equal(row$exit_price, 1.12)
  expect_equal(row$exit_datetime, "2026-09-02 10:00:00")
  expect_equal(row$outcome, "win")
  expect_equal(row$exit_reason, "tp")
  expect_equal(row$pnl_r, 2)
  expect_equal(row$pnl_pct, 0.04)
  expect_equal(row$account_balance_after, 20.8)
  expect_equal(row$emotion_rating, 4L)
  expect_equal(row$notes, "done")
})

test_that("only the trade with that id is changed", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)

  update_paper_trade_close(con, 1, full_close())
  other <- paper_row(read_paper(con), "p-2")

  expect_equal(other$status, "taken")
  expect_true(is.na(other$exit_price))
  expect_true(is.na(other$outcome))
})

test_that("fields that are not given are left as they were", {
  cfg <- make_valid_config()
  con <- open_paper(cfg)
  trade <- make_paper_trade("p-1", "2026-09-01 08:00:00")
  trade$notes <- "written when I opened it"
  insert_paper_trade(con, trade)

  update_paper_trade_close(con, 1, list(outcome = "loss", pnl_r = -1))
  row <- read_paper(con)

  expect_equal(row$notes, "written when I opened it")
  expect_equal(row$outcome, "loss")
  expect_true(is.na(row$exit_price))
})

test_that("the trade's own data is not touched", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)
  before <- read_paper(con)
  info <- full_close()

  update_paper_trade_close(con, 1, info)
  after <- read_paper(con)

  untouched <- setdiff(names(before), c(names(info), "status", "updated_at"))
  expect_equal(after[, untouched], before[, untouched])
})

test_that("updated_at moves for that trade only", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)
  DBI::dbExecute(
    con,
    "UPDATE paper_trades SET updated_at = '2020-01-01 00:00:00'"
  )

  update_paper_trade_close(con, 1, list(outcome = "win"))
  trades <- read_paper(con)

  expect_false(paper_row(trades, "p-1")$updated_at == "2020-01-01 00:00:00")
  expect_match(
    paper_row(trades, "p-1")$updated_at,
    "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  )
  expect_equal(paper_row(trades, "p-2")$updated_at, "2020-01-01 00:00:00")
})

test_that("returns TRUE without printing it", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)

  res <- withVisible(update_paper_trade_close(con, 1, list(outcome = "win")))

  expect_false(res$visible)
  expect_true(res$value)
})

test_that("the id can be a whole number, an integer or text", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)

  update_paper_trade_close(con, 1, list(notes = "numeric"))
  update_paper_trade_close(con, 2L, list(notes = "integer"))
  expect_equal(read_paper(con)$notes, c("numeric", "integer"))

  update_paper_trade_close(con, "1", list(notes = "text"))
  expect_equal(read_paper(con)$notes[1], "text")
})

test_that("a trade shows up as closed in get_paper_trades", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)

  update_paper_trade_close(con, 1, list(outcome = "win", pnl_r = 2))
  x <- get_paper_trades(con)

  expect_equal(x$status, c("closed", "taken"))
  expect_equal(x$pnl_r[1], 2)
})

# ---- What it refuses ---------------------------------------------------------

test_that("stops when no paper trade has that id, and changes nothing", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)
  before <- read_paper(con)

  expect_error(
    update_paper_trade_close(con, 99, list(outcome = "win")),
    "No paper trade has id 99.",
    fixed = TRUE
  )
  expect_equal(read_paper(con), before)
})

test_that("stops when id is not one whole number", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)
  msg <- "`id` must be one whole number."
  info <- list(outcome = "win")

  expect_error(update_paper_trade_close(con, NULL, info), msg, fixed = TRUE)
  expect_error(update_paper_trade_close(con, NA, info), msg, fixed = TRUE)
  expect_error(update_paper_trade_close(con, c(1, 2), info), msg, fixed = TRUE)
  expect_error(update_paper_trade_close(con, "abc", info), msg, fixed = TRUE)
  expect_error(update_paper_trade_close(con, 1.5, info), msg, fixed = TRUE)
})

test_that("stops when close_info is empty or has no field names", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)

  expect_error(
    update_paper_trade_close(con, 1, list()),
    "`close_info` is empty.",
    fixed = TRUE
  )
  expect_error(
    update_paper_trade_close(con, 1, NULL),
    "`close_info` is empty.",
    fixed = TRUE
  )
  expect_error(
    update_paper_trade_close(con, 1, list("win")),
    "`close_info` must be a named list.",
    fixed = TRUE
  )
})

test_that("stops on a field that cannot be changed here, and changes nothing", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)
  before <- read_paper(con)

  expect_error(
    update_paper_trade_close(con, 1, list(outcome = "win", status = "pending")),
    "Invalid close fields: status",
    fixed = TRUE
  )
  expect_error(
    update_paper_trade_close(con, 1, list(entry_price = 5)),
    "Invalid close fields: entry_price",
    fixed = TRUE
  )
  expect_equal(read_paper(con), before)
})

test_that("an outcome or rating the table does not allow is refused", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)

  expect_error(
    update_paper_trade_close(con, 1, list(outcome = "draw")),
    "CHECK"
  )
  expect_error(
    update_paper_trade_close(con, 1, list(emotion_rating = 9L)),
    "CHECK"
  )

  # the failed update did not half-close the trade
  expect_equal(paper_row(read_paper(con), "p-1")$status, "taken")
})

test_that("stops when the connection is closed", {
  cfg <- make_valid_config()
  con <- paper_with_two(cfg)
  DBI::dbDisconnect(con)

  expect_error(update_paper_trade_close(con, 1, list(outcome = "win")))
})
