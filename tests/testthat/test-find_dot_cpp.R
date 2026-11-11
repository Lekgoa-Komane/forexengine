# tests/testthat/test-find_dot_cpp.R

make_targets <- function(value, label, wstart, wend, fid, tid) {
  list(
    dol_value = as.numeric(value),
    dol_label = as.character(label),
    window_start = as.numeric(wstart),
    window_end = as.numeric(wend),
    formed_bias_id = as.integer(fid),
    type_id = as.integer(tid)
  )
}

test_that("a row inside a target's window with the same bias id gets its value, label and id", {
  res <- find_dot_cpp(
    close = 1.10,
    join_date = 5,
    bias_id = 1L,
    targets = make_targets(1.15, "pdh", 1, 10, 1L, 7L)
  )

  expect_equal(res$dot, 1.15)
  expect_equal(res$dot_name, "pdh")
  expect_equal(res$dot_id, 7L)
})

test_that("the result has the right types", {
  res <- find_dot_cpp(
    close = 1.10,
    join_date = 5,
    bias_id = 1L,
    targets = make_targets(1.15, "pdh", 1, 10, 1L, 7L)
  )

  expect_type(res$dot, "double")
  expect_type(res$dot_name, "character")
  expect_type(res$dot_id, "integer")
})

test_that("the window includes its start and excludes its end", {
  res <- find_dot_cpp(
    close = rep(1.10, 4),
    join_date = c(0.5, 1, 9.9, 10),
    bias_id = rep(1L, 4),
    targets = make_targets(1.15, "pdh", 1, 10, 1L, 7L)
  )

  expect_equal(!is.na(res$dot), c(FALSE, TRUE, TRUE, FALSE))
})

test_that("a target with a different bias id is not used", {
  res <- find_dot_cpp(
    close = 1.10,
    join_date = 5,
    bias_id = 2L,
    targets = make_targets(1.15, "pdh", 1, 10, 1L, 7L)
  )

  expect_true(is.na(res$dot))
  expect_true(is.na(res$dot_name))
  expect_true(is.na(res$dot_id))
})

test_that("with several targets, the one closest to the close is chosen", {
  targets <- make_targets(
    value = c(1.20, 1.12),
    label = c("far", "near"),
    wstart = c(1, 1),
    wend = c(10, 10),
    fid = c(1L, 1L),
    tid = c(1L, 2L)
  )

  res <- find_dot_cpp(1.10, 5, 1L, targets)

  expect_equal(res$dot, 1.12)
  expect_equal(res$dot_name, "near")
  expect_equal(res$dot_id, 2L)
})

test_that("the order of the targets does not change which is closest", {
  targets <- make_targets(
    value = c(1.12, 1.20),
    label = c("near", "far"),
    wstart = c(1, 1),
    wend = c(10, 10),
    fid = c(1L, 1L),
    tid = c(2L, 1L)
  )

  res <- find_dot_cpp(1.10, 5, 1L, targets)

  expect_equal(res$dot_name, "near")
})

test_that("a target below the close can be the closest as well", {
  targets <- make_targets(
    value = c(1.20, 1.09),
    label = c("above", "below"),
    wstart = c(1, 1),
    wend = c(10, 10),
    fid = c(1L, 1L),
    tid = c(1L, 2L)
  )

  res <- find_dot_cpp(1.10, 5, 1L, targets)

  expect_equal(res$dot_name, "below")
})

test_that("when two targets are equally close, the first one listed wins", {
  targets <- make_targets(
    value = c(8, 12),
    label = c("first", "second"),
    wstart = c(1, 1),
    wend = c(10, 10),
    fid = c(1L, 1L),
    tid = c(1L, 2L)
  )

  res <- find_dot_cpp(10, 5, 1L, targets)

  expect_equal(res$dot_name, "first")
})

test_that("each row picks its own target", {
  targets <- make_targets(
    value = c(1.15, 1.30),
    label = c("A", "B"),
    wstart = c(1, 10),
    wend = c(10, 20),
    fid = c(1L, 2L),
    tid = c(1L, 2L)
  )

  res <- find_dot_cpp(
    close = c(1.10, 1.25),
    join_date = c(5, 15),
    bias_id = c(1L, 2L),
    targets = targets
  )

  expect_equal(res$dot, c(1.15, 1.30))
  expect_equal(res$dot_name, c("A", "B"))
  expect_equal(res$dot_id, c(1L, 2L))
})

test_that("a row with no matching target stays NA while its neighbour matches", {
  res <- find_dot_cpp(
    close = c(1.10, 1.10),
    join_date = c(5, 50),
    bias_id = c(1L, 1L),
    targets = make_targets(1.15, "pdh", 1, 10, 1L, 7L)
  )

  expect_equal(res$dot, c(1.15, NA))
  expect_equal(res$dot_name, c("pdh", NA))
  expect_equal(res$dot_id, c(7L, NA))
})

test_that("with no targets, every result is NA", {
  none <- make_targets(
    numeric(0),
    character(0),
    numeric(0),
    numeric(0),
    integer(0),
    integer(0)
  )

  res <- find_dot_cpp(c(1.10, 1.11), c(5, 6), c(1L, 1L), none)

  expect_true(all(is.na(res$dot)))
  expect_true(all(is.na(res$dot_name)))
  expect_true(all(is.na(res$dot_id)))
})

test_that("with no rows, the result is empty", {
  res <- find_dot_cpp(
    numeric(0),
    numeric(0),
    integer(0),
    make_targets(1.15, "pdh", 1, 10, 1L, 7L)
  )

  expect_length(res$dot, 0L)
  expect_length(res$dot_name, 0L)
  expect_length(res$dot_id, 0L)
})

# This one fails until line 46 of dot_fast.cpp is changed to
# `if (!NumericVector::is_na(target_v)) {`
test_that("a target with a missing value is ignored", {
  only_na <- make_targets(NA_real_, "ghost", 1, 10, 1L, 9L)
  res_only <- find_dot_cpp(1.10, 5, 1L, only_na)

  expect_true(is.na(res_only$dot))
  expect_true(is.na(res_only$dot_name))
  expect_true(is.na(res_only$dot_id))

  mixed <- make_targets(
    value = c(NA_real_, 1.12),
    label = c("ghost", "real"),
    wstart = c(1, 1),
    wend = c(10, 10),
    fid = c(1L, 1L),
    tid = c(9L, 2L)
  )
  res_mixed <- find_dot_cpp(1.10, 5, 1L, mixed)

  expect_equal(res_mixed$dot_name, "real")
})
