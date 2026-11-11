# tests/testthat/test-bos_choch_flips.R

# A small price series for the BOS/CHoCH state machine.
# `volatility` is the same on every bar, so with choch_quant = 0.5 a close has
# to move more than 0.5 % beyond a level to count as a breach.
make_bars <- function(
  close,
  bias,
  sh = NULL,
  sl = NULL,
  high = NULL,
  low = NULL,
  volatility = 0.5
) {
  n <- length(close)
  data.table::data.table(
    high = if (is.null(high)) close + 0.001 else high,
    low = if (is.null(low)) close - 0.001 else low,
    close = close,
    swing_high = if (is.null(sh)) rep(NA_real_, n) else sh,
    swing_low = if (is.null(sl)) rep(NA_real_, n) else sl,
    bias = rep_len(bias, n),
    volatility = rep_len(volatility, n)
  )
}

NO_CHOCH <- "no_choch"
NO_BOS <- "no_bos"

# ---- The shape of the result --------------------------------------------------

test_that("the result has the nine documented vectors, one value per bar", {
  bars <- make_bars(
    c(1.09, 1.095, 1.11, 1.12),
    "bearish",
    sh = c(1.10, NA, NA, NA),
    sl = c(1.08, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_setequal(
    names(res),
    c(
      "prev_sh",
      "prev_sl",
      "bull_level",
      "bear_level",
      "choch_level",
      "choch_type",
      "bos_type",
      "sweep_type",
      "trend_state"
    )
  )
  for (nm in names(res)) {
    expect_length(res[[nm]], 4L)
  }
})

# ---- CHoCH: the trend flips ---------------------------------------------------

test_that("a break above the swing high in a bearish trend is a bull CHoCH", {
  bars <- make_bars(
    c(1.0900, 1.0950, 1.1100, 1.1200),
    "bearish",
    sh = c(1.10, NA, NA, NA),
    sl = c(1.08, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  # the event is on the breach bar only
  expect_equal(res$choch_type, c(NO_CHOCH, NO_CHOCH, "bull_choch", NO_CHOCH))
  expect_equal(res$bos_type, rep(NO_BOS, 4))
  # the level that was broken is reported on the breach bar
  expect_equal(res$choch_level, c(1.10, 1.10, 1.10, 1.08))
  # the broken level is cleared afterwards
  expect_equal(res$bull_level, c(1.10, 1.10, 1.10, NA))
  expect_equal(res$bear_level, rep(1.08, 4))
})

test_that("after a bull CHoCH the trend is bullish from the NEXT bar", {
  bars <- make_bars(
    c(1.0900, 1.0950, 1.1100, 1.1200),
    "bearish",
    sh = c(1.10, NA, NA, NA),
    sl = c(1.08, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(res$trend_state, c("bearish", "bearish", "bearish", "bullish"))
})

test_that("a break below the swing low in a bullish trend is a bear CHoCH", {
  bars <- make_bars(
    c(1.0900, 1.0850, 1.0700, 1.0600),
    "bullish",
    sh = c(1.10, NA, NA, NA),
    sl = c(1.08, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(res$choch_type, c(NO_CHOCH, NO_CHOCH, "bear_choch", NO_CHOCH))
  expect_equal(res$bos_type, rep(NO_BOS, 4))
  expect_equal(res$choch_level, c(1.08, 1.08, 1.08, 1.10))
  expect_equal(res$bear_level, c(1.08, 1.08, 1.08, NA))
  expect_equal(res$bull_level, rep(1.10, 4))
})

test_that("after a bear CHoCH the trend is bearish from the NEXT bar", {
  bars <- make_bars(
    c(1.0900, 1.0850, 1.0700, 1.0600),
    "bullish",
    sh = c(1.10, NA, NA, NA),
    sl = c(1.08, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(res$trend_state, c("bullish", "bullish", "bullish", "bearish"))
})

# ---- BOS: the trend continues, nothing flips --------------------------------

test_that("a break in the direction of the trend is a BOS and does not flip it", {
  up <- make_bars(
    c(1.0900, 1.0950, 1.1100, 1.1200),
    "bullish",
    sh = c(1.10, NA, NA, NA),
    sl = c(1.08, NA, NA, NA)
  )
  down <- make_bars(
    c(1.0900, 1.0850, 1.0700, 1.0600),
    "bearish",
    sh = c(1.10, NA, NA, NA),
    sl = c(1.08, NA, NA, NA)
  )

  res_up <- compute_dynamic_bos_choch(up, choch_quant = 0.5)
  res_down <- compute_dynamic_bos_choch(down, choch_quant = 0.5)

  expect_equal(res_up$bos_type, c(NO_BOS, NO_BOS, "bull_bos", NO_BOS))
  expect_equal(res_up$choch_type, rep(NO_CHOCH, 4))
  expect_equal(res_up$trend_state, rep("bullish", 4))
  expect_equal(res_up$choch_level, c(1.08, 1.08, 1.10, 1.08))

  expect_equal(res_down$bos_type, c(NO_BOS, NO_BOS, "bear_bos", NO_BOS))
  expect_equal(res_down$choch_type, rep(NO_CHOCH, 4))
  expect_equal(res_down$trend_state, rep("bearish", 4))
  expect_equal(res_down$choch_level, c(1.10, 1.10, 1.08, 1.10))
})

# ---- After a CHoCH --------------------------------------------------------------

test_that("after a CHoCH a fresh swing re-arms the level and the next break is a BOS", {
  # the bias column keeps saying "bearish"; the labels must follow the
  # internal trend state, which flipped at bar 3
  bars <- make_bars(
    close = c(1.09, 1.095, 1.11, 1.12, 1.125, 1.17),
    bias = "bearish",
    sh = c(1.10, NA, NA, NA, 1.15, NA),
    sl = c(1.08, NA, NA, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(
    res$choch_type,
    c(NO_CHOCH, NO_CHOCH, "bull_choch", NO_CHOCH, NO_CHOCH, NO_CHOCH)
  )
  expect_equal(
    res$bos_type,
    c(NO_BOS, NO_BOS, NO_BOS, NO_BOS, NO_BOS, "bull_bos")
  )
  expect_equal(
    res$trend_state,
    c("bearish", "bearish", "bearish", "bullish", "bullish", "bullish")
  )
  expect_equal(res$bull_level, c(1.10, 1.10, 1.10, NA, 1.15, 1.15))
  expect_equal(res$choch_level, c(1.10, 1.10, 1.10, 1.08, 1.08, 1.15))
})

test_that("a level is used once: more bars beyond it do not fire again", {
  bars <- make_bars(
    c(1.09, 1.095, 1.11, 1.12, 1.13, 1.14),
    "bearish",
    sh = c(1.10, NA, NA, NA, NA, NA),
    sl = c(1.08, NA, NA, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(sum(res$choch_type != NO_CHOCH), 1L)
  expect_equal(sum(res$bos_type != NO_BOS), 0L)
})

test_that("a newly confirmed swing replaces the previous one", {
  bars <- make_bars(
    rep(1.09, 4),
    "bullish",
    sh = c(1.10, NA, NA, 1.12),
    sl = c(1.08, NA, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(res$prev_sh, c(1.10, 1.10, 1.10, 1.12))
  expect_equal(res$bull_level, c(1.10, 1.10, 1.10, 1.12))
  expect_equal(res$prev_sl, rep(1.08, 4))
})

# ---- How big a move counts -----------------------------------------------------

test_that("a move smaller than the volatility threshold is not a breach", {
  # 1.103 is only 0.27 % above 1.10, below the 0.5 % threshold
  bars <- make_bars(
    c(1.0900, 1.1030, 1.1030),
    "bearish",
    sh = c(1.10, NA, NA),
    sl = c(1.08, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(res$choch_type, rep(NO_CHOCH, 3))
  expect_equal(res$bos_type, rep(NO_BOS, 3))
  expect_equal(res$bull_level, rep(1.10, 3))
  expect_equal(res$trend_state, rep("bearish", 3))
})

test_that("the size needed comes from the choch_quant quantile of volatility", {
  bars <- make_bars(
    close = c(1.09, 1.095, 1.11, 1.12, 1.13),
    bias = "bearish",
    sh = c(1.10, NA, NA, NA, NA),
    sl = c(1.08, NA, NA, NA, NA),
    volatility = c(0.2, 0.4, 0.6, 0.8, 1.0)
  )

  # the median is 0.6: the 0.9 % move at bar 3 counts
  middle <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)
  # the top value is 1.0: bar 3 is too small, bar 4 (1.8 %) counts
  strict <- compute_dynamic_bos_choch(bars, choch_quant = 1.0)

  expect_equal(which(middle$choch_type != NO_CHOCH), 3L)
  expect_equal(which(strict$choch_type != NO_CHOCH), 4L)
})

# ---- Where the trend state comes from ------------------------------------------

test_that("trend_state is set once, from the first bullish or bearish bias", {
  bars <- make_bars(rep(1.09, 4), bias = c(NA, "range", "bullish", "bearish"))

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  # NA and "range" do not seed it, and a later "bearish" does not change it
  expect_equal(res$trend_state, c(NA, NA, "bullish", "bullish"))
})

# ---- Liquidity sweeps -------------------------------------------------------------

test_that("a wick beyond a swing that closes back inside is a liquidity sweep", {
  bars <- make_bars(
    close = c(1.090, 1.095, 1.085),
    bias = "bullish",
    high = c(1.095, 1.105, 1.095),
    low = c(1.085, 1.090, 1.075),
    sh = c(1.10, NA, NA),
    sl = c(1.08, NA, NA)
  )

  res <- compute_dynamic_bos_choch(bars, choch_quant = 0.5)

  expect_equal(res$sweep_type, c(NA, "bull_liq_sweep", "bear_liq_sweep"))
})
