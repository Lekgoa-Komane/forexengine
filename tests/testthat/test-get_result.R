# tests/testthat/test-get_result.R

# Save the fake table (3 tickers, 2 asset classes) for one timeframe
save_fake <- function(cfg, table = "registry", timeframe = "weekly") {
  save_result(make_fake_registry(timeframe), table, cfg)
  invisible(cfg)
}

# ---- Reading everything back ----------------------------------------------

test_that("returns everything that was saved, sorted by asset class and ticker", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_fake(cfg)

  x <- get_result("registry", "weekly", config = cfg)

  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 6L)
  expect_setequal(names(x), names(reg))
  expect_equal(
    x$ticker,
    c("audcad", "audcad", "audcad", "eurusd", "eurusd", "xauusd")
  )
  expect_equal(x$asset_class, c(rep("Currency_Pairs", 5L), "Metals"))
})

test_that("rows inside one ticker keep the order they were saved in", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_fake(cfg)

  x <- get_result("registry", "weekly", config = cfg)

  expect_equal(
    x[ticker == "audcad"]$formed_at,
    reg[ticker == "audcad"]$formed_at
  )
  expect_equal(x[ticker == "eurusd"]$type, c("swing_low", "bull_fvg"))
})

test_that("always shows the files as they are now", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_result(reg, "registry", cfg)
  expect_equal(nrow(get_result("registry", "weekly", config = cfg)), 6L)

  save_result(reg[ticker == "audcad"][1:2], "registry", cfg)

  x <- get_result("registry", "weekly", ticker = "audcad", config = cfg)
  expect_equal(nrow(x), 2L)
})

# ---- Table and timeframe --------------------------------------------------

test_that("each timeframe only returns its own results", {
  cfg <- make_valid_config()
  save_fake(cfg, "registry", "weekly")
  save_fake(cfg, "registry", "daily")

  w <- get_result("registry", "weekly", config = cfg)
  d <- get_result("registry", "daily", config = cfg)

  expect_equal(nrow(w), 6L)
  expect_equal(nrow(d), 6L)
  expect_true(all(w$timeframe == "weekly"))
  expect_true(all(d$timeframe == "daily"))
})

test_that("each table only returns its own results", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_result(reg, "registry", cfg)
  save_result(reg[ticker == "audcad"], "bos_choch", cfg)

  bos <- get_result("bos_choch", "weekly", config = cfg)

  expect_equal(nrow(bos), 3L)
  expect_true(all(bos$ticker == "audcad"))
})

# ---- Filters --------------------------------------------------------------

test_that("filters by ticker, without needing the asset class", {
  cfg <- make_valid_config()
  save_fake(cfg)

  x <- get_result("registry", "weekly", ticker = "eurusd", config = cfg)

  expect_equal(nrow(x), 2L)
  expect_true(all(x$ticker == "eurusd"))
})

test_that("filters by several tickers at once", {
  cfg <- make_valid_config()
  save_fake(cfg)

  x <- get_result(
    "registry",
    "weekly",
    ticker = c("audcad", "xauusd"),
    config = cfg
  )

  expect_equal(nrow(x), 4L)
  expect_setequal(unique(x$ticker), c("audcad", "xauusd"))
})

test_that("filters by asset class", {
  cfg <- make_valid_config()
  save_fake(cfg)

  metals <- get_result(
    "registry",
    "weekly",
    asset_class = "Metals",
    config = cfg
  )
  both <- get_result(
    "registry",
    "weekly",
    asset_class = c("Currency_Pairs", "Metals"),
    config = cfg
  )

  expect_equal(nrow(metals), 1L)
  expect_equal(metals$ticker, "xauusd")
  expect_equal(nrow(both), 6L)
})

test_that("returns only the columns asked for, plus the three identity columns", {
  cfg <- make_valid_config()
  save_fake(cfg)

  x <- get_result("registry", "weekly", columns = "type", config = cfg)

  expect_setequal(names(x), c("ticker", "asset_class", "timeframe", "type"))
  expect_equal(nrow(x), 6L)
})

test_that("a column named twice is not returned twice", {
  cfg <- make_valid_config()
  save_fake(cfg)

  x <- get_result(
    "registry",
    "weekly",
    columns = c("ticker", "type"),
    config = cfg
  )

  expect_false(anyDuplicated(names(x)) > 0L)
})

test_that("a column that does not exist stops the call", {
  cfg <- make_valid_config()
  save_fake(cfg)

  expect_error(
    get_result("registry", "weekly", columns = "no_such_column", config = cfg),
    "get_result: could not read",
    fixed = TRUE
  )
})

# ---- A ticker with nothing saved ------------------------------------------

test_that("a known ticker with no file gives an empty table that keeps its columns", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_fake(cfg)

  # gbpjpy is in the config, but nothing was saved for it
  x <- get_result("registry", "weekly", ticker = "gbpjpy", config = cfg)

  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 0L)
  expect_setequal(names(x), names(reg))
})

test_that("the empty table also follows the `columns` argument", {
  cfg <- make_valid_config()
  save_fake(cfg)

  x <- get_result(
    "registry",
    "weekly",
    ticker = "gbpjpy",
    columns = "type",
    config = cfg
  )

  expect_equal(nrow(x), 0L)
  expect_setequal(names(x), c("ticker", "asset_class", "timeframe", "type"))
})

test_that("asking for one ticker with a file and one without gives the first", {
  cfg <- make_valid_config()
  save_fake(cfg)

  x <- get_result(
    "registry",
    "weekly",
    ticker = c("audcad", "gbpjpy"),
    config = cfg
  )

  expect_equal(nrow(x), 3L)
  expect_true(all(x$ticker == "audcad"))
})

test_that("a name that exists nowhere is treated as a typo and stops", {
  cfg <- make_valid_config()
  save_fake(cfg)

  expect_error(
    get_result("registry", "weekly", ticker = "zzzzzz", config = cfg),
    "Unknown ticker(s): zzzzzz.",
    fixed = TRUE
  )
  expect_error(
    get_result("registry", "weekly", ticker = "zzzzzz", config = cfg),
    "Tickers with registry results in timeframe 'weekly': audcad, eurusd, xauusd.",
    fixed = TRUE
  )
  # one typo stops the whole call, even when another ticker is fine
  expect_error(
    get_result(
      "registry",
      "weekly",
      ticker = c("audcad", "zzzzzz"),
      config = cfg
    ),
    "Unknown ticker(s): zzzzzz.",
    fixed = TRUE
  )
})

# ---- Asset class messages -------------------------------------------------

test_that("an unknown asset class is a typo and lists the valid ones", {
  cfg <- make_valid_config()
  save_fake(cfg)

  expect_error(
    get_result("registry", "weekly", asset_class = "Foo", config = cfg),
    "Unknown asset_class 'Foo'. Valid asset classes: Commodity_Futures, Currency_Pairs, Metals.",
    fixed = TRUE
  )
})

test_that("a real asset class with no results for the timeframe says which have", {
  cfg <- make_valid_config()
  save_fake(cfg)

  # Commodity_Futures is in the config but was never saved
  expect_error(
    get_result(
      "registry",
      "weekly",
      asset_class = "Commodity_Futures",
      config = cfg
    ),
    "'Commodity_Futures' has no weekly results. Asset classes with weekly results: Currency_Pairs, Metals.",
    fixed = TRUE
  )
})

test_that("an asset class with no files for THIS table says so", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_result(reg, "registry", cfg)
  save_result(reg[ticker == "audcad"], "bos_choch", cfg)

  # Metals has registry files, but no bos_choch files
  expect_error(
    get_result("bos_choch", "weekly", asset_class = "Metals", config = cfg),
    "'Metals' has no bos_choch files saved yet for timeframe 'weekly'.",
    fixed = TRUE
  )
})

# ---- Missing folders --------------------------------------------------------

test_that("stops when there is no folder for the timeframe", {
  cfg <- make_valid_config()

  expect_error(
    get_result("registry", "weekly", config = cfg),
    "No engine results folder for timeframe 'weekly'",
    fixed = TRUE
  )
})

test_that("stops when the timeframe folder is empty", {
  cfg <- make_valid_config()
  dir.create(file.path(cfg$paths$forexengine$output_base, "weekly"))

  expect_error(
    get_result("registry", "weekly", config = cfg),
    "No engine results found for timeframe 'weekly'",
    fixed = TRUE
  )
})

test_that("stops when nothing was saved for this table", {
  cfg <- make_valid_config()
  save_fake(cfg, "registry")

  expect_error(
    get_result("bos_choch", "weekly", config = cfg),
    "No bos_choch files found for timeframe 'weekly'",
    fixed = TRUE
  )
})

# ---- Files that should be ignored or that are broken ----------------------

test_that("temporary and other stray files are ignored", {
  cfg <- make_valid_config()
  save_fake(cfg)
  folder <- file.path(
    cfg$paths$forexengine$output_base,
    "weekly",
    "Currency_Pairs",
    "registry"
  )
  writeLines("half written", file.path(folder, "zzzzzz.parquet.tmp"))
  writeLines("some notes", file.path(folder, "notes.txt"))

  x <- get_result("registry", "weekly", config = cfg)

  expect_equal(nrow(x), 6L)
  expect_false("zzzzzz" %in% x$ticker)
})

test_that("a broken file stops the call and names the file", {
  cfg <- make_valid_config()
  save_fake(cfg)
  broken <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "eurusd"
  )
  writeLines("this is not parquet", broken)

  expect_error(
    get_result("registry", "weekly", config = cfg),
    "get_result: could not read 'eurusd.parquet'",
    fixed = TRUE
  )
})

test_that("only the files asked for are read", {
  cfg <- make_valid_config()
  save_fake(cfg)
  broken <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "eurusd"
  )
  writeLines("this is not parquet", broken)

  x <- get_result("registry", "weekly", ticker = "audcad", config = cfg)

  expect_equal(nrow(x), 3L)
})

# ---- Argument checks --------------------------------------------------------

test_that("`table` must be one non-empty text value", {
  cfg <- make_valid_config()
  msg <- "`table` must be one non-empty text value."

  expect_error(
    get_result(c("registry", "bos_choch"), "weekly", config = cfg),
    msg,
    fixed = TRUE
  )
  expect_error(get_result(NULL, "weekly", config = cfg), msg, fixed = TRUE)
  expect_error(
    get_result(NA_character_, "weekly", config = cfg),
    msg,
    fixed = TRUE
  )
  expect_error(get_result("", "weekly", config = cfg), msg, fixed = TRUE)
})

test_that("`timeframe` must be one non-empty text value", {
  cfg <- make_valid_config()
  msg <- "`timeframe` must be one non-empty text value."

  expect_error(
    get_result("registry", c("weekly", "daily"), config = cfg),
    msg,
    fixed = TRUE
  )
  expect_error(get_result("registry", NULL, config = cfg), msg, fixed = TRUE)
  expect_error(
    get_result("registry", NA_character_, config = cfg),
    msg,
    fixed = TRUE
  )
})

test_that("`ticker`, `asset_class` and `columns` must be text when given", {
  cfg <- make_valid_config()

  expect_error(
    get_result("registry", "weekly", ticker = 1, config = cfg),
    "`ticker` must be text (one or more non-empty values).",
    fixed = TRUE
  )
  expect_error(
    get_result("registry", "weekly", ticker = "", config = cfg),
    "`ticker` must be text (one or more non-empty values).",
    fixed = TRUE
  )
  expect_error(
    get_result("registry", "weekly", asset_class = NA_character_, config = cfg),
    "`asset_class` must be text (one or more non-empty values).",
    fixed = TRUE
  )
  expect_error(
    get_result("registry", "weekly", columns = 5, config = cfg),
    "`columns` must be text (one or more non-empty values).",
    fixed = TRUE
  )
})

test_that("stops when the table is not one of the config's table names", {
  cfg <- make_valid_config()

  expect_error(
    get_result("signal_log", "weekly", config = cfg),
    "`table` must be one of: registry, bos_choch, reversal_watch",
    fixed = TRUE
  )
})

test_that("stops when the forexengine config keys are missing", {
  cfg <- make_valid_config()
  cfg$paths$forexengine$table_names <- NULL
  expect_error(
    get_result("registry", "weekly", config = cfg),
    "Config is missing `paths$forexengine$table_names`.",
    fixed = TRUE
  )

  cfg <- make_valid_config()
  cfg$paths$forexengine$output_base <- NULL
  expect_error(
    get_result("registry", "weekly", config = cfg),
    "Config is missing `paths$forexengine$output_base`.",
    fixed = TRUE
  )
})

test_that("stops when the timeframe has no folder in the config", {
  cfg <- make_valid_config()

  expect_error(
    get_result("registry", "yearly", config = cfg),
    "Config has no `paths$frequency_folders` entry for 'yearly'.",
    fixed = TRUE
  )
})

test_that("with no asset classes in the config, an unlisted class just has no results", {
  cfg <- make_valid_config()
  save_fake(cfg)
  cfg$asset_classes$names <- NULL

  expect_error(
    get_result(
      "registry",
      "weekly",
      asset_class = "Commodity_Futures",
      config = cfg
    ),
    "'Commodity_Futures' has no weekly results. Asset classes with weekly results: Currency_Pairs, Metals.",
    fixed = TRUE
  )
})
