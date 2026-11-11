# tests/testthat/test-build_result_path.R

test_that("builds base/folder/asset_class/table/ticker.parquet with no suffix", {
  cfg <- make_valid_config()

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )

  expect_equal(
    path,
    file.path(
      cfg$paths$forexengine$output_base,
      "weekly",
      "Currency_Pairs",
      "registry",
      "audcad.parquet"
    )
  )
})

test_that("the table name picks the table folder", {
  cfg <- make_valid_config()

  for (tbl in cfg$paths$forexengine$table_names) {
    path <- build_result_path(cfg, tbl, "daily", "Metals", "xauusd")
    expect_equal(basename(dirname(path)), tbl)
    expect_equal(basename(path), "xauusd.parquet")
  }
})

test_that("the folder name comes from the config, not from the timeframe key", {
  cfg <- make_valid_config()
  cfg$paths$frequency_folders$h1 <- "hourly_results"

  path <- build_result_path(cfg, "registry", "h1", "Currency_Pairs", "eurusd")

  # path is base/hourly_results/Currency_Pairs/registry/eurusd.parquet
  expect_equal(basename(dirname(dirname(dirname(path)))), "hourly_results")
})

test_that("gives one path per ticker when given vectors", {
  cfg <- make_valid_config()

  paths <- build_result_path(
    cfg,
    "registry",
    "weekly",
    c("Currency_Pairs", "Metals"),
    c("audcad", "xauusd")
  )

  expect_length(paths, 2L)
  expect_equal(basename(paths), c("audcad.parquet", "xauusd.parquet"))
  expect_equal(
    basename(dirname(dirname(paths))),
    c("Currency_Pairs", "Metals")
  )
})

test_that("only builds the path: no folder or file is created", {
  cfg <- make_valid_config()

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )

  expect_false(dir.exists(dirname(path)))
  expect_false(file.exists(path))
})

test_that("ignores the old paths$output_base key", {
  cfg <- make_valid_config()
  cfg$paths$output_base <- "/old/place"

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )

  expect_false(startsWith(path, "/old/place"))
})

test_that("stops when `table` is not exactly one value", {
  cfg <- make_valid_config()
  msg <- "`table` must be exactly one value"

  expect_error(
    build_result_path(
      cfg,
      c("registry", "bos_choch"),
      "weekly",
      "Currency_Pairs",
      "audcad"
    ),
    msg,
    fixed = TRUE
  )
  expect_error(
    build_result_path(cfg, NA_character_, "weekly", "Currency_Pairs", "audcad"),
    msg,
    fixed = TRUE
  )
  expect_error(
    build_result_path(cfg, NULL, "weekly", "Currency_Pairs", "audcad"),
    msg,
    fixed = TRUE
  )
})

test_that("stops when `timeframe` is not exactly one value", {
  cfg <- make_valid_config()
  msg <- "`timeframe` must be exactly one value"

  expect_error(
    build_result_path(
      cfg,
      "registry",
      c("weekly", "daily"),
      "Currency_Pairs",
      "audcad"
    ),
    msg,
    fixed = TRUE
  )
  expect_error(
    build_result_path(
      cfg,
      "registry",
      NA_character_,
      "Currency_Pairs",
      "audcad"
    ),
    msg,
    fixed = TRUE
  )
  expect_error(
    build_result_path(cfg, "registry", NULL, "Currency_Pairs", "audcad"),
    msg,
    fixed = TRUE
  )
})

test_that("stops when asset_class or ticker contains NA", {
  cfg <- make_valid_config()
  msg <- "`asset_class` and `ticker` must not contain NA"

  expect_error(
    build_result_path(cfg, "registry", "weekly", NA_character_, "audcad"),
    msg,
    fixed = TRUE
  )
  expect_error(
    build_result_path(
      cfg,
      "registry",
      "weekly",
      "Currency_Pairs",
      NA_character_
    ),
    msg,
    fixed = TRUE
  )
})

test_that("stops when the table is not one of the config's table names", {
  cfg <- make_valid_config()

  # signal_log is on purpose not allowed: it lives in SQLite, not parquet
  expect_error(
    build_result_path(cfg, "signal_log", "weekly", "Currency_Pairs", "audcad"),
    "`table` must be one of: registry, bos_choch, reversal_watch",
    fixed = TRUE
  )
})

test_that("stops when the forexengine config keys are missing", {
  cfg <- make_valid_config()
  cfg$paths$forexengine$output_base <- NULL
  expect_error(
    build_result_path(cfg, "registry", "weekly", "Currency_Pairs", "audcad"),
    "Config is missing `paths$forexengine$output_base`.",
    fixed = TRUE
  )

  cfg <- make_valid_config()
  cfg$paths$forexengine$table_names <- NULL
  expect_error(
    build_result_path(cfg, "registry", "weekly", "Currency_Pairs", "audcad"),
    "Config is missing `paths$forexengine$table_names`.",
    fixed = TRUE
  )

  cfg <- make_valid_config()
  cfg$paths$forexengine <- NULL
  expect_error(
    build_result_path(cfg, "registry", "weekly", "Currency_Pairs", "audcad"),
    "Config is missing `paths$forexengine$output_base`.",
    fixed = TRUE
  )
})

test_that("stops when the timeframe has no folder in the config", {
  cfg <- make_valid_config()

  expect_error(
    build_result_path(cfg, "registry", "yearly", "Currency_Pairs", "audcad"),
    "Config has no `paths$frequency_folders` entry for 'yearly'.",
    fixed = TRUE
  )
})
