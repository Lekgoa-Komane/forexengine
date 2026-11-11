# tests/testthat/test-save_result.R

# ---- Where the files go ----------------------------------------------------

test_that("writes one parquet file per ticker in the table's folder", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()

  paths <- save_result(reg, "registry", cfg)

  base <- cfg$paths$forexengine$output_base
  expected <- c(
    file.path(base, "weekly", "Currency_Pairs", "registry", "audcad.parquet"),
    file.path(base, "weekly", "Currency_Pairs", "registry", "eurusd.parquet"),
    file.path(base, "weekly", "Metals", "registry", "xauusd.parquet")
  )

  expect_setequal(paths, expected)
  expect_true(all(file.exists(expected)))
  expect_length(engine_files(cfg), 3L)
})

test_that("each table goes to its own folder", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()

  for (tbl in cfg$paths$forexengine$table_names) {
    save_result(reg, tbl, cfg)
  }

  base <- cfg$paths$forexengine$output_base
  for (tbl in cfg$paths$forexengine$table_names) {
    expect_true(
      file.exists(
        file.path(base, "weekly", "Currency_Pairs", tbl, "audcad.parquet")
      ),
      info = tbl
    )
  }
})

test_that("creates the folders that are missing", {
  cfg <- make_valid_config()
  base <- cfg$paths$forexengine$output_base
  expect_false(dir.exists(file.path(base, "weekly")))

  save_result(make_fake_registry(), "registry", cfg)

  expect_true(dir.exists(file.path(base, "weekly", "Metals", "registry")))
})

test_that("the timeframe in the table decides the timeframe folder", {
  cfg <- make_valid_config()

  save_result(make_fake_registry("daily"), "registry", cfg)

  base <- cfg$paths$forexengine$output_base
  expect_true(
    file.exists(
      file.path(base, "daily", "Currency_Pairs", "registry", "audcad.parquet")
    )
  )
  expect_false(dir.exists(file.path(base, "weekly")))
})

test_that("returns the written paths without printing them", {
  cfg <- make_valid_config()

  res <- withVisible(save_result(make_fake_registry(), "registry", cfg))

  expect_false(res$visible)
  expect_type(res$value, "character")
  expect_length(res$value, 3L)
})

# ---- What is inside the files ---------------------------------------------

test_that("the data reads back unchanged, column by column", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_result(reg, "registry", cfg)

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )
  back <- data.table::as.data.table(arrow::read_parquet(path))
  expected <- reg[ticker == "audcad"]

  # column ORDER may differ (ticker and asset_class are written first)
  expect_setequal(names(back), names(expected))
  expect_equal(nrow(back), nrow(expected))
  for (col in names(expected)) {
    expect_equal(back[[col]], expected[[col]], info = col)
  }
})

test_that("times stay UTC and NA stays NA", {
  cfg <- make_valid_config()
  save_result(make_fake_registry(), "registry", cfg)

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )
  back <- data.table::as.data.table(arrow::read_parquet(path))

  expect_s3_class(back$formed_at, "POSIXct")
  expect_equal(attr(back$formed_at, "tzone"), "UTC")
  expect_true(is.na(back$invalidated_at[3]))
  expect_true(is.na(back$type_zone_bot_value[1]))
})

test_that("the 'never happened' placeholder date is saved exactly as given", {
  cfg <- make_valid_config()
  save_result(make_fake_registry(), "registry", cfg)

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )
  back <- data.table::as.data.table(arrow::read_parquet(path))

  expect_equal(
    back$invalidated_at[2],
    as.POSIXct("2027-02-05 23:00:00", tz = "UTC")
  )
})

test_that("each file holds only its own ticker", {
  cfg <- make_valid_config()
  save_result(make_fake_registry(), "registry", cfg)

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "eurusd"
  )
  back <- data.table::as.data.table(arrow::read_parquet(path))

  expect_equal(nrow(back), 2L)
  expect_true(all(back$ticker == "eurusd"))
  expect_true(all(back$asset_class == "Currency_Pairs"))
})

# ---- Saving again ----------------------------------------------------------

test_that("saving twice replaces the files and does not add rows", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()

  save_result(reg, "registry", cfg)
  save_result(reg, "registry", cfg)

  expect_length(engine_files(cfg, "\\.parquet$"), 3L)

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )
  expect_equal(nrow(arrow::read_parquet(path)), 3L)
})

test_that("a smaller table replaces the old rows of that ticker", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_result(reg, "registry", cfg)

  save_result(reg[ticker == "audcad"][1:2], "registry", cfg)

  path <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )
  expect_equal(nrow(arrow::read_parquet(path)), 2L)
})

test_that("tickers that are not in the table are left alone", {
  cfg <- make_valid_config()
  reg <- make_fake_registry()
  save_result(reg, "registry", cfg)

  save_result(reg[ticker == "audcad"], "registry", cfg)

  eurusd <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "eurusd"
  )
  expect_true(file.exists(eurusd))
  expect_equal(nrow(arrow::read_parquet(eurusd)), 2L)
})

test_that("no temporary files are left behind", {
  cfg <- make_valid_config()

  save_result(make_fake_registry(), "registry", cfg)

  expect_length(engine_files(cfg, "\\.tmp$"), 0L)
})

# ---- Input checks (nothing may be written) --------------------------------

test_that("stops when `dt` is not a data.table", {
  cfg <- make_valid_config()

  expect_error(
    save_result(as.data.frame(make_fake_registry()), "registry", cfg),
    "`dt` must be a data.table.",
    fixed = TRUE
  )
  expect_length(engine_files(cfg), 0L)
})

test_that("stops when `dt` is empty", {
  cfg <- make_valid_config()

  expect_error(
    save_result(make_fake_registry()[0L], "registry", cfg),
    "`dt` is empty. Nothing to save.",
    fixed = TRUE
  )
  expect_length(engine_files(cfg), 0L)
})

test_that("stops when a needed column is missing", {
  cfg <- make_valid_config()

  for (col in c("ticker", "asset_class", "timeframe")) {
    x <- data.table::copy(make_fake_registry())
    x[, (col) := NULL]
    expect_error(
      save_result(x, "registry", cfg),
      paste0("Missing column(s): ", col),
      fixed = TRUE,
      info = col
    )
  }
  expect_length(engine_files(cfg), 0L)
})

test_that("stops when `timeframe` contains NA", {
  cfg <- make_valid_config()
  x <- data.table::copy(make_fake_registry())
  x[1L, timeframe := NA_character_]

  expect_error(
    save_result(x, "registry", cfg),
    "`timeframe` must not contain NA.",
    fixed = TRUE
  )
  expect_length(engine_files(cfg), 0L)
})

test_that("stops when the table holds more than one timeframe", {
  cfg <- make_valid_config()
  x <- data.table::copy(make_fake_registry())
  x[1L, timeframe := "daily"]

  expect_error(
    save_result(x, "registry", cfg),
    "`dt` must hold exactly one timeframe.",
    fixed = TRUE
  )
  expect_length(engine_files(cfg), 0L)
})

test_that("stops when the table name is not allowed", {
  cfg <- make_valid_config()

  expect_error(
    save_result(make_fake_registry(), "signal_log", cfg),
    "`table` must be one of: registry, bos_choch, reversal_watch",
    fixed = TRUE
  )
  expect_length(engine_files(cfg), 0L)
})

test_that("stops when the timeframe is not in the config", {
  cfg <- make_valid_config()

  expect_error(
    save_result(make_fake_registry("yearly"), "registry", cfg),
    "Config has no `paths$frequency_folders` entry for 'yearly'.",
    fixed = TRUE
  )
  expect_length(engine_files(cfg), 0L)
})

# ---- Failures while writing -------------------------------------------------

test_that("a failed write is re-thrown and leaves no half-written file", {
  cfg <- make_valid_config()

  # Fake a disk that fills up half way: the temp file is started, then it fails
  testthat::local_mocked_bindings(
    write_parquet = function(x, sink, ...) {
      writeLines("partial", sink)
      stop("disk full")
    },
    .package = "arrow"
  )

  expect_error(
    save_result(make_fake_registry(), "registry", cfg),
    "disk full",
    fixed = TRUE
  )
  expect_length(engine_files(cfg), 0L)
})

test_that("a failed rename is reported and the temp file is removed", {
  cfg <- make_valid_config()

  # A FOLDER sits where the file should go, so the rename cannot work
  # (on Linux a file cannot be renamed onto a folder)
  blocker <- build_result_path(
    cfg,
    "registry",
    "weekly",
    "Currency_Pairs",
    "audcad"
  )
  dir.create(blocker, recursive = TRUE)

  expect_error(
    suppressWarnings(save_result(make_fake_registry(), "registry", cfg)),
    "Could not move the temporary file into place",
    fixed = TRUE
  )
  expect_length(engine_files(cfg, "\\.tmp$"), 0L)
})
