# build_path --------------------------------------------------------------

#' Build the file path of a saved engine result
#'
#'   forexengine output_base / folder / asset_class / table / ticker.parquet
#'
#' @param config The config list from `load_config()`. Needs
#'   `paths$forexengine$output_base`, `paths$forexengine$table_names` and
#'   `paths$frequency_folders`.
#' @param table ONE table name, one of `paths$forexengine$table_names`.
#' @param timeframe ONE timeframe key, for example "weekly" or "h1".
#' @param asset_class Asset class name(s). Can be a vector.
#' @param ticker Ticker name(s), same length as `asset_class`.
#'
#' @return A character vector of file paths (one per ticker).
#' @export
build_result_path <- function(config, table, timeframe, asset_class, ticker) {
  table <- as.character(table)
  timeframe <- as.character(timeframe)

  if (length(table) != 1L || is.na(table)) {
    stop(
      "`table` must be exactly one value, for example \"registry\".",
      call. = FALSE
    )
  }
  if (length(timeframe) != 1L || is.na(timeframe)) {
    stop(
      "`timeframe` must be exactly one value, for example \"weekly\".",
      call. = FALSE
    )
  }
  if (anyNA(asset_class) || anyNA(ticker)) {
    stop("`asset_class` and `ticker` must not contain NA.", call. = FALSE)
  }

  base <- config$paths$forexengine$output_base
  table_names <- config$paths$forexengine$table_names
  folder <- config$paths$frequency_folders[[timeframe]]

  if (is.null(base)) {
    stop("Config is missing `paths$forexengine$output_base`.", call. = FALSE)
  }
  if (is.null(table_names)) {
    stop("Config is missing `paths$forexengine$table_names`.", call. = FALSE)
  }
  if (!table %in% table_names) {
    stop(
      "`table` must be one of: ",
      paste(table_names, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(folder)) {
    stop(
      "Config has no `paths$frequency_folders` entry for '",
      timeframe,
      "'.",
      call. = FALSE
    )
  }

  file.path(base, folder, asset_class, table, paste0(ticker, ".parquet"))
}


# save_results ------------------------------------------------------------

#' Save an engine result table to parquet, one file per ticker
#'
#' @param dt A data.table with columns `timeframe`, `ticker` and
#'   `asset_class`. It must hold ONE timeframe, and its value must match a
#'   key in the config (for example "weekly" or "h1").
#' @param table ONE table name, one of `paths$forexengine$table_names`.
#' @param config The config list from `load_config()`.
#'
#' @return Invisibly, the file paths that were written.
#' @export
save_result <- function(dt, table, config) {
  logger::log_info("Starting to save engine result")

  # ---- 1. Safety checks ----
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.", call. = FALSE)
  }
  if (nrow(dt) == 0L) {
    stop("`dt` is empty. Nothing to save.", call. = FALSE)
  }

  needed <- c("timeframe", "ticker", "asset_class")
  missing <- setdiff(needed, names(dt))
  if (length(missing) > 0L) {
    stop("Missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyNA(dt$timeframe)) {
    stop("`timeframe` must not contain NA.", call. = FALSE)
  }

  # A COLUMN called `timeframe` wins over a variable with the same name
  # inside data.table's j, so the single values are kept under other names.
  tf_key <- as.character(unique(dt$timeframe))
  tbl_key <- table
  if (length(tf_key) != 1L) {
    stop(
      "`dt` must hold exactly one timeframe. Found: ",
      paste(tf_key, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 2. Work out every address ONCE (also checks the table name) ----
  groups <- unique(dt[, .(asset_class, ticker)])
  target_paths <- build_result_path(
    config,
    tbl_key,
    tf_key,
    groups$asset_class,
    groups$ticker
  )

  for (d in unique(dirname(target_paths))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # ---- 3. Write each ticker safely (temp file first, then swap in) ----
  logger::log_info("Saving ", tbl_key, " by asset_class and ticker")

  paths <- tryCatch(
    {
      dt[,
        {
          final <- build_result_path(
            config,
            tbl_key,
            tf_key,
            asset_class,
            ticker
          )
          tmp <- paste0(final, ".tmp")

          tryCatch(
            {
              arrow::write_parquet(
                data.table::as.data.table(c(.BY, .SD)),
                tmp,
                compression = "zstd"
              )
              if (!file.rename(tmp, final)) {
                stop("Could not move the temporary file into place: ", final)
              }
            },
            error = function(e) {
              unlink(tmp)
              stop(e)
            }
          )

          .(path = final)
        },
        by = .(asset_class, ticker)
      ]$path
    },
    error = function(e) {
      logger::log_error(paste0("Saving failed: ", conditionMessage(e)))
      stop(e)
    }
  )

  logger::log_info(paste0(
    "Saved ",
    length(paths),
    " file(s) for ",
    tbl_key,
    " / ",
    tf_key
  ))
  invisible(paths)
}


# get_results -------------------------------------------------------------

#' Get saved engine results back out (the "serving window")
#'
#' Reads the parquet files that [save_result()] wrote and gives you one
#' data.table. Nothing is stored inside the package, so the result always
#' reflects the files on your drive right now.
#'
#' The WHOLE history of each ticker is returned. Cutting the data down to a
#' date window is left to the chart, so you can scroll back and still see
#' every zone and level.
#'
#' @param table One table name from the config: "registry", "bos_choch" or
#'   "reversal_watch".
#' @param timeframe One timeframe key from the config, for example "weekly",
#'   "daily", "h4" or "h1".
#' @param ticker Optional. One or more tickers, for example "eurusd".
#'   Default: all tickers. A real ticker that has no saved rows in this table
#'   gives an empty table (not an error), because a ticker with nothing to
#'   draw simply has no file.
#' @param asset_class Optional. One or more asset classes, for example
#'   "Metals". Default: all asset classes that have results. Asking for one
#'   without results stops with a message that lists the ones that have.
#' @param columns Optional. Names of the columns you want. `ticker`,
#'   `asset_class` and `timeframe` are always returned, so every row can be
#'   identified. Default: all columns.
#' @param config The config list from [load_config()]. Pass it in when you
#'   call this many times, so the config file is not read again each time.
#'
#' @return A data.table sorted by `asset_class` and `ticker`. Rows inside one
#'   ticker keep the order they were saved in. When nothing is found for the
#'   ticker you asked for, it has zero rows but still has all the columns.
#' @export
#'
#' @examples
#' \dontrun{
#' get_result("registry", "weekly", ticker = "eurusd")
#' get_result("bos_choch", "h1", asset_class = "Metals")
#' get_result("reversal_watch", "daily")
#' }
get_result <- function(
  table,
  timeframe,
  ticker = NULL,
  asset_class = NULL,
  columns = NULL,
  config
) {
  # ---- 1. Check the arguments ----
  check_text(table, "table", one = TRUE, allow_null = FALSE)
  check_text(timeframe, "timeframe", one = TRUE, allow_null = FALSE)
  check_text(ticker, "ticker")
  check_text(asset_class, "asset_class")
  check_text(columns, "columns")

  # Different names from the data.table columns, so they cannot be mixed up
  tbl_key <- table
  tf_key <- timeframe
  wanted_tickers <- ticker
  wanted_classes <- asset_class

  table_names <- config$paths$forexengine$table_names
  if (is.null(table_names)) {
    stop("Config is missing `paths$forexengine$table_names`.", call. = FALSE)
  }
  if (!tbl_key %in% table_names) {
    stop(
      "`table` must be one of: ",
      paste(table_names, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 2. Find the timeframe folder ----
  base <- config$paths$forexengine$output_base
  folder <- config$paths$frequency_folders[[tf_key]]

  if (is.null(base)) {
    stop("Config is missing `paths$forexengine$output_base`.", call. = FALSE)
  }
  if (is.null(folder)) {
    stop(
      "Config has no `paths$frequency_folders` entry for '",
      tf_key,
      "'.",
      call. = FALSE
    )
  }

  tf_dir <- file.path(base, folder)
  if (!dir.exists(tf_dir)) {
    stop(
      "No engine results folder for timeframe '",
      tf_key,
      "' at: ",
      tf_dir,
      ". Is the drive connected?",
      call. = FALSE
    )
  }

  # The asset-class folders sit directly inside the timeframe folder.
  # What is on the drive decides which asset classes have results.
  all_classes <- list.dirs(tf_dir, full.names = FALSE, recursive = FALSE)
  if (length(all_classes) == 0L) {
    stop(
      "No engine results found for timeframe '",
      tf_key,
      "' in: ",
      tf_dir,
      call. = FALSE
    )
  }

  classes <- all_classes
  if (!is.null(wanted_classes)) {
    not_on_drive <- setdiff(wanted_classes, all_classes)

    if (length(not_on_drive) > 0L) {
      config_classes <- config$asset_classes$names
      typos <- if (is.null(config_classes)) {
        character(0)
      } else {
        setdiff(not_on_drive, config_classes)
      }

      if (length(typos) > 0L) {
        stop(
          "Unknown asset_class '",
          paste(typos, collapse = "', '"),
          "'. Valid asset classes: ",
          paste(sort(union(config_classes, all_classes)), collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      stop(
        "'",
        paste(not_on_drive, collapse = "', '"),
        "' ",
        if (length(not_on_drive) > 1L) "have" else "has",
        " no ",
        tf_key,
        " results. Asset classes with ",
        tf_key,
        " results: ",
        paste(sort(all_classes), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    classes <- wanted_classes
  }

  # ---- 3. List the files (folders come from the one map) ----
  class_dirs <- dirname(
    build_result_path(config, tbl_key, tf_key, classes, "x")
  )

  found <- data.table::rbindlist(lapply(seq_along(classes), function(i) {
    files <- list.files(
      class_dirs[i],
      pattern = "\\.parquet$",
      full.names = TRUE
    )
    data.table::data.table(
      file = files,
      ticker = sub("\\.parquet$", "", basename(files)),
      asset_class = rep(classes[i], length(files))
    )
  }))

  if (!is.null(wanted_classes)) {
    no_files <- setdiff(wanted_classes, found$asset_class)
    if (length(no_files) > 0L) {
      stop(
        "'",
        paste(no_files, collapse = "', '"),
        "' ",
        if (length(no_files) > 1L) "have" else "has",
        " no ",
        tbl_key,
        " files saved yet for timeframe '",
        tf_key,
        "'.",
        call. = FALSE
      )
    }
  }

  if (nrow(found) == 0L) {
    stop(
      "No ",
      tbl_key,
      " files found for timeframe '",
      tf_key,
      "' in: ",
      tf_dir,
      call. = FALSE
    )
  }

  # One file of this table, kept to give an empty result its columns
  template_file <- found$file[1L]

  if (!is.null(wanted_tickers)) {
    missing_tickers <- setdiff(wanted_tickers, found$ticker)

    if (length(missing_tickers) > 0L) {
      # A ticker that exists in the config but has no file just has nothing
      # saved. A name that exists nowhere is a typo, and stops the call.
      known <- unique(c(
        unlist(lapply(
          config$asset_classes$names,
          function(a) config$asset_classes[[a]]$tickers
        )),
        found$ticker
      ))
      typos <- setdiff(missing_tickers, known)

      if (length(typos) > 0L) {
        available <- sort(unique(found$ticker))
        shown <- utils::head(available, 20L)
        stop(
          "Unknown ticker(s): ",
          paste(typos, collapse = ", "),
          ". Tickers with ",
          tbl_key,
          " results in timeframe '",
          tf_key,
          "': ",
          paste(shown, collapse = ", "),
          if (length(available) > length(shown)) ", ..." else "",
          ".",
          call. = FALSE
        )
      }

      logger::log_info(paste0(
        "get_result: no ",
        tbl_key,
        " saved for ",
        paste(missing_tickers, collapse = ", "),
        " in timeframe '",
        tf_key,
        "'"
      ))
    }

    found <- found[ticker %in% wanted_tickers]
  }

  # ---- 4. Read them (any unreadable file stops the call) ----
  # Note: col_select must get either a literal choice or nothing at all.
  # Passing a variable that holds NULL makes arrow select zero columns.
  read_cols <- if (!is.null(columns)) {
    unique(c("ticker", "asset_class", "timeframe", columns))
  } else {
    NULL
  }

  read_one <- function(f) {
    tryCatch(
      if (is.null(read_cols)) {
        data.table::as.data.table(arrow::read_parquet(f))
      } else {
        data.table::as.data.table(arrow::read_parquet(
          f,
          col_select = tidyselect::all_of(read_cols)
        ))
      },
      error = function(e) {
        stop(
          "get_result: could not read '",
          basename(f),
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # Nothing saved for the ticker(s) asked for: zero rows, columns kept
  if (nrow(found) == 0L) {
    result <- read_one(template_file)[0L]
    logger::log_info(paste0(
      "get_result: 0 row(s), table '",
      tbl_key,
      "', timeframe '",
      tf_key,
      "'"
    ))
    return(result[])
  }

  result <- data.table::rbindlist(
    lapply(found$file, read_one),
    use.names = TRUE
  )

  # ---- 5. Order ----
  data.table::setorderv(result, c("asset_class", "ticker"))

  logger::log_info(paste0(
    "get_result: ",
    nrow(result),
    " row(s) from ",
    nrow(found),
    " file(s), table '",
    tbl_key,
    "', timeframe '",
    tf_key,
    "'"
  ))
  result[]
}


# ---- internal helper (not exported) ---

# Same check as in forexdatasets (copied, because that one is internal)
check_text <- function(x, name, one = FALSE, allow_null = TRUE) {
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    stop("`", name, "` must be one non-empty text value.", call. = FALSE)
  }
  bad <- !is.character(x) ||
    length(x) == 0L ||
    anyNA(x) ||
    any(!nzchar(x)) ||
    (one && length(x) != 1L)
  if (bad) {
    if (one) {
      stop("`", name, "` must be one non-empty text value.", call. = FALSE)
    }
    stop(
      "`",
      name,
      "` must be text (one or more non-empty values).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

################################################################################

# trading_journal ---------------------------------------------------------

#' Connect to the trading journal SQLite database
#'
#' Opens a connection to the SQLite database and creates the `trades` table
#' if it does not already exist. The database path is constructed from the
#' configuration object passed by the `targets` pipeline.
#'
#' @param config A list containing database configuration. Expected elements:
#'   \itemize{
#'     \item \code{config$paths$forexengine$db_dir} directory where the database
#'      file should be stored.
#'     \item \code{config$database$db_suffix} file suffix (e.g., \code{".sqlite"}).
#'     \item \code{config$database$table_prefixes$journal} prefix for the
#'           journal table name.
#'   }
#'
#' @return A DBI connection object to the SQLite database.
#' @export
#'
#' @importFrom DBI dbConnect dbExecute dbIsValid dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom logger log_info log_error
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' con <- connect_journal_db(config)
#' dbListTables(con)
#' dbDisconnect(con)
#' }

connect_journal_db <- function(config) {
  log_info("Starting SQLite database table creation")

  # ============================================================================
  # PRE-DEFINE DATABASE FILTERS (from config)
  # ============================================================================
  log_info(
    paste0("Pre-defining database filters")
  )

  db_dir <- config$paths$forexengine$db_dir
  db_suffix <- config$database$db_suffix
  journal_name <- config$database$table_prefixes$journal

  db_path <- file.path(db_dir, paste0(journal_name, db_suffix))

  log_info("Database filters loaded from config")

  # ============================================================================
  # DATABASE TABLE CREATION
  # ============================================================================
  con <- NULL
  con <- tryCatch(
    {
      log_info(paste0("Connecting to trading journal database at ", db_path))

      # --- 1. Ensure the directory exists -------------------------------
      # If the directory part of db_path does not exist, create it.
      if (!file.exists(db_dir)) {
        dir.create(db_dir, recursive = TRUE)
      }

      # --- 2. Connect to the SQLite database ----------------------------
      # If the file doesn't exist, SQLite will create it automatically.
      con <- dbConnect(SQLite(), db_path)

      # --- 3. Create the trades table -----------------------------------
      # The table definition is exactly what we agreed upon.
      # `IF NOT EXISTS` ensures this is safe to run every time.
      dbExecute(
        con,
        "
          CREATE TABLE IF NOT EXISTS trades (
            id INTEGER PRIMARY KEY AUTOINCREMENT,

            -- Identity & market
            ticker TEXT NOT NULL,
            asset_class TEXT,
            timeframe TEXT DEFAULT 'h1',
            signal_id TEXT NOT NULL UNIQUE,   -- unique identifier from bt

            -- Signal & bias
            signal_type TEXT,
            bias_alignment TEXT,
            trigger_type TEXT,
            confluence TEXT,
            weekly_bias TEXT,
            daily_bias TEXT,
            h4_bias TEXT,
            h1_bias TEXT,
            reason TEXT,

            -- Entry, risk & size
            entry_price REAL NOT NULL,
            stop_loss REAL NOT NULL,
            take_profit REAL NOT NULL,
            exit_price REAL,
            risk_reward REAL,
            signal_score INTEGER,
            lot_size REAL DEFAULT 1.0,
            risk_percent REAL,
            account_balance REAL,

            -- Timing (UTC)
            signal_occurred_at TEXT,   -- from signal_datetime
            opened_at TEXT,            -- from fill_datetime
            closed_at TEXT,            -- from exit_datetime

            -- Outcome
            status TEXT DEFAULT 'open'
              CHECK(status IN ('open','closed','breakeven','cancelled')),
            outcome TEXT,
            exit_reason TEXT,
            pnl_pips REAL,
            pnl_r REAL,
            pnl_pct REAL,
            hold_duration_hours REAL,
            candles_to_exit INTEGER,

            -- Self review
            emotion_rating INTEGER CHECK(emotion_rating BETWEEN 1 AND 5),
            setup_quality INTEGER CHECK(setup_quality BETWEEN 1 AND 5),
            mistakes TEXT,
            notes TEXT,
            tags TEXT,

            -- Metadata
            created_at TEXT DEFAULT (datetime('now')),
            updated_at TEXT DEFAULT (datetime('now'))
          )
        "
      )

      # Migration: add exit_reason if missing
      existing_cols <- dbGetQuery(con, "PRAGMA table_info(trades)")$name
      if (!"exit_reason" %in% existing_cols) {
        dbExecute(con, "ALTER TABLE trades ADD COLUMN exit_reason TEXT")
        log_info("Added exit_reason column to trades table")
      }

      log_info("Trades table is ready (created if not existing).")

      # Return connection
      con
    },
    error = function(e) {
      # Log the error and re-throw
      log_error(
        paste0(
          "Failed to connect or initialise journal database: ",
          e$message
        )
      )
      # If a connection was partially opened, close it to avoid leaks
      # if (exists("con", inherits = FALSE) && dbIsValid(con)) {
      #   dbDisconnect(con)
      # }
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
      stop(e) # re-throw
    }
  )

  # --- 4. Return the connection -------------------------------------
  return(con)
}

# Sync database journal from backtest_log ---------------------------------

#' Sync journal table with backtest results
#'
#' Rewrites the journal `trades` table from the current backtest output while
#' preserving the six manual/provenance fields: `emotion_rating`,
#' `setup_quality`, `mistakes`, `notes`, `tags`, and `created_at`. Any row in
#' the journal whose `signal_id` is no longer present in the backtest is
#' removed. All other (computed) fields are refreshed from the backtest.
#'
#' The whole operation runs in a single transaction, so a failure mid-way
#' leaves the journal untouched. Uses the
#' \code{signal_id} column as the unique key to avoid duplicates.
#'
#' @param con A DBI connection object to the trading journal database.
#' @param bt A \code{data.table} or \code{data.frame} containing backtest
#'   results. It must include at least the columns: \code{signal_id},
#'   \code{ticker}, \code{entry_price}, \code{stop_loss}, \code{take_profit},
#'   \code{signal_datetime}, \code{fill_datetime}, \code{exit_datetime},
#'   \code{sl_price}, \code{tp_price}, \code{outcome}, and P&L columns.
#'
#' @return Invisibly returns a list with two elements: \code{new} and
#'   \code{updated}.
#'
#' @export
#'
#' @importFrom data.table as.data.table setnames fcase fifelse rbindlist
#' @importFrom DBI dbGetQuery dbAppendTable dbExecute dbListFields dbIsValid
#' @importFrom DBI dbBegin dbCommit dbRollback
#' @importFrom logger log_info log_error
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' con <- connect_journal_db(config)
#' signals <- signals_generator(exec_list, config)
#' bt <- backtest_analysis(signals, config)
#' result <- sync_journal_from_bt(con, bt)
#' print(result)
#' dbDisconnect(con)
#' }

sync_journal_from_bt <- function(con, bt) {
  if (is.null(bt) || NROW(bt) == 0L) {
    stop(
      "`bt` has no rows. Nothing was synced, so the journal was left as it is.",
      call. = FALSE
    )
  }

  log_info(
    "Starting journal sync from backtest data with {n} rows.",
    n = nrow(bt)
  )

  if (!dbIsValid(con)) {
    stop("Invalid database connection.")
  }

  result <- tryCatch(
    {
      # -----------------------------------------------------------------
      # 1. CONVERT TO DATA.TABLE
      # -----------------------------------------------------------------
      # The backtest object may be a data.frame. We convert it to
      # data.table so we can use fast column operations below.
      bt <- as.data.table(bt)

      # -----------------------------------------------------------------
      # 2. RENAME BACKTEST COLUMNS TO JOURNAL NAMES
      # -----------------------------------------------------------------
      # The backtest uses names like signal_datetime / fill_datetime /
      # exit_datetime, while the journal table uses signal_occurred_at /
      # opened_at / closed_at. This step aligns the names so the insert
      # later matches the database schema.
      setnames(
        bt,
        old = c(
          "signal_datetime",
          "fill_datetime",
          "exit_datetime",
          "sl_price",
          "tp_price"
        ),
        new = c(
          "signal_occurred_at",
          "opened_at",
          "closed_at",
          "stop_loss",
          "take_profit"
        ),
        skip_absent = TRUE
      )

      # -----------------------------------------------------------------
      # 3. CONVERT TIMESTAMPS TO UTC CHARACTER
      # -----------------------------------------------------------------
      # SQLite stores timestamps as text, not as POSIXct. We convert every
      # timestamp column to a plain "YYYY-MM-DD HH:MM:SS" string in UTC so
      # the database can store and sort them correctly.
      ts_cols <- intersect(
        c("signal_occurred_at", "opened_at", "closed_at"),
        names(bt)
      )
      if (length(ts_cols) > 0) {
        bt[,
          (ts_cols) := lapply(.SD, function(x) {
            if (inherits(x, "POSIXct")) {
              format(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
            } else {
              as.character(x)
            }
          }),
          .SDcols = ts_cols
        ]
      }

      # -----------------------------------------------------------------
      # 4. DERIVE STATUS AND CLEAN FIELDS BY OUTCOME
      # -----------------------------------------------------------------
      # The journal uses a `status` column with values open / closed /
      # cancelled. We derive it from the backtest outcome:
      #   - "not_filled" -> cancelled
      #   - "open"       -> open
      #   - has closed_at -> closed
      # We also null-out price and P&L fields for trades that never closed,
      # because those numbers are meaningless for open/cancelled rows.
      bt[,
        status := fcase(
          outcome == "not_filled",
          "cancelled",
          outcome == "open",
          "open",
          !is.na(closed_at),
          "closed",
          default = NA_character_
        )
      ]

      bt[
        outcome == "not_filled",
        `:=`(
          opened_at = NA_character_,
          closed_at = NA_character_,
          exit_price = NA_real_,
          pnl_pips = NA_real_,
          pnl_r = NA_real_,
          pnl_pct = NA_real_,
          hold_duration_hours = NA_real_,
          candles_to_exit = NA_integer_
        )
      ]

      bt[
        outcome == "open",
        `:=`(
          closed_at = NA_character_,
          exit_price = NA_real_,
          pnl_pips = NA_real_,
          pnl_r = NA_real_,
          pnl_pct = NA_real_,
          hold_duration_hours = NA_real_,
          candles_to_exit = NA_integer_
        )
      ]

      # -----------------------------------------------------------------
      # 5. ADD MISSING COLUMNS WITH DEFAULTS
      # -----------------------------------------------------------------
      # The journal table has columns that the backtest doesn't produce,
      # such as timeframe, lot_size, or the manual review fields. We add
      # them here with sensible default values so the INSERT later doesn't
      # fail with a missing-column error.
      now_str <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

      defaults <- list(
        timeframe = "h1",
        lot_size = 1.0,
        risk_percent = NA_real_,
        account_balance = NA_real_,
        emotion_rating = NA_integer_,
        setup_quality = NA_integer_,
        mistakes = NA_character_,
        notes = NA_character_,
        tags = NA_character_,
        created_at = now_str,
        updated_at = now_str
      )

      missing_cols <- setdiff(names(defaults), names(bt))
      if (length(missing_cols) > 0) {
        bt[, (missing_cols) := defaults[missing_cols]]
      }

      bt[, signal_id := as.character(signal_id)]

      # -----------------------------------------------------------------
      # 6. KEEP ONLY JOURNAL COLUMNS (EXCLUDE AUTO-INCREMENT id)
      # -----------------------------------------------------------------
      # The journal table has an auto-incrementing `id` column that we must
      # not insert — SQLite assigns it. We drop `id` and keep everything
      # else, and we verify that all required columns are present in bt.
      journal_cols <- setdiff(dbListFields(con, "trades"), "id")
      missing_from_bt <- setdiff(journal_cols, names(bt))
      if (length(missing_from_bt) > 0) {
        stop(
          "Missing columns in bt after preparation: ",
          paste(missing_from_bt, collapse = ", ")
        )
      }
      bt <- bt[, ..journal_cols]

      # -----------------------------------------------------------------
      # 7. SAVE MANUAL FIELDS INTO A TEMPORARY TABLE
      # -----------------------------------------------------------------
      # Before we wipe the trades table, we copy the manual review columns
      # (emotion_rating, setup_quality, mistakes, notes, tags) and the
      # original created_at into a temporary SQLite table keyed by
      # signal_id. We will restore them after inserting the fresh rows.
      log_info("Saving manual review fields for preservation.")
      dbExecute(con, "DROP TABLE IF EXISTS manual_review_tmp")
      dbExecute(
        con,
        "
        CREATE TEMP TABLE manual_review_tmp AS
        SELECT signal_id,
               emotion_rating,
               setup_quality,
               mistakes,
               notes,
               tags,
               created_at
        FROM trades
      "
      )

      # -----------------------------------------------------------------
      # 8. REWRITE TRADES TABLE INSIDE A TRANSACTION
      # -----------------------------------------------------------------
      # We open a transaction so the whole operation is atomic. If any step
      # fails, SQLite rolls back and the journal is untouched. Inside:
      #   a) delete every row
      #   b) insert the fresh rows from the backtest
      #   c) restore the saved manual fields by matching on signal_id
      #   d) drop the temporary table
      log_info("Rewriting journal table inside transaction.")
      dbBegin(con)

      tryCatch(
        {
          # (a) Delete everything
          dbExecute(con, "DELETE FROM trades")

          # (b) Insert fresh rows from bt
          dbAppendTable(con, "trades", as.data.frame(bt))

          # (c) Restore manual fields for signals that existed before.
          #     Uses correlated subqueries in one SQL statement, so SQLite
          #     does the whole restore without any R-side loop.
          dbExecute(
            con,
            "
          UPDATE trades
          SET emotion_rating = (SELECT emotion_rating FROM manual_review_tmp
                                WHERE manual_review_tmp.signal_id = trades.signal_id),
              setup_quality  = (SELECT setup_quality  FROM manual_review_tmp
                                WHERE manual_review_tmp.signal_id = trades.signal_id),
              mistakes       = (SELECT mistakes       FROM manual_review_tmp
                                WHERE manual_review_tmp.signal_id = trades.signal_id),
              notes          = (SELECT notes          FROM manual_review_tmp
                                WHERE manual_review_tmp.signal_id = trades.signal_id),
              tags           = (SELECT tags           FROM manual_review_tmp
                                WHERE manual_review_tmp.signal_id = trades.signal_id),
              created_at     = (SELECT created_at     FROM manual_review_tmp
                                WHERE manual_review_tmp.signal_id = trades.signal_id)
          WHERE signal_id IN (SELECT signal_id FROM manual_review_tmp)
        "
          )

          # (d) Drop the temp table before committing
          dbExecute(con, "DROP TABLE manual_review_tmp")

          dbCommit(con)
        },
        error = function(e) {
          dbRollback(con)
          dbExecute(con, "DROP TABLE IF EXISTS manual_review_tmp")
          stop(e)
        }
      )

      log_info("Journal sync complete. Wrote {n} rows.", n = nrow(bt))
      invisible(list(total = nrow(bt)))
    },
    error = function(e) {
      log_error("Error during journal sync: {e$message}")
      stop(e)
    }
  )

  # return(result)
  invisible(result)
}

# update_journal_review ---------------------------------------------------

#' Update self review fields for a trade
#'
#' Updates only the self review columns (\code{emotion_rating},
#' \code{setup_quality}, \code{mistakes}, \code{notes}, \code{tags}) for the
#' trade with the given \code{signal_id}. Automated fields are left untouched.
#'
#' The trade is found by \code{signal_id} and not by the table's \code{id},
#' because \code{id} changes every time [sync_journal_from_bt()] rewrites the
#' table, while \code{signal_id} stays with the trade.
#'
#' @param con A DBI connection object to the trading journal database.
#' @param signal_id The \code{signal_id} of the trade. One text value.
#' @param review_data A named list with any of the allowed fields:
#'   \code{emotion_rating}, \code{setup_quality}, \code{mistakes},
#'   \code{notes}, \code{tags}. Only supplied fields are updated. Use `NA` to
#'   clear a field.
#'
#' @return Invisibly returns \code{TRUE} on success. Stops with an error when
#'   \code{review_data} is empty or has a wrong field, or when no trade has
#'   that \code{signal_id}.
#' @export
#'
#' @importFrom DBI dbExecute
#' @importFrom logger log_info log_error
#'
#' @examples
#' \dontrun{
#' con <- connect_journal_db(config)
#' update_journal_review(
#'   con,
#'   signal_id = "some-signal-id",
#'   review_data = list(emotion_rating = 4, notes = "Good setup")
#' )
#' dbDisconnect(con)
#' }
update_journal_review <- function(con, signal_id, review_data) {
  # ---- 1. Check the arguments ----
  signal_id <- as.character(signal_id)
  if (length(signal_id) != 1L || is.na(signal_id) || !nzchar(signal_id)) {
    stop("`signal_id` must be one non-empty text value.", call. = FALSE)
  }

  log_info(paste0("Updating review fields for signal ", signal_id))

  if (length(review_data) == 0L) {
    stop(
      "`review_data` is empty. Give at least one review field.",
      call. = FALSE
    )
  }

  fields <- names(review_data)
  if (!is.list(review_data) || is.null(fields) || any(!nzchar(fields))) {
    stop("`review_data` must be a named list.", call. = FALSE)
  }

  allowed <- c("emotion_rating", "setup_quality", "mistakes", "notes", "tags")
  invalid <- setdiff(fields, allowed)
  if (length(invalid) > 0) {
    stop(
      "Invalid review fields: ",
      paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 2. Build the update ----
  set_clause <- paste(paste0(fields, " = ?"), collapse = ", ")
  sql <- sprintf(
    "UPDATE trades SET %s, updated_at = ? WHERE signal_id = ?",
    set_clause
  )

  params <- unname(review_data[fields])
  params[[length(params) + 1]] <- format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%d %H:%M:%S"
  )
  params[[length(params) + 1]] <- signal_id

  # ---- 3. Run it ----
  tryCatch(
    {
      changed <- dbExecute(con, sql, params = params)
      if (changed == 0L) {
        stop("No trade has signal_id '", signal_id, "'.", call. = FALSE)
      }
      log_info(paste0("Updated review fields for signal ", signal_id))
      invisible(TRUE)
    },
    error = function(e) {
      log_error(
        paste0("Failed to update signal ", signal_id, ": ", conditionMessage(e))
      )
      stop(e)
    }
  )
}

# get_trading_journal -----------------------------------------------------

#' Get the backtest journal trades
#'
#' Reads the `trades` table of the trading journal database and returns it as
#' one data.table. The text timestamps in the database are turned back into
#' real date-times (UTC), so they can be compared with candle times.
#'
#' Everything is returned, including trades that are still open and signals
#' that were never filled (status "cancelled"). When you count wins, losses
#' and trades for the dashboard, filter on `status` first, for example
#' `status = c("closed", "breakeven")`.
#'
#' @param con An open connection from [connect_journal_db()].
#' @param ticker Optional. One or more tickers, for example "eurusd".
#' @param asset_class Optional. One or more asset classes, for example
#'   "Metals".
#' @param timeframe Optional. One or more timeframes, for example "h1".
#' @param status Optional. One or more of "open", "closed", "breakeven",
#'   "cancelled".
#' @param from,to Optional date limits. Each can be a Date, a POSIXct, or text
#'   such as "2026-08-31". Both ends are INCLUDED. A plain date for `to` means
#'   the WHOLE of that day (UTC), so a date picker in the dashboard does not
#'   lose the last day.
#' @param date_by Which time the `from` and `to` limits are checked against:
#'   "signal" (when the signal appeared, the default) or "closed" (when the
#'   trade closed). Trades that have not closed have no closing time, so they
#'   drop out when you use "closed".
#'
#' @return A data.table sorted by `signal_occurred_at`. It has zero rows (not
#'   an error) when there are no trades, or none match the filters, and it
#'   still has all the columns.
#' @export
#'
#' @importFrom DBI dbGetQuery dbIsValid dbExistsTable
#' @importFrom data.table as.data.table set
#' @importFrom logger log_info log_error
#'
#' @examples
#' \dontrun{
#' con <- connect_journal_db(config)
#' get_journal_trades(con)
#' get_journal_trades(con, ticker = "eurusd", status = "closed")
#' get_journal_trades(con, from = "2026-08-31", date_by = "closed")
#' DBI::dbDisconnect(con)
#' }
get_journal_trades <- function(
  con,
  ticker = NULL,
  asset_class = NULL,
  timeframe = NULL,
  status = NULL,
  from = NULL,
  to = NULL,
  date_by = c("signal", "closed")
) {
  # ---- 1. Check the arguments ----
  date_by <- match.arg(date_by)

  if (!DBI::dbIsValid(con)) {
    stop("`con` is not an open database connection.", call. = FALSE)
  }
  if (!DBI::dbExistsTable(con, "trades")) {
    stop(
      "The journal has no `trades` table. ",
      "Open the connection with connect_journal_db(config) first.",
      call. = FALSE
    )
  }

  check_text(ticker, "ticker")
  check_text(asset_class, "asset_class")
  check_text(timeframe, "timeframe")
  check_text(status, "status")

  allowed_status <- c("open", "closed", "breakeven", "cancelled")
  bad_status <- setdiff(status, allowed_status)
  if (length(bad_status) > 0L) {
    stop(
      "`status` must be one or more of: ",
      paste(allowed_status, collapse = ", "),
      ". Got: ",
      paste(bad_status, collapse = ", "),
      call. = FALSE
    )
  }

  from_txt <- as_utc_text(from, "from")
  to_txt <- as_utc_text(to, "to", end_of_day = TRUE)
  if (!is.null(from_txt) && !is.null(to_txt) && from_txt > to_txt) {
    stop("`from` is later than `to`.", call. = FALSE)
  }

  # ---- 2. Build the query (values go in as parameters, never pasted in) ----
  where <- character(0)
  params <- list()

  add_in <- function(column, values) {
    if (is.null(values)) {
      return(invisible(NULL))
    }
    marks <- paste(rep("?", length(values)), collapse = ", ")
    where <<- c(where, paste0(column, " IN (", marks, ")"))
    params <<- c(params, as.list(values))
    invisible(NULL)
  }

  add_in("ticker", ticker)
  add_in("asset_class", asset_class)
  add_in("timeframe", timeframe)
  add_in("status", status)

  date_col <- c(signal = "signal_occurred_at", closed = "closed_at")[[date_by]]
  if (!is.null(from_txt)) {
    where <- c(where, paste0(date_col, " >= ?"))
    params <- c(params, list(from_txt))
  }
  if (!is.null(to_txt)) {
    where <- c(where, paste0(date_col, " <= ?"))
    params <- c(params, list(to_txt))
  }

  sql <- "SELECT * FROM trades"
  if (length(where) > 0L) {
    sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  }
  sql <- paste(sql, "ORDER BY signal_occurred_at, id")

  # ---- 3. Read ----
  dt <- tryCatch(
    {
      raw <- if (length(params) > 0L) {
        DBI::dbGetQuery(con, sql, params = unname(params))
      } else {
        DBI::dbGetQuery(con, sql)
      }
      data.table::as.data.table(raw)
    },
    error = function(e) {
      log_error("Failed to retrieve journal trades: {e$message}")
      stop(e)
    }
  )

  # ---- 4. Turn the text timestamps back into real date-times (UTC) ----
  time_cols <- intersect(
    c(
      "signal_occurred_at",
      "opened_at",
      "closed_at",
      "created_at",
      "updated_at"
    ),
    names(dt)
  )
  for (col in time_cols) {
    data.table::set(
      dt,
      j = col,
      value = as.POSIXct(
        dt[[col]],
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%S"
      )
    )
  }

  # ---- 5. Report ----
  if (nrow(dt) == 0L) {
    if (length(where) == 0L) {
      log_info("No available trades yet")
    } else {
      log_info("No journal trades match these filters")
    }
  } else {
    log_info("get_journal_trades: {nrow(dt)} row(s)")
  }

  dt[]
}


# ---- internal helper (not exported) ---

# Turn a Date, POSIXct or text into UTC text "YYYY-MM-DD HH:MM:SS", the same
# form the journal stores. With `end_of_day = TRUE`, a plain date (no time)
# becomes the last second of that day.
as_utc_text <- function(x, name, end_of_day = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }

  is_plain_date <- inherits(x, "Date") ||
    (is.character(x) && length(x) == 1L && grepl("^\\d{4}-\\d{2}-\\d{2}$", x))

  out <- if (length(x) == 1L && !is.na(x)) {
    tryCatch(as.POSIXct(x, tz = "UTC"), error = function(e) NA)
  } else {
    NA
  }

  if (length(out) != 1L || is.na(out)) {
    stop(
      "`",
      name,
      "` must be one date or date-time, for example \"2026-08-31\".",
      call. = FALSE
    )
  }

  if (end_of_day && is_plain_date) {
    out <- out + (24 * 3600 - 1)
  }
  format(out, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
}

################################################################################

# paper_trading -----------------------------------------------------------

# R/utils_paper_trading.R

#' Connect to the paper trading SQLite database
#'
#' Opens a connection to the paper trading SQLite database. The database file
#' is located in the directory specified by `config$paths$forexengine$db_dir`.
#'
#' @param config A validated configuration list.
#'
#' @return A DBI connection object.
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @noRd
connect_paper_trading_db <- function(config) {
  # ============================================================================
  # PRE-DEFINE DATABASE FILTERS (from config)
  # ============================================================================
  log_info(
    paste0("Pre-defining database filters")
  )

  db_dir <- config$paths$forexengine$db_dir
  db_suffix <- config$database$db_suffix
  paper_trade <- config$database$table_prefixes$paper_trade
  db_path <- file.path(db_dir, paste0(paper_trade, db_suffix))

  log_info("Database filters loaded from config")

  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }

  dbConnect(SQLite(), db_path)
}

#' Create the paper trades table
#'
#' Creates the `paper_trades` table if it does not already exist.
#'
#' @param con A DBI connection object.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @importFrom DBI dbExecute
#' @noRd
create_paper_trades_table <- function(con) {
  dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS paper_trades (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      signal_id TEXT NOT NULL UNIQUE,
      ticker TEXT NOT NULL,
      asset_class TEXT,
      signal_datetime TEXT NOT NULL,
      direction TEXT CHECK(direction IN ('long','short')),
      entry_price REAL NOT NULL,
      stop_loss REAL NOT NULL,
      take_profit REAL NOT NULL,
      risk_reward REAL,
      confluence TEXT,
      signal_score INTEGER,
      status TEXT CHECK(status IN ('pending','taken','skipped','closed')) DEFAULT 'pending',
      skip_reason TEXT,
      actual_entry REAL,
      actual_sl REAL,
      actual_tp REAL,
      risk_pct REAL CHECK(risk_pct BETWEEN 1 AND 2),
      position_size REAL,
      account_balance_before REAL,
      account_balance_after REAL,
      exit_price REAL,
      exit_datetime TEXT,
      outcome TEXT CHECK(outcome IN ('win','loss','breakeven','open')),
      exit_reason TEXT,
      pnl_r REAL,
      pnl_pct REAL,
      emotion_rating INTEGER CHECK(emotion_rating BETWEEN 1 AND 5),
      notes TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now'))
    )
  "
  )
  invisible(TRUE)
}

#' Initialize paper trading database
#'
#' Opens a connection and ensures the `paper_trades` table exists. Returns
#' the connection for further operations.
#'
#' @param config A validated configuration list.
#'
#' @return A DBI connection object.
#'
#' @importFrom DBI dbIsValid dbDisconnect
#' @importFrom logger log_info log_error
#' @export
init_paper_trading_db <- function(config) {
  log_info("Initializing paper trading database")
  con <- tryCatch(
    {
      con <- connect_paper_trading_db(config)
      create_paper_trades_table(con)
      con
    },
    error = function(e) {
      log_error("Failed to initialize paper trading database: {e$message}")
      stop(e)
    }
  )
  return(con)
}

#' Insert a new paper trade
#'
#' Inserts one row into the `paper_trades` table. The row can represent a
#' skipped or taken signal. `status` defaults to `'pending'` if not given.
#'
#' @param con A DBI connection object.
#' @param trade A named list or one-row data.table with column names matching
#'   the paper_trades schema.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @importFrom DBI dbAppendTable
#' @importFrom data.table as.data.table
#' @importFrom logger log_info log_error
#' @export
insert_paper_trade <- function(con, trade) {
  log_info(
    "Inserting paper trade for signal {signal_id}",
    signal_id = trade$signal_id
  )

  tryCatch(
    {
      trade_dt <- as.data.table(trade)

      # Ensure required columns exist
      required <- c(
        "signal_id",
        "ticker",
        "signal_datetime",
        "direction",
        "entry_price",
        "stop_loss",
        "take_profit"
      )
      missing <- setdiff(required, names(trade_dt))
      if (length(missing) > 0) {
        stop("Missing required columns: ", paste(missing, collapse = ", "))
      }

      dbAppendTable(con, "paper_trades", as.data.frame(trade_dt))
      invisible(TRUE)
    },
    error = function(e) {
      log_error("Failed to insert paper trade: {e$message}")
      stop(e)
    }
  )
}

#' Update a paper trade when closed
#'
#' Updates the exit fields, outcome, P&L, and balance for a trade that has
#' been closed manually, and sets its `status` to `'closed'`.
#'
#' @param con A DBI connection object.
#' @param id One whole number. The paper trade `id`. (This table is never
#'   rewritten, so `id` stays the same for the life of a trade.)
#' @param close_info A named list with any of: `exit_price`, `exit_datetime`,
#'   `outcome`, `exit_reason`, `pnl_r`, `pnl_pct`, `account_balance_after`,
#'   `emotion_rating`, `notes`.
#'
#' @return Invisibly returns `TRUE`. Stops with an error when `id` is not one
#'   whole number, `close_info` is empty or has a wrong field, or no paper
#'   trade has that `id`.
#'
#' @importFrom DBI dbExecute
#' @importFrom logger log_info log_error
#' @export
update_paper_trade_close <- function(con, id, close_info) {
  # ---- 1. Check the arguments ----
  id_num <- suppressWarnings(as.numeric(id))
  if (length(id_num) != 1L || is.na(id_num) || id_num != round(id_num)) {
    stop("`id` must be one whole number.", call. = FALSE)
  }
  id_int <- as.integer(id_num)

  log_info("Closing paper trade id {id}", id = id_int)

  if (length(close_info) == 0L) {
    stop(
      "`close_info` is empty. Give at least one field to save.",
      call. = FALSE
    )
  }
  fields <- names(close_info)
  if (!is.list(close_info) || is.null(fields) || any(!nzchar(fields))) {
    stop("`close_info` must be a named list.", call. = FALSE)
  }

  allowed <- c(
    "exit_price",
    "exit_datetime",
    "outcome",
    "exit_reason",
    "pnl_r",
    "pnl_pct",
    "account_balance_after",
    "emotion_rating",
    "notes"
  )
  invalid <- setdiff(fields, allowed)
  if (length(invalid) > 0) {
    stop(
      "Invalid close fields: ",
      paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 2. Run the update ----
  tryCatch(
    {
      set_clause <- paste(paste0(fields, " = ?"), collapse = ", ")
      sql <- sprintf(
        "UPDATE paper_trades SET %s, status = 'closed', updated_at = ? WHERE id = ?",
        set_clause
      )
      params <- unname(close_info[fields])
      params[[length(params) + 1]] <- format(
        Sys.time(),
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%S"
      )
      params[[length(params) + 1]] <- id_int

      changed <- dbExecute(con, sql, params = params)
      if (changed == 0L) {
        stop("No paper trade has id ", id_int, ".", call. = FALSE)
      }
      invisible(TRUE)
    },
    error = function(e) {
      log_error("Failed to close paper trade: {e$message}")
      stop(e)
    }
  )
}


# get_paper_trades --------------------------------------------------------

#' Get all paper trades
#'
#' Returns all rows from the `paper_trades` table, ordered by signal_datetime.
#'
#' @param con A DBI connection object.
#'
#' @return A data.table.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table setorder
#' @importFrom logger log_info log_error
#' @export
get_paper_trades <- function(con) {
  # Safe to call every time: only creates the table if it is missing
  create_paper_trades_table(con)

  dt <- tryCatch(
    data.table::as.data.table(
      DBI::dbGetQuery(con, "SELECT * FROM paper_trades")
    ),
    error = function(e) {
      log_error("Failed to retrieve paper trades: {e$message}")
      stop(e)
    }
  )

  if (nrow(dt) == 0L) {
    log_info("No available trades yet")
    return(dt)
  }

  data.table::setorder(dt, signal_datetime)
  dt
}
