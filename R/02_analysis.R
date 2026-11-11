# signals_generator -------------------------------------------------------

#' Generate trading signals from execution data
#'
#' Takes the execution table produced by `market_context()` and constructs
#' a signal log containing one row per candidate trade setup. The function
#' calculates entry, stop loss, take profit, risk/reward, builds a reason
#' string, applies optional filters (minimum confluence, minimum risk/reward),
#' and returns the final signal log together with the original H1 data.
#'
#' @param exec_list A named list from `market_context()` with elements:
#'   \itemize{
#'     \item \code{exec_data} data.table containing candidate setups
#'     \item \code{h1_data} data.table with H1 candle data
#'   }
#' @param config A validated configuration list from `load_config()`.
#'   Must contain the \code{analysis$signal_filter} section with keys
#'   \code{min_confluence}, \code{risk_pct}, and optionally
#'   \code{min_risk_reward}.
#'
#' @return A named list with two elements:
#'   \itemize{
#'     \item \code{signal_log} a data.table with one row per signal,
#'           including \code{signal_datetime}, \code{signal_id},
#'           \code{signal_type}, entry/SL/TP prices, risk/reward,
#'           signal score, confluence, bias snapshots, reason, timeframe,
#'           ticker, and asset class.
#'     \item \code{h1_data} the original H1 data.table passed through
#'           unchanged.
#'   }
#'
#' @note
#' This function generates signals only. It does not evaluate outcomes.
#' Exit dates, profit and loss, and win/loss results are computed separately
#' in \code{backtest_engine()} during Phase 4. This separation ensures the
#' signal engine produces identical output whether it runs on historical
#' data or on a live feed.
#'
#' Active POI column naming convention from institutional_mapping():
#' \code{active\_\{timeframe\}\_\{zone\}} e.g. h1_active_bull_fvg_top,
#' daily_active_dol.
#' Do not confuse with h1_active_bull_fvg_top that prefix order is wrong.
#'
#' @section Dependency on institutional_mapping():
#' This function assumes \code{bull_fvg_id} and \code{bear_fvg_id} are
#' forward-filled in \code{institutional_mapping()} (Section 4, after the
#' shift-by-1 step). Without this, FVG mitigation tracking only evaluates a
#' single row per zone and \code{h1_active_bull_fvg_top} etc. will be NA on
#' almost every candle. If signal_engine() returns an empty signal_log, this
#' is the first thing to check.
#'
#' @import data.table
#' @importFrom logger log_info log_error
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' # After market_context()
#' exec_list <- market_context(
#'   weekly_anchor = weekly_anchor,
#'   daily_insight = daily_insight,
#'   h4_setup = h4_setup,
#'   h1_trigger = h1_trigger,
#'   config = config
#' )
#' signals <- signal_engine(exec_list, config)
#' head(signals$signal_log)
#' }

signals_generator <- function(exec_list, config) {
  log_info("Starting signal engine: Phase 3 signal generation.")

  # ============================================================================
  # 0. UNLISTING LIST DATA
  # ============================================================================
  log_info("Starting the unlisting the list data items")

  exec_data <- exec_list$exec_data
  h1_data <- exec_list$h1_data

  log_info("List data items unlisted successfully")

  # ============================================================================
  # 1. INPUT VALIDATION
  # ============================================================================
  # Before doing any work, confirm the inputs are what we expect.
  # A clear error message here saves hours of debugging later.
  tryCatch(
    {
      log_info("Validating signal engine inputs.")

      if (!inherits(exec_data, "data.table")) {
        stop("h1_executer must be a data.table.")
      }

      if (!is.list(config)) {
        stop("config must be a named list loaded from config.yml.")
      }

      # Minimum columns signal_engine needs to run.
      # Naming convention: active_{timeframe}_{zone}
      # e.g. h1_active_bull_fvg_top, daily_active_bear_fvg_bot
      required_cols <- c(
        "date_time",
        "ticker",
        "asset_class",
        "trend_context",
        "pullback_context",
        "trade_context",
        "weekly_target_dol",
        "daily_target_dol",
        "entry_dol",
        "sl_price",
        "signal_score",
        "confluence",
        "weekly_bias",
        "daily_bias",
        "h4_bias",
        "h1_bias"
      )

      missing_cols <- setdiff(required_cols, names(exec_data))

      if (length(missing_cols) > 0) {
        stop(sprintf(
          "h1_executer is missing required columns: %s",
          paste(missing_cols, collapse = ", ")
        ))
      }

      if (nrow(exec_data) == 0) {
        stop("h1_executer has no rows. Nothing to process.")
      }

      log_info(sprintf(
        "Input validation passed. Processing %s rows across %s ticker(s).",
        nrow(exec_data),
        length(unique(exec_data$ticker))
      ))
    },
    error = function(e) {
      log_error(sprintf("Input validation failed: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 2. READ SIGNAL SETTINGS FROM CONFIG
  # ============================================================================
  # Pull risk and scoring thresholds from config so they can be adjusted
  # in one place without touching the function logic.
  tryCatch(
    {
      log_info("Reading signal settings from config.")

      # Minimum confluence score needed to fire a signal alert.
      # Rows scoring below this are skipped not logged.
      min_score <- config$analysis$signal_filter$min_confluence

      # Risk percentage per trade noted on each signal.
      # Actual lot size is calculated at execution time from live balance.
      risk_pct <- config$analysis$signal_filter$risk_pct

      # ---- NEW: minimum risk/reward filter (optional) ----
      min_rr <- config$analysis$signal_filter$min_risk_reward

      if (is.null(min_rr)) {
        min_rr <- NA_real_
      }

      log_info(sprintf(
        "Min confluence score: %s | Risk per trade: %s%%",
        min_score,
        risk_pct * 100
      ))
    },
    error = function(e) {
      log_error(sprintf("Failed to read signal config settings: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 7a. ENTRY, STOP LOSS AND TAKE PROFIT CALCULATION
  # ============================================================================
  # For each surviving signal row, calculate the three price levels needed
  # to place a trade and the resulting risk/reward ratio.
  #
  # ENTRY: The top of the H1 trigger zone for bullish setups (the first
  #           price point reached on a retracement into the demand zone).
  #           The bottom of the H1 trigger zone for bearish setups.
  #           For sweeps, the entry is at the swept structural level itself.
  #
  # STOP LOSS: Placed at the H1 structural invalidation level.
  #             Bullish: below the most recent H1 swing low (h1_prev_sl).
  #             Bearish: above the most recent H1 swing high (h1_prev_sh).
  #             A small pip buffer should be added at execution time.
  #             Risk percentage is stored from config — lot size is
  #             calculated when account balance is known at execution.
  #
  # TAKE PROFIT: The Daily Draw on Liquidity (daily_dol) is the primary
  #               target. This is the nearest unmitigated level institutions
  #               are drawing price toward on the Daily timeframe.
  #
  # RISK/REWARD: |tp - entry| divided by |entry - sl|.
  #               A minimum of 1:1 is expected. Values below 1 are kept
  #               in the log but noted for review.
  tryCatch(
    {
      log_info("Computing risk-reward for every signal.")

      # Risk/reward ratio
      exec_data[,
        risk_reward := fcase(
          !is.na(entry_dol) &
            !is.na(sl_price) &
            !is.na(weekly_target_dol) &
            !is.na(daily_target_dol) &
            trade_context == "bull_candidate" &
            abs(entry_dol - sl_price) > 0,
          round(
            abs(
              pmin(weekly_target_dol, daily_target_dol, na.rm = TRUE) -
                entry_dol
            ) /
              abs(entry_dol - sl_price),
            2
          ),
          !is.na(entry_dol) &
            !is.na(sl_price) &
            !is.na(weekly_target_dol) &
            !is.na(daily_target_dol) &
            trade_context == "bear_candidate" &
            abs(entry_dol - sl_price) > 0,
          round(
            abs(
              pmax(weekly_target_dol, daily_target_dol, na.rm = TRUE) -
                entry_dol
            ) /
              abs(entry_dol - sl_price),
            2
          ),
          default = NA_real_
        )
      ]

      log_info("Entry, SL and TP calculation complete.")
    },
    error = function(e) {
      log_error(sprintf("Entry/SL/TP calculation failed: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 8. REASON STRING ASSEMBLY
  # ============================================================================
  # Each signal gets a short, auto-generated description of exactly what
  # triggered it. This is never written manually it is built from the
  # actual column values that fired on this row.
  #
  # Purpose: when you review a trade days or weeks later in the dashboard
  # or trading journal, the reason string tells you exactly what the system
  # saw without needing to re-run the analysis or remember the date.
  #
  # Example outputs:
  #   "W:bull D:bull H4:bull | pullback | bull_fvg @ 0.92340 | score:3"
  #   "W:bear D:bear H4:bear | pullback | bear_sweep @ 0.95120 | score:4"
  tryCatch(
    {
      log_info("Assembling reason strings.")

      exec_data[,
        reason := paste0(
          trend_context,
          " | ",
          pullback_context,
          " | ",
          trade_context,
          " | ",
          confluence,
          " | ",
          entry_dol_name,
          " @ ",
          round(entry_dol, 5),
          " | score:",
          signal_score
        ),
        by = .(asset_class, ticker)
      ]

      log_info("Reason strings assembled.")
    },
    error = function(e) {
      log_error(sprintf("Reason string assembly failed: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 9. SIGNAL LOG CONSTRUCTION
  # ============================================================================
  # Select the final output columns, clean up temporary working columns,
  # add a unique signal ID per row, and return the signal log.
  #
  # One row per signal. This is what gets saved to the database and
  # displayed in the Shiny dashboard alert panel.
  #
  # IMPORTANT: exit_datetime, exit_price, P/L, and win/loss are NOT here.
  # Those columns are added by backtest_engine() in Phase 4. This function
  # must produce the same output on historical data and on a live feed.
  tryCatch(
    {
      log_info("Building final signal log.")

      # Build the final output table
      signal_log <- exec_data[,
        .(
          signal_datetime = date_time,
          # Unique ID: ticker + datetime removes ambiguity across tickers
          signal_id = paste0(
            ticker,
            ": ",
            format(date_time, "%Y-%m-%d %H:%M:%S")
          ),

          # Direction: derived from trigger_type prefix
          signal_type = fcase(
            grepl("^bull", trade_context),
            "bullish",
            grepl("^bear", trade_context),
            "bearish"
          ),

          bias_alignment = trade_context,
          trigger_type = entry_dol_name,
          entry_price = entry_price,
          sl_price,
          tp_price = fcase(
            trade_context == "bull_candidate",
            pmin(weekly_target_dol, daily_target_dol, na.rm = TRUE),
            trade_context == "bear_candidate",
            pmax(weekly_target_dol, daily_target_dol, na.rm = TRUE)
          ),
          risk_reward,
          signal_score,
          confluence,

          # Bias snapshots at signal time
          weekly_bias,
          daily_bias,
          h4_bias,
          h1_bias,
          reason,
          timeframe,
          ticker,
          asset_class
        )
      ]

      # signal_log <- signal_log[signal_score >= min_score]

      # Remove rows where any of the three price levels could not be computed.
      # A signal without a complete entry/SL/TP set is not tradeable.
      signal_log <- signal_log[
        !is.na(entry_price) & !is.na(sl_price) & !is.na(tp_price)
      ]

      log_info()

      # ---- NEW: Sanity checks on price ordering ----
      signal_log <- signal_log[
        (signal_type == "bullish" &
          entry_price > sl_price &
          tp_price > entry_price) |
          (signal_type == "bearish" &
            entry_price < sl_price &
            tp_price < entry_price)
      ]

      # ---- NEW: Minimum risk/reward filter (optional) ----
      if (!is.na(min_rr)) {
        before_rr <- nrow(signal_log)
        signal_log <- signal_log[risk_reward >= min_rr]
        log_info(sprintf(
          "Minimum risk/reward filter (>= %s): %s signal(s) removed, %s remain.",
          min_rr,
          before_rr - nrow(signal_log),
          nrow(signal_log)
        ))
      }

      log_info(sprintf(
        paste0(
          "Signal engine complete. %s signal(s) generated across %s",
          " ticker(s). Returning signal_log."
        ),
        nrow(signal_log),
        length(unique(signal_log$ticker))
      ))

      # signal_log
    },
    error = function(e) {
      log_error(sprintf("Signal log construction failed: %s", e$message))
      stop(e)
    }
  )

  results <- setNames(
    list(signal_log, h1_data),
    c("signal_log", "h1_data")
  )

  return(results)
}

################################################################################

# backtest_analysis -------------------------------------------------------

#' Evaluate signals against historical H1 data
#'
#' Takes the signal log generated by `signals_generator()` and simulates trade
#' outcomes using H1 price data. The function applies a swing-based trailing
#' stop, checks for weekly target hits, opposite CHoCH/BOS exits, and
#' calculates profit/loss in pips, R multiples, and account percentage.
#'
#' @param signal_list A named list from `signals_generator()` with elements:
#'   \itemize{
#'     \item \code{signal_log} data.table containing trade signals.
#'     \item \code{h1_data} data.table with H1 candle data.
#'   }
#' @param config A validated configuration list from `load_config()`.
#'   Must contain \code{analysis$signal_filter$risk_pct},
#'   \code{analysis$signal_filter$buffer_pipette}, and optionally
#'   \code{analysis$backtest$max_holding_candles}.
#'
#' @return A data.table enriched with backtest outcomes. Each row represents
#'   one signal and includes original signal columns plus:
#'   \itemize{
#'     \item \code{fill_datetime} when the limit order was filled.
#'     \item \code{fill_candle_idx} index of the fill candle.
#'     \item \code{exit_datetime} when the trade was closed.
#'     \item \code{exit_price} actual exit price.
#'     \item \code{outcome} one of \code{"win"}, \code{"loss"},
#'           \code{"trailing_stop"}, \code{"choch_exit"}, \code{"bos_exit"},
#'           \code{"time_exit"}, \code{"not_filled"}, or \code{"open"}.
#'     \item \code{pip_conv} pip conversion factor used for this trade.
#'     \item \code{pnl_pips} profit/loss in pips.
#'     \item \code{pnl_r} profit/loss in R multiples.
#'     \item \code{pnl_pct} profit/loss as percentage of account.
#'     \item \code{hold_duration_hours} time between signal and exit.
#'     \item \code{candles_to_exit} number of candles held.
#'   }
#'
#' @import data.table
#' @importFrom logger log_info log_error
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' # After signals_generator()
#' signals <- signals_generator(exec_list, config)
#' backtest_results <- backtest_analysis(signals, config)
#' head(backtest_results)
#' }

backtest_analysis <- function(signal_list, config) {
  log_info("Starting backtest engine: Phase 4 signal evaluation.")

  # ============================================================================
  # 0. UNLISTING LIST DATA
  # ============================================================================
  log_info("Starting the unlisting the list data items")

  signal_log <- signal_list$signal_log
  h1_data <- signal_list$h1_data

  log_info("List data items unlisted successfully")

  # ============================================================================
  # 1. INPUT VALIDATION
  # ============================================================================
  # Same discipline as every other function in this system: check the inputs
  # before doing any work, so a bad input fails loudly and immediately
  # instead of producing a confusing error three steps later.
  tryCatch(
    {
      log_info("Validating backtest engine inputs.")

      if (!inherits(signal_log, "data.table")) {
        stop("signal_log must be a data.table.")
      }

      if (!inherits(h1_data, "data.table")) {
        stop("h1_data must be a data.table.")
      }

      if (!is.list(config)) {
        stop("config must be a named list loaded from config.yml.")
      }

      required_signal_cols <- c(
        "signal_id",
        "signal_datetime",
        "ticker",
        "asset_class",
        "signal_type",
        "entry_price",
        "sl_price",
        "tp_price"
      )

      missing_signal_cols <- setdiff(required_signal_cols, names(signal_log))

      if (length(missing_signal_cols) > 0) {
        stop(sprintf(
          "signal_log is missing required columns: %s",
          paste(missing_signal_cols, collapse = ", ")
        ))
      }

      required_h1_cols <- c(
        "date_time",
        "open",
        "high",
        "low",
        "close",
        "ticker",
        "asset_class",
        "pip_conv"
      )

      missing_h1_cols <- setdiff(required_h1_cols, names(h1_data))

      if (length(missing_h1_cols) > 0) {
        stop(sprintf(
          "h1_data is missing required columns: %s",
          paste(missing_h1_cols, collapse = ", ")
        ))
      }

      if (nrow(signal_log) == 0) {
        log_info("signal_log has no rows. Returning empty enriched_signal_log.")
        return(data.table())
      }

      log_info(sprintf(
        "Input validation passed. Evaluating %s signal(s) against %s H1 candles.",
        nrow(signal_log),
        nrow(h1_data)
      ))
    },
    error = function(e) {
      log_error(sprintf("Input validation failed: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 2. READ CONFIG SETTINGS
  # ============================================================================
  tryCatch(
    {
      log_info("Reading backtest settings from config.")

      # Same risk percentage signal_engine() uses. This turns the R-multiple
      # result (pnl_r) into a real account percentage (pnl_pct).
      risk_pct <- config$analysis$signal_filter$risk_pct

      # ---- NEW: optional max holding period (in H1 candles) ----
      max_bars <- config$analysis$backtest$max_holding_candles

      if (is.null(max_bars)) {
        max_bars <- NA_integer_
      }

      log_info(sprintf(
        "Risk per trade: %s%% | Max holding candles: %s",
        risk_pct * 100,
        if (is.na(max_bars)) "unlimited" else max_bars
      ))
    },
    error = function(e) {
      log_error(sprintf(
        "Failed to read backtest config settings: %s",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 3. PREPARE H1 DATA FOR FAST FORWARD-SCANNING
  # ============================================================================
  # We are about to search h1_data many times once per signal. Sorting
  # it by ticker and date_time up front means every search below can jump
  # straight to the right place instead of scanning the whole table each time.
  tryCatch(
    {
      log_info("Preparing h1_data for forward scanning.")

      setkey(h1_data, ticker, asset_class, date_time)

      log_info("h1_data sorted and indexed by ticker and date_time.")
    },
    error = function(e) {
      log_error(sprintf("h1_data preparation failed: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 4. EVALUATE EVERY SIGNAL, SWING TRAILING STOP + CHoCH/BOS
  # ============================================================================
  tryCatch(
    {
      log_info(
        "Evaluating signals with swing-based trailing stop, CHoCH and BOS."
      )

      # Buffer pipette same as used for entry and initial SL
      buffer_pip <- config$analysis$signal_filter$buffer_pipette

      exit_results <- h1_data[
        signal_log,
        on = .(ticker, asset_class, date_time > signal_datetime),
        by = .EACHI,
        j = {
          # ---------------------------------------------------------------
          # A. DETECT FILL
          # ---------------------------------------------------------------
          if (i.signal_type == "bullish") {
            fill_hit <- low <= i.entry_price
          } else {
            fill_hit <- high >= i.entry_price
          }
          first_fill_idx <- which(fill_hit)[1L]

          if (is.na(first_fill_idx)) {
            # No fill at all
            .(
              signal_id = i.signal_id,
              fill_datetime = as.POSIXct(NA, tz = "UTC"),
              fill_candle_idx = NA_integer_,
              exit_datetime = as.POSIXct(NA, tz = "UTC"),
              exit_price = NA_real_,
              outcome = "not_filled",
              exit_reason = "not_filled",
              pip_conv = NA_real_,
              exit_candle_idx = NA_integer_
            )
          } else {
            # ---------------------------------------------------------------
            # B. FILL RECORDED INITIALISE TRAILING STATE
            # ---------------------------------------------------------------
            fill_dt <- x.date_time[first_fill_idx]
            pip_conv_base <- pip_conv[first_fill_idx]
            buffer_price <- (buffer_pip * pip_conv_base) * 2

            current_sl <- i.sl_price

            # Track the last swing level that triggered a stop adjustment
            # (so we don't re-act on the same swing repeatedly)
            last_broken_level <- NA_real_

            # Post-fill indices
            post_start <- first_fill_idx + 1L

            if (post_start > .N) {
              # No post-fill data: exit at close of fill candle
              .(
                signal_id = i.signal_id,
                fill_datetime = fill_dt,
                fill_candle_idx = first_fill_idx,
                exit_datetime = fill_dt,
                exit_price = close[first_fill_idx],
                outcome = "time_exit",
                exit_reason = "time_exit",
                pip_conv = pip_conv_base,
                exit_candle_idx = first_fill_idx
              )
            } else {
              exit_found <- FALSE
              exit_info <- NULL

              for (k in post_start:.N) {
                high_k <- high[k]
                low_k <- low[k]
                close_k <- close[k]
                dt_k <- x.date_time[k]
                pip_conv_k <- pip_conv[k]

                # ----- Priority 1: Weekly DOL target -----
                if (i.signal_type == "bullish") {
                  if (!is.na(i.tp_price) && high_k >= i.tp_price) {
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = i.tp_price,
                      outcome = "win",
                      exit_reason = "take_profit",
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                } else {
                  if (!is.na(i.tp_price) && low_k <= i.tp_price) {
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = i.tp_price,
                      outcome = "win",
                      exit_reason = "take_profit",
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                }

                # ----- Priority 2: Current trailing stop -----
                if (i.signal_type == "bullish") {
                  if (low_k <= current_sl) {
                    exit_price <- current_sl
                    outcome <- if (exit_price > i.entry_price) {
                      "win"
                    } else if (exit_price < i.entry_price) {
                      "loss"
                    } else {
                      "breakeven"
                    }
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = exit_price,
                      outcome = outcome,
                      exit_reason = if (current_sl == i.sl_price) {
                        "stop_loss"
                      } else {
                        "trailing_stop"
                      },
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                } else {
                  if (high_k >= current_sl) {
                    exit_price <- current_sl
                    outcome <- if (exit_price < i.entry_price) {
                      "win"
                    } else if (exit_price > i.entry_price) {
                      "loss"
                    } else {
                      "breakeven"
                    }
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = exit_price,
                      outcome = outcome,
                      exit_reason = if (current_sl == i.sl_price) {
                        "stop_loss"
                      } else {
                        "trailing_stop"
                      },
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                }

                # ----- Priority 3: Opposite CHoCH -----
                # choch_type is "no_choch" on every non-breach bar, so any comparison
                # against a real label implies a fresh breach on this bar. The
                # _range_choch variants no longer exist — the walk's trend_state
                # distinguishes reversals from continuations directly.
                if (i.signal_type == "bullish") {
                  if (h4_choch_type[k] == "bear_choch") {
                    exit_price <- h4_choch_level[k]
                    outcome <- if (exit_price > i.entry_price) {
                      "win"
                    } else if (exit_price < i.entry_price) {
                      "loss"
                    } else {
                      "breakeven"
                    }
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = exit_price,
                      outcome = outcome,
                      exit_reason = "choch",
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                } else {
                  if (h4_choch_type[k] == "bull_choch") {
                    exit_price <- h4_choch_level[k]
                    outcome <- if (exit_price < i.entry_price) {
                      "win"
                    } else if (exit_price > i.entry_price) {
                      "loss"
                    } else {
                      "breakeven"
                    }
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = exit_price,
                      outcome = outcome,
                      exit_reason = "choch",
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                }

                # # ----- Priority 3: Opposite CHoCH -----
                # if (i.signal_type == "bullish") {
                #   if (
                #     h4_choch_type[k] %in% c("bear_choch", "bear_range_choch")
                #   ) {
                #     exit_price <- h4_choch_level[k]
                #     outcome <- if (exit_price > i.entry_price) {
                #       "win"
                #     } else if (exit_price < i.entry_price) {
                #       "loss"
                #     } else {
                #       "breakeven"
                #     }
                #     exit_info <- list(
                #       exit_datetime = dt_k,
                #       exit_price = exit_price,
                #       outcome = outcome,
                #       exit_reason = "choch",
                #       pip_conv = pip_conv_k,
                #       exit_idx = k
                #     )
                #     exit_found <- TRUE
                #     break
                #   }
                # } else {
                #   if (
                #     h4_choch_type[k] %in% c("bull_choch", "bull_range_choch")
                #   ) {
                #     exit_price <- h4_choch_level[k]
                #     outcome <- if (exit_price < i.entry_price) {
                #       "win"
                #     } else if (exit_price > i.entry_price) {
                #       "loss"
                #     } else {
                #       "breakeven"
                #     }
                #     exit_info <- list(
                #       exit_datetime = dt_k,
                #       exit_price = exit_price,
                #       outcome = outcome,
                #       exit_reason = "choch",
                #       pip_conv = pip_conv_k,
                #       exit_idx = k
                #     )
                #     exit_found <- TRUE
                #     break
                #   }
                # }

                # ----- Priority 4: Opposite BOS -----
                if (i.signal_type == "bullish") {
                  if (h4_bos_type[k] %in% c("bear_bos", "bear_range_bos")) {
                    exit_price <- h4_prev_sl[k]
                    outcome <- if (exit_price > i.entry_price) {
                      "win"
                    } else if (exit_price < i.entry_price) {
                      "loss"
                    } else {
                      "breakeven"
                    }
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = exit_price,
                      outcome = outcome,
                      exit_reason = "bos",
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                } else {
                  if (h4_bos_type[k] %in% c("bull_bos", "bull_range_bos")) {
                    exit_price <- h4_prev_sh[k]
                    outcome <- if (exit_price < i.entry_price) {
                      "win"
                    } else if (exit_price > i.entry_price) {
                      "loss"
                    } else {
                      "breakeven"
                    }
                    exit_info <- list(
                      exit_datetime = dt_k,
                      exit_price = exit_price,
                      outcome = outcome,
                      exit_reason = "bos",
                      pip_conv = pip_conv_k,
                      exit_idx = k
                    )
                    exit_found <- TRUE
                    break
                  }
                }

                # ----- Swing-based trailing stop update -----
                # Only update if price has broken a NEW swing level in the
                # direction of the trade, and that swing level is different
                # from the one that last triggered an update.
                if (i.signal_type == "bullish") {
                  # Long: move stop up when a new swing high is broken.
                  # The most recent swing high is h1_prev_sh[k].
                  # We check if high_k >= h4_prev_sh[k] (break) and
                  # h4_prev_sh[k] != last_broken_level.
                  if (
                    !is.na(h4_prev_sh[k]) &&
                      high_k >= h4_prev_sh[k] &&
                      (is.na(last_broken_level) ||
                        h4_prev_sh[k] != last_broken_level)
                  ) {
                    # New stop = most recent swing low - buffer
                    new_stop <- h4_prev_sl[k] - buffer_price
                    # Avoid moving stop down; only move if it improves.
                    if (new_stop > current_sl) {
                      current_sl <- new_stop
                      last_broken_level <- h4_prev_sh[k]
                    }
                  }
                } else {
                  # Short: move stop down when a new swing low is broken.
                  if (
                    !is.na(h4_prev_sl[k]) &&
                      low_k <= h4_prev_sl[k] &&
                      (is.na(last_broken_level) ||
                        h4_prev_sl[k] != last_broken_level)
                  ) {
                    new_stop <- h4_prev_sh[k] + buffer_price
                    if (new_stop < current_sl) {
                      current_sl <- new_stop
                      last_broken_level <- h4_prev_sl[k]
                    }
                  }
                }
              }

              if (!exit_found) {
                exit_info <- list(
                  exit_datetime = as.POSIXct(NA, tz = "UTC"),
                  exit_price = NA_real_,
                  outcome = "open",
                  exit_reason = "open",
                  pip_conv = NA_real_,
                  exit_idx = NA_integer_
                )
              }

              .(
                signal_id = i.signal_id,
                fill_datetime = fill_dt,
                fill_candle_idx = first_fill_idx,
                exit_datetime = exit_info$exit_datetime,
                exit_price = exit_info$exit_price,
                outcome = exit_info$outcome,
                exit_reason = exit_info$exit_reason,
                pip_conv = exit_info$pip_conv,
                exit_candle_idx = exit_info$exit_idx
              )
            }
          }
        }
      ]

      exit_results <- exit_results[,
        .(
          signal_id,
          fill_datetime,
          fill_candle_idx,
          exit_datetime,
          exit_price,
          outcome,
          exit_reason,
          pip_conv,
          exit_candle_idx
        )
      ]

      log_info(
        paste0(
          "Step trailing evaluation:\n------------------------\n",
          " win(s): ",
          sum(exit_results$outcome == "win"),
          "\nloss(es): ",
          sum(exit_results$outcome == "loss"),
          "\nbreakeven(s): ",
          sum(exit_results$outcome == "breakeven"),
          "\ntrailing_stop(s): ",
          sum(exit_results$exit_reason == "trailing_stop"),
          "\nchoch_exit(s): ",
          sum(exit_results$exit_reason == "choch"),
          "\nbos_exit(s): ",
          sum(exit_results$exit_reason == "bos"),
          "\ntime_exit(s): ",
          sum(exit_results$exit_reason == "time_exit"),
          "\nnot_filled: ",
          sum(exit_results$outcome == "not_filled"),
          "\nstill open: ",
          sum(exit_results$outcome == "open")
        )
      )
    },
    error = function(e) {
      log_error(sprintf("Swing trailing evaluation failed: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 5. CALCULATE PROFIT AND LOSS
  # ============================================================================
  # Three numbers are calculated for every resolved trade (win or loss):
  #
  # pnl_pips: the raw price movement converted into pips using pip_conv.
  #            Positive for a win, negative for a loss.
  #
  # pnl_r: pnl_pips divided by the ORIGINAL risk in pips (the distance
  #            from entry to stop loss). This is the "R-multiple" — how
  #            many multiples of your planned risk did you make or lose.
  #            A loss is always exactly -1.0 because the trade closed
  #            exactly at the stop loss by definition.
  #
  # pnl_pct: pnl_r multiplied by risk_pct from config. This converts the
  #            R-multiple into the real account percentage this trade
  #            would have produced, assuming the risk rule was followed
  #            exactly as planned.
  tryCatch(
    {
      log_info("Calculating profit and loss for resolved trades.")

      enriched_signal_log <- signal_log[
        exit_results,
        on = "signal_id"
      ]

      # # DIAGNOSTIC: remove after confirming fix
      # print("First 5 rows of exit_results:")
      # print(head(exit_results, 5))
      # print("Outcomes with NA exit_datetime in exit_results:")
      # print(exit_results[, .N, by = .(outcome, is.na(exit_datetime))])
      #
      # print("First 5 rows of enriched_signal_log (resolved trades):")
      # print(head(
      #   enriched_signal_log[
      #     outcome %in% c("win", "loss"),
      #     .(signal_id, outcome, exit_datetime)
      #   ],
      #   5
      # ))
      # print("NA exit_datetime count in enriched_signal_log (resolved trades):")
      # print(enriched_signal_log[
      #   outcome %in% c("win", "loss"),
      #   sum(is.na(exit_datetime))
      # ])

      # Define resolved outcomes: any trade that actually closed
      resolved_outcomes <- c("win", "loss", "breakeven", "time_exit")

      # Determine trade direction for PnL calculations
      enriched_signal_log[,
        direction_factor := fifelse(signal_type == "bullish", 1, -1)
      ]

      enriched_signal_log[,
        `:=`(
          pnl_pips = fcase(
            outcome %in% resolved_outcomes,
            (exit_price - entry_price) * direction_factor / pip_conv,
            default = NA_real_
          ),

          # Risk in pips: the distance from entry to stop loss.
          # This is the "1R" unit every other measurement is compared to.
          risk_pips = abs(entry_price - sl_price) / pip_conv
        )
      ]

      # R multiple
      enriched_signal_log[,
        pnl_r := fcase(
          outcome %in% resolved_outcomes & risk_pips > 0,
          round(pnl_pips / risk_pips, 2),
          default = NA_real_
        )
      ]

      # Account percentage impact
      enriched_signal_log[,
        pnl_pct := fcase(
          !is.na(pnl_r),
          round(pnl_r * risk_pct * 100, 2),
          default = NA_real_
        )
      ]

      # Hold duration (hours)
      enriched_signal_log[,
        hold_duration_hours := fcase(
          !is.na(exit_datetime),
          round(
            as.numeric(
              difftime(exit_datetime, signal_datetime, units = "hours")
            ),
            1
          ),
          default = NA_real_
        )
      ]

      # ---- NEW: candles to exit ----
      enriched_signal_log[,
        candles_to_exit := fcase(
          outcome %in% resolved_outcomes,
          exit_candle_idx,
          default = NA_integer_
        )
      ]

      # Remove temporary helper columns
      # risk_pips was only needed to compute pnl_r remove it from the
      # final output so the table stays focused on what matters.
      enriched_signal_log[,
        c("direction_factor", "risk_pips", "exit_candle_idx") := NULL
      ]

      log_info("Profit and loss calculation complete.")
    },
    error = function(e) {
      log_error(sprintf("Profit and loss calculation failed: %s", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 6. SUMMARY LOG
  # ============================================================================
  # A quick honest snapshot printed to the log every time this function
  # runs: win rate is calculated only from RESOLVED trades (win or loss),
  # never counting "open" trades as either a win or a loss.
  tryCatch(
    {
      # Resolved trades: those that actually closed
      resolved <- enriched_signal_log[
        outcome %in% c("win", "loss", "breakeven", "time_exit")
      ]

      if (nrow(resolved) > 0) {
        # Win rate: only "win" outcome counts as a win
        wins <- sum(resolved$outcome == "win")
        losses <- sum(resolved$outcome == "loss")
        breakevens <- sum(resolved$outcome == "breakeven")
        win_rate <- round(wins / nrow(resolved) * 100, 1)

        profit_rate <- round(
          sum(resolved$pnl_r > 0, na.rm = TRUE) / nrow(resolved) * 100,
          1
        )

        loss_rate <- round(
          sum(resolved$pnl_r < 0, na.rm = TRUE) / nrow(resolved) * 100,
          1
        )

        avg_r <- round(mean(resolved$pnl_r, na.rm = TRUE), 2)

        # Outcome counts for detailed logging
        n_trailing <- sum(resolved$exit_reason == "trailing_stop")
        n_choch <- sum(resolved$exit_reason == "choch")
        n_bos <- sum(resolved$exit_reason == "bos")
        n_time <- sum(resolved$exit_reason == "time_exit")

        # paste0(
        #   "Backtest summary:\n------------------------\n",
        #   "Win rate: %s%% (%s wins / %s resolved)\n",
        #   "Profit rate: %s%% (%s profittable/ %s resolved)\n",
        #   "Loss rate: %s%% (%s losses / %s resolved trades)\n",
        #   "Average R: %s\n------------------------\n",
        #   "Outcome breakdown:\n------------------------\n",
        #   "Win(s): %s\nTrailing stop(s): %s\n",
        #   "CHoCH exit(s): %s\nBOS exit(s): %s\nTime exi(s): %s\nOpen: %s\n",
        #   "Not filled: %s"
        # )

        log_info(sprintf(
          paste0(
            "Backtest summary:\n------------------------\n",
            "Win rate: %s%% (%s wins / %s resolved)\n",
            "Profit rate: %s%% (%s profitable/ %s resolved)\n",
            "Loss rate: %s%% (%s losses / %s resolved trades)\n",
            "Breakeven count: %s\n",
            "Average R: %s\n------------------------\n",
            "Outcome breakdown:\n------------------------\n",
            "Win(s): %s\nLoss(es): %s\nBreakeven(s): %s\n",
            "Trailing stop exits: %s\n",
            "CHoCH exits: %s\nBOS exits: %s\nTime exits: %s\n",
            "Open: %s\nNot filled: %s"
          ),
          win_rate,
          wins,
          nrow(resolved),
          profit_rate,
          sum(resolved$pnl_r > 0, na.rm = TRUE),
          nrow(resolved),
          loss_rate,
          sum(resolved$pnl_r < 0, na.rm = TRUE),
          nrow(resolved),
          breakevens,
          avg_r,
          wins,
          losses,
          breakevens,
          n_trailing,
          n_choch,
          n_bos,
          n_time,
          sum(resolved$outcome == "open"),
          sum(resolved$outcome == "not_filled")
        ))
      } else {
        log_info(
          "Backtest summary: No resolved trades yet. All signals still open."
        )
      }

      log_info("Backtest engine complete. Returning enriched_signal_log.")

      enriched_signal_log
    },
    error = function(e) {
      log_error(sprintf("Summary log failed: %s", e$message))
      stop(e)
    }
  )
}

################################################################################
