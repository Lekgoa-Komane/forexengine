# ==============================================================================
# DYNAMIC BOS / CHoCH STATE MACHINE
# ==============================================================================
# dynamic_bos_choch ------------------------------------------------------
#
# Walk one ticker's bars in chronological order and derive, per bar:
#   prev_sh     : last confirmed swing high
#   prev_sl     : last confirmed swing low
#   bull_level  : currently active unbreached swing high
#   bear_level  : currently active unbreached swing low
#   choch_level : reversal-side level for the current trend_state
#   choch_type  : CHoCH label - non-"no_choch" ONLY on the breach bar
#   bos_type    : BOS label   - non-"no_bos"   ONLY on the breach bar
#   sweep_type  : liquidity-sweep flag
#   trend_state : "bullish" | "bearish" - the structural trend, updated
#                 ONLY on a CHoCH event. Seeded from the first non-NA,
#                 non-range bias seen in the ticker.
#
# Semantics
#   * A confirmed swing replaces the previous candidate on that side.
#   * Both sides are tracked independently and simultaneously.
#   * A breach is only an event if the close moves more than vol_thresh
#     away from the level.
#   * After a breach the level is cleared; a fresh swing must confirm
#     before a new event can fire on that side.
#
# Label rule (uses internal trend_state, NOT bias[i])
#   * close > active_up and trend_state == "bearish" -> bull_choch
#   * close > active_up and trend_state == "bullish" -> bull_bos
#   * close < active_dn and trend_state == "bullish" -> bear_choch
#   * close < active_dn and trend_state == "bearish" -> bear_bos
# trend_state flips ONLY on a CHoCH, never on a BOS.

compute_dynamic_bos_choch <- function(dt, choch_quant, dbg_bars = NULL) {
  n <- nrow(dt)

  # ---- Pre-allocated outputs -------------------------------------------
  prev_sh_out <- rep(NA_real_, n)
  prev_sl_out <- rep(NA_real_, n)
  bull_lvl_out <- rep(NA_real_, n)
  bear_lvl_out <- rep(NA_real_, n)
  choch_lvl_out <- rep(NA_real_, n)
  choch_typ_out <- rep("no_choch", n)
  bos_typ_out <- rep("no_bos", n)
  sweep_out <- rep(NA_character_, n)
  trend_state_out <- rep(NA_character_, n) # <-- new

  # ---- Local bindings --------------------------------------------------
  hi <- dt$high
  lo <- dt$low
  cl <- dt$close
  sh <- dt$swing_high
  sl <- dt$swing_low
  bias <- dt$bias
  vol <- dt$volatility

  vol_thresh <- quantile(vol, choch_quant, na.rm = TRUE)

  # ---- Running state ---------------------------------------------------
  last_sh <- NA_real_
  last_sl <- NA_real_
  active_up <- NA_real_
  active_dn <- NA_real_
  range_hi <- NA_real_
  range_lo <- NA_real_
  mode <- NA_character_ # "trend" | "range"
  trend_state <- NA_character_ # "bullish" | "bearish"  (internal)

  dbg <- !is.null(dbg_bars)

  for (i in seq_len(n)) {
    # --- 0. Seed trend_state from the first non-NA, non-range bias -----
    # Done once per ticker. After this, trend_state flips ONLY on a
    # CHoCH event, so bias changes are ignored for label purposes.
    if (
      is.na(trend_state) &&
        !is.na(bias[i]) &&
        bias[i] %in% c("bullish", "bearish")
    ) {
      trend_state <- bias[i]
    }

    # --- 1. New swing confirmation --------------------------------------
    new_sh <- !is.na(sh[i]) && (is.na(last_sh) || sh[i] != last_sh)
    new_sl <- !is.na(sl[i]) && (is.na(last_sl) || sl[i] != last_sl)

    if (new_sh) {
      last_sh <- sh[i]
      if (!is.na(mode) && mode == "trend") {
        active_up <- sh[i]
      } else if (!is.na(mode) && mode == "range") {
        range_hi <- max(range_hi, sh[i], na.rm = TRUE)
      }
    }
    if (new_sl) {
      last_sl <- sl[i]
      if (!is.na(mode) && mode == "trend") {
        active_dn <- sl[i]
      } else if (!is.na(mode) && mode == "range") {
        range_lo <- min(range_lo, sl[i], na.rm = TRUE)
      }
    }

    # --- 2. Mode transitions --------------------------------------------
    new_mode <- if (is.na(bias[i])) {
      NA_character_
    } else if (bias[i] == "range") {
      "range"
    } else {
      "trend"
    }

    if (!is.na(new_mode) && (is.na(mode) || new_mode != mode)) {
      if (new_mode == "range") {
        range_hi <- last_sh
        range_lo <- last_sl
      } else {
        active_up <- last_sh
        active_dn <- last_sl
        range_hi <- NA_real_
        range_lo <- NA_real_
      }
      mode <- new_mode
    }

    # --- 3. Snapshot per-bar state --------------------------------------
    prev_sh_out[i] <- last_sh
    prev_sl_out[i] <- last_sl
    bull_lvl_out[i] <- active_up
    bear_lvl_out[i] <- active_dn
    trend_state_out[i] <- trend_state # <-- new

    # --- 4. Optional diagnostic -----------------------------------------
    if (dbg && i %in% dbg_bars) {
      cat(sprintf(
        "bar %5d  mode=%-5s bias=%-8s trend_state=%-8s  bull=%-10s bear=%-10s  cl=%-10s\n",
        i,
        ifelse(is.na(mode), "NA", mode),
        ifelse(is.na(bias[i]), "NA", bias[i]),
        ifelse(is.na(trend_state), "NA", trend_state),
        ifelse(is.na(active_up), "NA", format(active_up, digits = 8)),
        ifelse(is.na(active_dn), "NA", format(active_dn, digits = 8)),
        format(cl[i], digits = 8)
      ))
    }

    # --- 5. Liquidity sweep detection -----------------------------------
    if (!is.na(last_sh) && hi[i] > last_sh && cl[i] < last_sh) {
      sweep_out[i] <- "bull_liq_sweep"
    } else if (!is.na(last_sl) && lo[i] < last_sl && cl[i] > last_sl) {
      sweep_out[i] <- "bear_liq_sweep"
    }

    # --- 6. Breach detection (trend mode) -------------------------------
    if (!is.na(mode) && mode == "trend") {
      # Bull-side breach: label from trend_state, not bias[i]
      if (!is.na(active_up) && cl[i] > active_up) {
        move <- log(cl[i] / active_up) * 100
        if (move > vol_thresh) {
          if (!is.na(trend_state) && trend_state == "bearish") {
            choch_typ_out[i] <- "bull_choch"
            trend_state <- "bullish" # flip
          } else {
            bos_typ_out[i] <- "bull_bos"
          }
          choch_lvl_out[i] <- active_up
          active_up <- NA_real_
          next
        }
      }

      # Bear-side breach: label from trend_state, not bias[i]
      if (!is.na(active_dn) && cl[i] < active_dn) {
        move <- log(cl[i] / active_dn) * 100
        if (move < -vol_thresh) {
          if (!is.na(trend_state) && trend_state == "bullish") {
            choch_typ_out[i] <- "bear_choch"
            trend_state <- "bearish" # flip
          } else {
            bos_typ_out[i] <- "bear_bos"
          }
          choch_lvl_out[i] <- active_dn
          active_dn <- NA_real_
          next
        }
      }
    } else if (!is.na(mode) && mode == "range") {
      # --- 7. Breach detection (range mode) -----------------------------
      # Range breakout does NOT flip trend_state - it seeds the next
      # trend_state on the following bar via step 0 (bias will be
      # directional once the breakout is confirmed).
      if (!is.na(range_hi) && cl[i] > range_hi) {
        move <- log(cl[i] / range_hi) * 100
        if (move > vol_thresh) {
          bos_typ_out[i] <- "bull_range_bos"
          choch_lvl_out[i] <- range_hi
          range_hi <- NA_real_
          range_lo <- NA_real_
          next
        }
      }
      if (!is.na(range_lo) && cl[i] < range_lo) {
        move <- log(cl[i] / range_lo) * 100
        if (move < -vol_thresh) {
          bos_typ_out[i] <- "bear_range_bos"
          choch_lvl_out[i] <- range_lo
          range_hi <- NA_real_
          range_lo <- NA_real_
          next
        }
      }
    }

    # --- 8. Default choch_level = reversal-side level for trend_state ---
    if (is.na(choch_lvl_out[i])) {
      if (!is.na(mode) && mode == "range") {
        choch_lvl_out[i] <- if (!is.na(range_hi)) range_hi else range_lo
      } else if (!is.na(trend_state) && trend_state == "bullish") {
        choch_lvl_out[i] <- active_dn
      } else if (!is.na(trend_state) && trend_state == "bearish") {
        choch_lvl_out[i] <- active_up
      } else {
        choch_lvl_out[i] <- if (!is.na(active_up)) active_up else active_dn
      }
    }
  }

  list(
    prev_sh = prev_sh_out,
    prev_sl = prev_sl_out,
    bull_level = bull_lvl_out,
    bear_level = bear_lvl_out,
    choch_level = choch_lvl_out,
    choch_type = choch_typ_out,
    bos_type = bos_typ_out,
    sweep_type = sweep_out,
    trend_state = trend_state_out # <-- new
  )
}

################################################################################

# Institutional Mapping ---------------------------------------------------

#' Perform institutional mapping for one timeframe
#'
#' Applies Smart Money Concepts (SMC) analysis to a single timeframe dataset.
#' The function detects swing points, establishes bias, computes fair value
#' gaps (FVG), order blocks (OB), Break of Structure (BOS), Change of
#' Character (ChoCH), and liquidity levels. It also builds a unified registry
#' of zones, handles mitigation and invalidation, and constructs a Draw on
#' Liquidity (DOL) table. For H1, it additionally assigns session labels and
#' kill zones.
#'
#' @param dt A `data.table` with columns required for the chosen
#'   timeframe. Must include a date column `date_time`, OHLC prices,
#'   `timeframe`, `ticker`, and `asset_class`.
#' @param config A validated configuration list from `load_config()`.
#'   Must contain `analysis` section with trend filters, swing filters,
#'   session definitions, and other parameters.
#'
#' @return A named list with four elements:
#'   \itemize{
#'     \item `{timeframe}_data` - the original data.table enriched with SMC columns.
#'     \item `{timeframe}_registry` - registry of all zones (swing, FVG, OB).
#'     \item `{timeframe}_dol_table` - Draw on Liquidity search table.
#'     \item `swings` - swing point details used during analysis.
#'   }
#'   The actual names depend on the input timeframe (e.g., `weekly_data`,
#'   `weekly_registry`, `weekly_dol_table`).
#'
#' @import data.table
#' @importFrom logger log_info log_error log_warn log_debug
#' @importFrom rlang %||%
#' @importFrom IncDTW find_peaks
#' @importFrom zoo na.locf
#' @importFrom roll roll_quantile
#' @useDynLib forexengine, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom lubridate ceiling_date floor_date days minutes hours weeks make_date
#' @export
#'
#' @examples
#' \dontrun{
#' config <- forexdatasets::load_config()
#' weekly_result <- institutional_mapping(weekly_data, config)
#' names(weekly_result)
#' }

institutional_mapping <- function(dt, config) {
  log_info(
    paste0(
      "Starting ",
      unique(dt$timeframe),
      "_data institutional mapping."
    )
  )

  # ============================================================================
  # 1. INPUT VALIDATION & SETUP
  # ============================================================================
  time_frame <- unique(dt$timeframe)

  # 1. Store the dynamic name string in a variable
  registry <- paste0(time_frame, "_registry")

  # 1. Store the dynamic name string in a variable
  dol_table <- paste0(time_frame, "_dol_table")

  if (!inherits(dt, "data.table")) {
    log_error(
      paste0(
        "Input",
        time_frame,
        "_data must be a data.table."
      )
    )
    stop(
      paste0(
        time_frame,
        "_data must be a data.table."
      )
    )
  }

  required_cols <- if (time_frame %in% c("h1", "h4")) {
    c(
      "date_time",
      "open",
      "high",
      "low",
      "close",
      "timeframe",
      "ticker",
      "asset_class"
    )
  } else {
    c(
      "date_time",
      "open",
      "high",
      "low",
      "close",
      "volume",
      "open_interest",
      "timeframe",
      "ticker",
      "asset_class"
    )
  }

  missing <- setdiff(
    required_cols,
    names(dt)
  )

  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing columns: %s",
        paste(missing, collapse = ", ")
      )
    )
  }

  if (
    is.null(
      config$analysis$trend_filter[[time_frame]]$lookback_period
    )
  ) {
    stop(
      "config missing required sections: 'lookback_period'"
    )
  }

  dt[,
    `:=`(row_id = rowid(ticker))
  ][, `:=`(join_date = date_time), by = .(asset_class, ticker)]

  data.table::setkeyv(
    dt,
    c("asset_class", "ticker", "date_time", "join_date")
  )

  # ============================================================================
  # PRE-DEFINE TREND FILTERS (from config)
  # ============================================================================
  log_info(
    paste0("Pre-defining ", time_frame, " trend filters")
  )

  # Select the appropriate config section based on data_type
  lookback <- switch(
    time_frame,
    "weekly" = config$analysis$trend_filter[[time_frame]]$lookback_period %||%
      13L,
    "daily" = config$analysis$trend_filter[[time_frame]]$lookback_period %||%
      21L,
    "h4" = config$analysis$trend_filter[[time_frame]]$lookback_period %||% 12L,
    "h1" = config$analysis$trend_filter[[time_frame]]$lookback_period %||% 12L
  )

  # Swings constants
  noise_quant <- config$analysis$swing_filter$noise_filter %||% 0.92
  sw_quant_bot <- config$analysis$swing_filter$sw_gap_bot %||% 0.25
  sw_quant_top <- config$analysis$swing_filter$sw_gap_top %||% 0.75

  # fvg constants
  bot_quant <- config$analysis$trend_filter$signal_filter$lower_gap %||% 0.500
  top_quant <- config$analysis$trend_filter$signal_filter$upper_gap %||% 0.999

  # OB constant
  ob_quant <- config$analysis$swing_filter$ob_filter %||% 0.618

  # CHoCH constant
  choch_quant <- config$analysis$choch_filter$choch_quant %||% 0.618

  # Session time ranges
  sess_config <- config$analysis$sessions

  asian_start <- data.table::as.ITime(sess_config$asian$start)
  asian_end <- data.table::as.ITime(sess_config$asian$end)

  lon_std_start <- data.table::as.ITime(sess_config$london$standard_time$start)
  lon_std_end <- data.table::as.ITime(sess_config$london$standard_time$end)
  lon_dst_start <- data.table::as.ITime(sess_config$london$daylight_time$start)
  lon_dst_end <- data.table::as.ITime(sess_config$london$daylight_time$end)

  ny_std_start <- data.table::as.ITime(sess_config$newyork$standard_time$start)
  ny_std_end <- data.table::as.ITime(sess_config$newyork$standard_time$end)
  ny_dst_start <- data.table::as.ITime(sess_config$newyork$daylight_time$start)
  ny_dst_end <- data.table::as.ITime(sess_config$newyork$daylight_time$end)

  # Kill zone times (first kill zone from each session)
  lon_kz_start <- data.table::as.ITime(
    sess_config$london$kill_zones$london_open$start
  )
  lon_kz_end <- data.table::as.ITime(
    sess_config$london$kill_zones$london_open$end
  )
  ny_kz_start <- data.table::as.ITime(
    sess_config$newyork$kill_zones$ny_open$start
  )
  ny_kz_end <- data.table::as.ITime(sess_config$newyork$kill_zones$ny_open$end)

  # ============================================================================
  # VECTORIZED METRIC COMPUTATION (by-group)
  # ============================================================================
  tryCatch(
    {
      log_info(
        "Computing metrics for dt across {n} tickers with vectorized operations",
        n = length(unique(dt$ticker))
      )

      double_xmin <- .Machine$double.xmin

      dt[,
        `:=`(
          # Handle intra-candle log return safely
          body_return = signif(
            log(
              pmax(close, double_xmin) /
                pmax(open, double_xmin)
            ) *
              100,
            4
          ),

          # Handle intra-candle volatility safely
          volatility = signif(
            log(
              pmax(high, double_xmin) /
                pmax(low, double_xmin)
            ) *
              100,
            4
          ),

          # Time-series log_return: Correctly handles continuity
          return = signif(
            log(
              pmax(close, double_xmin) /
                pmax(shift(close, 1), double_xmin)
            ) *
              100,
            4
          )
        ),
        by = .(asset_class, ticker)
      ][,
        `:=`(
          body_ratio = signif(
            (abs(body_return) / volatility),
            4
          )
        ),
        by = .(asset_class, ticker)
      ]

      log_info(
        "Completed metric computation for new records across {n} tickers",
        n = length(unique(dt$ticker))
      )
    },
    error = function(e) {
      log_warn(paste0("metrics computation failed", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # 2. CORE LOGIC (Swing points, Bias, FVG, O.Blocks, POIs, Mitigation, D.O.L)
  # ============================================================================
  swings <- tryCatch(
    {
      log_info(
        paste0(
          "Detecting ",
          time_frame,
          " structural preliminary swings and bias ",
          "using a lookback period of ",
          lookback,
          " ",
          time_frame,
          " candles"
        )
      )

      # We use IncDTW's find_peaks function to identify swing highs and lows
      # based on the specified lookback period.
      dt_filtered <- tryCatch(
        {
          log_info(paste0(
            "Starting the detection of ",
            time_frame,
            " swing points"
          ))

          dt_cols <- c(
            "date_time",
            "open",
            "high",
            "low",
            "close",
            "body_return",
            "volatility",
            "ticker",
            "asset_class"
          )

          dt_filtered <- dt[, ..dt_cols]

          # We use IncDTW's find_peaks function to identify swing highs and lows
          # based on the specified lookback period.
          dt_filtered[,
            `:=`(
              swing_high = fifelse(
                seq_len(.N) %in%
                  IncDTW::find_peaks(high, lookback, get_min = FALSE),
                high,
                NA_real_
              ),
              swing_low = fifelse(
                seq_len(.N) %in%
                  IncDTW::find_peaks(low, lookback, get_min = TRUE),
                low,
                NA_real_
              )
            ),
            by = .(asset_class, ticker)
          ]

          # We then assign a unified 'swing_price' and 'swing_type' for
          # easier processing.
          dt_filtered[,
            `:=`(
              swing_type = fifelse(
                !is.na(swing_high),
                "high",
                fifelse(
                  !is.na(swing_low),
                  "low",
                  NA_character_
                )
              )
            ),
            by = .(asset_class, ticker)
          ]

          log_info(paste0(time_frame, " swing points detected successfully"))

          dt_filtered[]
        },
        error = function(e) {
          log_error(paste0(
            "Error in detecting ",
            time_frame,
            " swing points: ",
            e$message
          ))
          stop(e)
        }
      )

      # Detection & removal of anomalies, which are candle that obscure price
      tryCatch(
        {
          log_info("Starting the detection & removal of anomalies")

          window_size <- switch(
            time_frame,
            "weekly" = lookback,
            "daily" = lookback,
            "h4" = lookback + 1L,
            "h1" = lookback + 1L
          )

          # Rolling median that works on any group size and never forks
          roll_med <- function(x, win_sz) {
            x <- fifelse(is.na(x), 0, x) # runmed can't take NA
            n <- length(x)
            win_sz <- min(win_sz, if (n %% 2 == 0) n - 1 else n) # window can't be bigger than the data
            if (win_sz < 3) {
              return(rep(NA_real_, n))
            }
            runmed(x, k = win_sz, endrule = "keep")
          }

          flag_anomalies <- function(st, k = 10, win = window_size) {
            # win must be ODD
            # setorder(st, ticker, date_time)

            # Bar size, gap from last close, and each wick (all in %)
            st[,
              `:=`(
                gap_pct = signif(
                  abs(
                    log(
                      pmax(open, .Machine$double.xmin) /
                        pmax(shift(close), .Machine$double.xmin)
                    ) *
                      100
                  ),
                  4
                ),
                up_wick_pct = signif(
                  log(
                    pmax(high, .Machine$double.xmin) /
                      pmax(open, close, .Machine$double.xmin)
                  ) *
                    100,
                  4
                ), # catches bad highs
                lo_wick_pct = signif(
                  log(
                    pmin(open, close, .Machine$double.xmax) /
                      pmax(low, .Machine$double.xmin)
                  ) *
                    100,
                  4
                ) # catches bad lows
              ),
              by = ticker
            ]

            # "Normal" size for each ticker = rolling median (shifted by 1 bar)
            st[,
              `:=`(
                med_vol = shift(roll_med(volatility, win)),
                med_gap = shift(roll_med(gap_pct, win)),
                med_up = shift(roll_med(up_wick_pct, win)),
                med_lo = shift(roll_med(lo_wick_pct, win))
              ),
              by = ticker
            ]

            # Limits, with a floor so quiet periods don't make them too tiny
            st[, `:=`(
              limit = k * pmax(med_vol, 0.02),
              limit_up = k * pmax(med_up, 0.01),
              limit_lo = k * pmax(med_lo, 0.01)
            )]

            st[,
              is_anomaly := fcoalesce(
                (volatility > limit & gap_pct > limit) |
                  (volatility > limit & up_wick_pct > limit_up) |
                  (volatility > limit & lo_wick_pct > limit_lo),
                FALSE
              )
            ]
            st[]
          }

          dt_filtered <- flag_anomalies(dt_filtered, k = 10, win = window_size)

          log_info(paste0(
            time_frame,
            " anomalies detected & removed successfully"
          ))

          dt_filtered[]
        },
        error = function(e) {
          log_error(paste0(
            "Error in detecting & removing ",
            time_frame,
            " anomalies: ",
            e$message
          ))
          stop(e)
        }
      )

      # Use IncDTW's find_peaks function to identify swing highs and lows
      # based on the specified lookback period after removing anomalies.
      tryCatch(
        {
          log_info(paste0(
            "Starting the detection of ",
            time_frame,
            " swing points after removing anomalies"
          ))

          dt_cols <- c(
            "date_time",
            "open",
            "high",
            "low",
            "close",
            "body_return",
            "volatility",
            "ticker",
            "asset_class"
          )

          dt_filtered <- if (time_frame == "daily") {
            dt_filtered[
              !(is_anomaly == TRUE &
                !is.na(swing_type)) &
                !(ticker == "audcad" &
                  date_time == as.POSIXct("2019-01-02", tz = "UTC")),
              ..dt_cols
            ]
          } else {
            dt_filtered[
              !(is_anomaly == TRUE & !is.na(swing_type)),
              ..dt_cols
            ]
          }

          dt_filtered[,
            `:=`(
              swing_high = fifelse(
                seq_len(.N) %in%
                  IncDTW::find_peaks(high, lookback, get_min = FALSE),
                high,
                NA_real_
              ),
              swing_low = fifelse(
                seq_len(.N) %in%
                  IncDTW::find_peaks(low, lookback, get_min = TRUE),
                low,
                NA_real_
              )
            ),
            by = .(asset_class, ticker)
          ]

          dt_filtered[,
            `:=`(
              swing_type = fifelse(
                !is.na(swing_high),
                "high",
                fifelse(
                  !is.na(swing_low),
                  "low",
                  NA_character_
                )
              ),
              swing_price = fifelse(
                !is.na(swing_high),
                swing_high,
                swing_low
              )
            ),
            by = .(asset_class, ticker)
          ]

          log_info(paste0(
            time_frame,
            " swing points detected successfully",
            " after anomalies"
          ))

          dt_filtered[]
        },
        error = function(e) {
          log_error(paste0(
            "Error in detecting ",
            time_frame,
            " swing points after removing anomalies: ",
            e$message
          ))
          stop(e)
        }
      )

      # Handle duplicate swing prices
      # (e.g., multiple highs at the same level) by
      # Calculate previous swing highs/lows and the
      # percentage sw_gaperence between swings to determine the strength
      # of the swing and potential sweeps
      sw_pts <- tryCatch(
        {
          log_info(paste0(
            "Starting ",
            time_frame,
            " swing point deduplication & computing pct gap between swing points"
          ))

          sw_cols <- c(
            "date_time",
            "close",
            "swing_high",
            "swing_low",
            "swing_price",
            "swing_type",
            "ticker",
            "asset_class"
          )

          dt_filtered <- dt_filtered[!is.na(swing_price), ..sw_cols]

          # Handle duplicate swing prices
          # (e.g., multiple highs at the same level) by
          dt_filtered[,
            `:=`(
              dup_price = fifelse(
                swing_type == "low",
                min(swing_price),
                max(swing_price)
              )
            ),
            by = .(
              asset_class,
              ticker,
              rleid(swing_type)
            )
          ]

          dt_filtered <- dt_filtered[
            swing_price == dup_price
          ]

          dt_filtered <- dt_filtered[
            dt_filtered[, .I[1], by = .(rleid(swing_type, swing_price))]$V1
          ]

          dt_filtered[,
            next_sp := shift(swing_price, type = "lead"),
            by = .(asset_class, ticker, swing_type)
          ]

          # Calculate previous swing highs/lows and the
          # percentage sw_gaperence between swings to determine the strength
          # of the swing and potential sweeps
          dt_filtered[,
            `:=`(
              prev_sh = fcase(
                swing_type == "high" &
                  swing_type[.N] == "high" &
                  seq_len(.N) < .N,
                swing_price,
                swing_type == "high" &
                  swing_type[.N] != "high" &
                  seq_len(.N) < (.N - 1),
                swing_price,
                default = NA_real_
              ),
              prev_sl = fcase(
                swing_type == "low" &
                  swing_type[.N] == "low" &
                  seq_len(.N) < .N,
                swing_price,
                swing_type == "low" &
                  swing_type[.N] != "low" &
                  seq_len(.N) < (.N - 1),
                swing_price,
                default = NA_real_
              ),
              sw_gap = {
                swing_alt <- fifelse(
                  (swing_type[.N] == "high" |
                    swing_type[.N] == "low") &
                    seq_len(.N) < .N,
                  swing_type,
                  NA_character_
                )

                fcase(
                  swing_type == "low",
                  (log(
                    pmax(swing_low, .Machine$double.xmin) /
                      shift(pmax(swing_low, .Machine$double.xmin), 2)
                  ) *
                    100),

                  swing_type == "high",
                  (log(
                    pmax(swing_high, .Machine$double.xmin) /
                      shift(pmax(swing_high, .Machine$double.xmin), 2)
                  ) *
                    100),
                  default = NA_real_
                )
              }
            ),
            by = .(asset_class, ticker)
          ]

          log_info(paste0(
            time_frame,
            " swing point deduplication & pct gap computed successfully"
          ))

          dt_filtered[]
        },
        error = function(e) {
          log_error(paste0(
            "Error in deduplicating and computing pct gap of ",
            time_frame,
            " swing points: ",
            e$message
          ))
          stop(e)
        }
      )

      # Determine swing structure (HH, LH, LL, HL) and potential "Re-Tests"
      tryCatch(
        {
          log_info(paste0(
            "starting the determination of ",
            time_frame,
            " swing points structure"
          ))

          sw_pts[,
            `:=`(
              structure = {
                fcase(
                  swing_type == "high" & swing_price > shift(swing_price, 2),
                  "HH",
                  swing_type == "high" & swing_price < shift(swing_price, 2),
                  "LH",
                  swing_type == "low" & swing_price < shift(swing_price, 2),
                  "LL",
                  swing_type == "low" & swing_price > shift(swing_price, 2),
                  "HL",
                  default = NA_character_
                )
              }
            ),
            by = .(asset_class, ticker)
          ]

          sw_pts[
            is.na(structure),
            structure := fcase(
              swing_type == "low" & swing_price < next_sp,
              "LL",
              swing_type == "low" & swing_price > next_sp,
              "HL",
              swing_type == "high" & swing_price < next_sp,
              "LH",
              swing_type == "high" & swing_price > next_sp,
              "HH",
              default = NA_character_
            ),
            by = .(asset_class, ticker)
          ]

          log_info(paste0(
            time_frame,
            " swing points structure determined successfully"
          ))

          sw_pts[]
        },
        error = function(e) {
          log_error(paste0(
            "Error in determining ",
            time_frame,
            " swing points structure: ",
            e$message
          ))
          stop(e)
        }
      )

      # Determine Weekly Bias based on the sequence of swings:
      tryCatch(
        {
          log_info(paste0(
            "starting the determination of ",
            time_frame,
            " bias based on the sequence of swings ponts"
          ))

          sw_pts[,
            `:=`(
              bias = shift(
                {
                  sw_gap_pos <- sw_gap[swing_type == "high" & sw_gap > 0]
                  sw_gap_neg <- sw_gap[swing_type == "low" & sw_gap < 0]

                  fcase(
                    (structure == "HH" &
                      sw_gap >=
                        quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)),
                    "bullish",

                    (structure == "LL" &
                      sw_gap <=
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE)),
                    "bearish",

                    (sw_gap > quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                      sw_gap <
                        quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                      shift(sw_gap, 1) >
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                      shift(sw_gap, 1) <
                        quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                      shift(sw_gap, 2) >
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                      shift(sw_gap, 2) <
                        quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)) |

                      (sw_gap >
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        sw_gap <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, -1) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -1) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, -2) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -2) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)) |

                      (sw_gap >
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        sw_gap <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, -1) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -1) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, 1) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, 1) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)) |

                      (structure %in%
                        c("HH", "HL", "LH", "LL") &
                        !(shift(structure, 1) %in%
                          c("HH", "LL") &
                          shift(sw_gap, 1) <
                            quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                          shift(sw_gap, 1) >
                            quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)) &
                        shift(sw_gap, -1) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -1) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, -2) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -2) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, -3) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -3) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)) |

                      (sw_gap >
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        sw_gap <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(structure, -1) %in% c("HL", "LH") &
                        shift(sw_gap, -2) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -2) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, -3) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -3) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(sw_gap, -4) >
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(sw_gap, -4) <
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)),
                    "range",

                    (structure %in%
                      c("HH", "LH") &
                      !(shift(sw_gap, -1) <=
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(structure, -1) == "LL") &
                      shift(sw_gap, -2) >=
                        quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                      shift(structure, -2) == "HH") |

                      (structure %in%
                        c("HL", "LL") &
                        is.na(sw_gap) &
                        is.na(shift(sw_gap, 1)) &
                        shift(sw_gap, -1) >=
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(structure, -1) == "HH") |

                      (structure %in%
                        c("HH", "LH") &
                        !(shift(sw_gap, -1) <=
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE)) &
                        shift(structure, -2) %in%
                          c("HH", "LH") &
                        !(shift(sw_gap, -3) <=
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE)) &
                        shift(sw_gap, -4) >=
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(structure, -4) == "HH"),
                    "bullish",

                    (structure %in%
                      c("LL", "HL") &
                      !(shift(sw_gap, -1) >=
                        quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE) &
                        shift(structure, -1) == "HH") &
                      shift(sw_gap, -2) <=
                        quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                      shift(structure, -2) == "LL") |

                      (structure %in%
                        c("HH", "LH") &
                        is.na(sw_gap) &
                        is.na(shift(sw_gap, 1)) &
                        shift(sw_gap, -1) <=
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(structure, -1) == "LL") |

                      (structure %in%
                        c("LL", "HL") &
                        !(shift(sw_gap, -1) >=
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)) &
                        shift(structure, -2) %in%
                          c("LL", "HL") &
                        !(shift(sw_gap, -3) >=
                          quantile(sw_gap_pos, sw_quant_bot, na.rm = TRUE)) &
                        shift(sw_gap, -4) <=
                          quantile(sw_gap_neg, sw_quant_top, na.rm = TRUE) &
                        shift(structure, -4) == "LL"),
                    "bearish",

                    default = NA_character_
                  )
                },
                -1
              )
            ),
            by = .(asset_class, ticker)
          ]

          log_info(paste0(
            time_frame,
            " bias determined successfully"
          ))

          sw_pts[]
        },
        error = function(e) {
          log_error(paste0(
            "Error in determining ",
            time_frame,
            " bias: ",
            e$message
          ))
          stop(e)
        }
      )

      log_info(
        paste0(
          "Completed ",
          time_frame,
          " structural preliminary swings and ",
          "bias detection successfully."
        )
      )

      sw_final_cols <- c(
        "date_time",
        "swing_high",
        "swing_low",
        "prev_sh",
        "prev_sl",
        "swing_price",
        "sw_gap",
        "swing_type",
        "structure",
        "bias",
        "ticker",
        "asset_class"
      )

      sw_pts <- sw_pts[, ..sw_final_cols]
    },
    error = function(e) {
      log_error(paste0(
        "Error in detecting ",
        time_frame,
        " structural swings and bias: ",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 3. MERGE SWING INFORMATION BACK TO MAIN DATA.TABLE
  # ----------------------------------------------------------------------------
  # Brings per-swing flags back onto the bar-level `dt`:
  #   * swing_high / swing_low    : NA except on confirmation bars
  #   * bias                      : forward-filled so every bar has a direction
  #   * bias_id                   : rleid of bias, one id per segment
  #   * swing_high_id / _low_id   : sequence numbers, used by the registry
  #
  # NOTE: prev_sh / prev_sl are NO LONGER produced here. They are derived
  # bar-by-bar in Section 11 by the dynamic BOS/CHoCH state machine. Any
  # downstream code reading prev_sh / prev_sl must run after Section 11.
  # ============================================================================
  tryCatch(
    {
      log_info(paste0(
        "Merging ",
        time_frame,
        " swing information back to the main data.table"
      ))

      join_on <- c("date_time", "ticker", "asset_class")

      # --- 3a. Attach swing flags to bar-level dt -------------------------
      # Only swing_high, swing_low and bias are carried across; prev_sh /
      # prev_sl are left to Section 11's state machine.
      dt[
        swings,
        on = join_on,
        `:=`(
          swing_high = i.swing_high,
          swing_low = i.swing_low,
          bias = i.bias
        )
      ]

      # --- 3b. Forward-fill bias across all bars --------------------------
      # bias is only set on confirmation bars; LOCF makes it available on
      # every bar for downstream logic (including the Section 11 walk).
      dt[,
        `:=`(
          bias = zoo::na.locf(bias, fromLast = FALSE, na.rm = FALSE)
        ),
        by = .(asset_class, ticker)
      ][,
        `:=`(
          bias_id = rleid(bias)
        ),
        by = .(asset_class, ticker)
      ]

      # --- 3c. Assign swing sequence ids ----------------------------------
      # Used by the unified registry in Section 6 to key each swing
      # occurrence.
      dt[
        !is.na(swing_high),
        `:=`(
          swing_high_id = seq_len(.N)
        ),
        by = .(asset_class, ticker)
      ][
        !is.na(swing_low),
        `:=`(
          swing_low_id = seq_len(.N)
        ),
        by = .(asset_class, ticker)
      ]

      log_info(paste0(
        "Merged ",
        time_frame,
        " swing information successfully; ",
        "bias forward-filled; bias_id and swing ids assigned. ",
        "prev_sh / prev_sl deferred to Section 11."
      ))

      dt
    },
    error = function(e) {
      log_error(paste0(
        "Error in merging ",
        time_frame,
        " structural swings and bias: ",
        e$message
      ))
      stop(e)
    }
  )

  # ========================================================================
  # 4. FVG REGISTRY (with IDs) ---
  # ========================================================================
  tryCatch(
    {
      log_info(
        paste0("Starting ", time_frame, " fair value gap registry computation")
      )

      # A "big enough gap" ruler must not be the same for every ticker or
      # every timeframe. AUDCAD barely moves compared to GBPJPY or gold -
      # so the threshold for "meaningful" has to bend to each ticker's own
      # normal behaviour. window_size below decides how many candles of
      # history that rolling ruler looks back over - roughly 3 months of
      # real time, no matter which timeframe this is.
      window_size <- switch(
        time_frame,
        "weekly" = 26, # ~26 weeks in 6 months
        "daily" = 63, # ~63 trading days in 3 months
        "h4" = 126, # 6 H4 candles/day x 21 trading days
        "h1" = 504 # 24 H1 candles/day x 21 trading days
      )

      min_gap_obs <- switch(
        time_frame,
        "weekly" = 4, # window is only 13 candles - asking for 12 is impossible
        "daily" = 8, # your original number - reasonable, worth double-checking
        "h4" = 12, # 378 candles in window, 12 is a reasonable minimum
        "h1" = 24
      )

      dt[,
        `:=`(
          bull_fvg_gap = fcase(
            low > shift(high, 2),
            log(
              pmax(low, .Machine$double.xmin) /
                pmax(shift(high, 2), .Machine$double.xmin)
            ) *
              100,
            default = NA_real_
          ),

          bear_fvg_gap = fcase(
            high < shift(low, 2),
            log(
              pmax(high, .Machine$double.xmin) /
                pmax(shift(low, 2), .Machine$double.xmin)
            ) *
              100,
            default = NA_real_
          )
        ),
        by = .(asset_class, ticker)
      ]

      # These two numbers define your "big enough but not too big" band,
      # measured on the BULLISH scale (bull_fvg_gap is always positive).
      # Keeping them as named values here means you can tune them in one
      # place, and they will later move into config.yml alongside
      # min_confluence, risk_pct and buffer_pipette.
      # lower_gap <- 0.618 # below this = too small, likely noise
      # upper_gap <- 0.854 # above this = extreme outlier, unlikely to revisit

      # ROLLING quantile band - this is the fix for the lookahead bias.
      # Each row's boundary now uses only the PAST window_size candles,
      # never anything from the future.
      #
      # IMPORTANT: bear_fvg_gap is always NEGATIVE (it comes from a ratio
      # below 1 inside log()). A "bigger" bearish gap is a MORE negative
      # number, not a larger one - like -100 degrees being colder than
      # -10 degrees. So to target the exact same size band on the
      # bearish side, the percentiles must be flipped: use
      # (1 - upper_gap) and (1 - lower_gap) instead of using
      # lower_gap and upper_gap directly. Using them directly (as an
      # earlier version of this code did) would silently break bearish
      # FVG detection completely, because the resulting lower boundary
      # would end up NUMERICALLY GREATER than the upper boundary - and
      # %between% simply returns FALSE for every row when that happens.
      dt[,
        `:=`(
          bull_lower = roll::roll_quantile(
            bull_fvg_gap,
            width = window_size,
            p = bot_quant,
            min_obs = min_gap_obs
          ),
          bull_upper = roll::roll_quantile(
            bull_fvg_gap,
            width = window_size,
            p = top_quant,
            min_obs = min_gap_obs
          ),
          bear_lower = roll::roll_quantile(
            bear_fvg_gap,
            width = window_size,
            p = 1 - top_quant,
            min_obs = min_gap_obs
          ),
          bear_upper = roll::roll_quantile(
            bear_fvg_gap,
            width = window_size,
            p = 1 - bot_quant,
            min_obs = min_gap_obs
          )
        ),
        by = .(asset_class, ticker)
      ]
      # bear_fvg_gap is negative, so its 0.854 quantile (a MORE negative,
      # bigger-magnitude number) is the correct LOWER bound in raw value
      # terms, and its 0.618 quantile (a LESS negative, smaller-magnitude
      # number) is the correct UPPER bound. This mirrors bull_fvg_gap's
      # band exactly once you think in terms of "how big is the gap",
      # not just the raw signed number.
      dt[,
        `:=`(
          upper_boundary = fcase(
            !is.na(bull_fvg_gap),
            bull_upper,
            !is.na(bear_fvg_gap),
            bear_upper,
            default = NA_real_
          ),
          lower_boundary = fcase(
            !is.na(bull_fvg_gap),
            bull_lower,
            !is.na(bear_fvg_gap),
            bear_lower,
            default = NA_real_
          )
        ),
        by = .(asset_class, ticker)
      ]

      # Working columns no longer needed now that upper_boundary and
      # lower_boundary have been assembled from them.
      dt[, c("bull_lower", "bull_upper", "bear_lower", "bear_upper") := NULL]

      # We define the conditions for a bullish FVG (potential demand zone)
      # and a bearish FVG (potential supply zone) based on the relationship
      # between the current candle's low/high and the previous swing
      # high/low, as well as the current bias. We also shift the FVG levels
      # back by 2 candles to ensure they are based on the last confirmed
      # swing and not the current candle which may still be forming.
      dt[,
        `:=`(
          # --- Bullish FVG (Gap Up) ---
          # Condition: current low > high from 2 candles ago (gap up) the gap
          # is the space between that high (bottom) and current low (top).
          bull_fvg_top = fcase(
            bull_fvg_gap %between%
              list(lower_boundary, upper_boundary) &
              low > shift(high, 2),
            low
          ),

          bull_fvg_bot = fcase(
            bull_fvg_gap %between%
              list(lower_boundary, upper_boundary) &
              low > shift(high, 2),
            shift(high, 2)
          ),

          # --- Bearish FVG (Gap Down) ---
          # Condition: current high < low from 2 candles ago (gap down) the gap
          # is the space between that low (top) and current high (bottom).
          bear_fvg_bot = fcase(
            bear_fvg_gap %between%
              list(lower_boundary, upper_boundary) &
              high < shift(low, 2),
            high
          ),

          bear_fvg_top = fcase(
            bear_fvg_gap %between%
              list(lower_boundary, upper_boundary) &
              high < shift(low, 2),
            shift(low, 2)
          )
        ),
        by = .(asset_class, ticker)
      ]

      # --- Unified FVG IDs (one per gap occurrence) ---
      # Assign IDs to FVGs using cumsum to create a unique identifier for
      # each new FVG
      dt[
        !is.na(bull_fvg_top),
        `:=`(
          bull_fvg_id = seq_len(.N)
        ),
        by = .(asset_class, ticker)
      ][
        !is.na(bear_fvg_bot),
        `:=`(
          bear_fvg_id = seq_len(.N)
        ),
        by = .(asset_class, ticker)
      ]

      # Working columns no longer needed
      dt[,
        c(
          "bull_fvg_gap",
          "bear_fvg_gap",
          "lower_boundary",
          "upper_boundary"
        ) := NULL
      ]

      log_info(
        paste0(
          "Completed computing ",
          time_frame,
          " fair value gap registry successfully"
        )
      )

      dt
    },
    error = function(e) {
      log_error(paste0(
        "Error in computing ",
        time_frame,
        " fair value gap registry: ",
        e$message
      ))
      stop(e)
    }
  )

  # ========================================================================
  # 5. ORDER BLOCKS (OB with IDs) ---
  # ========================================================================
  tryCatch(
    {
      log_info(
        paste0("Starting ", time_frame, " order blocks registry computation")
      )

      # At the current candle (the confirmation candle), look back 4 bars
      # to check if there was a strong bearish OB candidate, and whether
      # price has already expanded strongly away from it.
      # This mirrors Section 4 exactly - detection on confirmation candle,
      # both zone boundaries on the same row, no lookahead.
      dt[,
        `:=`(
          # --- Bullish OB ---
          # 4 bars ago was a strong bearish candle (the OB candidate) & current
          # close shows strong bullish expansion from that candle's open
          bull_ob_top = fifelse(
            shift(open, 4) > shift(close, 4) &
              shift(body_ratio, 4) >=
                quantile(body_ratio, (1 - ob_quant), na.rm = TRUE) &
              log(
                pmax(close, .Machine$double.xmin) /
                  pmax(shift(open, 4), .Machine$double.xmin)
              ) *
                100 >=
                quantile(abs(body_return), ob_quant, na.rm = TRUE),
            shift(high, 4),
            NA_integer_
          ),

          bull_ob_bot = fifelse(
            shift(open, 4) > shift(close, 4) &
              shift(body_ratio, 4) >=
                quantile(body_ratio, (1 - ob_quant), na.rm = TRUE) &
              log(
                pmax(close, .Machine$double.xmin) /
                  pmax(shift(open, 4), .Machine$double.xmin)
              ) *
                100 >=
                quantile(abs(body_return), ob_quant, na.rm = TRUE),
            shift(low, 4),
            NA_integer_
          ),

          # --- Bearish OB ---
          # 4 bars ago was a strong bullish candle (the OB candidate) & current
          # close shows strong bearish expansion from that candle's open
          bear_ob_bot = fifelse(
            shift(open, 4) < shift(close, 4) &
              shift(body_ratio, 4) >=
                quantile(body_ratio, (1 - ob_quant), na.rm = TRUE) &
              log(
                pmax(close, .Machine$double.xmin) /
                  pmax(shift(open, 4), .Machine$double.xmin)
              ) *
                100 <=
                (quantile(abs(body_return), ob_quant, na.rm = TRUE) * -1),
            shift(low, 4),
            NA_integer_
          ),

          bear_ob_top = fifelse(
            shift(open, 4) < shift(close, 4) &
              shift(body_ratio, 4) >=
                quantile(body_ratio, (1 - ob_quant), na.rm = TRUE) &
              log(
                pmax(close, .Machine$double.xmin) /
                  pmax(shift(open, 4), .Machine$double.xmin)
              ) *
                100 <=
                (quantile(abs(body_return), ob_quant, na.rm = TRUE) * -1),
            shift(high, 4),
            NA_integer_
          )
        ),
        by = .(asset_class, ticker)
      ]

      # Assign IDs using cumsum
      dt[
        !is.na(bull_ob_top),
        `:=`(
          bull_ob_id = seq_len(.N)
        ),
        by = .(asset_class, ticker)
      ][
        !is.na(bear_ob_bot),
        `:=`(
          bear_ob_id = seq_len(.N)
        ),
        by = .(asset_class, ticker)
      ]

      log_info(
        paste0(
          "Completed computing ",
          time_frame,
          " order blocks registry successfully"
        )
      )

      dt
    },
    error = function(e) {
      log_error(paste0(
        "Error in computing ",
        time_frame,
        "order blocks registry: ",
        e$message
      ))
      stop(e)
    }
  )

  # ========================================================================
  # 6. UNIFIED REGISTRY COMPUTATION ---
  # ========================================================================
  tryCatch(
    {
      target_cols <- grep(
        "^(?!.*(gap|id)).*(swing|fvg|ob)",
        names(dt),
        value = TRUE,
        perl = TRUE
      )

      log_info(paste0(
        "Starting (",
        paste0(target_cols, collapse = "|"),
        ") one row per occurrence, ",
        "taken at its formation candle"
      ))

      id_cols <- unique(paste0(gsub("_bot|_top", "", target_cols), "_id"))

      assign(
        registry,
        rbindlist(
          lapply(id_cols, function(col) {
            dt[
              !is.na(get(col)),
              .(
                formed_at = date_time,
                type = if (grepl("_id", col)) {
                  gsub("_id", "", col)
                } else {
                  NA_character_
                },
                type_id = get(col),
                type_zone_top_value = if (grepl("bull|bear", col)) {
                  get(gsub("id", "top", col))
                } else {
                  NA_real_
                },
                type_zone_bot_value = if (grepl("bull|bear", col)) {
                  get(gsub("id", "bot", col))
                } else {
                  NA_real_
                },
                critical_value = if (grepl("bull", col)) {
                  get(gsub("id", "bot", col))
                } else if (grepl("bear", col)) {
                  get(gsub("id", "top", col))
                } else if (grepl("high|low", col)) {
                  get(gsub("_id", "", col))
                } else {
                  NA_real_
                },
                invalid_rule = "close",
                mitig_rule = if (grepl("bear|high", col)) "high" else "low",
                rel_op = if (grepl("bear|high", col)) " > " else " < ",
                timeframe = timeframe,
                ticker,
                asset_class
              )
            ][, .SD[1], by = .(asset_class, ticker, type_id)]
          }),
          use.names = TRUE,
          fill = TRUE
        )[
          dt,
          on = c("asset_class", "ticker", "formed_at" = "date_time"),
          `:=`(formed_bias = i.bias, formed_bias_id = i.bias_id)
        ]
      )

      setcolorder(
        get(registry),
        c(
          "formed_at",
          "type_id",
          "type",
          "type_zone_bot_value",
          "type_zone_top_value",
          "critical_value",
          "invalid_rule",
          "mitig_rule",
          "rel_op",
          "formed_bias",
          "formed_bias_id",
          "timeframe",
          "ticker",
          "asset_class"
        )
      )

      log_info(paste0(
        "formation candle first occurance for (",
        paste0(target_cols, collapse = "|"),
        ") completed successfully"
      ))

      # Use get() to return the dynamically generated table
      get(registry)
    },
    error = function(e) {
      log_error(paste0(
        "formation candle first occurance for (",
        paste0(target_cols, collapse = "|"),
        ") failed: ",
        e$message
      ))
      stop(e)
    }
  )

  # ========================================================================
  # 7. UNIFIED REGISTRY MITIGATION & INVALIDATION ---
  # ========================================================================
  tryCatch(
    {
      log_info(paste0(
        "Starting fast C++ (",
        paste0(unique(get(registry)$type), collapse = "|"),
        ") mitigation/invalidation processing... for ",
        time_frame,
        " registry"
      ))

      # Define the active interval for each level
      # (from formed_at to mitigated_at, inclusive)
      max_date <- switch(
        time_frame,
        "weekly" = ((dt[, max(date_time)] + weeks(13)) + days(4) + hours(23)),
        "daily" = ((dt[, max(date_time)] + weeks(13)) + hours(23)),
        "h4" = dt[, max(date_time)] + weeks(13),
        "h1" = dt[, max(date_time)] + weeks(13),
      )

      # if (is.null(max_date) || length(max_date) == 0L) {
      #   max_date <- dt[, max(date_time)] + weeks(13)
      # }

      # Pre-sort both tables by date to ensure perfect chronological
      # order inside C++
      setorder(get(registry), asset_class, ticker, formed_at)

      # Initialize columns in registry to accept POSIXct / Numeric timestamps
      get(registry)[, `:=`(
        invalidated_at = NA_real_,
        mit_swing_at = NA_real_,
        mit_zone_prox_at = NA_real_,
        mit_zone_dist_at = NA_real_
      )]

      # Grouped data.table assignment execution loop
      get(registry)[,
        c(
          "invalidated_at",
          "mit_swing_at",
          "mit_zone_prox_at",
          "mit_zone_dist_at"
        ) := {
          # Grab matching chronological data for ONLY this specific ticker group
          sub_dt <- dt[asset_class == .BY$asset_class & ticker == .BY$ticker]

          if (nrow(sub_dt) > 0 && .N > 0) {
            # Process all 8 rule logic variants at once in C++ memory
            process_mitigation_cpp(as.list(.SD), as.list(sub_dt))
          } else {
            list(
              rep(NA_real_, .N),
              rep(NA_real_, .N),
              rep(NA_real_, .N),
              rep(NA_real_, .N)
            )
          }
        },
        by = .(asset_class, ticker)
      ]

      # Re-apply original class format if your dates use POSIXct or
      # Date structures (Optional: uncomment if your raw data dates are classes,
      # not raw integers)
      # date_class <- class(dt$join_date)
      if (time_frame %in% names(config$paths$frequency_folders)[5:6]) {
        get(registry)[, `:=`(
          invalidated_at = as.POSIXct(invalidated_at, tz = "UTC"),
          mit_swing_at = as.POSIXct(mit_swing_at, tz = "UTC"),
          mit_zone_prox_at = as.POSIXct(mit_zone_prox_at, tz = "UTC"),
          mit_zone_dist_at = as.POSIXct(mit_zone_dist_at, tz = "UTC")
        )]
      } else {
        get(registry)[, `:=`(
          invalidated_at = as.POSIXct(invalidated_at, tz = "UTC"),
          mit_swing_at = as.POSIXct(mit_swing_at, tz = "UTC"),
          mit_zone_prox_at = as.POSIXct(mit_zone_prox_at, tz = "UTC"),
          mit_zone_dist_at = as.POSIXct(mit_zone_dist_at, tz = "UTC")
        )]
      }

      if (time_frame == names(config$paths$frequency_folders)[4]) {
        get(registry)[, `:=`(
          invalidated_at = (invalidated_at + hours(23) + minutes(59)),
          mit_swing_at = (mit_swing_at + hours(23) + minutes(59)),
          mit_zone_prox_at = (mit_zone_prox_at + hours(23) + minutes(59)),
          mit_zone_dist_at = (mit_zone_dist_at + hours(23) + minutes(59))
        )]
      } else if (time_frame == names(config$paths$frequency_folders)[3]) {
        get(registry)[, `:=`(
          invalidated_at = (invalidated_at + days(4) + hours(23) + minutes(59)),
          mit_swing_at = (mit_swing_at + days(4) + hours(23) + minutes(59)),
          mit_zone_prox_at = (mit_zone_prox_at +
            days(4) +
            hours(23) +
            minutes(59)),
          mit_zone_dist_at = (mit_zone_dist_at +
            days(4) +
            hours(23) +
            minutes(59))
        )]
      }

      get(registry)[, `:=`(
        invalidated_at = fifelse(
          is.na(invalidated_at),
          max_date,
          invalidated_at
        ),
        mit_swing_at = fifelse(is.na(mit_swing_at), max_date, mit_swing_at),
        mit_zone_prox_at = fifelse(
          is.na(mit_zone_prox_at),
          max_date,
          mit_zone_prox_at
        ),
        mit_zone_dist_at = fifelse(
          is.na(mit_zone_dist_at),
          max_date,
          mit_zone_dist_at
        )
      )]

      setcolorder(
        get(registry),
        c(
          "formed_at",
          "type_id",
          "type",
          "type_zone_bot_value",
          "type_zone_top_value",
          "critical_value",
          "invalid_rule",
          "mitig_rule",
          "rel_op",
          "invalidated_at",
          "mit_swing_at",
          "mit_zone_prox_at",
          "mit_zone_dist_at",
          "formed_bias",
          "formed_bias_id",
          "timeframe",
          "ticker",
          "asset_class"
        )
      )

      log_info(paste0(
        "mitigation/invalidation processing for (",
        paste0(unique(get(registry)$type), collapse = "|"),
        ") completed successfully."
      ))

      get(registry)
    },
    error = function(e) {
      log_error(paste0(
        "mitigation/invalidation processing failed: ",
        e$message
      ))
      stop(e)
    }
  )

  # ========================================================================
  # 8. DRAW ON LIQUIDITY/TRIGGER (D.O.L/D.O.T) SEARCH TABLE COMPUTATIION ---
  # ========================================================================
  tryCatch(
    {
      log_info(
        paste0(
          "Construction of a draw on liquidity/trigger ",
          "(dol/dot) search table"
        )
      )

      # Only zone types have a near-edge stage - swings have no width to speak of
      zone_types <- c("bull_fvg", "bull_ob", "bear_fvg", "bear_ob")

      stage1 <- get(registry)[
        type %in% zone_types,
        .(
          asset_class,
          ticker,
          type_id,
          window_start = formed_at,
          window_end = mit_zone_prox_at,
          dol_value = fifelse(
            type %like% "bull",
            type_zone_top_value,
            type_zone_bot_value
          ),
          dol_label = paste0(type, fifelse(type %like% "bull", "_top", "_bot")),
          formed_bias,
          formed_bias_id,
          timeframe
        )
      ]

      stage2 <- get(registry)[
        type %in% zone_types & !is.na(mit_zone_prox_at),
        .(
          asset_class,
          ticker,
          type_id,
          window_start = mit_zone_prox_at,
          window_end = mit_zone_dist_at,
          dol_value = critical_value,
          dol_label = paste0(type, fifelse(type %like% "bull", "_bot", "_top")),
          formed_bias,
          formed_bias_id,
          timeframe
        )
      ]

      swing_points <- get(registry)[
        !type %in% zone_types,
        .(
          asset_class,
          ticker,
          type_id,
          window_start = formed_at,
          window_end = mit_swing_at,
          dol_value = critical_value,
          dol_label = type,
          formed_bias,
          formed_bias_id,
          timeframe
        )
      ]

      assign(
        dol_table,
        rbind(stage1, stage2, swing_points)
      )

      setcolorder(
        get(dol_table),
        c(
          "window_start",
          "type_id",
          "dol_value",
          "dol_label",
          "formed_bias_id",
          "formed_bias",
          "window_end",
          "timeframe",
          "ticker",
          "asset_class"
        )
      )

      log_info(paste0(
        "Construction of a draw on liquidity/trigger (dol/dot) search table ",
        "completed successfuly"
      ))

      # dol_table
    },
    error = function(e) {
      log_error(paste0(
        "Construction of a draw on liquidity/trigger (dol/dot) search table ",
        "failed:",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 11. Weekly BOS and CHoCH Levels - dynamic state machine
  # ============================================================================
  tryCatch(
    {
      log_info(
        paste0("Starting ", time_frame, " BOS and CHoCH levels computation")
      )

      # ------------------------------------------------------------------------
      # 11a. Run the dynamic state machine.
      #      Walks each ticker once and emits 9 columns:
      #        prev_sh, prev_sl, bull_level, bear_level,
      #        choch_level, choch_type, bos_type, sweep_type, trend_state.
      #
      #      The walk requires chronological order within each ticker.
      #      The `setkeyv` at the top of Section 3 guarantees this.
      # ------------------------------------------------------------------------
      log_info(paste0(
        "Running ",
        time_frame,
        " dynamic BOS/CHoCH state machine"
      ))

      dt[,
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
        ) := compute_dynamic_bos_choch(.SD, choch_quant = choch_quant),
        by = .(asset_class, ticker)
      ]

      log_info(
        paste0(
          "Completed computing ",
          time_frame,
          " BOS and CHoCH levels successfully"
        )
      )
    },
    error = function(e) {
      log_error(paste0(
        "Error in computing ",
        time_frame,
        " BOS and CHoCH levels: ",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 12. EXTRACT DATE/TIME COMPONENTS & CALCULATE DST BOUNDARIES BY YEAR
  # ============================================================================
  if (time_frame == "h1") {
    log_info("Calculating DST boundaries per year")

    # Extract date and time components for easier comparisons
    dt[,
      `:=`(
        trade_date = as.IDate(date_time),
        trade_time = as.ITime(date_time),
        yr = lubridate::year(date_time)
      ),
      by = .(asset_class, ticker)
    ]

    dt_success <- tryCatch(
      {
        years_present <- unique(dt$yr)

        dst_lookup <- data.table(yr = years_present)[,
          {
            # London: Last Sun March to Last Sun Oct
            lds <- make_date(yr, 3, 31) -
              days(lubridate::wday(make_date(yr, 3, 31), week_start = 1) %% 7)
            lde <- make_date(yr, 10, 31) -
              days(lubridate::wday(make_date(yr, 10, 31), week_start = 1) %% 7)
            # NY: 2nd Sun March to 1st Sun Nov
            nys <- make_date(yr, 3, 1) +
              days(
                (7 - lubridate::wday(make_date(yr, 3, 1), week_start = 1)) %% 7
              ) +
              days(7)
            nye <- make_date(yr, 11, 1) +
              days(
                (7 - lubridate::wday(make_date(yr, 11, 1), week_start = 1)) %% 7
              )
            list(lds = lds, lde = lde, nys = nys, nye = nye)
          },
          by = yr
        ]

        dt <- dst_lookup[dt, on = "yr"]

        log_info(
          "DST boundaries calculated for years: ",
          paste0(years_present, collapse = ", ")
        )

        TRUE
      },
      error = function(e) {
        log_error("DST calculation failed: %s", e$message)
        FALSE
      }
    )

    if (!dt_success) {
      stop("add_session_tags_1h: DST boundary calculation failed")
    } else {
      log_info("DST boundaries calculated and merged successfully")
    }
  }

  # ============================================================================
  # 13. ASSIGN SESSION LABELS + KILL ZONES (vectorized)
  # ============================================================================
  if (time_frame == "h1") {
    tryCatch(
      {
        log_info("Assigning sessions via vectorized operations")

        # dt <- copy(hourly_data)

        dt[,
          `:=`(
            # Single session label (more efficient than multiple booleans)
            tokyo = fcase(
              # Asian Session (no DST)
              trade_time >= asian_start & trade_time < asian_end,
              "Tokyo",
              default = NA_character_
            ),

            london = fcase(
              # London Session (handle Standard and DST)
              (trade_date < lds | trade_date >= lde) &
                trade_time >= lon_std_start &
                trade_time < lon_std_end,
              "London",
              trade_date >= lds &
                trade_date < lde &
                trade_time >= lon_dst_start &
                trade_time < lon_dst_end,
              "London",
              default = NA_character_
            ),

            new_york = fcase(
              # NY Session (handle Standard and DST)
              (trade_date < nys | trade_date >= nye) &
                trade_time >= ny_std_start &
                trade_time < ny_std_end,
              "NY",
              trade_date >= nys &
                trade_date < nye &
                trade_time >= ny_dst_start &
                trade_time < ny_dst_end,
              "NY",
              default = NA_character_
            )

            # # Fallbak for any time not in a session
            # default = "Other"
          ),
          by = .(asset_class, ticker)
        ]

        dt[,
          # Kill zone flag (TRUE if in London or NY opening window)
          kill_zone := fifelse(
            ((london == "London" &
              trade_time >= lon_kz_start &
              trade_time <= lon_kz_end) |
              (new_york == "NY" &
                trade_time >= ny_kz_start &
                trade_time <= ny_kz_end)),
            TRUE,
            FALSE
          ),
          by = .(asset_class, ticker)
        ]

        dt[,
          pip_conv := fcase(
            grepl(config$transformation$pip_factor$regex_pattern, ticker),
            config$transformation$pip_factor$value_for_match,
            default = config$transformation$pip_factor$default_value
          ),
          by = .(asset_class, ticker)
        ]

        dt <- dt[,
          c(
            "yr",
            "lds",
            "lde",
            "nys",
            "nye",
            "trade_date",
            "trade_time"
          ) := NULL
        ]

        dt

        log_info(sprintf(
          "Session tagging complete - Kill zone coverage: %.1f%%",
          mean(dt$kill_zone) * 100
        ))
      },
      error = function(e) {
        log_error("Session assignment failed: %s", e$message)
        stop(e)
      }
    )
  }

  # ========================================================================
  # 14. COLUMN SELECTION (Required Output) ---
  # ========================================================================
  if (time_frame == "h1") {
    tryCatch(
      {
        log_info(
          paste0(
            "Renaming columns according to ",
            time_frame,
            " timeframe ",
            "and finalising column selection"
          )
        )

        old_names <- grep(
          "^(?!.*((?<!bias)_id)).*(bias|level|prev|swing|state|type)",
          names(dt),
          value = TRUE,
          perl = TRUE
        )

        new_names <- paste0(
          time_frame,
          "_",
          grep(
            "^(?!.*((?<!bias)_id)).*(bias|level|prev|swing|state|type)",
            names(dt),
            value = TRUE,
            perl = TRUE
          )
        )

        # 2. Rename in one call
        setnames(dt, old_names, new_names)

        required_cols <- c(
          "date_time",
          "open",
          "high",
          "low",
          "close",
          "tokyo",
          "london",
          "new_york",
          "kill_zone",
          "pip_conv",
          grep("^(?!.*(id)).*(swing)", names(dt), value = TRUE, perl = TRUE),
          grep("prev", names(dt), value = TRUE, perl = TRUE),
          grep("level|type", names(dt), value = TRUE),
          grep("bias_id", names(dt), value = TRUE),
          grep(
            "^(?!.*(id)).*(bias|state)",
            names(dt),
            value = TRUE,
            perl = TRUE
          ),
          "timeframe",
          "ticker",
          "asset_class"
        )

        log_info(
          paste0(
            "Column renaming and column selection completed successfully ",
            " and completed ",
            time_frame,
            "_data institutional mapping successfully ",
            "resulting in a data.table with ",
            length(names(dt[, ..required_cols])),
            " columns and ",
            nrow(dt),
            " rows."
          )
        )

        dt <- dt[, ..required_cols]
      },
      error = function(e) {
        log_error(paste0(
          "Column renaming and column selection failed: ",
          e$message
        ))
        stop(e)
      }
    )
  } else {
    tryCatch(
      {
        log_info(
          paste0(
            "Renaming columns according to ",
            time_frame,
            " timeframe ",
            "and finalising column selection"
          )
        )

        old_names <- grep(
          "^(?!.*((?<!bias)_id)).*(bias|level|prev|swing|state|type)",
          names(dt),
          value = TRUE,
          perl = TRUE
        )

        new_names <- paste0(
          time_frame,
          "_",
          grep(
            "^(?!.*((?<!bias)_id)).*(bias|level|prev|swing|state|type)",
            names(dt),
            value = TRUE,
            perl = TRUE
          )
        )

        # 2. Rename in one call
        setnames(dt, old_names, new_names)

        required_cols <- c(
          "date_time",
          "open",
          "high",
          "low",
          "close",
          grep("^(?!.*(id)).*(swing)", names(dt), value = TRUE, perl = TRUE),
          grep("prev", names(dt), value = TRUE, perl = TRUE),
          grep("level|type", names(dt), value = TRUE),
          grep("bias_id", names(dt), value = TRUE),
          grep(
            "^(?!.*(id)).*(bias|state)",
            names(dt),
            value = TRUE,
            perl = TRUE
          ),
          "timeframe",
          "ticker",
          "asset_class"
        )

        log_info(
          paste0(
            "Column renaming and column selection completed successfully ",
            " and completed ",
            time_frame,
            "_data institutional mapping successfully ",
            "resulting in a data.table with ",
            length(names(dt[, ..required_cols])),
            " columns and ",
            nrow(dt),
            " rows."
          )
        )

        dt <- dt[, ..required_cols]
      },
      error = function(e) {
        log_error(paste0(
          "Column renaming and column selection failed: ",
          e$message
        ))
        stop(e)
      }
    )
  }

  data_name <- paste0(time_frame, "_data")

  results <- setNames(
    list(
      dt,
      get(registry),
      get(dol_table),
      swings
    ),
    c(data_name, registry, dol_table, "swings")
  )

  return(results)
}


# Reversal_watch ----------------------------------------------------------

#' Compute Stage 1 Reversal Watch
#'
#' Detects weekly swing points inside active weekly or daily FVG/OB zones,
#' and records H1 CHoCH levels inside the same zone. Optionally flags H4
#' FVG/OB zones that also sit inside the larger zone. This is an early
#' warning detector, not a trade signal.
#'
#' @param weekly_mutated List from `institutional_mapping()` for weekly data.
#' @param daily_mutated List from `institutional_mapping()` for daily data.
#' @param h4_mutated List from `institutional_mapping()` for H4 data.
#' @param h1_mutated List from `institutional_mapping()` for H1 data.
#'
#' @return A data.table with swing point, zone, H1 CHoCH, and confluence string.
#'
#' @import data.table
#' @importFrom logger log_info log_error
#' @export

compute_reversal_watch <- function(
  weekly_mutated,
  daily_mutated,
  h4_mutated,
  h1_mutated
) {
  log_info("Starting Stage 1 reversal watch computation")

  # ---------------------------------------------------------------------------
  # 1. EXTRACT DATA
  # ---------------------------------------------------------------------------
  h1_data <- tryCatch(
    {
      h1 <- h1_mutated$h1_data
      log_info("Extracted h1_data successfully")
      h1
    },
    error = function(e) {
      log_error("Failed to extract h1_data: {e$message}")
      stop(e)
    }
  )

  h1_registry <- tryCatch(
    {
      h1_registry <- h1_mutated$h1_registry
      log_info("Extracted h1_registry successfully")
      h1_registry
    },
    error = function(e) {
      log_error("Failed to extract h1_registry: {e$message}")
      stop(e)
    }
  )

  daily_data <- tryCatch(
    {
      dd <- daily_mutated$daily_data[
        h1_data[, date_time[1], by = ticker],
        on = .(ticker),
        {
          .SD[date_time >= i.V1]
        },
        by = .EACHI
      ]
      log_info("Extracted daily_data successfully")
      dd
    },
    error = function(e) {
      log_error("Failed to extract daily_data: {e$message}")
      stop(e)
    }
  )

  daily_registry <- tryCatch(
    {
      dr <- daily_mutated$daily_registry
      log_info("Extracted daily_registry successfully")
      dr
    },
    error = function(e) {
      log_error("Failed to extract daily_registry: {e$message}")
      stop(e)
    }
  )

  weekly_data <- tryCatch(
    {
      wd <- weekly_mutated$weekly_data[
        h1_data[, date_time[1], by = ticker],
        on = .(ticker),
        {
          .SD[date_time >= i.V1]
        },
        by = .EACHI
      ]
      log_info("Extracted weekly_data successfully")
      wd
    },
    error = function(e) {
      log_error("Failed to extract weekly_data: {e$message}")
      stop(e)
    }
  )

  weekly_registry <- tryCatch(
    {
      wr <- weekly_mutated$weekly_registry
      log_info("Extracted weekly_registry successfully")
      wr
    },
    error = function(e) {
      log_error("Failed to extract weekly_registry: {e$message}")
      stop(e)
    }
  )

  # ---------------------------------------------------------------------------
  # 2. VALIDATE REQUIRED COLUMNS
  # ---------------------------------------------------------------------------
  tryCatch(
    {
      required_weekly_data <- c(
        "date_time",
        "ticker",
        "asset_class",
        "weekly_swing_high",
        "weekly_swing_low"
      )
      required_daily_data <- c(
        "date_time",
        "ticker",
        "asset_class",
        "daily_swing_high",
        "daily_swing_low"
      )
      required_registry <- c(
        "ticker",
        "asset_class",
        "type",
        "type_zone_bot_value",
        "type_zone_top_value",
        "formed_at",
        "invalidated_at"
      )
      required_h1 <- c(
        "date_time",
        "ticker",
        "asset_class",
        "close",
        "h1_choch_level",
        "h1_choch_type"
      )

      stopifnot(
        all(required_weekly_data %in% names(weekly_data)),
        all(required_daily_data %in% names(daily_data)),
        all(required_registry %in% names(weekly_registry)),
        all(required_registry %in% names(daily_registry)),
        all(required_h1 %in% names(h1_data))
      )
      log_info("Required columns validation passed")
    },
    error = function(e) {
      log_error("Column validation failed: {e$message}")
      stop(e)
    }
  )

  # ---------------------------------------------------------------------------
  # 3. PREPARE WEEKLY AND DAILY SWING POINTS
  # ---------------------------------------------------------------------------
  weekly_swing_points <- tryCatch(
    {
      sh <- weekly_data[
        !is.na(weekly_swing_high),
        .(
          ticker,
          asset_class,
          swing_time = as.POSIXct(date_time, tz = "UTC"),
          swing_type = "high",
          swing_price = weekly_swing_high,
          swing_time_end = as.POSIXct(date_time, tz = "UTC") +
            days(4) +
            hours(23) +
            minutes(59)
        )
      ]

      sh <- sh[,
        c(
          "swing_val",
          "swing_date",
          "swing_h1_high",
          "swing_h1_low"
        ) := h1_data[
          .SD,
          on = .(
            ticker,
            asset_class,
            date_time >= swing_time,
            date_time <= swing_time_end
          ),
          {
            if (.N > 0) {
              idx <- which.max(x.high)
              .(
                swing_val = x.high[idx],
                swing_date = x.date_time[idx],
                swing_h1_high = x.high[idx],
                swing_h1_low = x.low[idx]
              )
            } else {
              .(
                swing_val = NA_real_,
                swing_date = as.POSIXct(NA, tz = "UTC"),
                swing_h1_high = NA_real_,
                swing_h1_low = NA_real_
              )
            }
          },
          by = .EACHI
        ][, .(swing_val, swing_date, swing_h1_high, swing_h1_low)]
      ][!is.na(swing_val)]

      sh[, swing_time := swing_date]
      sh[, c("swing_date", "swing_time_end") := NULL]

      sl <- weekly_data[
        !is.na(weekly_swing_low),
        .(
          ticker,
          asset_class,
          swing_time = as.POSIXct(date_time, tz = "UTC"),
          swing_type = "low",
          swing_price = weekly_swing_low,
          swing_time_end = as.POSIXct(date_time, tz = "UTC") +
            days(4) +
            hours(23) +
            minutes(59)
        )
      ]

      sl <- sl[,
        c(
          "swing_val",
          "swing_date",
          "swing_h1_high",
          "swing_h1_low"
        ) := h1_data[
          .SD,
          on = .(
            ticker,
            asset_class,
            date_time >= swing_time,
            date_time <= swing_time_end
          ),
          {
            if (.N > 0) {
              idx <- which.min(x.low)
              .(
                swing_val = x.low[idx],
                swing_date = x.date_time[idx],
                swing_h1_high = x.high[idx],
                swing_h1_low = x.low[idx]
              )
            } else {
              .(
                swing_val = NA_real_,
                swing_date = as.POSIXct(NA, tz = "UTC"),
                swing_h1_high = NA_real_,
                swing_h1_low = NA_real_
              )
            }
          },
          by = .EACHI
        ][, .(swing_val, swing_date, swing_h1_high, swing_h1_low)]
      ][!is.na(swing_val)]

      sl[, swing_time := swing_date]
      sl[, c("swing_date", "swing_time_end") := NULL]

      sp <- rbindlist(list(sh, sl), use.names = TRUE)
      setorder(sp, ticker, swing_time)
      log_info(sprintf("Prepared %d weekly swing points", nrow(sp)))
      sp
    },
    error = function(e) {
      log_error("Failed to prepare weekly swing points: {e$message}")
      stop(e)
    }
  )

  daily_swing_points <- tryCatch(
    {
      sh <- daily_data[
        !is.na(daily_swing_high),
        .(
          ticker,
          asset_class,
          swing_time = as.POSIXct(date_time, tz = "UTC"),
          swing_type = "high",
          swing_price = daily_swing_high,
          swing_time_end = as.POSIXct(date_time, tz = "UTC") +
            hours(23) +
            minutes(59)
        )
      ]

      sh <- sh[,
        c(
          "swing_val",
          "swing_date",
          "swing_h1_high",
          "swing_h1_low"
        ) := h1_data[
          .SD,
          on = .(
            ticker,
            asset_class,
            date_time >= swing_time,
            date_time <= swing_time_end
          ),
          {
            if (.N > 0) {
              idx <- which.max(x.high)
              .(
                swing_val = x.high[idx],
                swing_date = x.date_time[idx],
                swing_h1_high = x.high[idx],
                swing_h1_low = x.low[idx]
              )
            } else {
              .(
                swing_val = NA_real_,
                swing_date = as.POSIXct(NA, tz = "UTC"),
                swing_h1_high = NA_real_,
                swing_h1_low = NA_real_
              )
            }
          },
          by = .EACHI
        ][, .(swing_val, swing_date, swing_h1_high, swing_h1_low)]
      ][!is.na(swing_val)]

      sh[, swing_time := swing_date]
      sh[, c("swing_date", "swing_time_end") := NULL]

      sl <- daily_data[
        !is.na(daily_swing_low),
        .(
          ticker,
          asset_class,
          swing_time = as.POSIXct(date_time, tz = "UTC"),
          swing_type = "low",
          swing_price = daily_swing_low,
          swing_time_end = as.POSIXct(date_time, tz = "UTC") +
            hours(23) +
            minutes(59)
        )
      ]

      sl <- sl[,
        c(
          "swing_val",
          "swing_date",
          "swing_h1_high",
          "swing_h1_low"
        ) := h1_data[
          .SD,
          on = .(
            ticker,
            asset_class,
            date_time >= swing_time,
            date_time <= swing_time_end
          ),
          {
            if (.N > 0) {
              idx <- which.min(x.low)
              .(
                swing_val = x.low[idx],
                swing_date = x.date_time[idx],
                swing_h1_high = x.high[idx],
                swing_h1_low = x.low[idx]
              )
            } else {
              .(
                swing_val = NA_real_,
                swing_date = as.POSIXct(NA, tz = "UTC"),
                swing_h1_high = NA_real_,
                swing_h1_low = NA_real_
              )
            }
          },
          by = .EACHI
        ][, .(swing_val, swing_date, swing_h1_high, swing_h1_low)]
      ][!is.na(swing_val)]

      sl[, swing_time := swing_date]
      sl[, c("swing_date", "swing_time_end") := NULL]

      sp <- rbindlist(list(sh, sl), use.names = TRUE)
      setorder(sp, ticker, swing_time)
      log_info(sprintf("Prepared %d daily swing points", nrow(sp)))
      sp
    },
    error = function(e) {
      log_error("Failed to prepare daily swing points: {e$message}")
      stop(e)
    }
  )

  # ---------------------------------------------------------------------------
  # 4. PREPARE WEEKLY + DAILY ZONES
  # ---------------------------------------------------------------------------
  zones <- tryCatch(
    {
      zone_types <- c("bull_fvg", "bear_fvg", "bull_ob", "bear_ob")

      wk_zones <- weekly_registry[
        type %in% zone_types,
        .(
          ticker,
          asset_class,
          zone_timeframe = "weekly",
          zone_type = type,
          zone_bot = type_zone_bot_value,
          zone_top = type_zone_top_value,
          formed_at,
          invalidated_at
        )
      ]

      day_zones <- daily_registry[
        type %in% zone_types,
        .(
          ticker,
          asset_class,
          zone_timeframe = "daily",
          zone_type = type,
          zone_bot = type_zone_bot_value,
          zone_top = type_zone_top_value,
          formed_at,
          invalidated_at
        )
      ]

      z <- rbindlist(list(wk_zones, day_zones), use.names = TRUE, fill = TRUE)
      z[
        is.na(invalidated_at),
        invalidated_at := as.POSIXct("2099-01-01", tz = "UTC")
      ]
      log_info(sprintf("Prepared %d total weekly/daily zones", nrow(z)))
      z
    },
    error = function(e) {
      log_error("Failed to prepare zones: {e$message}")
      stop(e)
    }
  )

  # ---------------------------------------------------------------------------
  # 5. PREPARE H1 CLOSE AND CHoCH EVENTS
  # ---------------------------------------------------------------------------
  h1_close <- tryCatch(
    {
      hc <- h1_data[, .(ticker, asset_class, date_time, close)]
      setorder(hc, ticker, asset_class, date_time)
      log_info("Prepared h1_close successfully")
      hc
    },
    error = function(e) {
      log_error("Failed to prepare h1_close: {e$message}")
      stop(e)
    }
  )

  h1_choch_events <- tryCatch(
    {
      ev <- h1_data[
        !is.na(h1_choch_level) &
          h1_choch_type %in%
            c(
              "bull_choch",
              "bear_choch",
              "bull_range_choch",
              "bear_range_choch"
            ),
        .(
          ticker,
          asset_class,
          event_time = date_time,
          choch_price = h1_choch_level,
          choch_dir = ifelse(grepl("bull", h1_choch_type), "bullish", "bearish")
        )
      ]
      setorder(ev, ticker, asset_class, event_time)
      log_info(sprintf("Prepared %d H1 CHoCH events", nrow(ev)))
      ev
    },
    error = function(e) {
      log_error("Failed to prepare H1 CHoCH events: {e$message}")
      stop(e)
    }
  )

  # ---------------------------------------------------------------------------
  # 5b. PREPARE H1 SWING EVENTS (NEW)
  # ---------------------------------------------------------------------------
  # Explanation: We need a sorted list of every H1 swing high and swing low
  # so we can find the FIRST opposite swing after each weekly/daily anchor.
  # Adjust "h1_swing_high" / "h1_swing_low" to match your actual column names.
  h1_swing_events <- tryCatch(
    {
      ev <- rbindlist(
        list(
          h1_data[
            !is.na(h1_swing_high) & h1_bias == "bearish",
            .(
              ticker,
              asset_class,
              event_time = date_time,
              h1_swing_type = "high",
              h1_swing_price = h1_swing_high,
              h1_bias
            )
          ],
          h1_data[
            !is.na(h1_swing_low) & h1_bias == "bullish",
            .(
              ticker,
              asset_class,
              event_time = date_time,
              h1_swing_type = "low",
              h1_swing_price = h1_swing_low,
              h1_bias
            )
          ]
        ),
        use.names = TRUE
      )
      setorder(ev, ticker, asset_class, event_time)
      log_info(sprintf("Prepared %d H1 swing events", nrow(ev)))
      ev
    },
    error = function(e) {
      log_error("Failed to prepare H1 swing events: {e$message}")
      stop(e)
    }
  )

  # ---------------------------------------------------------------------------
  # INTERNAL HELPER (UNCHANGED)
  # ---------------------------------------------------------------------------
  process_swing_source <- function(swing_points, source_label) {
    log_info(sprintf("Processing %s swing points", source_label))

    matches <- tryCatch(
      {
        overlaps <- zones[
          swing_points,
          on = .(
            ticker,
            asset_class,
            formed_at <= swing_time,
            invalidated_at > swing_time
          ),
          .(
            ticker,
            asset_class,
            swing_time = i.swing_time,
            swing_type = i.swing_type,
            swing_price = i.swing_price,
            zone_timeframe = x.zone_timeframe,
            zone_type = x.zone_type,
            zone_bot = x.zone_bot,
            zone_top = x.zone_top
          ),
          nomatch = 0
        ][swing_price >= zone_bot & swing_price <= zone_top]

        overlaps <- overlaps[
          (swing_type == "high" & zone_type %in% c("bear_fvg", "bear_ob")) |
            (swing_type == "low" & zone_type %in% c("bull_fvg", "bull_ob"))
        ]
        log_info(sprintf(
          "[%s] Matched zones: %d",
          source_label,
          nrow(overlaps)
        ))
        overlaps
      },
      error = function(e) {
        log_error("[%s] Swing/zone match failed: {e$message}", source_label)
        stop(e)
      }
    )

    zone_clusters <- tryCatch(
      {
        if (nrow(matches) == 0) {
          data.table()
        } else {
          matches[,
            {
              zones_unique <- unique(.SD[, .(zone_timeframe, zone_type)])
              zones_unique[,
                tf_rank := match(zone_timeframe, c("weekly", "daily"))
              ]
              zones_unique <- zones_unique[order(tf_rank, zone_type)]

              tf_str <- paste(
                unique(zones_unique$zone_timeframe),
                collapse = ", "
              )
              type_str <- paste(unique(zones_unique$zone_type), collapse = ", ")
              conf_str <- paste0(
                zones_unique$zone_timeframe,
                "(",
                zones_unique$zone_type,
                ")",
                collapse = ", "
              )

              .(
                asset_class = asset_class[1],
                swing_price = swing_price[1],
                setup_type = "zone_swing",
                zone_timeframes = tf_str,
                zone_types = type_str,
                zone_bot = min(zone_bot),
                zone_top = max(zone_top),
                confluence = conf_str
              )
            },
            by = .(ticker, swing_time, swing_type)
          ]
        }
      },
      error = function(e) {
        log_error("[%s] Zone aggregation failed: {e$message}", source_label)
        stop(e)
      }
    )

    naked_swings <- tryCatch(
      {
        if (nrow(zone_clusters) == 0) {
          naked <- copy(swing_points)
        } else {
          matched <- zone_clusters[, .(ticker, swing_time, swing_type)]
          naked <- copy(swing_points)
          naked[
            matched,
            on = .(ticker, swing_time, swing_type),
            matched_flag := TRUE
          ]
          naked <- naked[is.na(matched_flag)]
          naked[, matched_flag := NULL]
        }

        if (nrow(naked) > 0) {
          naked[, `:=`(
            setup_type = "naked_swing",
            zone_timeframes = NA_character_,
            zone_types = NA_character_,
            zone_bot = swing_h1_low,
            zone_top = swing_h1_high,
            confluence = "naked"
          )]
        }
        log_info(sprintf("[%s] Naked swings: %d", source_label, nrow(naked)))
        naked
      },
      error = function(e) {
        log_error(
          "[%s] Naked swing detection failed: {e$message}",
          source_label
        )
        stop(e)
      }
    )

    all_swings <- tryCatch(
      {
        dt <- rbindlist(
          list(zone_clusters, naked_swings),
          use.names = TRUE,
          fill = TRUE
        )

        if (nrow(dt) == 0) {
          return(data.table())
        }

        dt[,
          reference_edge := fcase(
            swing_type == "high",
            zone_bot,
            swing_type == "low",
            zone_top,
            default = NA_real_
          )
        ]

        log_info(sprintf(
          "[%s] Total swings to process: %d",
          source_label,
          nrow(dt)
        ))
        dt
      },
      error = function(e) {
        log_error("[%s] Combining swings failed: {e$message}", source_label)
        stop(e)
      }
    )

    if (nrow(all_swings) == 0) {
      return(data.table())
    }

    aggressive_moves <- tryCatch(
      {
        bear_aggressive <- h1_close[
          all_swings[swing_type == "high"],
          on = .(
            ticker,
            asset_class,
            date_time > swing_time,
            close < reference_edge
          ),
          mult = "first",
          .(
            ticker,
            asset_class,
            swing_time = i.swing_time,
            swing_type = i.swing_type,
            swing_price = i.swing_price,
            setup_type = i.setup_type,
            zone_timeframes = i.zone_timeframes,
            zone_types = i.zone_types,
            zone_bot = i.zone_bot,
            zone_top = i.zone_top,
            aggressive_close_time = x.date_time,
            aggressive_close_price = x.close,
            confluence = i.confluence
          ),
          nomatch = 0
        ]

        bull_aggressive <- h1_close[
          all_swings[swing_type == "low"],
          on = .(
            ticker,
            asset_class,
            date_time > swing_time,
            close > reference_edge
          ),
          mult = "first",
          .(
            ticker,
            asset_class,
            swing_time = i.swing_time,
            swing_type = i.swing_type,
            swing_price = i.swing_price,
            setup_type = i.setup_type,
            zone_timeframes = i.zone_timeframes,
            zone_types = i.zone_types,
            zone_bot = i.zone_bot,
            zone_top = i.zone_top,
            aggressive_close_time = x.date_time,
            aggressive_close_price = x.close,
            confluence = i.confluence
          ),
          nomatch = 0
        ]

        ag <- rbindlist(
          list(bear_aggressive, bull_aggressive),
          use.names = TRUE,
          fill = TRUE
        )
        log_info(sprintf("[%s] Aggressive closes: %d", source_label, nrow(ag)))
        ag
      },
      error = function(e) {
        log_error(
          "[%s] Aggressive close detection failed: {e$message}",
          source_label
        )
        stop(e)
      }
    )

    hits_with_choch <- tryCatch(
      {
        bear_choch_after <- h1_choch_events[choch_dir == "bearish"][
          aggressive_moves[swing_type == "high"],
          on = .(ticker, asset_class, event_time >= aggressive_close_time),
          mult = "first",
          .(
            ticker,
            asset_class,
            swing_time = i.swing_time,
            swing_type = i.swing_type,
            swing_price = i.swing_price,
            setup_type = i.setup_type,
            zone_timeframes = i.zone_timeframes,
            zone_types = i.zone_types,
            zone_bot = i.zone_bot,
            zone_top = i.zone_top,
            aggressive_close_time = i.aggressive_close_time,
            aggressive_close_price = i.aggressive_close_price,
            h1_choch_time = x.event_time,
            h1_choch_price = x.choch_price,
            h1_choch_dir = x.choch_dir,
            confluence = i.confluence
          ),
          nomatch = 0
        ]

        bull_choch_after <- h1_choch_events[choch_dir == "bullish"][
          aggressive_moves[swing_type == "low"],
          on = .(ticker, asset_class, event_time >= aggressive_close_time),
          mult = "first",
          .(
            ticker,
            asset_class,
            swing_time = i.swing_time,
            swing_type = i.swing_type,
            swing_price = i.swing_price,
            setup_type = i.setup_type,
            zone_timeframes = i.zone_timeframes,
            zone_types = i.zone_types,
            zone_bot = i.zone_bot,
            zone_top = i.zone_top,
            aggressive_close_time = i.aggressive_close_time,
            aggressive_close_price = i.aggressive_close_price,
            h1_choch_time = x.event_time,
            h1_choch_price = x.choch_price,
            h1_choch_dir = x.choch_dir,
            confluence = i.confluence
          ),
          nomatch = 0
        ]

        res <- rbindlist(
          list(bear_choch_after, bull_choch_after),
          use.names = TRUE,
          fill = TRUE
        )
        log_info(sprintf("[%s] Valid sequences: %d", source_label, nrow(res)))
        res
      },
      error = function(e) {
        log_error("[%s] CHoCH filtering failed: {e$message}", source_label)
        stop(e)
      }
    )

    final_dt <- tryCatch(
      {
        dt <- copy(hits_with_choch)
        dt[, timeframe := source_label]

        setcolorder(
          dt,
          c(
            "ticker",
            "asset_class",
            "timeframe",
            "swing_time",
            "swing_type",
            "swing_price",
            "setup_type",
            "zone_timeframes",
            "zone_types",
            "zone_bot",
            "zone_top",
            "aggressive_close_time",
            "aggressive_close_price",
            "h1_choch_time",
            "h1_choch_price",
            "h1_choch_dir",
            "confluence"
          )
        )

        setorder(dt, ticker, swing_time, swing_type)
        log_info(sprintf("[%s] Final rows: %d", source_label, nrow(dt)))
        dt
      },
      error = function(e) {
        log_error("[%s] Final assembly failed: {e$message}", source_label)
        stop(e)
      }
    )

    final_dt
  }

  # ---------------------------------------------------------------------------
  # 7. AUGMENT FINAL OUTPUT WITH FIRST OPPOSITE H1 SWING (NEW)
  # ---------------------------------------------------------------------------
  # Explanation: For each final row, find the first H1 swing in the OPPOSITE
  # direction after the anchor swing_time. This is the pivot the CHoCH will
  # eventually reference, and it defines the structural edge of the twilight.
  augment_first_opposite_h1_swing <- function(final_dt, h1_swing_events) {
    if (nrow(final_dt) == 0) {
      return(final_dt)
    }

    h1_lows <- h1_swing_events[h1_swing_type == "low"]
    h1_highs <- h1_swing_events[h1_swing_type == "high"]

    # First H1 swing low AFTER each swing high
    first_low <- h1_lows[
      final_dt[swing_type == "high"],
      on = .(ticker, asset_class, event_time > swing_time),
      mult = "first",
      .(
        ticker,
        swing_time = i.swing_time,
        swing_type = i.swing_type,
        first_h1_opposite_time = x.event_time,
        first_h1_opposite_price = x.h1_swing_price
      ),
      nomatch = 0
    ]

    # First H1 swing high AFTER each swing low
    first_high <- h1_highs[
      final_dt[swing_type == "low"],
      on = .(ticker, asset_class, event_time > swing_time),
      mult = "first",
      .(
        ticker,
        swing_time = i.swing_time,
        swing_type = i.swing_type,
        first_h1_opposite_time = x.event_time,
        first_h1_opposite_price = x.h1_swing_price
      ),
      nomatch = 0
    ]

    ref <- rbindlist(list(first_low, first_high), use.names = TRUE)

    final_dt[
      ref,
      on = .(ticker, swing_time, swing_type),
      `:=`(
        first_h1_opposite_time = i.first_h1_opposite_time,
        first_h1_opposite_price = i.first_h1_opposite_price
      )
    ]

    final_dt
  }

  # ---------------------------------------------------------------------------
  # 8. RUN HELPER FOR WEEKLY AND DAILY
  # ---------------------------------------------------------------------------
  weekly_final_dt <- tryCatch(
    process_swing_source(weekly_swing_points, "weekly"),
    error = function(e) {
      log_error("Weekly reversal watch failed: {e$message}")
      stop(e)
    }
  )

  daily_final_dt <- tryCatch(
    process_swing_source(daily_swing_points, "daily"),
    error = function(e) {
      log_error("Daily reversal watch failed: {e$message}")
      stop(e)
    }
  )

  # ---------------------------------------------------------------------------
  # 9. ATTACH FIRST OPPOSITE H1 SWING TO EACH OUTPUT
  # ---------------------------------------------------------------------------
  weekly_final_dt <- tryCatch(
    augment_first_opposite_h1_swing(weekly_final_dt, h1_swing_events),
    error = function(e) {
      log_error("Weekly augmentation failed: {e$message}")
      stop(e)
    }
  )

  daily_final_dt <- tryCatch(
    augment_first_opposite_h1_swing(daily_final_dt, h1_swing_events),
    error = function(e) {
      log_error("Daily augmentation failed: {e$message}")
      stop(e)
    }
  )

  log_info(sprintf(
    "Stage 1 reversal watch complete: weekly=%d, daily=%d",
    nrow(weekly_final_dt),
    nrow(daily_final_dt)
  ))

  list(
    weekly_final_dt = weekly_final_dt,
    daily_final_dt = daily_final_dt
  )
}

# Market Context ----------------------------------------------------------

#' Combine multi-timeframe institutional analysis and compute market context
#'
#' Takes the outputs from `institutional_mapping()` for weekly, daily, H4, and
#' H1 timeframes. It merges higher timeframe bias and levels onto H1 data,
#' classifies trend, pullback, and trade context, identifies higher timeframe
#' confluence, computes Draw on Liquidity (DOL) and Draw on Trigger (DOT)
#' levels, and produces an execution table with candidate trades.
#'
#' @param weekly_mutate List returned by `institutional_mapping()` for weekly
#'   data. Must contain `weekly_data`, `weekly_registry`, and
#'   `weekly_dol_table`.
#' @param daily_mutate List for daily data with `daily_data`,
#'   `daily_registry`, `daily_dol_table`.
#' @param h4_mutate List for H4 data with `h4_data`, `h4_registry`,
#'   `h4_dol_table`.
#' @param h1_mutate List for H1 data with `h1_data`, `h1_registry`,
#'   `h1_dol_table`.
#' @param reversal_watch A list containing weekly and daily reversal watch tables
#'   produced by `compute_reversal_watch()`.
#' @param config A validated configuration list from `load_config()`.
#'
#' @return A named list with two elements:
#'   \itemize{
#'     \item `h1_data` - the H1 data enriched with multi-timeframe columns.
#'     \item `exec_data` - execution table with candidate trade setups.
#'   }
#'
#' @import data.table
#' @importFrom logger log_info log_error log_warn
#' @importFrom Rcpp sourceCpp
#' @importFrom lubridate floor_date ceiling_date minutes weeks day days
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' context <- market_context(
#'   weekly_mutate = weekly_mutate,
#'   daily_mutate = daily_mutate,
#'   h4_mutate = h4_mutate,
#'   h1_mutate = h1_mutate,
#'   reversal_watch = reversal_watch,
#'   config = config
#' )
#' head(context$exec_data)
#' }

market_context <- function(
  weekly_mutate,
  daily_mutate,
  h4_mutate,
  h1_mutate,
  reversal_watch,
  config
) {
  log_info(
    paste0(
      "Starting (weekly, daily, h4, h1) market_context engine."
    )
  )

  # ============================================================================
  # 1. UNLISTING LIST DATA
  # ============================================================================
  log_info("Starting the unlisting the list data items")

  h1_data <- h1_mutate$h1_data
  weekly_data <- weekly_mutate$weekly_data
  daily_data <- daily_mutate$daily_data
  h4_data <- h4_mutate$h4_data
  rev_w <- reversal_watch$weekly_final_dt
  rev_d <- reversal_watch$daily_final_dt

  weekly_reg <- weekly_mutate$weekly_registry
  daily_reg <- daily_mutate$daily_registry
  h4_reg <- h4_mutate$h4_registry
  h1_reg <- h1_mutate$h1_registry

  weekly_dol <- weekly_mutate$weekly_dol_table
  daily_dol <- daily_mutate$daily_dol_table
  h4_dol <- h4_mutate$h4_dol_table
  h1_dol <- h1_mutate$h1_dol_table

  data.table::setkeyv(h1_data, c("asset_class", "ticker", "date_time"))

  log_info("List data items unlisted successfully")

  # ============================================================================
  # 2. INPUT VALIDATION & SETUP
  # ============================================================================

  if (
    !(inherits(weekly_data, "data.table") &
      inherits(daily_data, "data.table") &
      inherits(h4_data, "data.table") &
      inherits(h1_data, "data.table"))
  ) {
    log_error(
      paste0(
        "All inputs must be data.tables."
      )
    )
    stop(
      paste0(
        "All inputs must be data.tables."
      )
    )
  }

  required_cols <- c(
    "prev_sh",
    "prev_sl",
    "choch_level",
    "choch_type",
    "bos_type",
    "sweep_type",
    "bias_id",
    "bias"
  )

  validate_cols <- function(dt, required_cols) {
    log_info(
      paste0(
        "Validating required columns for ",
        unique(dt$timeframe),
        "_data"
      )
    )
    expected_cols <- paste0(unique(dt$timeframe), "_", required_cols)
    missing <- setdiff(expected_cols, names(dt))

    if (length(missing) > 0) {
      stop(sprintf(
        "The %s_data has the following Missing column(s): %s",
        unique(dt$timeframe),
        paste(missing, collapse = ", ")
      ))
    } else {
      message(
        "The ",
        unique(dt$timeframe),
        "_data has all expected columns present."
      )
    }
  }

  # Validate each data input
  validate_cols(weekly_data, required_cols)
  validate_cols(daily_data, required_cols)
  validate_cols(h4_data, required_cols)
  validate_cols(h1_data, required_cols)

  # ============================================================================
  # PRE-DEFINE AGGREGATION FILTERS & SESSION FILTERS (from config)
  # ============================================================================
  log_info(
    paste0("Pre-defining multi_timeframe aggregation filters & session filters")
  )

  # Select the appropriate config section based on aggregation type
  h4 <- config$transformation$aggregation$h1_to_h4
  day <- config$transformation$aggregation$h1_to_daily
  week <- config$transformation$aggregation$daily_to_weekly
  buffer <- (config$analysis$signal_filter$buffer_pipette *
    config$analysis$trend_filter$h1$lookback_period)

  log_info("Aggregation filters & session filters loaded from config")

  # ============================================================================
  # 3. STACK UNIFIED REGISTRIES & DOL TABLES
  # ============================================================================
  zone_registry <- rbindlist(
    list(weekly_reg, daily_reg, h4_reg, h1_reg),
    use.names = TRUE,
    fill = TRUE
  )

  dol_registry <- rbindlist(
    list(weekly_dol, daily_dol, h4_dol, h1_dol),
    use.names = TRUE,
    fill = TRUE
  )

  #=============================================================================
  # 4. MERGING MULTI-TIMEFRAMES
  #=============================================================================
  tryCatch(
    {
      log_info(
        "Starting multi-timeframe merging of weekly, daily, H4, and reversal_watch onto H1"
      )

      # To determine market context, we need to compare the h1 bias with the
      # multi_time-frame bias. We first need to merge the multi_time-frame
      # information onto the h1 data.table.

      # ---- Weekly ------------------------------------------------------------
      # trade_date is start of week (Monday). Close is Friday 23:59 UTC.
      # Stamp each weekly row with its CLOSE time, not its open.
      # h1_data[
      #   weekly_data[, .(
      #     # date_time = Monday 00:00 -> +4 days -> Friday 00:00 -> +23:59
      #     # -> Fri 23:59. This is the exact moment the weekly candle completes.
      #     # Without this shift, every H1 bar during the week would inherit a
      #     # weekly bias that hadn't been confirmed yet (lookahead bias).
      #     date_time = as.POSIXct(date_time, tz = "UTC") +
      #       days(4) +
      #       hours(23) +
      #       minutes(59),
      #     weekly_prev_sh,
      #     weekly_prev_sl,
      #     weekly_choch_level,
      #     weekly_choch_type,
      #     weekly_bos_type,
      #     weekly_bias,
      #     weekly_trend_state,
      #     weekly_bias_id,
      #     ticker
      #   )],
      #   on = .(ticker, date_time),
      #   # roll = TRUE -> no exact match? carry forward the last row whose
      #   # date_time <= this bar's date_time. Because we stamped each weekly row
      #   # at Friday 23:59, "last completed weekly" is always the previous
      #   # Friday's close until this Friday 23:59 actually passes.
      #   roll = TRUE,
      #   `:=`(
      #     weekly_bias = i.weekly_bias,
      #     weekly_bias_id = i.weekly_bias_id,
      #     weekly_trend_state = i.weekly_trend_state,
      #     weekly_prev_sl = i.weekly_prev_sl,
      #     weekly_prev_sh = i.weekly_prev_sh,
      #     weekly_choch_level = i.weekly_choch_level,
      #     weekly_choch_type = i.weekly_choch_type,
      #     weekly_bos_type = i.weekly_bos_type
      #   )
      # ]

      # ---- Weekly ----
      wk_cols <- c(
        "weekly_prev_sh",
        "weekly_prev_sl",
        "weekly_choch_level",
        "weekly_choch_type",
        "weekly_bos_type",
        "weekly_bias",
        "weekly_trend_state",
        "weekly_bias_id"
      )

      wk <- weekly_data[, c(
        list(
          ticker = ticker,
          date_time = as.POSIXct(date_time, tz = "UTC") +
            days(4) +
            hours(23) +
            minutes(59)
        ),
        mget(wk_cols)
      )]

      h1_data[,
        (wk_cols) := wk[
          .SD,
          on = .(ticker, date_time),
          roll = TRUE,
          mget(wk_cols)
        ]
      ]

      # ---- Daily -------------------------------------------------------------
      # trade_date is start of day. Close is same day 23:59 UTC.
      # h1_data[
      #   daily_data[, .(
      #     # Stamp each daily row with its CLOSE time.
      #     # date_time = 00:00 today -> +23:59 -> today 23:59.
      #     # This is the moment the daily candle completes. Joining on raw
      #     # trade_date would let an H1 bar at 10:00 today see today's daily bias,
      #     # which is still being formed - classic lookahead.
      #     date_time = as.POSIXct(date_time, tz = "UTC") +
      #       hours(23) +
      #       minutes(59),
      #     daily_prev_sh,
      #     daily_prev_sl,
      #     daily_choch_level,
      #     daily_choch_type,
      #     daily_bos_type,
      #     daily_bias,
      #     daily_trend_state,
      #     daily_bias_id,
      #     ticker
      #   )],
      #   on = .(ticker, date_time),
      #   # roll = TRUE -> each H1 bar inherits the most recently CLOSED daily
      #   # candle. An H1 bar at 10:00 today still sees yesterday's 23:59 close,
      #   # not today's (still-open) candle.
      #   roll = TRUE,
      #   `:=`(
      #     daily_bias = i.daily_bias,
      #     daily_bias_id = i.daily_bias_id,
      #     daily_trend_state = i.daily_trend_state,
      #     daily_prev_sl = i.daily_prev_sl,
      #     daily_prev_sh = i.daily_prev_sh,
      #     daily_choch_level = i.daily_choch_level,
      #     daily_choch_type = i.daily_choch_type,
      #     daily_bos_type = i.daily_bos_type
      #   )
      # ]
      #
      # ---- Daily ----
      d_cols <- c(
        "daily_prev_sh",
        "daily_prev_sl",
        "daily_choch_level",
        "daily_choch_type",
        "daily_bos_type",
        "daily_bias",
        "daily_trend_state",
        "daily_bias_id"
      )

      dy <- daily_data[, c(
        list(
          ticker = ticker,
          date_time = as.POSIXct(date_time, tz = "UTC") +
            hours(23) +
            minutes(59)
        ),
        mget(d_cols)
      )]

      h1_data[,
        (d_cols) := dy[
          .SD,
          on = .(ticker, date_time),
          roll = TRUE,
          mget(d_cols)
        ]
      ]

      # ---- H4 ----------------------------------------------------------------
      # h4_data$date_time already carries the H4 close timestamp - no anchor
      # construction needed.
      # h1_data[
      #   h4_data[, .(
      #     # No timestamp arithmetic here. Unlike weekly/daily whose date_time@
      #     # is the OPEN of the period, the H4 aggregation upstream already
      #     # stamped each H4 row with its CLOSE. So date_time is already the
      #     # moment the H4 candle completed - the join key is correct as-is.
      #     date_time,
      #     h4_prev_sl,
      #     h4_prev_sh,
      #     h4_choch_level,
      #     h4_choch_type,
      #     h4_bos_type,
      #     h4_bias,
      #     h4_trend_state,
      #     h4_bias_id,
      #     ticker
      #   )],
      #   on = .(ticker, date_time),
      #   # roll = TRUE -> each H1 bar inherits the most recently CLOSED H4 candle.
      #   # An H1 bar at 11:00 inherits the 08:00 H4 candle, not the (still-open)
      #   # 12:00 one.
      #   roll = TRUE,
      #   `:=`(
      #     h4_bias = i.h4_bias,
      #     h4_bias_id = i.h4_bias_id,
      #     h4_trend_state = i.h4_trend_state,
      #     h4_prev_sl = i.h4_prev_sl,
      #     h4_prev_sh = i.h4_prev_sh,
      #     h4_choch_level = i.h4_choch_level,
      #     h4_choch_type = i.h4_choch_type,
      #     h4_bos_type = i.h4_bos_type
      #   )
      # ]
      #
      # ---- H4 ----
      h4_cols <- c(
        "h4_prev_sl",
        "h4_prev_sh",
        "h4_choch_level",
        "h4_choch_type",
        "h4_bos_type",
        "h4_bias",
        "h4_trend_state",
        "h4_bias_id"
      )

      h4 <- h4_data[, c(
        list(ticker = ticker, date_time = date_time),
        mget(h4_cols)
      )]

      h1_data[,
        (h4_cols) := h4[
          .SD,
          on = .(ticker, date_time),
          roll = TRUE,
          mget(h4_cols)
        ]
      ]

      # # Extract the last exact Friday timestamp for each ticker and week
      # weekly_end_timestamps <- h1_data[
      #   wday(date_time) == 6, # Filter for Fridays only (6 = Friday)
      #   .(last_friday_time = date_time[.N]), # Grab the exact last timestamp as-is
      #   by = .(ticker, as.Date(date_time)) # Group by ticker and individual day
      # ][,
      #   `:=`(
      #     trade_year = year(last_friday_time),
      #     trade_week = week(last_friday_time)
      #   ),
      #   by = ticker
      # ]
      #
      # # 1. Ensure the data is ordered by time
      # setorder(h1_data, ticker, date_time)
      #
      # # 2. Get the exact last timestamp for every calendar day
      # daily_end_timestamps <- h1_data[,
      #   .(last_day_time = date_time[.N]),
      #   by = .(ticker, trade_date = as.Date(date_time))
      # ][,
      #   `:=`(
      #     trade_year = year(last_day_time),
      #     trade_month = month(last_day_time),
      #     trade_day = day(last_day_time)
      #   ),
      #   by = ticker
      # ]

      h1_data

      log_info(
        paste0(
          "Multi-timeframe merging completed successfully for ",
          length(unique(h1_data$ticker)),
          " tickers resulting in a ",
          "data.table with ",
          length(names(h1_data)),
          " columns and ",
          nrow(h1_data),
          " rows."
        )
      )
    },
    error = function(e) {
      log_error(sprintf(
        "Error in merging multi-timeframe data: %s",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 5. TREND CONTEXT CLASSIFICATION ---
  # ============================================================================
  tryCatch(
    {
      log_info(
        "Starting trend context classification based on multi_time_frame bias"
      )

      # Now that we have the multi_time_frame bias and swing levels on the h1
      # data.table, we can determine trend context. We will create a new
      # column 'trend_context' that categorizes each candle into one of the
      # following:
      # - 'bull_strong_setup': multi_time_frame bias is bullish
      # - 'bear_strong_setup': multi_time_frame bias is bearish
      # - "pullback": multi_time_frame bias is bullish but h1 bias is bearish
      # (or vice versa)
      h1_data[,
        `:=`(
          # Trend context classification based on multi_time_frame bias and
          # swing levels
          trend_context = fcase(
            # ===========================================================
            # a. FULL ALIGNMENT - strongest signals
            # ===========================================================
            weekly_bias == "bullish" &
              daily_bias == "bullish",
            "bull_macro_trend",

            weekly_bias == "bearish" &
              daily_bias == "bearish",
            "bear_macro_trend",

            default = NA_character_
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data

      log_info(
        "Trend context classification completed successfully"
      )
    },
    error = function(e) {
      log_error(sprintf(
        "Error in computing trend context: %s",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 6. PULLBACK CONTEXT CLASSIFICATION ---
  # ============================================================================
  tryCatch(
    {
      log_info(
        paste0(
          "Starting pullback context classification based on trend ",
          "context and H1 bias"
        )
      )

      # Pullback context classification is based on the trend context and the
      # H1 bias. This classification helps identify whether the current pullback
      # is a bullish or bearish pullback within the context of the higher
      # timeframe trend.
      h1_data[,
        `:=`(
          pullback_context = fcase(
            trend_context == "bull_macro_trend" &
              (h4_bias == "bearish" & h1_bias == "bearish") |
              (h4_bias == "bullish" & h1_bias == "bearish"),
            "bull_trend_pullback",

            trend_context == "bear_macro_trend" &
              (h4_bias == "bullish" & h1_bias == "bullish") |
              (h4_bias == "bearish" & h1_bias == "bullish"),
            "bear_trend_pullback",

            default = NA_character_
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data

      log_info(
        "Pullback context classification completed successfully"
      )
    },
    error = function(e) {
      log_error(sprintf(
        "Error in computing pullback context: %s",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 7. TRADE CONTEXT CLASSIFICATION ---
  # ============================================================================
  tryCatch(
    {
      log_info(
        paste0(
          "Starting trade context classification based on trend context, ",
          "pullback context, and D.O.T. level"
        )
      )

      # Trade context classification is based on the trend context,
      # pullback context, and whether a valid dot level exists.
      # This classification helps identify potential trade setups based on
      # the alignment of higher timeframe trends and the current pullback.
      h1_data[,
        `:=`(
          trade_context = fcase(
            trend_context %in%
              c("bull_macro_trend") &
              pullback_context == "bull_trend_pullback" &
              kill_zone == TRUE,
            "bull_candidate",

            trend_context %in%
              c("bear_macro_trend") &
              pullback_context == "bear_trend_pullback" &
              kill_zone == TRUE,
            "bear_candidate",

            default = NA_character_
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data

      log_info(
        "Trade context classification completed successfully"
      )
    },
    error = function(e) {
      log_error(sprintf(
        "Error in computing trade context classification: %s",
        e$message
      ))
      stop(e)
    }
  )

  # ========================================================================
  # 8. WEEKLY TARGET D.O.L (Merge back onto main data.table) ---
  # ========================================================================
  tryCatch(
    {
      log_info(paste0(
        "Starting fast C++ weekly macro target draw on liquidity computation..."
      ))

      # NOTE: sourceCpp("src/dol_fast.cpp") HAS BEEN REMOVED.
      # find_dol_cpp is now natively available via devtools::document()

      # Compile the C++ function
      # sourceCpp("src/dol_fast.cpp")

      resistance_labels <- c(
        "swing_high",
        "bear_fvg_bot",
        "bear_fvg_top",
        "bear_ob_bot",
        "bear_ob_top"
      )
      support_labels <- c(
        "swing_low",
        "bull_fvg_top",
        "bull_fvg_bot",
        "bull_ob_top",
        "bull_ob_bot"
      )

      # Filter target tables
      resistance_table <- dol_registry[
        dol_label %in%
          resistance_labels &
          timeframe == "weekly" &
          formed_bias == "bearish"
      ]
      support_table <- dol_registry[
        dol_label %in%
          support_labels &
          timeframe == "weekly" &
          formed_bias == "bullish"
      ]

      # Process Bullish Rows (Looking for Resistance)
      h1_data[
        trend_context == "bull_macro_trend",
        c(
          "weekly_target_dol",
          "weekly_target_dol_name",
          "weekly_target_dol_id"
        ) := {
          # Subset targets matching the current group metadata to keep
          # C++ loop small
          sub_target <- resistance_table[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]

          if (nrow(sub_target) > 0 && .N > 0) {
            # Ensure target table values are strictly above close to
            # replicate your logic. Filtered here to speed up C++ scans
            find_dol_cpp(close, date_time, weekly_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      # Process Bearish Rows (Looking for Support)
      h1_data[
        trend_context == "bear_macro_trend",
        c(
          "weekly_target_dol",
          "weekly_target_dol_name",
          "weekly_target_dol_id"
        ) := {
          sub_target <- support_table[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]

          if (nrow(sub_target) > 0 && .N > 0) {
            find_dol_cpp(close, date_time, weekly_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      h1_data[, c("weekly_target_dol_id") := NULL]

      h1_data

      log_info(paste0(
        "Completed computing the weekly macro target",
        " draw on liquidity successfully."
      ))
    },
    error = function(e) {
      log_error(paste0(
        "Error in computing weekly macro target DOL: ",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # DAILY TARGET (D.O.L)
  # ============================================================================
  tryCatch(
    {
      log_info(
        "Starting fast C++ daily macro target draw on liquidity computation..."
      )

      # NOTE: sourceCpp("src/dol_fast.cpp") HAS BEEN REMOVED.
      # find_dol_cpp is now natively available via devtools::document()

      # Compile the C++ function
      # sourceCpp("src/dol_fast.cpp")

      resistance_labels <- c(
        "swing_high",
        "bear_fvg_bot",
        "bear_fvg_top",
        "bear_ob_bot",
        "bear_ob_top"
      )
      support_labels <- c(
        "swing_low",
        "bull_fvg_top",
        "bull_fvg_bot",
        "bull_ob_top",
        "bull_ob_bot"
      )

      # Filter target tables
      resistance_table_daily <- dol_registry[
        dol_label %in%
          resistance_labels &
          timeframe == "daily" &
          formed_bias == "bearish"
      ]
      support_table_daily <- dol_registry[
        dol_label %in%
          support_labels &
          timeframe == "daily" &
          formed_bias == "bullish"
      ]

      # Bullish counter-trend: look for resistance above
      h1_data[
        trend_context == "bull_macro_trend",
        c(
          "daily_target_dol",
          "daily_target_dol_name",
          "daily_target_dol_id"
        ) := {
          sub_target <- resistance_table_daily[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]
          if (nrow(sub_target) > 0 && .N > 0) {
            find_dol_cpp(close, date_time, daily_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      # Bearish counter-trend: look for support below
      h1_data[
        trend_context == "bear_macro_trend",
        c(
          "daily_target_dol",
          "daily_target_dol_name",
          "daily_target_dol_id"
        ) := {
          sub_target <- support_table_daily[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]
          if (nrow(sub_target) > 0 && .N > 0) {
            find_dol_cpp(close, date_time, daily_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      h1_data[, c("daily_target_dol_id") := NULL]

      h1_data

      log_info(paste0(
        "Completed computing the daily macro target",
        " draw on liquidity successfully."
      ))
    },
    error = function(e) {
      log_error("Error in computing daily macro target DOL:  {e$message}")
      stop(e)
    }
  )

  # ============================================================================
  # 9. DAILY DRAW ON THE TRIGGER (INTERACTION TARGET: D.O.T) ---
  # ============================================================================
  tryCatch(
    {
      log_info(paste0(
        "Starting fast C++ Daily draw on trigger (D.O.T) computation..."
      ))

      # NOTE: sourceCpp("src/dol_fast.cpp") HAS BEEN REMOVED.
      # find_dot_cpp is now natively available via devtools::document()

      # Compile the C++ function
      # sourceCpp("src/dot_fast.cpp")

      resistance_labels <- c(
        "swing_high",
        "bear_fvg_bot",
        "bear_fvg_top",
        "bear_ob_bot",
        "bear_ob_top"
      )
      support_labels <- c(
        "swing_low",
        "bull_fvg_top",
        "bull_fvg_bot",
        "bull_ob_top",
        "bull_ob_bot"
      )

      # Filter target tables
      resistance_table <- dol_registry[
        dol_label %in%
          resistance_labels &
          timeframe == "daily" &
          formed_bias == "bearish"
      ]

      support_table <- dol_registry[
        dol_label %in%
          support_labels &
          timeframe == "daily" &
          formed_bias == "bullish"
      ]

      daily_reg <- zone_registry[timeframe == "daily"]

      # Process Bullish Rows (Looking for Resistance)
      h1_data[
        trade_context == "bear_candidate",
        c("daily_dot", "daily_dot_name", "daily_dot_id") := {
          # Subset targets matching the current group metadata to keep C++
          # loop small
          sub_target <- resistance_table[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]

          if (nrow(sub_target) > 0 && .N > 0) {
            # Ensure target table values are strictly above close to
            # replicate your logic. Filtered here to speed up C++ scans
            find_dot_cpp(
              close,
              date_time,
              daily_bias_id,
              as.list(sub_target)
            )
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      # Process Bearish Rows (Looking for Support)
      h1_data[
        trade_context == "bull_candidate",
        c("daily_dot", "daily_dot_name", "daily_dot_id") := {
          sub_target <- support_table[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]

          if (nrow(sub_target) > 0 && .N > 0) {
            find_dot_cpp(
              close,
              date_time,
              daily_bias_id,
              as.list(sub_target)
            )
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      h1_data[,
        `:=`(
          daily_dot_type = fifelse(
            daily_dot_name %like% "bull|bear",
            gsub("_bot|_top", "", daily_dot_name),
            daily_dot_name
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data[
        daily_reg,
        on = .(
          asset_class,
          ticker,
          daily_dot_type == type,
          daily_dot_id == type_id
        ),
        `:=`(
          daily_dot_top = i.type_zone_top_value,
          daily_dot_bot = i.type_zone_bot_value
        )
      ]

      h1_data[,
        `:=`(
          sl_price_d = fcase(
            !is.na(daily_dot_type) & daily_dot_type %like% "bear",
            # dot_top + ((buffer * pip_conv) * 3),
            daily_dot_top,
            !is.na(daily_dot_type) & daily_dot_type %like% "bull",
            # dot_bot - ((buffer * pip_conv) * 3),
            daily_dot_bot,
            !is.na(daily_dot_type) & daily_dot_type %like% "high",
            # dot + ((buffer * pip_conv) * 3),
            daily_dot,
            !is.na(daily_dot_type) & daily_dot_type %like% "low",
            # dot - ((buffer * pip_conv) * 3),
            daily_dot,
            default = NA_real_
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data[, c("daily_dot_id", "daily_dot_type") := NULL]

      h1_data

      log_info(paste0(
        "Completed computing draw on trigger (D.O.T) completed successfully."
      ))
    },
    error = function(e) {
      log_error(paste0("Error in loop execution: ", e$message))
      stop(e)
    }
  )

  # ============================================================================
  # H4 D.O.T (INTERACTION TARGET: D.O.T)
  # ============================================================================
  # Uncomment when ready. Uses H4 DOT instead of daily.

  tryCatch(
    {
      log_info("Computing H4 counter-trend DOT...")

      # Compile the C++ function
      # sourceCpp("src/dot_fast.cpp")

      resistance_labels <- c(
        "swing_high",
        "bear_fvg_bot",
        "bear_fvg_top",
        "bear_ob_bot",
        "bear_ob_top"
      )
      support_labels <- c(
        "swing_low",
        "bull_fvg_top",
        "bull_fvg_bot",
        "bull_ob_top",
        "bull_ob_bot"
      )

      # Filter target tables
      resistance_table_h4 <- dol_registry[
        dol_label %in%
          resistance_labels &
          timeframe == "h4" &
          formed_bias == "bearish"
      ]
      support_table_h4 <- dol_registry[
        dol_label %in%
          support_labels &
          timeframe == "h4" &
          formed_bias == "bullish"
      ]

      h4_reg <- zone_registry[timeframe == "h4"]

      # Bullish counter-trend: look for support below
      h1_data[
        trade_context == "bull_candidate",
        c("h4_dot", "h4_dot_name", "h4_dot_id") := {
          sub_target <- support_table_h4[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]
          if (nrow(sub_target) > 0 && .N > 0) {
            find_dot_cpp(close, date_time, h4_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      # Bearish counter-trend: look for resistance above
      h1_data[
        trade_context == "bear_candidate",
        c("h4_dot", "h4_dot_name", "h4_dot_id") := {
          sub_target <- resistance_table_h4[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]
          if (nrow(sub_target) > 0 && .N > 0) {
            find_dot_cpp(close, date_time, h4_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      # Same dot_type/dot_top/dot_bot assignment as daily DOT, but using h4_reg
      h1_data[,
        `:=`(
          h4_dot_type = fifelse(
            h4_dot_name %like% "bull|bear",
            gsub("_bot|_top", "", h4_dot_name),
            h4_dot_name
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data[
        h4_reg,
        on = .(
          asset_class,
          ticker,
          h4_dot_type == type,
          h4_dot_id == type_id
        ),
        `:=`(
          h4_dot_top = i.type_zone_top_value,
          h4_dot_bot = i.type_zone_bot_value
        )
      ]

      h1_data[,
        `:=`(
          sl_price_h4 = fcase(
            !is.na(h4_dot_type) & h4_dot_type %like% "bear",
            # dot_top + ((buffer * pip_conv) * 3),
            h4_dot_top,
            !is.na(h4_dot_type) & h4_dot_type %like% "bull",
            # dot_bot - ((buffer * pip_conv) * 3),
            h4_dot_bot,
            !is.na(h4_dot_type) & h4_dot_type %like% "high",
            # dot + ((buffer * pip_conv) * 3),
            h4_dot,
            !is.na(h4_dot_type) & h4_dot_type %like% "low",
            # dot - ((buffer * pip_conv) * 3),
            h4_dot,
            default = NA_real_
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data[, c("h4_dot_id", "h4_dot_type") := NULL]

      h1_data

      log_info(paste0(
        "Completed computing H4 draw on trigger (D.O.T) completed successfully."
      ))
    },
    error = function(e) {
      log_error("Counter-trend H4 DOT failed: {e$message}")
      stop(e)
    }
  )

  # ========================================================================
  # 10. H1 ENTRY TRIGGER (Merge back onto main data.table) ---
  # ========================================================================
  tryCatch(
    {
      log_info(paste0(
        "Starting fast C++ H1 draw on liquidity computation..."
      ))

      # NOTE: sourceCpp("src/dol_fast.cpp") HAS BEEN REMOVED.
      # find_dol_cpp is now natively available via devtools::document()

      # Compile the C++ function
      # sourceCpp("src/dol_fast.cpp")

      resistance_labels <- c(
        "swing_high",
        "bear_fvg_bot",
        "bear_fvg_top",
        "bear_ob_bot",
        "bear_ob_top"
      )
      support_labels <- c(
        "swing_low",
        "bull_fvg_top",
        "bull_fvg_bot",
        "bull_ob_top",
        "bull_ob_bot"
      )

      # Filter target tables
      resistance_table <- dol_registry[
        dol_label %in%
          resistance_labels &
          timeframe == "h1" &
          formed_bias == "bearish"
      ]
      support_table <- dol_registry[
        dol_label %in%
          support_labels &
          timeframe == "h1" &
          formed_bias == "bullish"
      ]

      h1_reg <- zone_registry[timeframe == "h1"]

      # Process Bullish Rows (Looking for Resistance)
      h1_data[
        h1_bias == "bullish" &
          trade_context %like% "bear|bull" &
          kill_zone == TRUE,
        c("entry_dol", "entry_dol_name", "entry_dol_id") := {
          # Subset targets matching the current group metadata to keep
          # C++ loop small
          sub_target <- resistance_table[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]

          if (nrow(sub_target) > 0 && .N > 0) {
            # Ensure target table values are strictly above close to
            # replicate your logic. Filtered here to speed up C++ scans
            find_dol_cpp(close, date_time, h1_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      # Process Bearish Rows (Looking for Support)
      h1_data[
        h1_bias == "bearish" &
          trade_context %like% "bear|bull" &
          kill_zone == TRUE,
        c("entry_dol", "entry_dol_name", "entry_dol_id") := {
          sub_target <- support_table[
            asset_class == .BY$asset_class & ticker == .BY$ticker
          ]

          if (nrow(sub_target) > 0 && .N > 0) {
            find_dol_cpp(close, date_time, h1_bias_id, as.list(sub_target))
          } else {
            list(rep(NA_real_, .N), rep(NA_character_, .N))
          }
        },
        by = .(asset_class, ticker)
      ]

      h1_data[,
        `:=`(
          entry_dol_type = fifelse(
            entry_dol_name %like% "bull|bear",
            gsub("_bot|_top", "", entry_dol_name),
            entry_dol_name
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data[
        h1_reg,
        on = .(
          asset_class,
          ticker,
          entry_dol_type == type,
          entry_dol_id == type_id
        ),
        `:=`(
          entry_dol_top = i.type_zone_top_value,
          entry_dol_bot = i.type_zone_bot_value
        )
      ]

      h1_data[,
        `:=`(
          # sl_price = fcase(
          #   !is.na(entry_dol) &
          #     trade_context %like% "bear" &
          #     entry_dol_name %like% "bear",
          #   entry_dol_top + ((buffer * pip_conv) * 3),
          #   !is.na(entry_dol) &
          #     trade_context %like% "bear" &
          #     entry_dol_name %like% "high",
          #   entry_dol + ((buffer * pip_conv) * 6),
          #   !is.na(entry_dol) &
          #     trade_context %like% "bull" &
          #     entry_dol_name %like% "bull",
          #   entry_dol_bot - ((buffer * pip_conv) * 3),
          #   !is.na(entry_dol) &
          #     trade_context %like% "bull" &
          #     dot_name %like% "low",
          #   entry_dol + ((buffer * pip_conv) * 6),
          #   default = NA_real_
          # ),
          deal_range_mid = fcase(
            trade_context %like% "bear|bull",
            (h4_prev_sh + h4_prev_sl) / 2,
            default = NA_real_
          ),
          target_range_mid = fcase(
            trade_context == "bear_candidate",
            daily_prev_sh -
              ((daily_prev_sh -
                pmax(weekly_target_dol, daily_target_dol, na.rm = TRUE)) *
                0.5),
            trade_context == "bull_candidate",
            daily_prev_sl +
              ((pmin(weekly_target_dol, daily_target_dol, na.rm = TRUE) -
                daily_prev_sl) *
                0.5),
            default = NA_real_
          )
        ),
        by = .(asset_class, ticker)
      ]

      # h1_data[,
      #   `:=`(
      #     entry_price = fcase(
      #       grepl("^bear", trade_context) &
      #         entry_dol > deal_range_mid &
      #         entry_dol > target_range_mid,
      #       entry_dol,
      #       grepl("^bull", trade_context) &
      #         entry_dol < deal_range_mid &
      #         entry_dol < target_range_mid,
      #       entry_dol,
      #       default = NA_real_
      #     ),
      #     sl_price = fifelse(
      #       trade_context %like% "bull",
      #       pmin(sl_price_d, sl_price_h4, na.rm = TRUE),
      #       pmax(sl_price_d, sl_price_h4, na.rm = TRUE)
      #     )
      #   ),
      #   by = .(asset_class, ticker)
      # ]

      h1_data[,
        `:=`(
          entry_price = fcase(
            trade_context %like%
              "bear" &
              (entry_dol > deal_range_mid & entry_dol > target_range_mid),
            fcase(
              !is.na(h4_dot) & entry_dol < h4_dot & entry_dol < daily_dot,
              h4_dot,
              is.na(h4_dot) & entry_dol < daily_dot,
              entry_dol,
              (entry_dol > h4_dot & entry_dol < daily_dot) |
                (entry_dol > h4_dot & entry_dol > daily_dot),
              entry_dol,
              default = NA_real_
            ),
            trade_context %like%
              "bull" &
              (entry_dol < deal_range_mid & entry_dol < target_range_mid),
            fcase(
              !is.na(h4_dot) & entry_dol > h4_dot & entry_dol > daily_dot,
              h4_dot,
              is.na(h4_dot) & entry_dol > daily_dot,
              entry_dol,
              (entry_dol < h4_dot & entry_dol > daily_dot) |
                (entry_dol < h4_dot & entry_dol < daily_dot),
              entry_dol,
              default = NA_real_
            ),
            default = NA_real_
          )
          # sl_price = fifelse(
          #   trade_context %like% "bull",
          #   pmin(sl_price_d, sl_price_h4, na.rm = TRUE),
          #   pmax(sl_price_d, sl_price_h4, na.rm = TRUE)
          # )
        ),
        by = .(asset_class, ticker)
      ][,
        `:=`(
          sl_price = fifelse(
            trade_context %like% "bull",
            (entry_price - (buffer * pip_conv)),
            (entry_price + (buffer * pip_conv))
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data[, c("entry_dol_id", "entry_dol_type") := NULL]

      h1_data

      log_info(paste0(
        "Completed computing the H1",
        " draw on liquidity successfully."
      ))
    },
    error = function(e) {
      log_error(paste0(
        "Error in computing H1 DOL: ",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 11. CONFLUENCE CHECKING (HTF Zone Overlap)
  # ============================================================================
  tryCatch(
    {
      log_info("Checking higher timeframe confluence for entry_dol")

      max_date <- h1_data[, max(date_time)] + weeks(13)

      zone_registry[,
        alive_until := pmin(
          invalidated_at,
          mit_swing_at,
          mit_zone_prox_at,
          mit_zone_dist_at,
          na.rm = TRUE
        )
      ]

      zone_registry[,
        alive_until_sentinel := fifelse(
          is.na(alive_until),
          max_date,
          alive_until
        )
      ]

      # 2. Add an explicit trade_context flag directly onto the master
      # zone registry. This aligns the zone 'type' with h1_data's
      # 'trade_context' strings for exact matching
      zones <- zone_registry[,
        expected_context := fcase(
          type %in% c("bull_fvg", "bull_ob") & formed_bias == "bullish",
          "bull_candidate",
          type %in% c("bear_fvg", "bear_ob") & formed_bias == "bearish",
          "bear_candidate",
          default = NA_character_
        )
      ][timeframe %in% c("weekly", "daily", "h4")]

      # 3. Perform a single unified non-equi join across all timeframes &
      # directions simultaneously
      all_matches <- zones[!is.na(expected_context)][
        h1_data[
          trade_context %in%
            c(
              "bull_candidate",
              "bear_candidate"
            )
        ],
        on = .(
          asset_class,
          ticker,
          # Enforces direction alignment (bull-to-bull, bear-to-bear)
          expected_context == trade_context,
          type_zone_bot_value <= entry_dol,
          type_zone_top_value >= entry_dol,
          formed_at <= date_time,
          alive_until_sentinel > date_time
        ),
        .(
          date_time = i.date_time,
          ticker = i.ticker,
          # Captures "weekly", "daily", "h4" dynamically
          timeframe = x.timeframe,
          matched_type = x.type
        ),
        nomatch = NULL # Drops unmatched rows immediately instead of binding NAs
      ]

      # 4. Generate dynamic confluence strings per timeframe, per ticker/datetime
      confluence_summary <- all_matches[,
        .(
          has_fvg = any(matched_type %like% "fvg"),
          has_ob = any(matched_type %like% "ob")
        ),
        by = .(date_time, ticker, timeframe)
      ]

      confluence_summary[,
        label := fcase(
          has_fvg & has_ob,
          paste0(timeframe, "(FVG+OB)"),
          has_fvg,
          paste0(timeframe, "(FVG)"),
          has_ob,
          paste0(timeframe, "(OB)"),
          default = NA_character_
        )
      ]

      # 5. Pivot wide to separate weekly, daily, and h4 timeframes into
      # explicit column vectors. This automatically matches your required
      # structure for 'i.weekly_confluence'
      confluence_wide <- dcast(
        confluence_summary[timeframe %in% c("weekly", "daily", "h4")],
        date_time + ticker ~ paste0("confl_", timeframe),
        value.var = "label"
      )

      # 6. Dynamically score matching criteria using integer flags
      # (1L = base score). Each matching structural framework adds +1L onto the
      # profile. Combine the three timeframe labels into one comma-separated
      # string. Blank when no higher timeframe zone overlaps at all.
      confluence_wide[,
        `:=`(
          signal_score = 1L +
            (!is.na(confl_weekly)) +
            (!is.na(confl_daily)) +
            (!is.na(confl_h4)),
          confluence = apply(
            confluence_wide[, .(confl_h4, confl_daily, confl_weekly)],
            1,
            function(row) {
              parts <- row[!is.na(row)]
              if (length(parts) == 0) {
                "H1 only"
              } else {
                paste(parts, collapse = ", ")
              }
            }
          )
        )
      ]

      # 7. Atomically assign confluence updates directly into candidates
      # by-reference. Fill remaining unmatched rows using localized vectorized
      # fallbacks
      h1_data[
        confluence_wide,
        on = .(date_time, ticker),
        `:=`(
          signal_score = i.signal_score,
          confl_weekly = i.confl_weekly,
          confl_daily = i.confl_daily,
          confl_h4 = i.confl_h4,
          confluence = i.confluence
        )
      ]

      # 8. Complete fallback definitions for clean downstream analytical parsing
      h1_data[,
        `:=`(
          confluence = fifelse(
            trade_context %like%
              "bear|bull" &
              is.na(confl_weekly) &
              is.na(confl_daily) &
              is.na(confl_h4),
            paste0(
              timeframe,
              "(",
              toupper(sub(
                "^(bear_|bull_)",
                "",
                sub("_(top|bot)$", "", entry_dol_name)
              )),
              ")"
            ),
            confluence
          ),
          signal_score = fifelse(
            trade_context %like%
              "bear|bull" &
              is.na(confl_weekly) &
              is.na(confl_daily) &
              is.na(confl_h4),
            1L,
            signal_score
          )
        ),
        by = .(asset_class, ticker)
      ]

      h1_data

      log_info("Higher timeframe confluence completed successfully")
    },
    error = function(e) {
      log_error(paste0(
        "Error in computing higher timeframe confluence: ",
        e$message
      ))
      stop(e)
    }
  )

  # ============================================================================
  # 12. COLUMN SELECTION  (Required Output for exec_dt & h1_data) ---
  # ============================================================================
  tryCatch(
    {
      log_info("finalising column selection for execution table output")

      required_cols <- c(
        "date_time",
        "h1_prev_sh",
        "h1_prev_sl",
        grep(
          "^(?!.*(h1|id)).*(bias)",
          names(h1_data),
          value = TRUE,
          perl = TRUE
        ),
        grep(
          "^(?!.*(weekly|daily|h4|id)).*(bias)",
          names(h1_data),
          value = TRUE,
          perl = TRUE
        ),
        "trend_context",
        "pullback_context",
        "trade_context",
        "weekly_target_dol",
        "weekly_target_dol_name",
        "daily_target_dol",
        "daily_target_dol_name",
        "daily_dot",
        "daily_dot_name",
        "daily_dot_bot",
        "daily_dot_top",
        "sl_price_d",
        "h4_dot",
        "h4_dot_name",
        "h4_dot_bot",
        "h4_dot_top",
        "sl_price_h4",
        "entry_dol",
        "entry_dol_name",
        "entry_dol_bot",
        "entry_dol_top",
        "entry_price",
        "sl_price",
        "deal_range_mid",
        "target_range_mid",
        "signal_score",
        "confluence",
        "timeframe",
        "ticker",
        "asset_class"
      )

      exec_dt <- h1_data[trade_context %like% "bear|bull", ..required_cols]

      exec_dt <- exec_dt[
        !duplicated(
          exec_dt,
          by = c(
            "ticker",
            "entry_dol"
          )
        )
      ]

      exec_dt <- exec_dt[!is.na(entry_price)]

      log_info(
        paste0(
          "Required data.table output completed successfully for ",
          length(unique(exec_dt$ticker)),
          " tickers resulting in a ",
          "data.table with ",
          length(names(exec_dt)),
          " columns and ",
          nrow(exec_dt),
          " rows."
        )
      )
    },
    error = function(e) {
      log_error(paste0(
        "Execution data column selection failed: ",
        e$message
      ))
      stop(e)
    }
  )

  # h1_data column selection
  tryCatch(
    {
      log_info("finalising column selection for h1 table output")

      required_h1_cols <- c(
        "date_time",
        "open",
        "high",
        "low",
        "close",
        "pip_conv",
        "h1_trend_state",
        "h1_prev_sl",
        "h1_prev_sh", # for trailing stop
        "h1_choch_level",
        "h1_choch_type", # for CHoCH exit
        "h1_bos_type", # for BOS fallback exit
        "h4_trend_state",
        "h4_prev_sl",
        "h4_prev_sh", # for trailing stop
        "h4_choch_level",
        "h4_choch_type", # for CHoCH exit
        "h4_bos_type", # for BOS fallback exit
        "ticker",
        "asset_class"
      )

      h1_data <- h1_data[, ..required_h1_cols]

      log_info(
        paste0(
          "Required data.table output completed successfully for ",
          length(unique(h1_data$ticker)),
          " tickers resulting in a ",
          "data.table with ",
          length(names(h1_data)),
          " columns and ",
          nrow(h1_data),
          " rows."
        )
      )
    },
    error = function(e) {
      log_error(paste0(
        "H1 data column selection failed: ",
        e$message
      ))
      stop(e)
    }
  )

  results <- setNames(
    list(h1_data, exec_dt),
    c("h1_data", "exec_data")
  )

  return(results)
}

################################################################################
