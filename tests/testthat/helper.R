# tests/testthat/helper.R

make_valid_config <- function() {
  local_base <- tempfile("local_base")
  forexdataset_base <- tempfile("forexdatasets")
  forexengine_base <- tempfile("forexengine")
  db_dir <- file.path(forexengine_base, "Databases")

  dir.create(local_base, recursive = TRUE)
  dir.create(forexdataset_base, recursive = TRUE)
  dir.create(forexengine_base, recursive = TRUE)
  dir.create(db_dir, recursive = TRUE)

  list(
    paths = list(
      local_base = local_base,
      forexengine = list(
        output_base = forexengine_base,
        table_names = c("registry", "bos_choch", "reversal_watch"),
        db_dir = db_dir
      ),
      forexdataset = list(output_base = forexdataset_base),
      frequency_folders = list(
        h1 = "h1",
        daily = "daily",
        h4 = "h4",
        weekly = "weekly",
        monthly = "monthly",
        quarterly = "quarterly"
      ),
      file_suffixes = list(
        h1 = "h1",
        daily = "d",
        h4 = "h4",
        weekly = "w",
        monthly = "m",
        quarterly = "q"
      ),
      subfolder = "csv"
    ),
    web = list(
      stooq = list(
        base_url = "https://stooq.com",
        url_pattern = "https://stooq.com/q/d/l/?s={ticker}&i=d",
        table_id = "#f13",
        x_path = "//table"
      ),
      investing = list(
        base_url = "https://www.investing.com",
        url_pattern = "https://www.investing.com/{asset_class}/{ticker}-historical-data",
        table_id = "#curr_table",
        x_path = "//table"
      ),
      user_agent = c("Googlebot"),
      delay_between_requests = 2,
      timeout = 30
    ),
    asset_classes = list(
      names = c("Currency_Pairs", "Commodity_Futures"),
      Currency_Pairs = list(
        source = "local",
        frequencies = c("daily", "hourly"),
        tickers = c("audcad", "eurusd", "gbpjpy"),
        other_tickers = c("gbpjpy")
      ),
      Commodity_Futures = list(
        source = c("local", "stooq", "investing"),
        frequencies = "daily",
        tickers = c("GC", "SI"),
        investing_tickers = c("gold", "silver")
      )
    ),
    directory_map = list(
      Currency_Pairs = list(
        daily = list(major = "major", other = "other"),
        hourly = list(major = "major", other = "other")
      ),
      Commodity_Futures = list(
        daily = list(stooq = "stooq", investing = "investing")
      )
    ),
    data_format = list(
      local_hourly = c("date", "time", "open", "high", "low", "close"),
      local_daily = c("date", "time", "open", "high", "low", "close"),
      stooq_daily = c(
        "Date",
        "Open",
        "High",
        "Low",
        "Close",
        "Volume",
        "Open Interest"
      ),
      investing_daily = c(
        "Date",
        "Price",
        "Open",
        "High",
        "Low",
        "Vol.",
        "Change %"
      ),
      file_extension = ".csv",
      fut_extension = ".csv",
      date_format = "%Y%m%d",
      date_time_format = "%Y%m%d%H%M%S",
      column_mapping = list(
        hourly = list(
          date = "date",
          time = "time",
          open = "open",
          high = "high",
          low = "low",
          close = "close"
        ),
        daily = list(
          date = "date",
          time = "time",
          open = "open",
          high = "high",
          low = "low",
          close = "close"
        ),
        stooq = list(
          Date = "date_time",
          Open = "open",
          High = "high",
          Low = "low",
          Close = "close",
          Volume = "volume",
          `Open Interest` = "open_interest"
        ),
        investing = list(
          Date = "date_time",
          Price = "close",
          Open = "open",
          High = "high",
          Low = "low",
          `Vol.` = "volume",
          `Change %` = "change_pct"
        )
      )
    ),
    transformation = list(
      missing_value_fill = 0,
      pip_factor = list(
        regex_pattern = "jpy|xau|xag|xpd|xpt",
        value_for_match = 0.001,
        default_value = 0.00001
      ),
      aggregation = list(
        h1_to_h4 = "4 hours",
        h1_to_daily = "day",
        daily_to_weekly = "week",
        daily_to_monthly = "month",
        daily_to_quarterly = "quarter"
      )
    ),
    analysis = list(
      trend_filter = list(
        weekly = list(lookback_period = 12L),
        daily = list(lookback_period = 30L),
        h4 = list(lookback_period = 13L),
        h1 = list(lookback_period = 21L),
        signal_filter = list(
          lower_gap = 0.500,
          upper_gap = 0.999,
          min_confluence = 1,
          risk_pct = 0.01,
          buffer_pipette = 0.5,
          min_risk_reward = 1.5
        )
      ),
      swing_filter = list(
        noise_filter = 0.92,
        sw_gap_bot = 0.25,
        sw_gap_top = 0.75,
        ob_filter = 0.618
      ),
      choch_filter = list(choch_quant = 0.618),
      sessions = list(
        asian = list(start = "00:00", end = "08:00"),
        london = list(
          standard_time = list(start = "08:00", end = "16:00"),
          daylight_time = list(start = "07:00", end = "15:00"),
          kill_zones = list(london_open = list(start = "08:00", end = "11:00"))
        ),
        newyork = list(
          standard_time = list(start = "13:00", end = "21:00"),
          daylight_time = list(start = "12:00", end = "20:00"),
          kill_zones = list(ny_open = list(start = "13:00", end = "16:00"))
        )
      )
    ),
    database = list(
      db_suffix = ".sqlite",
      table_prefixes = list(
        market_data = "candles",
        registry = "registry",
        dol_table = "dol",
        journal = "trading_journal",
        paper_trade = "paper_trading"
      )
    ),
    pipeline = list()
  )
}
