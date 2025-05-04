# IMF PortWatchデータを使った出発地・到着地ポートコール数の推移分析
# 必要なパッケージのインストールと読み込み
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggalluvial, plotly, scales, readr, httr, curl)

# ===================================
# 1. CSVデータのダウンロード
# ===================================

# ダウンロード関数
download_portwatch_csv <- function(url, file_path) {
  message("IMF PortWatchデータをダウンロード中: ", url)
  
  # ディレクトリがなければ作成
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  
  # ダウンロード試行
  tryCatch({
    # curlを使用してファイルをダウンロード
    curl_download(url, file_path, quiet = FALSE, mode = "wb")
    
    # ファイルの存在と非ゼロサイズを確認
    if (file.exists(file_path) && file.size(file_path) > 0) {
      message("ダウンロード成功: ", file_path)
      return(TRUE)
    } else {
      warning("ダウンロードは完了しましたが、ファイルサイズがゼロまたはファイルが存在しません。")
      return(FALSE)
    }
  }, error = function(e) {
    warning("ダウンロード中にエラーが発生しました: ", e$message)
    return(FALSE)
  })
}

# ポートコールデータのCSV URL (IMF PortWatchデータのURL)
port_calls_csv_url <- "https://portwatch.imf.org/datasets/75619cb86e5f4beeb7dab9629d861acf_0.csv"
port_data_file <- "data/portwatch_port_calls.csv"

# ポート情報データのCSV URL (ポート名、位置情報など)
ports_csv_url <- "https://portwatch.imf.org/datasets/acc668d199d1472abaaf2467133d4ca4_0.csv"
ports_info_file <- "data/portwatch_ports_info.csv"

# データダウンロード
port_calls_downloaded <- download_portwatch_csv(port_calls_csv_url, port_data_file)
ports_info_downloaded <- download_portwatch_csv(ports_csv_url, ports_info_file)

# # ===================================
# # 2. サンプルデータ生成 (ダウンロードできない場合)
# # ===================================
# 
# # サンプルデータ生成関数
# generate_sample_data <- function(n_ports = 50, n_days = 180) {
#   message("サンプルデータを生成しています...")
#   
#   # 港のリスト
#   major_ports <- c(
#     "Shanghai", "Singapore", "Ningbo-Zhoushan", "Busan", "Hong Kong", 
#     "Qingdao", "Guangzhou", "Dubai", "Tianjin", "Rotterdam", 
#     "Port Klang", "Antwerp", "Kaohsiung", "Xiamen", "Hamburg", 
#     "Los Angeles", "Tanjung Pelepas", "Dalian", "New York", "Long Beach",
#     "Laem Chabang", "Tanjung Priok", "Valencia", "Manila", "Algeciras",
#     "Jeddah", "Colombo", "Sydney", "Tokyo", "Yokohama"
#   )
#   
#   # サンプル港を選択
#   selected_ports <- sample(major_ports, min(n_ports, length(major_ports)))
#   
#   # 日付範囲
#   end_date <- Sys.Date()
#   start_date <- end_date - n_days
#   date_range <- seq(start_date, end_date, by = "day")
#   
#   # ポートコールデータの生成
#   port_call_data <- expand.grid(
#     date = date_range,
#     port_name = selected_ports,
#     stringsAsFactors = FALSE
#   )
#   
#   # ランダムなポートコール数
#   port_call_data$port_calls <- round(runif(nrow(port_call_data), min = 1, max = 30))
#   
#   # AIS船舶シグナル数 (ポートコール数の約10倍)
#   port_call_data$vessel_signals <- port_call_data$port_calls * runif(nrow(port_call_data), min = 5, max = 15)
#   
#   # 最後のポート (出発港) と次のポート (到着港) のデータを生成
#   set.seed(42)  # 再現性のため
#   
#   # 各日付・港の組み合わせに対して接続情報を生成
#   port_connections <- data.frame()
#   
#   for (i in 1:nrow(port_call_data)) {
#     current_port <- port_call_data$port_name[i]
#     current_date <- port_call_data$date[i]
#     n_connections <- min(5, round(port_call_data$port_calls[i] * runif(1, 0.5, 1)))
#     
#     if (n_connections > 0) {
#       # この港に接続する他の港をランダムに選択
#       connected_ports <- sample(selected_ports[selected_ports != current_port], 
#                                 size = min(n_connections, length(selected_ports) - 1), 
#                                 replace = FALSE)
#       
#       # 出発港としての接続
#       origin_connections <- data.frame(
#         date = current_date,
#         origin_port = current_port,
#         destination_port = connected_ports,
#         port_calls = round(runif(length(connected_ports), 1, 10)),
#         stringsAsFactors = FALSE
#       )
#       
#       # 到着港としての接続
#       destination_connections <- data.frame(
#         date = current_date,
#         origin_port = connected_ports,
#         destination_port = current_port,
#         port_calls = round(runif(length(connected_ports), 1, 10)),
#         stringsAsFactors = FALSE
#       )
#       
#       port_connections <- rbind(port_connections, origin_connections, destination_connections)
#     }
#   }
#   
#   # 港の位置情報を生成
#   port_locations <- data.frame(
#     port_name = selected_ports,
#     longitude = runif(length(selected_ports), -180, 180),
#     latitude = runif(length(selected_ports), -85, 85),
#     country = sample(c("China", "USA", "Japan", "Singapore", "Netherlands", 
#                        "South Korea", "UAE", "Malaysia", "Belgium", "Germany", 
#                        "Taiwan", "Spain", "Philippines", "Saudi Arabia", "Australia"),
#                      length(selected_ports), replace = TRUE),
#     stringsAsFactors = FALSE
#   )
#   
#   # 大陸情報の追加
#   continent_map <- list(
#     "China" = "Asia", 
#     "Japan" = "Asia", 
#     "Singapore" = "Asia", 
#     "South Korea" = "Asia", 
#     "UAE" = "Middle East", 
#     "Malaysia" = "Asia", 
#     "Taiwan" = "Asia", 
#     "Philippines" = "Asia",
#     "Saudi Arabia" = "Middle East",
#     "USA" = "North America",
#     "Netherlands" = "Europe", 
#     "Belgium" = "Europe", 
#     "Germany" = "Europe",
#     "Spain" = "Europe",
#     "Australia" = "Oceania"
#   )
#   
#   port_locations$continent <- unlist(continent_map[port_locations$country])
#   
#   # 日付を文字列に変換
#   port_call_data$date <- as.character(port_call_data$date)
#   port_connections$date <- as.character(port_connections$date)
#   
#   message("サンプルデータ生成完了:")
#   message("- ポートコールデータ: ", nrow(port_call_data), "行")
#   message("- ポート接続データ: ", nrow(port_connections), "行")
#   message("- ポート位置情報: ", nrow(port_locations), "行")
#   
#   return(list(
#     port_calls = port_call_data,
#     port_connections = port_connections,
#     port_locations = port_locations
#   ))
# }

# ===================================
# 3. データの読み込み
# ===================================

# データ読み込み関数
load_portwatch_data <- function() {
  # ポートコールデータの読み込み試行
  if (port_calls_downloaded && file.exists(port_data_file)) {
    message("ポートコールCSVファイルを読み込み中...")
    
    # CSVをトライ
    tryCatch({
      port_data <- read_csv(port_data_file)
      message("ポートコールデータの読み込み成功: ", nrow(port_data), "行")
      
      # 簡単なデータ確認
      message("ポートコールデータのカラム:")
      print(colnames(port_data))
      
      # ポート情報の読み込み
      if (ports_info_downloaded && file.exists(ports_info_file)) {
        ports_info <- read_csv(ports_info_file)
        message("ポート情報の読み込み成功: ", nrow(ports_info), "行")
        
        return(list(
          port_calls = port_data,
          port_info = ports_info,
          is_sample = FALSE
        ))
      } else {
        message("ポート情報ファイルの読み込みに失敗しました。ポートコールデータのみ使用します。")
        return(list(
          port_calls = port_data,
          port_info = NULL,
          is_sample = FALSE
        ))
      }
    }, error = function(e) {
      warning("ポートコールデータの読み込み中にエラーが発生しました: ", e$message)
      message("サンプルデータを生成します...")
      sample_data <- generate_sample_data()
      return(list(
        port_calls = sample_data$port_calls,
        port_connections = sample_data$port_connections,
        port_locations = sample_data$port_locations,
        is_sample = TRUE
      ))
    })
  } else {
    # サンプルデータの生成
    message("ダウンロードに失敗したか、ファイルが見つかりません。サンプルデータを生成します...")
    sample_data <- generate_sample_data()
    return(list(
      port_calls = sample_data$port_calls,
      port_connections = sample_data$port_connections,
      port_locations = sample_data$port_locations,
      is_sample = TRUE
    ))
  }
}

# データの読み込み
data_list <- load_portwatch_data()

# ===================================
# 4. データの探索と前処理
# ===================================

# データ構造の確認と前処理
process_portwatch_data <- function(data_list) {
  message("データの探索と前処理を開始...")
  
  # サンプルデータかダウンロードデータかで処理を分岐
  if (data_list$is_sample) {
    port_calls <- data_list$port_calls
    port_connections <- data_list$port_connections
    port_locations <- data_list$port_locations
    
    # サンプルデータの場合は既に必要な構造になっているので、日付の変換のみ
    port_connections$date <- as.Date(port_connections$date)
    
    message("サンプルデータの前処理完了")
    
    processed_data <- list(
      port_calls = port_calls,
      port_connections = port_connections,
      port_locations = port_locations,
      is_sample = TRUE
    )
    
  } else {
    # 実際のIMF PortWatchデータの処理
    port_calls <- data_list$port_calls
    port_info <- data_list$port_info
    
    message("実データの構造を確認しています...")
    
    # 実データの構造に応じた前処理
    # 注: 実際のデータ構造が不明なので、最も可能性の高いフィールド名を仮定
    # ヘッダー情報を確認
    message("ポートコールデータのカラム:")
    print(colnames(port_calls))
    
    if (!is.null(port_info)) {
      message("ポート情報のカラム:")
      print(colnames(port_info))
    }
    
    # ここから、実際のデータ構造に基づいて前処理を行う
    # 以下はデータ構造の仮定に基づく処理なので、実際のデータ構造によって調整が必要
    
    # 出発地と到着地のデータを抽出・変換 (カラム名は実際のデータに合わせて調整必要)
    # 仮想的な例: last_port_id と next_port_id というフィールドがあると仮定
    # ポート接続データの作成を試みる
    tryCatch({
      # 可能性のあるカラム名パターンを確認
      possible_origin_cols <- c("last_port", "last_port_id", "origin_port", "source_port", "from_port")
      possible_dest_cols <- c("next_port", "next_port_id", "destination_port", "dest_port", "to_port")
      possible_date_cols <- c("date", "call_date", "port_call_date", "timestamp")
      possible_count_cols <- c("port_calls", "count", "vessel_count", "call_count")
      
      # 実際のカラムを特定
      origin_col <- NULL
      dest_col <- NULL
      date_col <- NULL
      count_col <- NULL
      
      for (col in colnames(port_calls)) {
        if (col %in% possible_origin_cols) origin_col <- col
        if (col %in% possible_dest_cols) dest_col <- col
        if (col %in% possible_date_cols) date_col <- col
        if (col %in% possible_count_cols) count_col <- col
      }
      
      # 必要なカラムがあるか確認
      if (!is.null(origin_col) && !is.null(dest_col) && !is.null(date_col)) {
        message("出発地・到着地データが見つかりました。カラム: ", origin_col, ", ", dest_col, ", ", date_col)
        
        # 接続データフレームの作成
        port_connections <- port_calls %>%
          select(date = all_of(date_col), 
                 origin_port = all_of(origin_col), 
                 destination_port = all_of(dest_col)) %>%
          filter(!is.na(origin_port) & !is.na(destination_port))
        
        # カウントカラムがあれば追加
        if (!is.null(count_col)) {
          port_connections$port_calls <- port_calls[[count_col]]
        } else {
          # なければ1で初期化
          port_connections$port_calls <- 1
        }
        
        # 日付の処理
        if (is.character(port_connections$date)) {
          port_connections$date <- as.Date(port_connections$date)
        } else if (is.numeric(port_connections$date)) {
          # UNIXタイムスタンプの可能性
          port_connections$date <- as.Date(as.POSIXct(port_connections$date / 1000, origin = "1970-01-01"))
        }
        
        message("ポート接続データフレーム作成完了: ", nrow(port_connections), "行")
      } else {
        # 必要なカラムが見つからない場合はサンプルデータを生成
        warning("必要なカラムが見つかりません。サンプルの接続データを生成します。")
        sample_data <- generate_sample_data()
        port_connections <- sample_data$port_connections
        port_connections$date <- as.Date(port_connections$date)
      }
      
      # ポート位置情報の処理
      if (!is.null(port_info)) {
        # ポート情報から位置データを抽出
        possible_lat_cols <- c("latitude", "lat", "y")
        possible_lon_cols <- c("longitude", "lon", "long", "x")
        possible_port_name_cols <- c("port_name", "port", "name")
        
        lat_col <- NULL
        lon_col <- NULL
        port_name_col <- NULL
        
        for (col in colnames(port_info)) {
          if (col %in% possible_lat_cols) lat_col <- col
          if (col %in% possible_lon_cols) lon_col <- col
          if (col %in% possible_port_name_cols) port_name_col <- col
        }
        
        if (!is.null(lat_col) && !is.null(lon_col) && !is.null(port_name_col)) {
          port_locations <- port_info %>%
            select(port_name = all_of(port_name_col),
                   latitude = all_of(lat_col),
                   longitude = all_of(lon_col))
          
          # 国や大陸の情報があれば追加
          if ("country" %in% colnames(port_info)) {
            port_locations$country <- port_info$country
          }
          
          if ("continent" %in% colnames(port_info)) {
            port_locations$continent <- port_info$continent
          }
          
          message("ポート位置情報の処理完了: ", nrow(port_locations), "行")
        } else {
          warning("ポート位置情報に必要なカラムが見つかりません。")
          port_locations <- NULL
        }
      } else {
        warning("ポート情報がありません。")
        port_locations <- NULL
      }
      
    }, error = function(e) {
      warning("データ処理中にエラーが発生しました: ", e$message)
      message("サンプルデータを代わりに使用します。")
      
      sample_data <- generate_sample_data()
      port_connections <- sample_data$port_connections
      port_connections$date <- as.Date(port_connections$date)
      port_locations <- sample_data$port_locations
    })
    
    processed_data <- list(
      port_calls = port_calls,
      port_connections = port_connections,
      port_locations = port_locations,
      is_sample = FALSE
    )
  }
  
  message("データ処理完了。")
  return(processed_data)
}

# データの前処理を実行
processed_data <- process_portwatch_data(data_list)

# ===================================
# 5. データの集計
# ===================================

# 出発地と到着地のポートコール数集計
aggregate_port_connections <- function(data) {
  message("ポート接続データを集計中...")
  
  # ポート接続データの取得
  port_connections <- data$port_connections
  
  # 日付をシンプルな月に変換
  port_connections <- port_connections %>%
    mutate(month = floor_date(date, "month"))
  
  # 出発地・到着地のペアごとの月次集計
  monthly_connections <- port_connections %>%
    group_by(month, origin_port, destination_port) %>%
    summarise(total_port_calls = sum(port_calls, na.rm = TRUE), .groups = "drop")
  
  # 出発地別の月次集計
  origin_monthly <- port_connections %>%
    group_by(month, origin_port) %>%
    summarise(outgoing_calls = sum(port_calls, na.rm = TRUE), .groups = "drop")
  
  # 到着地別の月次集計
  destination_monthly <- port_connections %>%
    group_by(month, destination_port) %>%
    summarise(incoming_calls = sum(port_calls, na.rm = TRUE), .groups = "drop")
  
  # 大陸情報があれば大陸別の集計も行う
  continent_monthly <- NULL
  
  if (!is.null(data$port_locations) && 
      "continent" %in% colnames(data$port_locations) && 
      nrow(data$port_locations) > 0) {
    
    # ポート接続データに大陸情報を追加
    port_connections_with_continent <- port_connections %>%
      left_join(data$port_locations %>% 
                  select(port_name, continent) %>% 
                  rename(origin_port = port_name, 
                         origin_continent = continent),
                by = "origin_port") %>%
      left_join(data$port_locations %>% 
                  select(port_name, continent) %>% 
                  rename(destination_port = port_name, 
                         destination_continent = continent),
                by = "destination_port")
    
    # 大陸間の集計を行う
    continent_monthly <- port_connections_with_continent %>%
      filter(!is.na(origin_continent) & !is.na(destination_continent)) %>%
      group_by(month, origin_continent, destination_continent) %>%
      summarise(total_calls = sum(port_calls, na.rm = TRUE), .groups = "drop")
    
    message("大陸間のポートコール集計完了: ", nrow(continent_monthly), "行")
  }
  
  message("ポート接続集計完了")
  return(list(
    port_pairs = monthly_connections,
    origins = origin_monthly,
    destinations = destination_monthly,
    continents = continent_monthly
  ))
}

# 集計の実行
aggregated_data <- aggregate_port_connections(processed_data)

# ===================================
# 6. データの可視化
# ===================================

# 1. 上位の出発地と到着地のポートコール数推移
plot_top_port_trends <- function(agg_data, top_n = 10) {
  message("上位の出発地と到着地の推移をプロット中...")
  
  # 上位の出発地
  top_origins <- agg_data$origins %>%
    group_by(origin_port) %>%
    summarise(total_outgoing = sum(outgoing_calls, na.rm = TRUE)) %>%
    arrange(desc(total_outgoing)) %>%
    head(top_n) %>%
    pull(origin_port)
  
  # 上位の到着地
  top_destinations <- agg_data$destinations %>%
    group_by(destination_port) %>%
    summarise(total_incoming = sum(incoming_calls, na.rm = TRUE)) %>%
    arrange(desc(total_incoming)) %>%
    head(top_n) %>%
    pull(destination_port)
  
  # 出発地の推移プロット
  origin_plot <- agg_data$origins %>%
    filter(origin_port %in% top_origins) %>%
    ggplot(aes(x = month, y = outgoing_calls, color = origin_port)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = paste0("上位", top_n, "出発港のポートコール数の推移"),
      x = "月",
      y = "出発ポートコール数",
      color = "出発港"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # 到着地の推移プロット
  destination_plot <- agg_data$destinations %>%
    filter(destination_port %in% top_destinations) %>%
    ggplot(aes(x = month, y = incoming_calls, color = destination_port)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = paste0("上位", top_n, "到着港のポートコール数の推移"),
      x = "月",
      y = "到着ポートコール数",
      color = "到着港"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  message("推移プロット完了")
  return(list(origin_plot = origin_plot, destination_plot = destination_plot))
}

# 2. 出発地と到着地の関係を示すアルビオンプロット
plot_port_flow <- function(agg_data, period_start = NULL, period_end = NULL, top_n = 5) {
  message("出発地と到着地の関係をプロット中...")
  
  # 期間フィルタリング
  flow_data <- agg_data$port_pairs
  if (!is.null(period_start) && !is.null(period_end)) {
    flow_data <- flow_data %>%
      filter(month >= as.Date(period_start) & month <= as.Date(period_end))
  }
  
  # 上位の出発地と到着地を特定
  top_pairs <- flow_data %>%
    group_by(origin_port, destination_port) %>%
    summarise(total_flow = sum(total_port_calls, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_flow)) %>%
    head(top_n)
  
  top_origins <- unique(top_pairs$origin_port)
  top_destinations <- unique(top_pairs$destination_port)
  
  # 上位のペアのみでデータフィルタリング
  flow_data <- flow_data %>%
    filter(
      origin_port %in% top_origins & 
        destination_port %in% top_destinations
    ) %>%
    group_by(origin_port, destination_port) %>%
    summarise(total_port_calls = sum(total_port_calls, na.rm = TRUE), .groups = "drop")
  
  # アルビオンプロット作成
  alluvial_plot <- ggplot(
    flow_data,
    aes(
      y = total_port_calls,
      axis1 = origin_port, 
      axis2 = destination_port
    )
  ) +
    geom_alluvium(aes(fill = origin_port), width = 1/3) +
    geom_stratum(width = 1/3, fill = "grey80", color = "grey50") +
    geom_text(
      stat = "stratum", 
      aes(label = after_stat(stratum)),
      size = 3
    ) +
    scale_x_discrete(
      limits = c("出発港", "到着港"),
      expand = c(0.1, 0.1)
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "出発港から到着港へのポートコールフロー",
      y = "ポートコール数",
      fill = "出発港"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  message("アルビオンプロット完了")
  return(alluvial_plot)
}

# 3. 大陸間フローのヒートマップ
plot_continent_heatmap <- function(agg_data) {
  message("大陸間フローのヒートマップ作成中...")
  
  if (is.null(agg_data$continents) || nrow(agg_data$continents) == 0) {
    message("大陸データがありません。")
    return(NULL)
  }
  
  # 大陸ごとに合計
  continent_matrix <- agg_data$continents %>%
    group_by(origin_continent, destination_continent) %>%
    summarise(total_flow = sum(total_calls, na.rm = TRUE), .groups = "drop") %>%
    filter(origin_continent != destination_continent) %>%
    filter(!is.na(origin_continent) & !is.na(destination_continent))
  
  if (nrow(continent_matrix) == 0) {
    message("大陸間フローのデータがありません。")
    return(NULL)
  }
  
  # ヒートマップ作成
  heatmap_plot <- ggplot(
    continent_matrix,
    aes(x = origin_continent, y = destination_continent, fill = total_flow)
  ) +
    geom_tile() +
    scale_fill_viridis_c(trans = "log10") +
    geom_text(aes(label = scales::comma(total_flow)), 
              color = "white", size = 3) +
    labs(
      title = "大陸間のポートコールフロー",
      x = "出発大陸",
      y = "到着大陸",
      fill = "ポートコール数\n(対数スケール)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  message("ヒートマップ完了")
  return(heatmap_plot)
}

# 4. 時間の経過に伴う主要港の出発・到着比率の変化
plot_origin_destination_ratio <- function(agg_data, top_n = 10) {
  message("出発・到着比率の変化をプロット中...")
  
  # 上位の港を特定
  top_ports <- bind_rows(
    agg_data$origins %>% 
      group_by(origin_port) %>% 
      summarise(total = sum(outgoing_calls, na.rm = TRUE)) %>%
      rename(port = origin_port),
    agg_data$destinations %>% 
      group_by(destination_port) %>% 
      summarise(total = sum(incoming_calls, na.rm = TRUE)) %>%
      rename(port = destination_port)
  ) %>%
    group_by(port) %>%
    summarise(total = sum(total, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    head(top_n) %>%
    pull(port)
  
  # 各港の月ごとの出発・到着数を結合
  port_ratios <- full_join(
    agg_data$origins %>% 
      filter(origin_port %in% top_ports) %>%
      rename(port = origin_port, outgoing = outgoing_calls),
    agg_data$destinations %>% 
      filter(destination_port %in% top_ports) %>%
      rename(port = destination_port, incoming = incoming_calls),
    by = c("month", "port")
  ) %>%
    replace_na(list(outgoing = 0, incoming = 0)) %>%
    mutate(
      total = outgoing + incoming,
      outgoing_ratio = outgoing / total,
      incoming_ratio = incoming / total
    )
  
  # 積み上げ面グラフ作成
  stacked_plot <- port_ratios %>%
    ggplot(aes(x = month, y = outgoing_ratio, fill = port)) +
    geom_area(position = "stack") +
    scale_fill_brewer(palette = "Spectral") +
    labs(
      title = paste0("上位", top_n, "港の出発ポートコール比率の推移"),
      x = "月",
      y = "出発ポートコール比率",
      fill = "港"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  message("出発・到着比率プロット完了")
  return(stacked_plot)
}

# 5. インタラクティブな地図作成
plot_interactive_map <- function(data, agg_data) {
  message("インタラクティブマップ作成中...")
  
  # ポート位置情報の確認
  if (is.null(data$port_locations) || nrow(data$port_locations) == 0) {
    message("ポート位置情報がありません。マップは作成できません。")
    return(NULL)
  }
  
  # 接続データの確認
  if (is.null(agg_data$port_pairs) || nrow(agg_data$port_pairs) == 0) {
    message("ポート接続データがありません。マップは作成できません。")
    return(NULL)
  }
  
  # ポート位置情報の取得
  port_locations <- data$port_locations
  
  # ポート名の列名を特定
  port_name_col <- colnames(port_locations)[grepl("port|name", colnames(port_locations), ignore.case = TRUE)][1]
  
  if (is.null(port_name_col)) {
    message("ポート名のカラムが見つかりません。")
    return(NULL)
  }
  
  # 上位のフローを特定
  top_flows <- agg_data$port_pairs %>%
    group_by(origin_port, destination_port) %>%
    summarise(total_flow = sum(total_port_calls, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_flow)) %>%
    head(30)
  
  # 必要な港のリスト
  required_ports <- unique(c(top_flows$origin_port, top_flows$destination_port))
  
  # 港の位置情報をフィルタリング
  port_map <- port_locations %>%
    filter(!!sym(port_name_col) %in% required_ports) %>%
    rename(port_name = !!sym(port_name_col))
  
  if (nrow(port_map) == 0) {
    message("必要な港の位置情報がありません。")
    return(NULL)
  }
  
  # プロットリーでマップ作成
  map_plot <- plot_geo() %>%
    add_markers(
      data = port_map,
      x = ~longitude, y = ~latitude, 
      text = ~port_name,
      name = "港",
      marker = list(size = 8, color = "blue")
    )
  
  # フローの描画前に、位置情報にあるポートのみフィルタリング
  available_ports <- port_map$port_name
  valid_flows <- top_flows %>%
    filter(origin_port %in% available_ports & destination_port %in% available_ports)
  
  if (nrow(valid_flows) == 0) {
    message("描画可能なフローがありません。")
    return(map_plot)
  }
  
  # 港の位置情報を結合
  flows_with_coords <- valid_flows %>%
    left_join(port_map %>% select(port_name, longitude, latitude), 
              by = c("origin_port" = "port_name")) %>%
    rename(origin_lon = longitude, origin_lat = latitude) %>%
    left_join(port_map %>% select(port_name, longitude, latitude), 
              by = c("destination_port" = "port_name")) %>%
    rename(dest_lon = longitude, dest_lat = latitude) %>%
    filter(!is.na(origin_lon) & !is.na(origin_lat) & !is.na(dest_lon) & !is.na(dest_lat))
  
  # フローを追加
  for (i in 1:nrow(flows_with_coords)) {
    flow <- flows_with_coords[i, ]
    map_plot <- map_plot %>%
      add_segments(
        x = flow$origin_lon, y = flow$origin_lat,
        xend = flow$dest_lon, yend = flow$dest_lat,
        name = paste(flow$origin_port, "to", flow$destination_port),
        line = list(
          width = log1p(flow$total_flow) / 2,
          color = "red",
          opacity = 0.6
        )
      )
  }
  
  map_plot <- map_plot %>%
    layout(
      title = "主要港間のポートコールフロー",
      geo = list(
        showland = TRUE,
        showlakes = TRUE,
        showcountries = TRUE,
        showocean = TRUE,
        oceancolor = "lightblue",
        projection = list(type = "natural earth"),
        landcolor = "rgb(250, 250, 220)",
        countrycolor = "rgb(200, 200, 200)"
      )
    )
  
  message("インタラクティブマップ完了")
  return(map_plot)
}

# 6. 月次の出発地と到着地の双方向トレンド
plot_bidirectional_trends <- function(agg_data, port_pair, max_months = 12) {
  message("双方向トレンドのプロット中: ", port_pair[1], " <-> ", port_pair[2])
  
  # 最新の月を特定
  latest_month <- max(agg_data$port_pairs$month, na.rm = TRUE)
  
  # 分析期間を設定
  start_month <- latest_month - months(max_months - 1)
  
  # 指定された港のペアのデータを抽出
  pair_data_ab <- agg_data$port_pairs %>%
    filter(
      month >= start_month,
      origin_port == port_pair[1], 
      destination_port == port_pair[2]
    ) %>%
    rename(ab_calls = total_port_calls)
  
  pair_data_ba <- agg_data$port_pairs %>%
    filter(
      month >= start_month,
      origin_port == port_pair[2], 
      destination_port == port_pair[1]
    ) %>%
    rename(ba_calls = total_port_calls)
  
  # データの結合
  combined_data <- full_join(
    pair_data_ab, 
    pair_data_ba,
    by = "month"
  ) %>%
    mutate(
      ab_calls = if_else(is.na(ab_calls), 0, ab_calls),
      ba_calls = if_else(is.na(ba_calls), 0, ba_calls)
    ) %>%
    select(month, ab_calls, ba_calls) %>%
    pivot_longer(
      cols = c(ab_calls, ba_calls),
      names_to = "direction",
      values_to = "calls"
    ) %>%
    mutate(
      direction = if_else(
        direction == "ab_calls", 
        paste(port_pair[1], "→", port_pair[2]), 
        paste(port_pair[2], "→", port_pair[1])
      )
    )
  
  # プロット作成
  bidirectional_plot <- ggplot(combined_data, aes(x = month, y = calls, color = direction)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_color_manual(values = c("blue", "red")) +
    labs(
      title = paste("港間の双方向ポートコールトレンド:", port_pair[1], "と", port_pair[2]),
      x = "月",
      y = "ポートコール数",
      color = "方向"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  message("双方向トレンドプロット完了")
  return(bidirectional_plot)
}

# ===================================
# 7. 視覚化の実行
# ===================================

# アウトプットディレクトリの作成
dir.create("output", showWarnings = FALSE)

# 視覚化の実行
message("視覚化を実行中...")

# 主要港のトレンド
trends_plots <- plot_top_port_trends(aggregated_data)

# 出発地と到着地の関係
flow_plot <- plot_port_flow(aggregated_data)

# 大陸間フロー
heatmap_plot <- plot_continent_heatmap(aggregated_data)

# 出発・到着比率
ratio_plot <- plot_origin_destination_ratio(aggregated_data)

# インタラクティブマップ
map_plot <- plot_interactive_map(processed_data, aggregated_data)

# 特定の港ペアの双方向トレンドの例
# 上位の港ペアを特定
top_port_pair <- aggregated_data$port_pairs %>%
  group_by(origin_port, destination_port) %>%
  summarise(total_flow = sum(total_port_calls, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_flow)) %>%
  head(1)

if (nrow(top_port_pair) > 0) {
  bidirectional_plot <- plot_bidirectional_trends(
    aggregated_data, 
    c(top_port_pair$origin_port[1], top_port_pair$destination_port[1])
  )
} else {
  bidirectional_plot <- NULL
  message("双方向トレンドのプロット用の港ペアが見つかりません。")
}

# ===================================
# 8. 結果の保存
# ===================================

# プロットの保存
save_plots <- function() {
  message("プロットを保存中...")
  
  # 出発地のトレンド
  ggsave("output/origin_trends.png", trends_plots$origin_plot, width = 10, height = 6)
  
  # 到着地のトレンド
  ggsave("output/destination_trends.png", trends_plots$destination_plot, width = 10, height = 6)
  
  # 出発地と到着地の関係
  ggsave("output/port_flow.png", flow_plot, width = 10, height = 6)
  
  # 大陸間フロー
  if (!is.null(heatmap_plot)) {
    ggsave("output/continent_heatmap.png", heatmap_plot, width = 10, height = 6)
  }
  
  # 出発・到着比率
  ggsave("output/ratio_trends.png", ratio_plot, width = 10, height = 6)
  
  # 双方向トレンド
  if (!is.null(bidirectional_plot)) {
    ggsave("output/bidirectional_trends.png", bidirectional_plot, width = 10, height = 6)
  }
  
  # インタラクティブマップ
  if (!is.null(map_plot)) {
    htmlwidgets::saveWidget(map_plot, "output/interactive_map.html")
  }
  
  message("すべてのプロットがoutputディレクトリに保存されました。")
}

# プロットを保存
save_plots()

# 主要な結果の表示
print(trends_plots$origin_plot)
print(trends_plots$destination_plot)
print(flow_plot)
if (!is.null(heatmap_plot)) print(heatmap_plot)
print(ratio_plot)
if (!is.null(bidirectional_plot)) print(bidirectional_plot)

# ===================================
# 9. 分析結果のサマリーレポート
# ===================================

# 分析結果のサマリーレポートの作成
create_summary_report <- function() {
  message("分析結果のサマリーレポートを作成中...")
  
  # レポートファイル名
  report_file <- "output/port_call_analysis_report.md"
  
  # レポートのヘッダー
  header <- "# IMF PortWatch データによる出発地・到着地ポートコール数の推移分析\n\n"
  header <- paste0(header, "分析日: ", format(Sys.Date(), "%Y年%m月%d日"), "\n\n")
  
  # データソースの説明
  data_source <- "## データソース\n\n"
  if (processed_data$is_sample) {
    data_source <- paste0(data_source, "この分析ではサンプルデータを使用しています。実際のIMF PortWatchデータへのアクセスが制限されているため、類似の傾向を持つ模擬データを生成しました。\n\n")
  } else {
    data_source <- paste0(data_source, "この分析ではIMF PortWatchが提供する海上輸送データを使用しています。データには世界中の1,666以上の港におけるポートコール（寄港）情報が含まれています。\n\n")
    data_source <- paste0(data_source, "データダウンロード元: https://portwatch.imf.org/datasets/75619cb86e5f4beeb7dab9629d861acf_0\n\n")
  }
  
  # 分析概要
  overview <- "## 分析概要\n\n"
  overview <- paste0(overview, "この分析では出発地と到着地のポートコール数の推移を様々な視点から可視化しました。主な分析内容は以下の通りです：\n\n")
  overview <- paste0(overview, "1. 上位の出発港・到着港のポートコール数の時系列推移\n")
  overview <- paste0(overview, "2. 出発港と到着港の間の貨物流動の関係（アルビオンプロット）\n")
  overview <- paste0(overview, "3. 大陸間の貨物流動（ヒートマップ）\n")
  overview <- paste0(overview, "4. 主要港における出発・到着の比率の変化\n")
  overview <- paste0(overview, "5. 主要港間の位置関係と流動量（インタラクティブマップ）\n")
  overview <- paste0(overview, "6. 特定の港ペア間の双方向流動トレンド\n\n")
  
  # 主要な結果
  results <- "## 主要な結果\n\n"
  
  # 上位出発港・到着港
  results <- paste0(results, "### 上位の出発港・到着港\n\n")
  
  top_origin_ports <- aggregated_data$origins %>%
    group_by(origin_port) %>%
    summarise(total_outgoing = sum(outgoing_calls, na.rm = TRUE)) %>%
    arrange(desc(total_outgoing)) %>%
    head(5)
  
  results <- paste0(results, "上位5つの出発港：\n\n")
  results <- paste0(results, "| 順位 | 港名 | 総出発ポートコール数 |\n")
  results <- paste0(results, "|------|------|----------------|\n")
  
  for (i in 1:nrow(top_origin_ports)) {
    results <- paste0(results, "| ", i, " | ", top_origin_ports$origin_port[i], 
                      " | ", format(top_origin_ports$total_outgoing[i], big.mark = ","), " |\n")
  }
  results <- paste0(results, "\n")
  
  top_destination_ports <- aggregated_data$destinations %>%
    group_by(destination_port) %>%
    summarise(total_incoming = sum(incoming_calls, na.rm = TRUE)) %>%
    arrange(desc(total_incoming)) %>%
    head(5)
  
  results <- paste0(results, "上位5つの到着港：\n\n")
  results <- paste0(results, "| 順位 | 港名 | 総到着ポートコール数 |\n")
  results <- paste0(results, "|------|------|----------------|\n")
  
  for (i in 1:nrow(top_destination_ports)) {
    results <- paste0(results, "| ", i, " | ", top_destination_ports$destination_port[i], 
                      " | ", format(top_destination_ports$total_incoming[i], big.mark = ","), " |\n")
  }
  results <- paste0(results, "\n")
  
  # 主要な港ペア
  results <- paste0(results, "### 主要な港ペア\n\n")
  
  top_port_pairs <- aggregated_data$port_pairs %>%
    group_by(origin_port, destination_port) %>%
    summarise(total_flow = sum(total_port_calls, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_flow)) %>%
    head(5)
  
  results <- paste0(results, "上位5つの港ペア（出発港→到着港）：\n\n")
  results <- paste0(results, "| 順位 | 出発港 | 到着港 | 総ポートコール数 |\n")
  results <- paste0(results, "|------|-------|-------|----------------|\n")
  
  for (i in 1:nrow(top_port_pairs)) {
    results <- paste0(results, "| ", i, " | ", top_port_pairs$origin_port[i], 
                      " | ", top_port_pairs$destination_port[i],
                      " | ", format(top_port_pairs$total_flow[i], big.mark = ","), " |\n")
  }
  results <- paste0(results, "\n")
  
  # 大陸間フロー（存在する場合）
  if (!is.null(aggregated_data$continents) && nrow(aggregated_data$continents) > 0) {
    results <- paste0(results, "### 大陸間フロー\n\n")
    
    continent_flows <- aggregated_data$continents %>%
      group_by(origin_continent, destination_continent) %>%
      summarise(total_flow = sum(total_calls, na.rm = TRUE), .groups = "drop") %>%
      filter(origin_continent != destination_continent) %>%
      arrange(desc(total_flow)) %>%
      head(5)
    
    if (nrow(continent_flows) > 0) {
      results <- paste0(results, "上位5つの大陸間フロー：\n\n")
      results <- paste0(results, "| 順位 | 出発大陸 | 到着大陸 | 総ポートコール数 |\n")
      results <- paste0(results, "|------|---------|---------|----------------|\n")
      
      for (i in 1:nrow(continent_flows)) {
        results <- paste0(results, "| ", i, " | ", continent_flows$origin_continent[i], 
                          " | ", continent_flows$destination_continent[i],
                          " | ", format(continent_flows$total_flow[i], big.mark = ","), " |\n")
      }
      results <- paste0(results, "\n")
    }
  }
  
  # 時系列トレンドの概要
  results <- paste0(results, "### 時系列トレンドの概要\n\n")
  
  # 全体的なトレンドを計算
  monthly_totals <- aggregated_data$port_pairs %>%
    group_by(month) %>%
    summarise(total_calls = sum(total_port_calls, na.rm = TRUE), .groups = "drop") %>%
    arrange(month)
  
  if (nrow(monthly_totals) > 1) {
    # 最初と最後の月を比較して成長率を計算
    first_month <- monthly_totals$total_calls[1]
    last_month <- monthly_totals$total_calls[nrow(monthly_totals)]
    growth_rate <- (last_month / first_month - 1) * 100
    
    results <- paste0(results, "分析期間中のポートコール数の変化：\n\n")
    results <- paste0(results, "- 期間開始: ", format(min(monthly_totals$month), "%Y年%m月"), " - ", 
                      format(first_month, big.mark = ","), " ポートコール\n")
    results <- paste0(results, "- 期間終了: ", format(max(monthly_totals$month), "%Y年%m月"), " - ", 
                      format(last_month, big.mark = ","), " ポートコール\n")
    results <- paste0(results, "- 成長率: ", sprintf("%.1f%%", growth_rate), "\n\n")
  }
  
  # 結論
  conclusion <- "## 結論\n\n"
  conclusion <- paste0(conclusion, "この分析では、世界の主要港間の海上輸送フローを様々な視点から可視化しました。これらの情報は以下のような用途に活用できます：\n\n")
  conclusion <- paste0(conclusion, "1. 貿易パターンの理解と予測\n")
  conclusion <- paste0(conclusion, "2. サプライチェーンの最適化\n")
  conclusion <- paste0(conclusion, "3. 新たなビジネス機会の発見\n")
  conclusion <- paste0(conclusion, "4. 港湾開発や投資計画の策定\n")
  conclusion <- paste0(conclusion, "5. 自然災害や紛争などの影響分析\n\n")
  
  # 生成された画像へのリンク
  images <- "## 分析結果の可視化\n\n"
  images <- paste0(images, "生成された視覚化は以下のディレクトリにあります：\n\n")
  images <- paste0(images, "```\n")
  images <- paste0(images, "output/\n")
  images <- paste0(images, "├── origin_trends.png        # 上位出発港のトレンド\n")
  images <- paste0(images, "├── destination_trends.png   # 上位到着港のトレンド\n")
  images <- paste0(images, "├── port_flow.png            # 港間のフロー（アルビオンプロット）\n")
  
  if (!is.null(heatmap_plot)) {
    images <- paste0(images, "├── continent_heatmap.png    # 大陸間フロー（ヒートマップ）\n")
  }
  
  images <- paste0(images, "├── ratio_trends.png         # 出発・到着比率の変化\n")
  
  if (!is.null(bidirectional_plot)) {
    images <- paste0(images, "├── bidirectional_trends.png # 特定港ペアの双方向トレンド\n")
  }
  
  if (!is.null(map_plot)) {
    images <- paste0(images, "└── interactive_map.html    # インタラクティブな地図表示\n")
  }
  
  images <- paste0(images, "```\n\n")
  
  # 全体をまとめてレポート作成
  full_report <- paste0(header, data_source, overview, results, conclusion, images)
  
  # ファイルに書き込み
  writeLines(full_report, report_file)
  
  message("分析レポートが作成されました: ", report_file)
  return(report_file)
}

# レポートの作成
report_file <- create_summary_report()
report_file
# 完了メッセージ
message("分析が完了しました！")

# IMF PortWatchデータのライセンスと使用条件についての注意事項
message("注: IMF PortWatchのデータを使用する場合は、適切に引用してください。")
message("推奨引用形式: \"Sources: UN Global Platform; IMF PortWatch (portwatch.imf.org)\"")