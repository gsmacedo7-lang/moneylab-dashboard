# ==============================================================================
# LABTRADER v7.0 - MOTOR QUÂNTICO DINÂMICO & RADAR DE DISPARO (8 PLANOS)
# Alinhado com a enciclopédia quantitativa de Granger & Hatanaka (1964)
# ==============================================================================

library(httr)
library(jsonlite)
library(RSQLite)
library(dplyr)

# Carregar credenciais se disponíveis
if (file.exists("config_auth.R")) {
  tryCatch(source("config_auth.R", encoding = "UTF-8"), error = function(e) NULL)
}

# --- PARÂMETROS DE VOLUME HARMONICUS ULTRA-DEEP (CALIBRAÇÃO GERAÇÃO 500) ---
VALOR_GUIANA_BRL     <- 150.0  # R$ 150 - Plano 1: Guiana Brasileira (PAXG <-> BTC 5h | Posse 123.3h | CV 7.0%)
VALOR_ESCUDO_BRL     <- 200.0  # R$ 200 - Plano 2: Escudo de Aquiles (BRL -> BTC 4h | Posse 176.0h | CV 26.5%)
VALOR_VIX_BRL        <- 200.0  # R$ 200 - Alias para Escudo de Aquiles
VALOR_PATRIA_BRL     <- 280.0  # R$ 280 - Plano 3: Pátria Volátil (Reserva Passiva Simple Earn 6,88% a.a.)
VALOR_TITA_BRL       <- 205.0  # R$ 205 (40 USDT) - Plano 4: Titã do Silício (NVDABUSDT 5h | Posse 51.0h | CV 0.6%)
VALOR_GRAVIDADE_BRL  <- 180.0  # R$ 180 - Plano 5: Gravidade Zero (BTC -> SOL -> BRL 1h | Posse 18.1h | Modelo A: Alta Velocidade)
VALOR_CHOQUE_BRL     <- 90.0   # R$ 90 (18 USDT) - Plano 6: Choque Energético (XLE Hedge 5h | Posse 475.5h)
VALOR_TITAS_BRL      <- 100.0  # R$ 100 - Plano 7: Duelo de Titãs (BTC -> ETH -> BRL 1h | Posse 310.0h | Modelo A: Alta Velocidade)
VALOR_SAGARANA_BRL   <- 220.0  # R$ 220 - Plano 8: Flecha de Sagarana (BRL <-> BTC 4h | Posse 176.0h | CV 5.9%)
VALOR_MIDAS_BRL      <- 50.0   # R$ 50  - Plano 9: Cofre de Midas (DCA 5 Dias Simple Earn Ouro)
VALOR_BNB_BRL        <- 90.0   # R$ 90  - Plano 10: Sentinela de Minas (BRL <-> BNB 3h | Posse 177.9h | CV 7.4%)
VALOR_TLT_BRL        <- 80.0   # R$ 80 (16 USDT) - Plano 11: Escudo de Washington (TLT T-Bonds 5h | Posse 331.9h)
VALOR_SQQQB_BRL      <- 90.0   # R$ 90 (18 USDT) - Plano 12: Sentinela Antifrágil (SQQQB 1h | Posse 4.9h)
VALOR_BRUCE_BRL      <- 300.0  # R$ 300 - Plano 13: Bruce Wayne (Desativado Temporariamente)
VALOR_WALLSTREET_BRL <- 100.0  # R$ 100 (20 USDT) - Plano 14: Sentinela Wall Street (SPYBUSDT 1h | Posse 174.7h)
VALOR_PERRY_BRL      <- 150.0  # R$ 150 - Plano 15: Adeus, Perry (Desativado Temporariamente)

obter_stats_macro_btc_30d <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    # Janela Macro Real de 30 dias (43.200 minutos)
    df <- dbGetQuery(con, "SELECT BTCBRL FROM Historico_binance WHERE BTCBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 43200;")
    if (nrow(df) >= 720) {
      # Reamostragem horária (a cada 60 pontos) para DSP e Z-score macro limpo sem ruído intradiário
      p_rec <- rev(df$BTCBRL)[seq(1, nrow(df), by = 60)]
      m_val <- mean(p_rec, na.rm = TRUE)
      s_val <- sd(p_rec, na.rm = TRUE)
      if (is.na(s_val) || s_val <= 0) s_val <- 2000.0
      
      dsp <- obter_dsp_ativo(p_rec)
      return(list(media = m_val, sd = s_val, serie = p_rec, dsp = dsp))
    }
  }, error = function(e) NULL)
  return(list(media = 415000.0, sd = 8000.0, serie = c(415000.0), dsp = list(theta = 0.0, d2Z = 0.0, snr = 5.0)))
}

obter_stats_near_10h <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT NEARBRL FROM Historico_binance WHERE NEARBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 600;")
    if (nrow(df) >= 15) {
      p_rec <- rev(df$NEARBRL)
      n_r <- length(p_rec)
      smooth_val <- mean(tail(p_rec, min(10, n_r)))
      detrend <- p_rec - smooth_val
      sd_val <- max(0.01, sd(tail(detrend, min(20, n_r))))
      return(list(media = smooth_val, sd = sd_val, serie = p_rec))
    }
  }, error = function(e) NULL)
  return(list(media = 9.65, sd = 0.15, serie = rep(9.65, 16)))
}

obter_stats_avax_1h <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT AVAXBRL FROM Historico_binance WHERE AVAXBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 300;")
    if (nrow(df) >= 15) {
      p_rec <- rev(df$AVAXBRL)
      n_r <- length(p_rec)
      smooth_val <- mean(tail(p_rec, min(10, n_r)))
      detrend <- p_rec - smooth_val
      sd_val <- max(0.05, sd(tail(detrend, min(20, n_r))))
      return(list(media = smooth_val, sd = sd_val, serie = p_rec))
    }
  }, error = function(e) NULL)
  return(list(media = 37.0, sd = 0.50, serie = rep(37.0, 16)))
}

obter_preco_binance <- function(symbol) {
  url <- paste0("https://api.binance.com/api/v3/ticker/price?symbol=", symbol)
  tryCatch({
    res <- GET(url, timeout(5))
    if (status_code(res) == 200) {
      return(as.numeric(content(res, "parsed")$price))
    }
  }, error = function(e) NULL)
  return(NULL)
}

obter_ultimo_vix <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT VIX_Index FROM Historico_macro WHERE VIX_Index IS NOT NULL ORDER BY Data DESC LIMIT 1;")
    if (nrow(df) > 0) return(as.numeric(df$VIX_Index[1]))
  }, error = function(e) NULL)
  return(16.09)
}

obter_ultimo_usd_comercial <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT USD_BRL FROM Historico_rapido WHERE USD_BRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 1;")
    if (nrow(df) > 0) return(as.numeric(df$USD_BRL[1]))
  }, error = function(e) NULL)
  return(5.0115)
}

obter_stats_btc_dual_scale <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT BTCBRL FROM Historico_binance WHERE BTCBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 1200;")
    if (nrow(df) >= 30) {
      p_rec <- rev(df$BTCBRL)
      # Reamostragem em candles de 5m para o modelo intradiário G500
      step_5m <- seq(1, length(p_rec), by = 5)
      p_5m <- p_rec[step_5m]
      n_5m <- length(p_5m)
      
      # Calibração G500: Period = 48 candles de 5m (240 min = 4 horas)
      p_fast <- tail(p_5m, min(48, n_5m))
      smooth_fast <- mean(p_fast)
      sd_fast <- max(50.0, sd(p_fast))
      dsp_fast <- obter_dsp_ativo(p_fast)
      
      # Escala Macro (288 candles de 5m = 24 horas)
      p_macro <- tail(p_5m, min(288, n_5m))
      smooth_macro <- mean(p_macro)
      sd_macro <- max(200.0, sd(p_macro))
      dsp_macro <- obter_dsp_ativo(p_macro)
      
      return(list(
        media_fast = smooth_fast,
        sd_fast = sd_fast,
        dsp_fast = dsp_fast,
        media_macro = smooth_macro,
        sd_macro = sd_macro,
        dsp_macro = dsp_macro,
        media = smooth_fast,
        sd = sd_fast,
        serie = p_5m
      ))
    }
  }, error = function(e) NULL)
  return(list(
    media_fast = 405000.0, sd_fast = 500.0, dsp_fast = list(theta = 0, d2Z = 0),
    media_macro = 405000.0, sd_macro = 2000.0, dsp_macro = list(theta = 0, d2Z = 0),
    media = 405000.0, sd = 1500.0, serie = rep(405000.0, 30)
  ))
}
obter_stats_btc_6h <- obter_stats_btc_dual_scale


obter_stats_guiana_72h <- function(p_gold = 4639.0) {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT BTCBRL, USDTBRL FROM Historico_binance WHERE BTCBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 600;")
    if (nrow(df) >= 30) {
      ratios <- rev((df$USDTBRL * p_gold) / df$BTCBRL)
      step_5m <- seq(1, length(ratios), by = 5)
      r_5m <- ratios[step_5m]
      # Calibração G500: Period = 60 candles de 5m (300 min = 5 horas)
      r_sub <- tail(r_5m, min(60, length(r_5m)))
      m_val <- mean(r_sub, na.rm = TRUE)
      s_val <- max(0.0001, sd(r_sub, na.rm = TRUE))
      dsp   <- obter_dsp_ativo(r_sub)
      return(list(media = m_val, sd = s_val, serie = r_sub, dsp = dsp))
    }
  }, error = function(e) NULL)
  return(list(media = 0.05920, sd = 0.00350, serie = rep(0.05920, 16), dsp = list(theta = 0, d2Z = 0)))
}

obter_stats_link_dual_scale <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT LINKBRL FROM Historico_binance WHERE LINKBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 800;")
    if (nrow(df) >= 30) {
      p_rec <- rev(df$LINKBRL)
      n_r <- length(p_rec)
      
      # Escala Rápida Intradiária (15 minutos)
      p_fast <- tail(p_rec, min(45, n_r))
      smooth_fast <- mean(tail(p_fast, min(15, length(p_fast))))
      detrend_fast <- p_fast - smooth_fast
      sd_fast <- max(0.05, sd(tail(detrend_fast, min(20, length(p_fast)))))
      dsp_fast <- obter_dsp_ativo(p_fast)
      
      # Escala Macro de Ressonância Fourier (10.0 horas / 600 minutos)
      macro_len <- min(600, n_r)
      p_macro <- tail(p_rec, macro_len)
      smooth_macro <- mean(p_macro)
      sd_macro <- max(0.30, sd(p_macro))
      dsp_macro <- obter_dsp_ativo(p_macro)
      
      return(list(
        media_fast = smooth_fast,
        sd_fast = sd_fast,
        dsp_fast = dsp_fast,
        media_macro = smooth_macro,
        sd_macro = sd_macro,
        dsp_macro = dsp_macro,
        media = smooth_fast, # retrocompatibilidade
        sd = sd_fast,
        serie = p_rec
      ))
    }
  }, error = function(e) NULL)
  return(list(
    media_fast = 59.50, sd_fast = 0.20, dsp_fast = list(theta = 0, d2Z = 0),
    media_macro = 59.50, sd_macro = 1.50, dsp_macro = list(theta = 0, d2Z = 0),
    media = 59.50, sd = 0.30, serie = rep(59.50, 30)
  ))
}
obter_stats_link_1h <- obter_stats_link_dual_scale

obter_stats_vecm_ativo <- function(col_ativo = "LINKBRL", col_ref = "BTCBRL", n_barras = 400) {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    query <- sprintf("SELECT %s, %s FROM Historico_binance WHERE %s IS NOT NULL AND %s IS NOT NULL ORDER BY Data_Hora DESC LIMIT %d;",
                     col_ativo, col_ref, col_ativo, col_ref, n_barras)
    df <- dbGetQuery(con, query)
    if (nrow(df) >= 60) {
      p_at <- as.numeric(df[[col_ativo]])
      p_rf <- as.numeric(df[[col_ref]])
      
      lp <- log(p_at)
      lrf <- log(p_rf)
      
      v_rf <- var(lrf)
      beta <- if (is.na(v_rf) || v_rf <= 0) 1.0 else cov(lp, lrf) / v_rf
      spread <- lp - beta * lrf
      
      sd_sp <- sd(spread)
      if (is.na(sd_sp) || sd_sp <= 0) sd_sp <- 0.01
      
      z_vecm <- (spread[1] - mean(spread)) / sd_sp
      ret_at <- diff(lp)
      sigma_langevin <- sd(ret_at)
      
      # 1. Escala temporal corrigida (180 barras = 3h | 360 barras = 6h reais):
      idx_3h <- min(180, length(p_at))
      ret_3h <- (p_at[1] / p_at[idx_3h]) - 1.0
      
      idx_6h <- min(360, length(p_at))
      ret_6h <- (p_at[1] / p_at[idx_6h]) - 1.0
      
      # 2. Choque sistêmico do Bitcoin (16 barras = 15m reais):
      idx_15m <- min(16, length(p_rf))
      ret_btc_15m <- (p_rf[1] / p_rf[idx_15m]) - 1.0
      
      # 3. Alerta de Vale 6h:
      # Aciona em queda real de 3h (< -2.0%), 6h (< -3.0%), choque de BTC (< -0.78%) ou quebra VECM (< -2.0)
      alerta_vale_6h <- (ret_3h < -0.020) || (ret_6h < -0.030) || (ret_btc_15m < -0.0078) || (z_vecm < -2.0)
      
      return(list(
        z_vecm = z_vecm,
        beta = beta,
        sigma_langevin = sigma_langevin,
        ret_3h = ret_3h,
        ret_6h = ret_6h,
        ret_btc_15m = ret_btc_15m,
        alerta_vale_6h = alerta_vale_6h
      ))
    }
  }, error = function(e) NULL)
  return(list(z_vecm = 0.0, beta = 1.0, sigma_langevin = 0.01, ret_3h = 0, ret_6h = 0, ret_btc_15m = 0, alerta_vale_6h = FALSE))
}


obter_stats_sol_btc_dual_scale <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT SOLBRL, BTCBRL FROM Historico_binance WHERE SOLBRL IS NOT NULL AND BTCBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 1200;")
    if (nrow(df) >= 30) {
      r_rec <- rev(df$SOLBRL / df$BTCBRL)
      step_5m <- seq(1, length(r_rec), by = 5)
      r_5m <- r_rec[step_5m]
      n_5m <- length(r_5m)
      
      # Calibração G500: Period = 12 candles de 5m (60 min = 1 hora)
      r_fast <- tail(r_5m, min(12, n_5m))
      smooth_fast <- mean(r_fast)
      sd_fast <- max(0.000005, sd(r_fast))
      dsp_fast <- obter_dsp_ativo(r_fast)
      
      # Escala Macro (48 candles de 5m = 4 horas)
      r_macro <- tail(r_5m, min(48, n_5m))
      smooth_macro <- mean(r_macro)
      sd_macro <- max(0.00002, sd(r_macro))
      dsp_macro <- obter_dsp_ativo(r_macro)
      
      return(list(
        media_fast = smooth_fast,
        sd_fast = sd_fast,
        dsp_fast = dsp_fast,
        media_macro = smooth_macro,
        sd_macro = sd_macro,
        dsp_macro = dsp_macro,
        media = smooth_fast,
        sd = sd_fast,
        serie = r_5m
      ))
    }
  }, error = function(e) NULL)
  return(list(
    media_fast = 0.00122, sd_fast = 0.00001, dsp_fast = list(theta = 0, d2Z = 0),
    media_macro = 0.00122, sd_macro = 0.00008, dsp_macro = list(theta = 0, d2Z = 0),
    media = 0.00122, sd = 0.00008, serie = rep(0.00122, 30)
  ))
}
obter_stats_sol_btc_72h <- obter_stats_sol_btc_dual_scale

obter_stats_sol_dual_scale <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT SOLBRL FROM Historico_binance WHERE SOLBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 600;")
    if (nrow(df) >= 30) {
      p_rec <- rev(df$SOLBRL)
      n_r <- length(p_rec)
      
      # Escala Rápida Intradiária (15 minutos)
      p_fast <- tail(p_rec, min(45, n_r))
      smooth_fast <- mean(tail(p_fast, min(15, length(p_fast))))
      detrend_fast <- p_fast - smooth_fast
      sd_fast <- max(0.20, sd(tail(detrend_fast, min(20, length(p_fast)))))
      dsp_fast <- obter_dsp_ativo(p_fast)
      
      # Escala Macro Fourier (4.0 horas / 240 minutos)
      macro_len <- min(240, n_r)
      p_macro <- tail(p_rec, macro_len)
      smooth_macro <- mean(p_macro)
      sd_macro <- max(1.00, sd(p_macro))
      dsp_macro <- obter_dsp_ativo(p_macro)
      
      return(list(
        media_fast = smooth_fast,
        sd_fast = sd_fast,
        dsp_fast = dsp_fast,
        media_macro = smooth_macro,
        sd_macro = sd_macro,
        dsp_macro = dsp_macro,
        media = smooth_fast, # retrocompatibilidade
        sd = sd_fast,
        serie = p_rec
      ))
    }
  }, error = function(e) NULL)
  return(list(
    media_fast = 550.0, sd_fast = 2.0, dsp_fast = list(theta = 0, d2Z = 0),
    media_macro = 550.0, sd_macro = 8.0, dsp_macro = list(theta = 0, d2Z = 0),
    media = 550.0, sd = 3.0, serie = rep(550.0, 30)
  ))
}
obter_stats_sol_15m <- obter_stats_sol_dual_scale

obter_stats_bnb_15m <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT BNBBRL FROM Historico_binance WHERE BNBBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 300;")
    if (nrow(df) >= 30) {
      p_rec <- rev(df$BNBBRL)
      step_5m <- seq(1, length(p_rec), by = 5)
      p_5m <- p_rec[step_5m]
      # Calibração G500: Period = 36 candles de 5m (180 min = 3 horas)
      p_sub <- tail(p_5m, min(36, length(p_5m)))
      return(list(media = mean(p_sub, na.rm = TRUE), sd = max(0.20, sd(p_sub, na.rm = TRUE)), serie = p_sub))
    }
  }, error = function(e) NULL)
  return(list(media = 3450.0, sd = 15.0, serie = rep(3450.0, 16)))
}

obter_stats_ada_30m <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT ADABRL FROM Historico_binance WHERE ADABRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 60;")
    if (nrow(df) >= 10) {
      return(list(media = mean(df$ADABRL[1:min(30, nrow(df))], na.rm = TRUE), sd = max(0.005, sd(df$ADABRL[1:min(30, nrow(df))], na.rm = TRUE)), serie = rev(df$ADABRL)))
    }
  }, error = function(e) NULL)
  return(list(media = 4.80, sd = 0.04, serie = rep(4.80, 16)))
}

obter_stats_near_24h <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT NEARBRL FROM Historico_binance WHERE NEARBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 60;")
    if (nrow(df) >= 10) {
      return(list(media = mean(df$NEARBRL[1:min(30, nrow(df))], na.rm = TRUE), sd = max(0.01, sd(df$NEARBRL[1:min(30, nrow(df))], na.rm = TRUE)), serie = rev(df$NEARBRL)))
    }
  }, error = function(e) NULL)
  return(list(media = 22.50, sd = 0.35, serie = rep(22.50, 16)))
}

obter_dsp_ativo <- function(vetor_precos) {
  if (is.null(vetor_precos) || length(vetor_precos) < 6) {
    return(list(theta = 0.0, T0 = 24.0, dZ = 0.0, d2Z = 0.0))
  }
  p <- as.numeric(tail(vetor_precos, 16))
  n <- length(p)
  smooth <- mean(p[max(1, n-3):n])
  detrend <- p - smooth
  I <- detrend[n]
  Q <- (detrend[n] - detrend[max(1, n-4)]) * 0.707
  theta <- atan2(Q, I + 1e-9)
  
  dZ <- (p[n] - p[n-1]) / (p[n-1] + 1e-9)
  d2Z <- if (n >= 3) ((p[n] - p[n-1]) - (p[n-1] - p[n-2])) / (p[n-1] + 1e-9) else 0.0
  
  prev_detrend <- detrend[n-1]
  prev_Q <- (detrend[n-1] - detrend[max(1, n-5)]) * 0.707
  ang_prev <- atan2(prev_Q, prev_detrend + 1e-9)
  ang_diff <- abs(theta - ang_prev)
  if (is.na(ang_diff) || ang_diff < 0.05) ang_diff <- 0.2618
  T0 <- max(6.0, min(60.0, (2 * pi) / ang_diff))
  
  return(list(theta = theta, T0 = T0, dZ = dZ, d2Z = d2Z))
}

obter_stats_eth_btc_24h <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT ETHBRL, BTCBRL FROM Historico_binance WHERE ETHBRL IS NOT NULL AND BTCBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 180;")
    if (nrow(df) >= 15) {
      ratios <- rev(df$ETHBRL / df$BTCBRL)
      step_5m <- seq(1, length(ratios), by = 5)
      r_5m <- ratios[step_5m]
      # Calibração G500: Period = 12 candles de 5m (60 min = 1 hora)
      r_sub <- tail(r_5m, min(12, length(r_5m)))
      m_val <- mean(r_sub, na.rm = TRUE)
      sd_val <- max(0.0001, sd(r_sub, na.rm = TRUE))
      return(list(media = m_val, sd = sd_val, serie = r_sub))
    }
  }, error = function(e) NULL)
  return(list(media = 0.03140, sd = 0.00030, serie = rep(0.03140, 16)))
}

obter_retorno_btc_5m <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT BTCBRL FROM Historico_binance WHERE BTCBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 6;")
    if (nrow(df) >= 6) {
      p_agora <- df$BTCBRL[1]
      p_passado <- df$BTCBRL[nrow(df)]
      return((p_agora / p_passado) - 1)
    }
  }, error = function(e) NULL)
  return(0.0)
}

obter_ultimo_harmonicus <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT * FROM Harmonicus_Metricas_Globais ORDER BY Data_Hora DESC LIMIT 1;")
    if (nrow(df) > 0) return(as.list(df[1, ]))
  }, error = function(e) NULL)
  return(list(Razao_Absorcao_PC1 = 0.3939, Entropia_Espectral = 1.75, Fluxo_Informacao_STE = 0.13, Energia_Wavelet_Morlet = 5.0))
}

obter_stats_wallstreet_vix_hedge <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df_sp <- dbGetQuery(con, "SELECT SP500_Pts FROM Historico_rapido WHERE SP500_Pts IS NOT NULL ORDER BY Data_Hora DESC LIMIT 288;")
    df_vix <- dbGetQuery(con, "SELECT VIX_Index FROM Historico_macro WHERE VIX_Index IS NOT NULL ORDER BY Data DESC LIMIT 24;")
    
    sp_vals <- if (nrow(df_sp) >= 10) rev(df_sp$SP500_Pts) else rep(5800, 20)
    vix_val <- if (nrow(df_vix) >= 1) as.numeric(df_vix$VIX_Index[1]) else 16.5
    
    m_sp <- mean(sp_vals, na.rm = TRUE)
    s_sp <- max(5.0, sd(sp_vals, na.rm = TRUE))
    
    return(list(
      sp500_media = m_sp,
      sp500_sd = s_sp,
      sp500_ultimo = tail(sp_vals, 1),
      vix_atual = vix_val
    ))
  }, error = function(e) NULL)
  return(list(sp500_media = 5800, sp500_sd = 30, sp500_ultimo = 5800, vix_atual = 16.5))
}

obter_stats_dollarus_quantum_peg <- function() {
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    df_u <- dbGetQuery(con, "SELECT USDTBRL FROM Historico_binance WHERE USDTBRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 1440;")
    df_r <- dbGetQuery(con, "SELECT USD_BRL FROM Historico_rapido WHERE USD_BRL IS NOT NULL ORDER BY Data_Hora DESC LIMIT 1440;")
    
    if (nrow(df_u) >= 20 && nrow(df_r) >= 20) {
      p_usdt <- df_u$USDTBRL[1]
      p_usd  <- df_r$USD_BRL[1]
      spread_peg <- p_usdt - p_usd
      
      # Veredito do oráculo intradiário se disponível
      oraculo_estresse <- FALSE
      if (exists("Daniel_tekel_dollar")) {
        veredito <- tryCatch(Daniel_tekel_dollar(), error = function(e) NULL)
        if (!is.null(veredito) && grepl("ESTRESSE|ALERTA|GARCH", veredito)) {
          oraculo_estresse <- TRUE
        }
      }
      
      return(list(
        usdt_atual = p_usdt,
        usd_oficial = p_usd,
        spread_peg = spread_peg,
        oraculo_estresse = oraculo_estresse
      ))
    }
  }, error = function(e) NULL)
  return(list(usdt_atual = 5.20, usd_oficial = 5.20, spread_peg = 0.0, oraculo_estresse = FALSE))
}

verificar_cooldown_veto <- function(estrategia_nome, timeout_seg = 300) {
  veto_file <- if (file.exists("vetos_recentes.rds")) "vetos_recentes.rds" else "/app/vetos_recentes.rds"
  if (!file.exists(veto_file)) return(FALSE)
  vetos <- tryCatch(readRDS(veto_file), error = function(e) list())
  if (!is.list(vetos) || is.null(vetos[[estrategia_nome]])) return(FALSE)
  
  registro <- vetos[[estrategia_nome]]
  ts_veto <- as.numeric(registro$timestamp)
  agora <- as.numeric(Sys.time())
  if (!is.na(ts_veto) && (agora - ts_veto) < timeout_seg) {
    return(TRUE)
  }
  return(FALSE)
}

# Subtrava de Coordenação Anti-Canibalização Flecha vs Escudo (mesmo candle de 5m)
verificar_compra_recente_btc <- function(estrategia_nome, timeout_seg = 300) {
  hist_file <- if (file.exists("ordens_executadas.rds")) "ordens_executadas.rds" else "/app/ordens_executadas.rds"
  if (!file.exists(hist_file)) return(FALSE)
  h <- tryCatch(readRDS(hist_file), error = function(e) NULL)
  if (!is.null(h) && nrow(h) > 0 && all(c("Estrategia", "Destino", "Data_Hora") %in% names(h))) {
    sub <- h[h$Estrategia == estrategia_nome & h$Destino == "BTC" & grepl("EXECUTADO_REAL", h$Status), ]
    if (nrow(sub) > 0) {
      last_t <- as.POSIXct(tail(sub$Data_Hora, 1))
      diff_s <- as.numeric(difftime(Sys.time(), last_t, units = "secs"))
      if (!is.na(diff_s) && diff_s < timeout_seg) return(TRUE)
    }
  }
  return(FALSE)
}

obter_lote_aberto_estrategia <- function(estrategia_nome, ativo) {
  hist_exec_file <- if (file.exists("ordens_executadas.rds")) "ordens_executadas.rds" else "/app/ordens_executadas.rds"
  if (!file.exists(hist_exec_file)) return(list(tem_lote = TRUE, minutos_posse = 999.0))
  
  hist_exec <- tryCatch(readRDS(hist_exec_file), error = function(e) NULL)
  if (is.null(hist_exec) || nrow(hist_exec) == 0 || !"Estrategia" %in% names(hist_exec)) {
    return(list(tem_lote = TRUE, minutos_posse = 999.0))
  }
  
  exec_est <- hist_exec[grepl("EXECUTADO_REAL", hist_exec$Status) & hist_exec$Estrategia == estrategia_nome, ]
  if (nrow(exec_est) == 0) {
    return(list(tem_lote = FALSE, minutos_posse = 0.0))
  }
  
  trades_ativo <- exec_est[exec_est$Destino == ativo | exec_est$Origem == ativo, ]
  if (nrow(trades_ativo) == 0) {
    return(list(tem_lote = FALSE, minutos_posse = 0.0))
  }
  
  ultimo_trade <- tail(trades_ativo, 1)
  if (ultimo_trade$Destino == ativo) {
    minutos_posse <- as.numeric(difftime(Sys.time(), as.POSIXct(ultimo_trade$Data_Hora), units = "mins"))
    return(list(
      tem_lote = TRUE,
      minutos_posse = minutos_posse,
      preco_compra = ultimo_trade$Preco_Exec,
      valor_compra = ultimo_trade$Valor_BRL,
      data_compra = ultimo_trade$Data_Hora
    ))
  }
  
  return(list(tem_lote = FALSE, minutos_posse = 0.0))
}

obter_vwap_ativo <- function(ativo_sym) {
  hist_exec_file <- "ordens_executadas.rds"
  if (file.exists(hist_exec_file)) {
    h_exec <- tryCatch(readRDS(hist_exec_file), error = function(e) NULL)
    if (!is.null(h_exec) && nrow(h_exec) > 0 && "Destino" %in% names(h_exec)) {
      compras <- h_exec[h_exec$Destino == ativo_sym & h_exec$Status == "EXECUTADO_REAL_BINANCE", ]
      if (nrow(compras) > 0) {
        tot_qtd <- sum(compras$Valor_BRL / compras$Preco_Exec, na.rm = TRUE)
        tot_val <- sum(compras$Valor_BRL, na.rm = TRUE)
        return(if (tot_qtd > 0) tot_val / tot_qtd else as.numeric(tail(compras$Preco_Exec, 1)))
      }
    }
  }
  return(0.0)
}

# ==============================================================================
# EXECUÇÃO DO RADAR COMPLETO (8 MOTORES QUANT)
# ==============================================================================
executar_radar_labtrader <- function() {
  agora_ts <- Sys.time()
  agora_str <- format(agora_ts, "%Y-%m-%d %H:%M:%S")
  
  if (file.exists("solicitacao.rds")) {
    cat(sprintf("[%s] ⏳ [LABTRADER] Solicitação anterior ainda em processamento pelo LabPolice.\n", agora_str))
    return(NULL)
  }
  
  db_path <- if (file.exists("MoneyBot_Local.db")) "MoneyBot_Local.db" else "/home/ubuntu/moneylab-dashboard/MoneyBot_Local.db"
  
  # 1. Cotações Binance em Tempo Real
  p_btc_brl   <- obter_preco_binance("BTCBRL")
  p_paxg_usdt <- obter_preco_binance("PAXGUSDT")
  p_usdt_brl  <- obter_preco_binance("USDTBRL")
  p_sol_brl   <- obter_preco_binance("SOLBRL")
  p_eth_brl   <- obter_preco_binance("ETHBRL")
  p_link_brl  <- obter_preco_binance("LINKBRL")
  p_bnb_brl   <- obter_preco_binance("BNBBRL")
  p_ada_brl   <- obter_preco_binance("ADABRL")
  p_near_brl  <- obter_preco_binance("NEARBRL")
  p_avax_brl  <- obter_preco_binance("AVAXBRL")
  
  if (is.null(p_btc_brl) || is.null(p_paxg_usdt) || is.null(p_usdt_brl)) {
    cat(sprintf("[%s] ⚠️ [LABTRADER] Cotações temporariamente indisponíveis na API.\n", agora_str))
    return(NULL)
  }
  
  p_paxg_brl   <- p_paxg_usdt * p_usdt_brl
  vix_atual    <- obter_ultimo_vix()
  usd_oficial  <- obter_ultimo_usd_comercial()
  harm_atual   <- obter_ultimo_harmonicus()
  pc1_atual    <- as.numeric(harm_atual$Razao_Absorcao_PC1)
  ent_atual    <- as.numeric(harm_atual$Entropia_Espectral)
  ste_atual    <- ifelse(!is.null(harm_atual$Fluxo_Informacao_STE) && !is.na(harm_atual$Fluxo_Informacao_STE), as.numeric(harm_atual$Fluxo_Informacao_STE), 0.0)
  w_energy     <- ifelse(!is.null(harm_atual$Energia_Wavelet_Morlet) && !is.na(harm_atual$Energia_Wavelet_Morlet), as.numeric(harm_atual$Energia_Wavelet_Morlet), 5.0)
  
  stats_guiana  <- obter_stats_guiana_72h(p_paxg_usdt)
  stats_link    <- obter_stats_link_1h()
  stats_sol_btc <- obter_stats_sol_btc_72h()
  stats_sol_15m <- obter_stats_sol_15m()
  stats_eth_btc <- obter_stats_eth_btc_24h()
  stats_bnb     <- obter_stats_bnb_15m()
  stats_ada     <- obter_stats_ada_30m()
  stats_near    <- obter_stats_near_24h()
  stats_near_10h <- obter_stats_near_10h()
  stats_avax    <- obter_stats_avax_1h()
  ret_btc_5m    <- obter_retorno_btc_5m()
  
  # Modulação Dinâmica de Lote Harmonicus Ultra-Deep
  fator_lote   <- ifelse(ste_atual >= 0.02 && pc1_atual <= 0.38 && w_energy < 50.0, 1.35, 
                        ifelse(w_energy >= 55.0, 0.50, 1.0))
  
  # Cálculo de Custódia e Peso de Bitcoin em Tempo Real
  df_w <- tryCatch(carteira(silent = TRUE), error = function(e) NULL)
  saldo_btc_brl   <- 0
  saldo_caixa_brl <- 0
  saldo_paxg_brl  <- 0
  saldo_sol_brl   <- 0
  saldo_eth_brl   <- 0
  saldo_link_brl  <- 0
  saldo_bnb_brl   <- 0
  saldo_ada_brl   <- 0
  saldo_near_brl  <- 0
  saldo_avax_brl  <- 0
  saldo_usdt_brl  <- 0
  saldo_usdt_usd  <- 0
  saldo_nvdab_usd <- 0
  saldo_spyb_usd  <- 0
  saldo_sqqqb_usd <- 0
  saldo_tlt_usd   <- 0
  
  if (!is.null(df_w) && is.data.frame(df_w) && nrow(df_w) > 0) {
    if (any(df_w$asset %in% c("BTC", "LDBTC"))) saldo_btc_brl   <- sum(df_w$free[df_w$asset %in% c("BTC", "LDBTC")], na.rm = TRUE) * p_btc_brl
    if ("BRL" %in% df_w$asset)  saldo_caixa_brl <- sum(df_w$free[df_w$asset == "BRL"], na.rm = TRUE)
    if (any(df_w$asset %in% c("PAXG", "LDPAXG"))) saldo_paxg_brl <- sum(df_w$free[df_w$asset %in% c("PAXG", "LDPAXG")], na.rm = TRUE) * p_paxg_brl
    if (any(df_w$asset %in% c("SOL", "LDSOL"))) saldo_sol_brl   <- sum(df_w$free[df_w$asset %in% c("SOL", "LDSOL")], na.rm = TRUE) * p_sol_brl
    if (any(df_w$asset %in% c("ETH", "LDETH"))) saldo_eth_brl   <- sum(df_w$free[df_w$asset %in% c("ETH", "LDETH")], na.rm = TRUE) * p_eth_brl
    if (any(df_w$asset %in% c("LINK", "LDLINK"))) saldo_link_brl  <- sum(df_w$free[df_w$asset %in% c("LINK", "LDLINK")], na.rm = TRUE) * p_link_brl
    if (any(df_w$asset %in% c("BNB", "LDBNB")) && !is.null(p_bnb_brl))   saldo_bnb_brl  <- sum(df_w$free[df_w$asset %in% c("BNB", "LDBNB")], na.rm = TRUE) * p_bnb_brl
    if (any(df_w$asset %in% c("ADA", "LDADA")) && !is.null(p_ada_brl))   saldo_ada_brl  <- sum(df_w$free[df_w$asset %in% c("ADA", "LDADA")], na.rm = TRUE) * p_ada_brl
    if (any(df_w$asset %in% c("NEAR", "LDNEAR")) && !is.null(p_near_brl)) saldo_near_brl <- sum(df_w$free[df_w$asset %in% c("NEAR", "LDNEAR")], na.rm = TRUE) * p_near_brl
    if (any(df_w$asset %in% c("AVAX", "LDAVAX")) && !is.null(p_avax_brl)) saldo_avax_brl <- sum(df_w$free[df_w$asset %in% c("AVAX", "LDAVAX")], na.rm = TRUE) * p_avax_brl
    if (any(df_w$asset %in% c("USDT", "LDUSDT"))) {
      saldo_usdt_usd  <- sum(df_w$free[df_w$asset %in% c("USDT", "LDUSDT")], na.rm = TRUE)
      saldo_usdt_brl  <- saldo_usdt_usd * p_usdt_brl
    }
    if (any(df_w$asset %in% c("NVDAB", "NVDA"))) saldo_nvdab_usd <- sum(df_w$free[df_w$asset %in% c("NVDAB", "NVDA")], na.rm = TRUE)
    if (any(df_w$asset %in% c("SPYB", "SP500"))) saldo_spyb_usd  <- sum(df_w$free[df_w$asset %in% c("SPYB", "SP500")], na.rm = TRUE)
    if (any(df_w$asset %in% c("SQQQB", "BITI"))) saldo_sqqqb_usd <- sum(df_w$free[df_w$asset %in% c("SQQQB", "BITI")], na.rm = TRUE)
    if (any(df_w$asset %in% c("TLT", "TLTB")))   saldo_tlt_usd   <- sum(df_w$free[df_w$asset %in% c("TLT", "TLTB")], na.rm = TRUE)
  }
  
  # 🛡️ Governança de Dólar: Preservação de Piso de 30% no Simple Earn e 70% Livre para Rotação US
  piso_30_usdt <- saldo_usdt_usd * 0.30
  usdt_livre_rotacao <- max(0.0, saldo_usdt_usd - piso_30_usdt)
  
  total_patrimonio_est <- saldo_caixa_brl + saldo_btc_brl + saldo_paxg_brl + saldo_sol_brl + saldo_eth_brl + saldo_link_brl + saldo_bnb_brl + saldo_ada_brl + saldo_near_brl + saldo_avax_brl + saldo_usdt_brl + (saldo_nvdab_usd + saldo_spyb_usd + saldo_sqqqb_usd + saldo_tlt_usd) * p_usdt_brl
  peso_btc <- ifelse(total_patrimonio_est > 0, saldo_btc_brl / total_patrimonio_est, 0.35)
  
  pedido <- NULL
  
  # ----------------------------------------------------------------------------
  # MOTOR 1: PLANO GUIANA BRASILEIRA (PAXG <-> BTC | Calibrado G500 - 60p / 5h)
  # Metricas G500: +4,35 reais/m | Posse: 123,3h | Platô CV: 7,0%
  # Desengasgo 1 & 2: Lote Ouro calibrado na folga do piso de R$ 500 e giro livre de BTC
  # ----------------------------------------------------------------------------
  ratio_guiana <- p_paxg_brl / p_btc_brl
  z_guiana     <- (ratio_guiana - stats_guiana$media) / stats_guiana$sd
  dsp_guiana   <- if (!is.null(stats_guiana$dsp)) stats_guiana$dsp else list(theta = 0, d2Z = 0)
  
  # Ponta A: Bitcoin eufórico / Ouro com desconto -> Vende BTC e compra PAXG
  can_sell_btc_guiana <- saldo_btc_brl >= 65.0 && saldo_paxg_brl < 800.0
  if (z_guiana <= -1.00 && dsp_guiana$d2Z >= -0.015 && can_sell_btc_guiana) {
    lote_g <- min(75.0 * fator_lote, max(60.0, saldo_btc_brl * 0.45))
    if (lote_g >= 60.0) {
      pedido <- list(
        estrategia = "PLANO_GUIANA_BRASILEIRA",
        origem = "BTC", destino = "PAXG",
        valor_brl = lote_g, lucro_esperado_pct = 0.40, timestamp = agora_ts
      )
    }
  } else if (z_guiana >= 0.95) {
    # Ponta B: Ouro valorizado / Bitcoin em dip -> Vende PAXG e compra BTC (preservando piso de Ouro em R$ 500)
    folga_ouro <- saldo_paxg_brl - 505.0
    if (folga_ouro >= 65.0) {
      lote_g <- min(90.0 * fator_lote, folga_ouro)
      if (lote_g >= 60.0) {
        pedido <- list(
          estrategia = "PLANO_GUIANA_BRASILEIRA",
          origem = "PAXG", destino = "BTC",
          valor_brl = lote_g,
          lucro_esperado_pct = 0.40, timestamp = agora_ts
        )
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 2: PLANO ESCUDO DE AQUILES (BRL -> BTC | Calibrado G500 - 48p / 4h)
  # Metricas G500: +1,74 reais/m | Posse: 176,0h | Platô CV: 26,5%
  # Coordenação Anti-Canibalização: Bloqueado se Flecha de Sagarana comprou BTC nos últimos 300s
  # ----------------------------------------------------------------------------
  if (is.null(pedido)) {
    bloqueio_canibalizacao_escudo <- verificar_compra_recente_btc("PLANO_FLECHA_DE_SAGARANA", 300)
    stats_btc_escudo <- obter_stats_btc_dual_scale()
    z_btc_escudo <- (p_btc_brl - stats_btc_escudo$media_fast) / stats_btc_escudo$sd_fast
    acc_btc_escudo <- stats_btc_escudo$dsp_fast$d2Z
    
    # Ponta A: Compra BTC em estresse real (VIX >= 21.0 ou Z <= -0.60 com d2Z >= 0.014)
    cond_compra_escudo <- (vix_atual >= 21.00 || z_btc_escudo <= -0.60) && (acc_btc_escudo >= 0.014) && peso_btc < 0.50 && saldo_caixa_brl >= 100.0 && !bloqueio_canibalizacao_escudo
    
    if (cond_compra_escudo) {
      pedido <- list(
        estrategia = "PLANO_ESCUDO_DE_AQUILES",
        origem = "BRL", destino = "BTC",
        valor_brl = min(VALOR_ESCUDO_BRL * fator_lote, saldo_caixa_brl * 0.40), 
        lucro_esperado_pct = 0.57, timestamp = agora_ts
      )
    } else if ((vix_atual < 18.50 || z_btc_escudo >= 1.20) && ret_btc_5m >= 0.0050 && saldo_btc_brl >= 50.0 && peso_btc > 0.18) {
      # Ponta B: Normalização do VIX ou Z >= 1.20 -> Realização para Caixa BRL
      pedido <- list(
        estrategia = "PLANO_ESCUDO_DE_AQUILES",
        origem = "BTC", destino = "BRL",
        valor_brl = min(VALOR_ESCUDO_BRL * 0.60 * fator_lote, saldo_btc_brl * 0.35),
        lucro_esperado_pct = 0.57, timestamp = agora_ts
      )
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 3: PLANO PÁTRIA VOLÁTIL (BRL <-> USDT | Gestão Passiva / Simple Earn Flexível)
  # Auditoria G500: Scalping intradiário de 5m desativado (spread/taxas inviabilizam giro rápido).
  # Capital 100% alocado defensivamente no Simple Earn Flexível (6,88% a.a.) e reserva cambial.
  # ----------------------------------------------------------------------------
  # Scalping intradiário inativo: pedido permanece NULL para este motor.
  
  # ----------------------------------------------------------------------------
  # MOTOR 4: PLANO TITÃ DO SILÍCIO (USDT <-> NVDAB | Calibrado G500 - 60p / 5h)
  # Metricas G500: +1,78 reais/m | Posse: 51,0h (~2,1 dias) | Platô CV: 0,6% (Ultra-Estável)
  # Binance Backed Equity Spot: NVDABUSDT | Lote 40 USDT (~R$ 205)
  # ----------------------------------------------------------------------------
  if (is.null(pedido)) {
    # 1. REALIZAÇÃO DE LUCRO: Venda NVDAB -> USDT sob Trava 6 (>= +0.40% e Z >= +0.84)
    if (saldo_nvdab_usd > 0.01) {
      p_nvda_live <- tryCatch(as.numeric(content(GET("https://api.binance.com/api/v3/ticker/price?symbol=NVDABUSDT"), "parsed")$price), error = function(e) NULL)
      pm_nvda <- obter_vwap_ativo("NVDAB")
      if (!is.null(p_nvda_live) && p_nvda_live > 0 && pm_nvda > 0) {
        ret_nvda <- (p_nvda_live / pm_nvda) - 1.0
        if (ret_nvda >= 0.0040) {
          val_venda_brl <- saldo_nvdab_usd * p_nvda_live * p_usdt_brl
          pedido <- list(
            estrategia = "PLANO_TITA_DO_SILICIO",
            origem = "NVDAB", destino = "USDT",
            valor_brl = val_venda_brl,
            lucro_esperado_pct = round(ret_nvda * 100, 2), timestamp = agora_ts
          )
        }
      }
    } else if (usdt_livre_rotacao >= 20.0) {
      # 2. ENTRADA EM DIP: Compra USDT -> NVDAB (Lote 40.00 USDT / ~R$ 205)
      nvda_serie <- tryCatch({
        con_nv <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con_nv))
        df_nv <- dbGetQuery(con_nv, "SELECT NVDABUSDT FROM Historico_binance WHERE NVDABUSDT IS NOT NULL ORDER BY Data_Hora DESC LIMIT 300;")
        if (nrow(df_nv) >= 30) {
          r_nv <- rev(df_nv$NVDABUSDT)
          r_nv[seq(1, length(r_nv), by = 5)]
        } else {
          rep(224.0, 16)
        }
      }, error = function(e) rep(224.0, 16))
      
      dsp_nvda <- obter_dsp_ativo(nvda_serie)
      m_nvda <- mean(nvda_serie, na.rm = TRUE)
      s_nvda <- sd(nvda_serie, na.rm = TRUE)
      if (is.na(s_nvda) || s_nvda <= 0) s_nvda <- 1.5
      z_nvda <- (tail(nvda_serie, 1) - m_nvda) / s_nvda
      
      # Calibração G500: Z <= -1.06 com aceleração d2Z >= 0.074
      if (z_nvda <= -1.06 && dsp_nvda$d2Z >= 0.074) {
        lote_usdt_nv <- min(40.0 * fator_lote, usdt_livre_rotacao)
        lote_brl_nv <- lote_usdt_nv * p_usdt_brl
        pedido <- list(
          estrategia = "PLANO_TITA_DO_SILICIO",
          origem = "USDT", destino = "NVDAB",
          valor_brl = lote_brl_nv,
          lucro_esperado_pct = 0.40, timestamp = agora_ts
        )
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 5: PLANO GRAVIDADE ZERO (BTC -> SOL -> BRL | Calibrado G500 - 12p / 1h)
  # Metricas G500: +3,43 reais/m (+309%) | Posse: 18,1h (-24,4%) | Platô Otimizado
  # Modelo A: Hub de Alta Velocidade - Giro dinâmico de BTC para SOL e realização para BRL
  # Teto Máximo de SOL de 180 reais
  # ----------------------------------------------------------------------------
  stats_sol_btc <- obter_stats_sol_btc_dual_scale()
  if (is.null(pedido) && !is.null(p_sol_brl) && !is.null(p_btc_brl) && pc1_atual < 0.75) {
    ratio_sol_btc <- p_sol_brl / p_btc_brl
    z_sol_btc     <- (ratio_sol_btc - stats_sol_btc$media_fast) / stats_sol_btc$sd_fast
    dsp_sol_btc   <- stats_sol_btc$dsp_fast
    acc_r         <- dsp_sol_btc$d2Z
    
    can_trade_btc <- saldo_btc_brl >= 30.0
    can_add_sol   <- saldo_sol_brl < 180.0
    
    # Calibração G500: Z <= -0.90 com aceleração d2Z >= 0.070
    if (z_sol_btc <= -0.90 && acc_r >= 0.070 && can_trade_btc && can_add_sol) {
      lote_g <- min(50.0, max(25.0, saldo_btc_brl * 0.45))
      pedido <- list(
        estrategia = "PLANO_GRAVIDADE_ZERO",
        origem = "BTC", destino = "SOL",
        valor_brl = lote_g,
        lucro_esperado_pct = 1.07, timestamp = agora_ts
      )
    } else if (saldo_sol_brl >= 25.0) {
      # Ponta B: Realização de topo de Solana para BRL sob Trava 6 (>= +1.07% e Z >= 0.99)
      lote_grav <- obter_lote_aberto_estrategia("PLANO_GRAVIDADE_ZERO", "SOL")
      if (lote_grav$tem_lote) {
        em_cooldown_grav <- verificar_cooldown_veto("PLANO_GRAVIDADE_ZERO", timeout_seg = 300)
        if (z_sol_btc >= 0.99 && lote_grav$minutos_posse >= 15.0 && !em_cooldown_grav) {
          pedido <- list(
            estrategia = "PLANO_GRAVIDADE_ZERO",
            origem = "SOL", destino = "BRL",
            valor_brl = min(saldo_sol_brl, VALOR_GRAVIDADE_BRL * fator_lote),
            lucro_esperado_pct = 1.07, timestamp = agora_ts
          )
        }
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 6: PLANO CHOQUE ENERGÉTICO (USDT <-> XLE | Calibrado G500 - 60p / 5h)
  # Metricas G500: +1,48 reais/m | Posse: 475,5h | Platô CV: 0,0% | OOS Ratio: 0,94x
  # Hedge de Petróleo/Energia | Lote R$ 90 (~18 USDT)
  # ----------------------------------------------------------------------------
  if (is.null(pedido) && usdt_livre_rotacao >= 18.0) {
    xle_serie <- tryCatch({
      con_xle <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con_xle))
      df_xle <- dbGetQuery(con_xle, "SELECT XLE_Energy FROM Historico_rapido WHERE XLE_Energy IS NOT NULL ORDER BY Data_Hora DESC LIMIT 300;")
      if (nrow(df_xle) >= 30) {
        r_xle <- rev(df_xle$XLE_Energy)
        r_xle[seq(1, length(r_xle), by = 5)]
      } else rep(88.0, 16)
    }, error = function(e) rep(88.0, 16))
    
    dsp_xle <- obter_dsp_ativo(xle_serie)
    m_xle <- mean(xle_serie, na.rm = TRUE)
    s_xle <- sd(xle_serie, na.rm = TRUE)
    if (is.na(s_xle) || s_xle <= 0) s_xle <- 1.5
    z_xle <- (tail(xle_serie, 1) - m_xle) / s_xle
    
    # Calibração G500: Z <= -1.17 com aceleração d2Z >= 0.036
    if (z_xle <= -1.17 && dsp_xle$d2Z >= 0.036) {
      lote_usdt_xle <- min(18.0 * fator_lote, usdt_livre_rotacao)
      pedido <- list(
        estrategia = "PLANO_CHOQUE_ENERGETICO",
        origem = "USDT", destino = "XLE",
        valor_brl = lote_usdt_xle * p_usdt_brl,
        lucro_esperado_pct = 0.43, timestamp = agora_ts
      )
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 7: PLANO DUELO DE TITÃS (BTC -> ETH -> BRL | Calibrado G500 - 12p / 1h)
  # Metricas G500: +1,30 reais/m (+61,4%) | Posse: 310,0h (-32,7%) | Platô CV: 3,3%
  # Modelo A: Hub de Alta Velocidade - Giro dinâmico de BTC para ETH e realização para BRL
  # Lote moderado base para desafogar ratio ETH/BTC
  # ----------------------------------------------------------------------------
  if (is.null(pedido) && !is.null(p_eth_brl) && !is.null(p_btc_brl) && pc1_atual < 0.75) {
    ratio_eth_btc <- p_eth_brl / p_btc_brl
    z_eth_btc     <- (ratio_eth_btc - stats_eth_btc$media) / stats_eth_btc$sd
    dsp_eth_btc   <- obter_dsp_ativo(stats_eth_btc$serie)
    
    # Calibração G500: Z <= -1.10 com d2Z >= 0.015
    cond_entrada_titas <- (z_eth_btc <= -1.10) && (dsp_eth_btc$d2Z >= 0.015)
    can_trade_btc_eth  <- saldo_btc_brl >= 30.0
    
    if (cond_entrada_titas && saldo_eth_brl < 260.0 && can_trade_btc_eth) {
      lote_t <- min(45.0, max(25.0, saldo_btc_brl * 0.40))
      pedido <- list(
        estrategia = "PLANO_DUELO_DE_TITAS",
        origem = "BTC", destino = "ETH",
        valor_brl = lote_t,
        lucro_esperado_pct = 0.53, timestamp = agora_ts
      )
    } else if (saldo_eth_brl >= 25.0) {
      # Ponta B: Realização para Caixa BRL sob Trava 6 (>= +0.53% e Z >= 1.10)
      em_cooldown_titas <- verificar_cooldown_veto("PLANO_DUELO_DE_TITAS", timeout_seg = 300)
      if (z_eth_btc >= 1.10 && !em_cooldown_titas) {
        pedido <- list(
          estrategia = "PLANO_DUELO_DE_TITAS",
          origem = "ETH", destino = "BRL",
          valor_brl = saldo_eth_brl,
          lucro_esperado_pct = 0.53, timestamp = agora_ts
        )
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 8: PLANO FLECHA DE SAGARANA (BRL <-> BTC | Calibrado G500 - 48p / 4h)
  # Metricas G500: +1,91 reais/m (+82,2%) | Posse: 176,0h (-22,4%) | Platô CV: 5,9%
  # Coordenação Anti-Canibalização: Bloqueado se Escudo de Aquiles comprou BTC nos últimos 300s
  # ----------------------------------------------------------------------------
  stats_btc <- obter_stats_btc_dual_scale()
  if (is.null(pedido) && !is.null(p_btc_brl) && ste_atual >= -0.02 && pc1_atual < 0.75 && w_energy < 55.0) {
    bloqueio_canibalizacao_flecha <- verificar_compra_recente_btc("PLANO_ESCUDO_DE_AQUILES", 300)
    z_btc <- (p_btc_brl - stats_btc$media_fast) / stats_btc$sd_fast
    dsp_btc <- stats_btc$dsp_fast
    acc_btc <- dsp_btc$d2Z
    
    # Calibração G500: Z <= -0.60 com aceleração d2Z >= 0.014
    cond_entrada_flecha <- (z_btc <= -0.60) && (acc_btc >= 0.014) && saldo_caixa_brl >= 45.0 && saldo_btc_brl < 220.0 && !bloqueio_canibalizacao_flecha
    
    if (cond_entrada_flecha) {
      lote_s <- min(VALOR_SAGARANA_BRL * fator_lote, saldo_caixa_brl * 0.45)
      pedido <- list(
        estrategia = "PLANO_FLECHA_DE_SAGARANA",
        origem = "BRL", destino = "BTC",
        valor_brl = lote_s,
        lucro_esperado_pct = 0.57, timestamp = agora_ts
      )
    } else if (saldo_btc_brl >= 30.0) {
      # Saída sob Trava 6 com Z >= 1.20
      em_cooldown_sag <- verificar_cooldown_veto("PLANO_FLECHA_DE_SAGARANA", timeout_seg = 300)
      if (z_btc >= 1.20 && !em_cooldown_sag) {
        valor_desova_btc <- min(saldo_btc_brl, VALOR_SAGARANA_BRL * fator_lote)
        if (valor_desova_btc >= 25.0) {
          pedido <- list(
            estrategia = "PLANO_FLECHA_DE_SAGARANA",
            origem = "BTC", destino = "BRL",
            valor_brl = valor_desova_btc,
            lucro_esperado_pct = 0.57, timestamp = agora_ts
          )
        }
      }
    }
  }
  
  
  # ----------------------------------------------------------------------------
  # MOTOR 9: PLANO COFRE DE MIDAS (BRL -> PAXG | Acumulação Passiva de Ouro)
  # [DESATIVADO PELA GOVERNANÇA: Ineficiência Estrutural Comprovada (-26,25 reais/mês)]
  # Simple Earn PAXG de 0,01% a.a. inviabiliza retorno passivo. Ouro alocado via Plano 1.
  # ----------------------------------------------------------------------------
  PLANO_COFRE_DE_MIDAS_ATIVO <- FALSE
  if (is.null(pedido) && PLANO_COFRE_DE_MIDAS_ATIVO) {
    hist_exec_file <- "ordens_executadas.rds"
    horas_desde_midas <- 999.0
    if (file.exists(hist_exec_file)) {
      hist_exec_tmp <- tryCatch(readRDS(hist_exec_file), error = function(e) NULL)
      if (!is.null(hist_exec_tmp) && nrow(hist_exec_tmp) > 0 && "Estrategia" %in% names(hist_exec_tmp)) {
        hist_midas <- hist_exec_tmp[hist_exec_tmp$Estrategia == "PLANO_COFRE_DE_MIDAS", ]
        if (nrow(hist_midas) > 0) {
          ultimo_midas_ts <- as.POSIXct(tail(hist_midas$Data_Hora, 1))
          horas_desde_midas <- as.numeric(difftime(Sys.time(), ultimo_midas_ts, units = "hours"))
        }
      }
    }
    
    # Condição DCA Ressonante: 5 dias completos (120h) + Caixa livre >= R$ 120 + Cooldown 1h
    # Ouro alocado entra diretamente no Simple Earn Flexível e eleva o Piso Ratchet Inviolável
    em_cooldown_midas <- verificar_cooldown_veto("PLANO_COFRE_DE_MIDAS", timeout_seg = 3600)
    if (!em_cooldown_midas && horas_desde_midas >= 120.0 && saldo_caixa_brl >= 120.0 && w_energy < 55.0) {
      pedido <- list(
        estrategia = "PLANO_COFRE_DE_MIDAS",
        origem = "BRL", destino = "PAXG",
        valor_brl = VALOR_MIDAS_BRL, # R$ 60,00 fixo (garante >= 10.50 USDT para filtro Binance)
        lucro_esperado_pct = 3.50,   # Rendimento passivo Simple Earn Flexible
        timestamp = agora_ts
      )
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 10: PLANO SENTINELA DE MINAS (BRL <-> BNB | Calibrado G500 - 36p / 3h)
  # Metricas G500: +1,86 reais/m (+59,8%) | Platô CV: 7,4% | Tax Fee Discount 25%
  # ----------------------------------------------------------------------------
  if (is.null(pedido) && !is.null(p_bnb_brl) && ste_atual >= -0.02 && pc1_atual < 0.75 && w_energy < 55.0) {
    z_bnb_36p <- (p_bnb_brl - stats_bnb$media) / stats_bnb$sd
    dsp_bnb   <- obter_dsp_ativo(stats_bnb$serie)
    
    # Calibração G500: Z <= -1.15 com aceleração d2Z >= 0.050
    if (z_bnb_36p <= -1.15 && dsp_bnb$d2Z >= 0.050 && saldo_caixa_brl >= 50.0 && saldo_bnb_brl < 180.0) {
      lote_b <- min(VALOR_BNB_BRL * fator_lote, max(45.0, saldo_caixa_brl * 0.45))
      pedido <- list(
        estrategia = "PLANO_SENTINELA_DE_MINAS",
        origem = "BRL", destino = "BNB",
        valor_brl = lote_b,
        lucro_esperado_pct = 0.86, timestamp = agora_ts
      )
    } else if (saldo_bnb_brl >= 20.0) {
      # Saída sob Trava 6 com Z >= 0.65
      em_cooldown_bnb <- verificar_cooldown_veto("PLANO_SENTINELA_DE_MINAS", timeout_seg = 300)
      if (z_bnb_36p >= 0.65 && !em_cooldown_bnb) {
        pedido <- list(
          estrategia = "PLANO_SENTINELA_DE_MINAS",
          origem = "BNB", destino = "BRL",
          valor_brl = saldo_bnb_brl,
          lucro_esperado_pct = 0.86, timestamp = agora_ts
        )
      }
    }
  }

  # ----------------------------------------------------------------------------
  # MOTOR 11: PLANO ESCUDO DE WASHINGTON (USDT <-> TLT | Calibrado G500 - 60p / 5h)
  # Metricas G500: +0,23 reais/m | Posse: 331,9h (-26,2%) | Platô CV: 0,0%
  # Renda Fixa Soberana Americana (T-Bonds 20Y) | Lote 16 USDT (~R$ 80)
  # ----------------------------------------------------------------------------
  if (is.null(pedido)) {
    if (saldo_tlt_usd > 0.01) {
      pm_tlt <- obter_vwap_ativo("TLT")
      p_tlt_live <- tryCatch(as.numeric(tail(getQuote("TLT")$Last, 1)), error = function(e) 95.0)
      if (pm_tlt > 0 && p_tlt_live > 0) {
        ret_tlt <- (p_tlt_live / pm_tlt) - 1.0
        if (ret_tlt >= 0.0052) {
          pedido <- list(
            estrategia = "PLANO_ESCUDO_DE_WASHINGTON",
            origem = "TLT", destino = "USDT",
            valor_brl = saldo_tlt_usd * p_tlt_live * p_usdt_brl,
            lucro_esperado_pct = round(ret_tlt * 100, 2), timestamp = agora_ts
          )
        }
      }
    } else if (usdt_livre_rotacao >= 16.0) {
      tlt_serie <- tryCatch({
        con_tlt <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con_tlt))
        df_tlt <- dbGetQuery(con_tlt, "SELECT TLT_Bond FROM Historico_rapido WHERE TLT_Bond IS NOT NULL ORDER BY Data_Hora DESC LIMIT 300;")
        if (nrow(df_tlt) >= 30) {
          r_tlt <- rev(df_tlt$TLT_Bond)
          r_tlt[seq(1, length(r_tlt), by = 5)]
        } else rep(95.0, 16)
      }, error = function(e) rep(95.0, 16))
      
      dsp_tlt <- obter_dsp_ativo(tlt_serie)
      m_tlt <- mean(tlt_serie, na.rm = TRUE)
      s_tlt <- sd(tlt_serie, na.rm = TRUE)
      if (is.na(s_tlt) || s_tlt <= 0) s_tlt <- 1.2
      z_tlt <- (tail(tlt_serie, 1) - m_tlt) / s_tlt
      
      # Calibração G500: Z <= -1.11 com aceleração d2Z >= 0.074
      if (z_tlt <= -1.11 && dsp_tlt$d2Z >= 0.074) {
        lote_usdt_tlt <- min(16.0 * fator_lote, usdt_livre_rotacao)
        pedido <- list(
          estrategia = "PLANO_ESCUDO_DE_WASHINGTON",
          origem = "USDT", destino = "TLT",
          valor_brl = lote_usdt_tlt * p_usdt_brl,
          lucro_esperado_pct = 0.52, timestamp = agora_ts
        )
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 12: PLANO SENTINELA ANTIFRÁGIL (USDT <-> SQQQB | Calibrado G500 - 12p / 1h)
  # Metricas G500: +0,14 reais/m | Posse: 4,9h (Scalp Rápido) | Platô CV: 0,0%
  # ProShares UltraPro Short QQQ Spot Binance | Lote 18 USDT (~R$ 90)
  # ----------------------------------------------------------------------------
  if (is.null(pedido)) {
    if (saldo_sqqqb_usd > 0.01) {
      p_sqqq_live <- tryCatch(as.numeric(content(GET("https://api.binance.com/api/v3/ticker/price?symbol=SQQQBUSDT"), "parsed")$price), error = function(e) NULL)
      pm_sqqq <- obter_vwap_ativo("SQQQB")
      if (!is.null(p_sqqq_live) && p_sqqq_live > 0 && pm_sqqq > 0) {
        ret_sqqq <- (p_sqqq_live / pm_sqqq) - 1.0
        if (ret_sqqq >= 0.0052) {
          val_venda_brl <- saldo_sqqqb_usd * p_sqqq_live * p_usdt_brl
          pedido <- list(
            estrategia = "PLANO_SENTINELA_ANTIFRAGIL",
            origem = "SQQQB", destino = "USDT",
            valor_brl = val_venda_brl,
            lucro_esperado_pct = round(ret_sqqq * 100, 2), timestamp = agora_ts
          )
        }
      }
    } else if (usdt_livre_rotacao >= 18.0) {
      # Calibração G500: Entrada com ret_btc_5m <= -0.0060 (queda rápida)
      if (!is.null(ret_btc_5m) && ret_btc_5m <= -0.0060) {
        lote_usdt_anti <- min(18.0 * fator_lote, usdt_livre_rotacao)
        pedido <- list(
          estrategia = "PLANO_SENTINELA_ANTIFRAGIL",
          origem = "USDT", destino = "SQQQB",
          valor_brl = lote_usdt_anti * p_usdt_brl,
          lucro_esperado_pct = 0.52, timestamp = agora_ts
        )
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # MOTOR 13: PLANO BRUCE WAYNE (Contingência de Crise Cripto / Tail-Risk Macro Hedge)
  # [DESATIVADO TEMPORARIAMENTE CONFORME DIRETRIZ DE GOVERNANÇA]
  # ----------------------------------------------------------------------------
  PLANO_BRUCE_WAYNE_ATIVO <- FALSE
  
  # ----------------------------------------------------------------------------
  # MOTOR 14: PLANO SENTINELA DE WALL STREET (SPYB / USDT - S&P 500 Trust)
  # Metricas G500: +0,25 reais/m (+54,3%) | Posse: 174,7h (-47,6%) | Platô CV: 0,0%
  # Binance Backed Equity Spot: SPYBUSDT | Lote 20 USDT (~R$ 100)
  # ----------------------------------------------------------------------------
  if (is.null(pedido)) {
    # 1. REALIZAÇÃO DE LUCRO: Venda SPYB -> USDT sob a Trava 6 (>= +0.48%)
    if (saldo_spyb_usd > 0.01) {
      p_spy_live <- tryCatch(as.numeric(content(GET("https://api.binance.com/api/v3/ticker/price?symbol=SPYBUSDT"), "parsed")$price), error = function(e) NULL)
      pm_spy <- obter_vwap_ativo("SPYB")
      if (!is.null(p_spy_live) && p_spy_live > 0 && pm_spy > 0) {
        ret_spy <- (p_spy_live / pm_spy) - 1.0
        if (ret_spy >= 0.0048) {
          val_venda_brl <- saldo_spyb_usd * p_spy_live * p_usdt_brl
          pedido <- list(
            estrategia = "PLANO_SENTINELA_WALLSTREET",
            origem = "SPYB", destino = "USDT",
            valor_brl = val_venda_brl,
            lucro_esperado_pct = round(ret_spy * 100, 2), timestamp = agora_ts
          )
        }
      }
    } else if (usdt_livre_rotacao >= 20.0) {
      # 2. ENTRADA EM DIP: Compra USDT -> SPYB (Lote 20.00 USDT / ~R$ 100)
      sp500_serie <- tryCatch({
        con_sp <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con_sp))
        df_sp <- dbGetQuery(con_sp, "SELECT SPYBUSDT FROM Historico_binance WHERE SPYBUSDT IS NOT NULL ORDER BY Data_Hora DESC LIMIT 60;")
        if (nrow(df_sp) >= 12) {
          r_sp <- rev(df_sp$SPYBUSDT)
          r_sp[seq(1, length(r_sp), by = 5)]
        } else rep(765.0, 12)
      }, error = function(e) rep(765.0, 12))
      
      dsp_sp500 <- obter_dsp_ativo(sp500_serie)
      m_sp <- mean(sp500_serie, na.rm = TRUE)
      s_sp <- sd(sp500_serie, na.rm = TRUE)
      if (is.na(s_sp) || s_sp <= 0) s_sp <- 2.0
      z_sp <- (tail(sp500_serie, 1) - m_sp) / s_sp
      
      # Calibração G500: Z <= -0.95 com d2Z >= 0.026
      if (z_sp <= -0.95 && dsp_sp500$d2Z >= 0.026) {
        lote_usdt_ws <- min(20.0 * fator_lote, usdt_livre_rotacao)
        pedido <- list(
          estrategia = "PLANO_SENTINELA_WALLSTREET",
          origem = "USDT", destino = "SPYB",
          valor_brl = lote_usdt_ws * p_usdt_brl,
          lucro_esperado_pct = 0.48, timestamp = agora_ts
        )
      }
    }
  }

  # ----------------------------------------------------------------------------
  # MOTOR 15: PLANO ADEUS, PERRY (Desova e Liquidação Cirúrgica de Ativos Legados)
  # [DESATIVADO TEMPORARIAMENTE CONFORME DIRETRIZ DE GOVERNANÇA]
  # ----------------------------------------------------------------------------
  PLANO_ADEUS_PERRY_ATIVO <- FALSE
  if (is.null(pedido) && PLANO_ADEUS_PERRY_ATIVO) {
    em_cooldown_perry <- verificar_cooldown_veto("PLANO_ADEUS_PERRY", timeout_seg = 300)
    if (!em_cooldown_perry) {
      saldo_legados <- list(
        LINK = saldo_link_brl,
        ADA  = saldo_ada_brl,
        NEAR = saldo_near_brl,
        AVAX = saldo_avax_brl
      )
      
      for (ast_leg in names(saldo_legados)) {
        if (!is.null(pedido)) break
        val_leg <- saldo_legados[[ast_leg]]
        if (!is.null(val_leg) && !is.na(val_leg) && val_leg >= 15.0) {
          # Consulta lote em aberto e VWAP
          pm_leg <- 0.0
          hist_exec_file <- "ordens_executadas.rds"
          if (file.exists(hist_exec_file)) {
            h_exec <- tryCatch(readRDS(hist_exec_file), error = function(e) NULL)
            if (!is.null(h_exec) && nrow(h_exec) > 0 && "Destino" %in% names(h_exec)) {
              compras_leg <- h_exec[h_exec$Destino == ast_leg & h_exec$Status == "EXECUTADO_REAL_BINANCE", ]
              if (nrow(compras_leg) > 0) {
                tot_qtd <- sum(compras_leg$Valor_BRL / compras_leg$Preco_Exec, na.rm = TRUE)
                tot_val <- sum(compras_leg$Valor_BRL, na.rm = TRUE)
                pm_leg <- if (tot_qtd > 0) tot_val / tot_qtd else as.numeric(tail(compras_leg$Preco_Exec, 1))
              }
            }
          }
          
          # Fallback auditado de preço de aquisição na Binance:
          if (pm_leg <= 0) {
            precos_aquisicao_legados <- list(ADA = 1.083, LINK = 59.00, NEAR = 22.50, AVAX = 135.0)
            if (!is.null(precos_aquisicao_legados[[ast_leg]])) pm_leg <- precos_aquisicao_legados[[ast_leg]]
          }
          
          p_now_leg <- tryCatch(as.numeric(content(GET(sprintf("https://api.binance.com/api/v3/ticker/price?symbol=%sBRL", ast_leg)), "parsed")$price), error = function(e) NULL)
          
          # Se o preço de mercado estiver em lucro ou >= VWAP * 1.0040 (Trava 6)
          lucro_atingido <- FALSE
          if (!is.null(p_now_leg) && p_now_leg > 0 && pm_leg > 0) {
            ret_leg <- (p_now_leg / pm_leg) - 1.0
            if (ret_leg >= 0.0040) {
              lucro_atingido <- TRUE
            }
          } else {
            # Se não há histórico de compra registrado, permite desova controlada
            lucro_atingido <- TRUE
          }
          
          if (lucro_atingido) {
            pedido <- list(
              estrategia = "PLANO_ADEUS_PERRY",
              origem = ast_leg, destino = "BRL",
              valor_brl = val_leg,
              lucro_esperado_pct = 0.50, timestamp = agora_ts
            )
          }
        }
      }
    }
  }
  
  # Log do Radar em labtrader_radar.log
  z_bnb_val  <- if (!is.null(p_bnb_brl)) (p_bnb_brl - stats_bnb$media) / stats_bnb$sd else 0.0
  z_ada_val  <- if (!is.null(p_ada_brl)) (p_ada_brl - stats_ada$media) / stats_ada$sd else 0.0
  z_near_val <- if (!is.null(p_near_brl)) (p_near_brl - stats_near$media) / stats_near$sd else 0.0
  z_avax_val <- if (!is.null(p_avax_brl)) (p_avax_brl - stats_avax$media) / stats_avax$sd else 0.0
  
  log_line <- sprintf("[%s] RADAR: Z_Guiana=%.2f | VIX=%.2f | SpreadPeg=%.4f | Z_Link=%.2f | Z_SOL=%.2f | Z_ETH=%.2f | Z_BNB=%.2f | Z_ADA=%.2f | Z_NEAR=%.2f | Z_AVAX=%.2f | RetBTC5m=%.2f%% | Disparo=%s\n",
                      agora_str, z_guiana, vix_atual, ifelse(!is.null(usd_oficial), p_usdt_brl - usd_oficial, 0),
                      (p_link_brl - stats_link$media) / stats_link$sd,
                      (p_sol_brl / p_btc_brl - stats_sol_btc$media) / stats_sol_btc$sd,
                      (p_eth_brl / p_btc_brl - stats_eth_btc$media) / stats_eth_btc$sd,
                      z_bnb_val, z_ada_val, z_near_val, z_avax_val,
                      ret_btc_5m * 100,
                      ifelse(!is.null(pedido), pedido$estrategia, "NENHUM"))
  cat(log_line, file = "labtrader_radar.log", append = TRUE)
  
  # Envio para o Gatekeeper se houver disparo
  if (!is.null(pedido)) {
    saveRDS(pedido, "solicitacao.rds")
    cat(sprintf("[%s] 🎯 [DISPARO LABTRADER] Ordem gerada: %s (%s -> %s | R$ %.2f). Enviando ao LabPolice...\n",
                agora_str, pedido$estrategia, pedido$origem, pedido$destino, pedido$valor_brl))
  }
  
  return(pedido)
}