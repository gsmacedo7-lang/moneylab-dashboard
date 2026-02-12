# ==============================================================================
# LabAnalyst.R — Motor Analítico Híbrido (Kelly & KNN & Sharpe)
# Versão: 4.1 (Correção de Tickers e Normalização)
# ==============================================================================
options(encoding = "UTF-8", scipen = 999)

# 1. Carregamento e Dependências
pkgs <- c("quantmod", "dplyr", "lubridate", "jsonlite", "PerformanceAnalytics", "TTR", "zoo", "tidyr")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE)) install.packages(p, quiet = TRUE)
  library(p, character.only = TRUE)
}))

if(file.exists("config_auth.R")) source("config_auth.R")

log_analyst <- function(msg) cat(sprintf("[LabAnalyst | %s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

# ==============================================================================
# MÓDULO 1: SIMULAÇÃO KNN (CORRIGIDO)
# ==============================================================================

# Dicionário de Tickers (O Segredo da Correção)
# Mapeia o nome "humanizado" para o símbolo real do Yahoo Finance
MAPA_TICKERS <- list(
  "USD"  = "USDBRL=X",
  "BTC"  = "BTC-USD",
  "IBOV" = "^BVSP",
  "ETH"  = "ETH-USD"
)

# Carregamento de Dados para KNN (Genérico)
carregar_dados_sincronizados <- function(anos = 5) {
  inicio <- Sys.Date() - 365 * anos
  
  get_c <- function(tkr) {
    tryCatch({
      s <- getSymbols(tkr, src="yahoo", from=inicio, auto.assign=FALSE)
      return(na.locf(Cl(s)))
    }, error = function(e) return(NULL))
  }
  
  # Baixa os principais drivers de mercado
  usd  <- get_c(MAPA_TICKERS$USD)
  btc  <- get_c(MAPA_TICKERS$BTC)
  ibov <- get_c(MAPA_TICKERS$IBOV)
  
  # Merge seguro (inner join por data)
  dados <- merge(usd, btc, ibov)
  colnames(dados) <- c("USD", "BTC", "IBOV")
  return(na.omit(dados))
}

# Função de Cálculo de Distância (Euclidiana Normalizada)
calc_dist <- function(ref_usd, ref_btc, ref_ibov, h_usd, h_btc, h_ibov) {
  d_u <- (h_usd - ref_usd) / ref_usd
  d_b <- (h_btc - ref_btc) / ref_btc
  d_i <- (h_ibov - ref_ibov) / ref_ibov
  return(sqrt(d_u^2 + d_b^2 + d_i^2))
}

# A Função Principal que o LabInvest chama
simular_knn <- function(ativo_alvo, data_compra, data_venda, valor_investido = 1000, k = 50) {
  
  # 1. Normalização de Nomes (CORREÇÃO AQUI)
  ativo_alvo <- toupper(trimws(ativo_alvo))
  
  # Alias comuns
  if(ativo_alvo %in% c("DOLAR", "USDBRL")) ativo_alvo <- "USD"
  if(ativo_alvo %in% c("BITCOIN")) ativo_alvo <- "BTC"
  if(ativo_alvo %in% c("IBOVESPA", "INDICE")) ativo_alvo <- "IBOV"
  
  # Validação
  if (!ativo_alvo %in% c("USD", "BTC", "IBOV")) {
    return(paste("⚠️ Ativo não suportado pelo KNN. Tente: BTC, USD ou IBOV."))
  }
  
  # 2. Preparação de Datas
  d_compra <- as.Date(data_compra)
  d_venda  <- as.Date(data_venda)
  dias_h   <- as.numeric(d_venda - d_compra)
  
  if(dias_h <= 0) return("⚠️ Data futura deve ser maior que hoje.")
  
  # 3. Carrega Base Histórica
  base_xts <- carregar_dados_sincronizados()
  df_hist  <- data.frame(Data = index(base_xts), coredata(base_xts))
  
  # Localizar Referência (Hoje ou último dia útil)
  idx_ref <- which.min(abs(df_hist$Data - d_compra))
  ref     <- df_hist[idx_ref, ]
  
  # 4. Algoritmo KNN (Busca de Padrões)
  janela_exclusao <- interval(d_compra - days(10), d_compra + days(10))
  
  # Fator de Decaimento Temporal (Recência)
  max_date <- max(df_hist$Data)
  min_date <- min(df_hist$Data)
  total_days <- as.numeric(max_date - min_date)
  
  df_knn <- df_hist %>%
    filter(!(Data %within% janela_exclusao)) %>%
    mutate(
      # Distância vetorial considerando USD, BTC e IBOV
      Dist = calc_dist(ref$USD, ref$BTC, ref$IBOV, USD, BTC, IBOV),
      Recencia = as.numeric(Data - min_date) / total_days,
      # Score: Quanto menor a distância e mais recente, maior o peso
      Peso = (1 / (Dist + 0.000001)) * (1 + (Recencia^2)) 
    ) %>%
    arrange(desc(Peso)) %>%
    head(k)
  
  # 5. Simulação Financeira dos Vizinhos
  sim_resultados <- data.frame(Final = numeric(), Peso = numeric())
  
  for (i in 1:nrow(df_knn)) {
    # Projeta N dias para frente a partir do vizinho encontrado
    d_futura   <- df_knn$Data[i] + days(dias_h)
    idx_futuro <- which(df_hist$Data >= d_futura)[1]
    
    if (!is.na(idx_futuro) && idx_futuro <= nrow(df_hist)) {
      preco_entrada <- df_knn[[ativo_alvo]][i]
      preco_saida   <- df_hist[[ativo_alvo]][idx_futuro]
      
      retorno <- preco_saida / preco_entrada
      v_final <- valor_investido * retorno
      
      sim_resultados <- rbind(sim_resultados, data.frame(Final = v_final, Peso = df_knn$Peso[i]))
    }
  }
  
  if(nrow(sim_resultados) == 0) return("⚠️ Dados insuficientes para projetar esse prazo.")
  
  # 6. Estatísticas Finais
  media_pond <- weighted.mean(sim_resultados$Final, sim_resultados$Peso)
  lucro_abs  <- media_pond - valor_investido
  lucro_pct  <- (lucro_abs / valor_investido) * 100
  # Probabilidade de Win (Cenários positivos / Total de cenários ponderados)
  prob_win   <- sum(sim_resultados$Peso[sim_resultados$Final > valor_investido]) / sum(sim_resultados$Peso) * 100
  
  # 7. Formatação da Resposta
  res_txt <- paste0(
    "💡 <b>SIMULAÇÃO KNN (", ativo_alvo, ")</b>\n",
    "------------------------\n",
    "📆 <b>Prazo:</b> ", dias_h, " dias\n",
    "🧩 <b>Cenários Similares:</b> ", nrow(sim_resultados), "\n",
    "------------------------\n",
    "🎯 <b>Probabilidade de Lucro:</b> ", format(round(prob_win, 1), nsmall=1), "%\n",
    "💰 <b>Resultado Esperado:</b> R$ ", format(round(media_pond, 2), big.mark=".", decimal.mark=","), "\n",
    "📈 <b>Retorno Médio:</b> ", ifelse(lucro_pct >= 0, "+", ""), format(round(lucro_pct, 2), decimal.mark=","), "%"
  )
  
  return(res_txt)
}

# ==============================================================================
# MÓDULO 2: GESTÃO DE RISCO (KELLY)
# ==============================================================================

# Coleta de Preço (Fallback Yahoo se Binance falhar)
consultar_preco_atual <- function(ativo = "BTC") {
  tryCatch({
    if(ativo == "BTC") {
      res <- fromJSON("https://api.binance.com/api/v3/ticker/price?symbol=BTCBRL")
      return(as.numeric(res$price))
    } else {
      # Yahoo Fallback
      sym <- MAPA_TICKERS[[ativo]]
      if(is.null(sym)) sym <- paste0(ativo, ".SA")
      q <- getQuote(sym)
      return(q$Last)
    }
  }, error = function(e) return(0))
}

log_analyst("Motor 4.1 (Corrigido para IBOV/USD/BTC) Carregado.")