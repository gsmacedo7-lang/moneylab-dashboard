# ==============================================================================
# ARQUIVO: config_auth.R
# DESCRIÇÃO: Central de Credenciais e Tokens (SSOT - Single Source of Truth)
# STATUS: CONFIDENCIAL (Adicionar ao .gitignore)
# ==============================================================================


# --- 1. COMUNICAÇÃO (TELEGRAM) ---
# Usado em: LabInvest.R, LabFariaLimer.R, LabPolice.R

TG_USERID <- "7805501689"
TG_USERLAI <- "1544343665"
TG_USERTORI <- "8519768227"

# Bot LabInvest: Informações Gerais / Auditoria (Para Contador e Advogada)
TG_INVEST_TOKEN  <- "8396362645:AAF7a0AnNuePoTh4jopHGTVkVMzSuJpveqE" #Token LabInvest Bot
TG_INVEST_CHATID <- "-1003513530275" # ID do Grupo Geral

# Bot LabTrade: Pessoal e Execução (Só para você)
TG_TRADE_TOKEN   <- "8436675987:AAH1c4ICdJGYDUiF-PmBh_s5NpLEsp6BOqk" 
TG_TRADE_CHATID  <- "7805501689" # ID do seu Chat Privado

# --- 2. E-MAIL & NOTIFICAÇÕES (GMAIL SMTP) ---
# Usado em: LabFariaLimer.R, LabInvest.R
MAIL_USER     <- "g.s.macedo7@gmail.com"  # Antigo: EMAIL_REMETENTE / GMAIL_USER
MAIL_DEST     <- "g.s.macedo7@gmail.com"  # Antigo: EMAIL_DESTINO (Quem recebe o alerta)
MAIL_PASSWORD <- "kfar njcv gxzt ckve"  # Antigo: SENHA_APP_GMAIL (Senha de 16 dígitos)

# --- 3. BANCO DE DADOS (GOOGLE SHEETS) ---
# Usado em: LabFariaLimer.R
SHEET_ID_DB       <- "1UTOknqiNgTrIryUOKrHUl-PEm2s8pLTz6misilyo95Y"  # Antigo: ID_PLANILHA_DB (Micro e Macro dados)
SHEET_ID_PERSONAL <- "1lOgnOU4f05DM7_z-HdPlszJz3bRWxa7Tf1bkBEl8t0Y"  # Antigo: ID_PLANILHA_PESSOAL (Gestão Financeira)

# --- 4. INTELIGÊNCIA ARTIFICIAL (GEMINI) ---
# Usado em: LabInvest.R
GEMINI_TRADE_KEY <- "xxx"
GEMINI_INVEST_KEY <- "AIzaSyDs0iLwryBSaM9wFX1C0S3vwiW8PIzvzbk"  # API Key do Google AI Studio

# --- 5. TRADING (BINANCE) ---
# Usado em: LabTrader.R, LabPolice.R
# (Preencher quando a conta for aprovada)
BINANCE_KEY    <- "8P2tIrXTUjG7NXwAHSWgyko3bhxrZWw6DRebSgQEmGALmuokDbSxVYAZNOaqrK4m"
BINANCE_SECRET <- "0bKyq261rmEcOSBzLtIzHW2RA4FrN0tLoVLlpkX5fSIHZd9Kht5Q54UHs0DDdZia"

# --- 6. Dados para o Deploy via Git --- 
GIT_USER_EMAIL <- "g.s.macedo7@gmail.com"
GIT_USER_NAME  <- "Guilherme Santos"
PATH_PROJETO <- "C:/Users/Guilherme/OneDrive/Área de Trabalho/Doutorado/Health/MoneyLab"

message("[System] config_auth.R: Todas as credenciais foram carregadas.")