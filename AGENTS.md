# 🛡️ Diretrizes e Matriz de Governança Unificada do Ecossistema MoneyLab (LabInvest)

Este documento é a **Regra Soberana Sempre Ativa (Always-On Rule)** do repositório. O assistente DEVE aplicar automaticamente os protocolos e roteamentos abaixo para qualquer tarefa, sem exigir que o usuário invoque comandos ou skills manualmente.

---

## 🧭 1. Roteamento Automático de Contexto por Atividade (Matriz Lab*)

Sempre que o usuário solicitar uma tarefa ou o assistente for interagir com o sistema, o assistente DEVE incorporar compulsoriamente as regras da respectiva especialidade:

| Se a Atividade / Solicitação Envolver... | Módulo / Skill Vinculada | Protocolo Obrigatório de Execução |
| :--- | :--- | :--- |
| **Simulações, Cenários, Backtests, Análise de Risco ou Modelos PnL** | labanalyst-quantitative-engine e labanalyst-library | • **REGRA DE OURO: SIMULAÇÕES EXCLUSIVAMENTE INTRADIÁRIAS EM CANDLES DE 5 MINUTOS:** O ecossistema MoneyLab opera estrita e exclusivamente com trading intradiário de alta frequência baseado em **candles contínuos de 5 minutos** (reamostrados de 60s/1m). É TERMINANTEMENTE PROIBIDO rodar simulações com candles diários agregados ou projetar lucros decenais estáticos toscos.<br>• **Métricas Oficiais Obrigatórias:** Todo resultado deve ser apresentado em: (1) **Lucro Médio Mensal** (reais/mês e % do patrimônio real dinâmico), (2) **Mediana Mensal** (reais/mês e %), (3) **Trades por Mês**, (4) **Tempo Médio de Posse** (em horas/minutos), (5) **Taxa de Acerto (%)** e (6) **Quantidade de Meses Analisados**.<br>• **Base Oficial:** As 12.800+ horas reais do MoneyBot_Local.db (17,5 meses contínuos) com patrimônio real dinâmico.<br>• **Auditoria Anti-Trapaça & Ceticismo Científico:** Descarte obrigatório de qualquer modelo que alegue risco zero, Drawdown de 0 reais, "nunca fura o piso" ou Win Rate de 100% sem justificativa de arbitragem estrita. Toda simulação exige no mínimo 50 a 100 iterações estocásticas (Block Bootstrap / Monte Carlo) com ruído de microestrutura e slippage. |
| **Consulta de Cotações, Variação de Preços, Ingestão ou Base de Dados** | labfarialimer-market-watcher | • Consultar Historico_binance (60s), Historico_rapido (300s) e Historico_macro (1h).<br>• Sensor Adaptativo Micro vs Macro ({\sigma}$): ajustar cadência (60s micro, 300s híbrido, 3600s macro).<br>• Tagging Obrigatório: Is_Backfill = 1 e origem.<br>• **LEI DO DADO EMPÍRICO:** NUNCA usar adjetivos ('subiu', 'caiu') sem extrair e apresentar os candles reais (, P_t, High, Low, \Delta\%$). |
| **Custódia, Saldo, Carteira, Valuation, Gatekeeper ou Auditoria de Ordens** | labpolice-custody-auditor | • **REGRA SSOT ANTI-DUPLICAÇÃO:** A conta Spot (/api/v3/account) já lista Simple Earn como LD* (LDUSDT, LDPAXG, LDLINK). NUNCA somar com /sapi/v1/simple-earn sem deduplicação.<br>• **LEITURA DINÂMICA DE APORTES E P2P:** A apuração de capital injetado e de patrimônio consolidado NUNCA deve ser travada em um valor fixo (como ~2.2k). A baseline histórica conhecida é de 2.230,00 reais em espécie (2.030,00 reais Fiat PIX/TED + 200,00 reais via P2P/C2C em 20/01/2026), mas novos aportes aumentam essa base dinamicamente. Requisições a `/sapi/v1/c2c/orderMatch/listUserOrderHistory` DEVEM compulsoriamente usar janelas de no máximo 30 dias com `startTimestamp` e `endTimestamp` explícitos para nunca omitir P2P.<br>• **VALUATION PATRIMONIAL DINÂMICO:** O patrimônio consolidado é **estritamente dinâmico e apurado em tempo real via API** somando todas as carteiras (Spot deduplicado de LD*, Simple Earn totalAmount e Funding) sem limites ou faixas artificiais fixas. À medida que novos aportes entram ou os lucros acumulam, o valor é recalculado transparentemente pela realidade da carteira.<br>• Aplicar as Travas do Gatekeeper: Breakeven Lock FIFO >= +0,40%, Quarentena Bruce Wayne de 12h, Banda Ratchet -1,5% a -4,5% e Teto Global de 50% em Criptos/Altcoins.<br>• Precificação de Ouro: {PAXG/BRL} = Preco_{PAXG/USDT} \times Preco_{USDT/BRL}$. |
| **Relatórios Patrimoniais por E-mail, Resumos Mensais ou Newsletters** | labnews | • Formatar e-mails otimizados para leitura do Gemini Spark (tags semânticas no topo).<br>• Incluir Mapa de Calor semanal de PnL por dia da semana.<br>• Enviar via Gmail SMTP (smtp.gmail.com:465) com as credenciais oficiais de config_auth.R.<br>• Cumprir a regra SSOT de custódia deduplicada com apuração dinâmica e em tempo real da carteira. |
| **Deploy em Produção, Containers Docker, Servidor Cloud ou Telegram** | labinvest-bot-orchestrator | • Validar sintaxe R/Python antes de enviar para produção.<br>• Sincronizar simultaneamente o workspace local e o container remoto moneybot_core (/app/).<br>• Respeitar o pipeline de mensageria assíncrona do Telegram e a integridade de startLab.R e LabDeploy.R.<br>• **PADRÃO CONCISO DE ALERTAS TELEGRAM (Anti-Sobrecarga de Leitura):** Todas as notificações de ordens, vetos e simulações no Telegram devem ser estritamente concisas, diretas e informativas, sem textos prolixos ou burocráticos. Moldes padronizados: `⛔ [ORDEM VETADA]`, `🟢 [ORDEM EXECUTADA]` (com Lucro Projetado e Lucro Obtido em % e em valor), `🧪 [SIMULAÇÃO]` (Status: TESTE) e `⚠️ [FALHA NA EXECUÇÃO]`. |

---

## 🚫 2. Padrão Tipográfico Mandatório (Anti-Corrupção de Markdown)
* **Proibição Estrita:** NUNCA use o caractere de cifrão literal solto ou a sigla R$ no meio de textos ou tabelas (o renderizador Markdown interpreta como LaTeX KaTeX, quebrando o layout).
* **Padrão Obrigatório:** Escreva sempre a palavra por extenso **'reais'** após o número (ex: 2.162,24 reais, 50,00 reais) ou use a sigla **BRL**.

---

## 🔒 3. Governança do Gatekeeper e Blindagem do Bruce Wayne
* **Trava 6 (Breakeven Lock FIFO):** Nenhuma estratégia convencional pode vender abaixo de +0,40% líquido sobre o preço de aquisição do lote aberto.
* **Subtrava 2.2 (Teto Global de 80% em Criptos e Altcoins):** A exposição combinada em criptoativos voláteis (`BTC`, `ETH`, `SOL`, `LINK`, `BNB`, `ADA`, `NEAR`, `AVAX`, `DOGE`) não pode ultrapassar **80% do patrimônio total consolidado**. Ativos não-cripto (`BRL` Caixa, `USDT` Dólar/FX e `PAXG` Ouro Físico) ficam estritamente de fora desse teto. Se a exposição em criptos/altcoins exceder 80%, **qualquer nova compra de cripto é terminantemente vetada**; vendas e realizações de lucro para BRL, USDT ou PAXG permanecem 100% livres para desestocagem.
* **Quarentena Bruce Wayne (Subtrava 2.0):** Se o Bruce Wayne desovar um ativo em crise, o Gatekeeper decreta **12 horas de veto total de recompras daquele ativo específico** e congelamento de compras em Reais (`origem == 'BRL'`) para preservar o caixa líquido.
* **Banda Ratchet Anti-Venda de Fundo:** O Bruce Wayne só pode estancar perdas se o prejuízo calculado via Preço Médio Ponderado (VWAP) estiver entre -1,5% e -4,5% e houver estresse macro comprovado (janela de 30 dias com VIX >= 24 ou PC1 >= 0,40). Se a perda já for maior que -5,0%, a venda é vetada para evitar torrar dinheiro no fundo do poço.

---

## 🔬 4. Princípio do Ceticismo Científico e Auditoria Anti-Trapaça (A Falácia dos Extremos)
* **Banimento de Absolutos ("Nunca", "Garantido", "0%" e "100%"):** Em finanças quantitativas, métricas de 0% ou 100% (Win Rate de 100%, Drawdown de 0 reais, "0 horas de rompimento de piso") NÃO atestam perfeição — **expõem a falha metodológica, a cegueira de regime ou o viés do modelo**.
* **Proibição Estrita de Modelos com Métricas Pobres / Benchmark Mínimo Exigido:** É TERMINANTEMENTE PROIBIDO aprovar, homologar, sugerir ou recomendar modelos estatísticos, preditivos ou operacionais que apresentem Precision, Recall, F1-Score ou taxa de falsos alarmes abaixo de um benchmark sólido para o cenário testado. Se um modelo preditivo apresenta F1-Score ou Precision medíocres (ex: < 50%, ou resultados ruidosos de 5% a 10%), ele DEVE ser expressamente rotulado como **REJEITADO / INVIÁVEL PARA PRODUÇÃO**, declarando abertamente sua incapacidade de separar sinal de ruído, jamais propondo sua adoção para o usuário.
* **O Exemplo Canônico do "Bitcoin Nunca Mais é Drenado":** Uma simulação determinística estática concluiu falsamente que uma trava de software garantia "0,0 horas de violação do piso de 180 reais". Ao submeter o modelo a **100 simulações estocásticas com Block Bootstrap, ruído de microestrutura e slippage**, revelou-se que em **52% dos cenários o saldo fura o piso unicamente pela desvalorização de mercado do próprio Bitcoin**, passando **45,6% do tempo abaixo do piso**.
* **Generalização para Todas as Trapaças Quantitativas:**
  1. **Confundir Trava de Código com Imunidade de Mercado:** Travas de software impedem a *drenagem ativa pelo robô*, mas JAMAIS anulam o risco de mercado (*beta* intrínseco do ativo).
  2. **Viés de Sobrevivência / Descarte de MTM:** É proibido medir risco apenas em operações fechadas com lucro sob a Trava 6; o Drawdown Mark-to-Market flutuante deve ser calculado a cada tick/candle intradiário.
  3. **Determinismo Monotrilho:** É terminantemente proibido validar modelos com uma única série determinística sem ruído. Exige-se no mínimo **50 a 100 iterações estocásticas com distribuição completa de percentis (Min, P10, P25, Mediana, Média, P75, P90, Max)**.

---

## 🏛️ 6. Regras Soberanas de Dados e Execução de Mercado

1. **Negociação Nativa de Ações e ETFs Americanos na Binance:**
   * A Binance lista e negocia nativamente mais de 80 ações e ETFs americanos no mercado Spot cotados contra USDT sob a classe *Backed Equities* (identificados com o sufixo `B`, ex: `NVDABUSDT`, `TSLABUSDT`, `SPYBUSDT`, `QQQBUSDT`, `AAPLBUSDT`, `MSFTBUSDT`, `SQQQBUSDT`, `SOXSBUSDT`) e contratos perpétuos em Futuros (`TRADIFI_PERPETUAL`). **NUNCA mais cometa o erro de alegar que a Binance não negocia ações e ETFs americanos.**
2. **Proibição Estrita de Dados Sintéticos (Lei do Dado Empírico Real):**
   * O usuário **raramente usa dados sintéticos para alguma coisa**, exceto quando expressamente solicitado. O assistente DEVE utilizar rigorosamente e exclusivamente séries históricas e ticks empíricos reais coletados diretamente das APIs oficiais (Binance 60s/1m, TradFi 300s, Macro 1h). É terminantemente vetado mascarar ou substituir dados reais por preenchimentos sintéticos artificiais.
3. **Ciclo Compulsório de Atualização do Harmonicus SX & Planos:**
   * A cada plano de investimento concebido, recalibrado, modificado ou descomissionado, o assistente DEVE compulsoriamente:
     1. Atualizar o dashboard do Harmonicus SX (`data/planos_data.js` e `harmonicus-sx/data/planos_data.js`).
     2. Atualizar o documento executivo `planos_de_investimento.md`.
     3. Sincronizar a Matriz de Governança em `AGENTS.md` e `GEMINI.md`.
     4. Realizar a validação de sintaxe e o deploy em produção via skill `labdeploy`.
4. **Execução Exclusiva em Dólar (`USDT`) para Ações Americanas & Piso de 30% no Simple Earn:**
   * Todas as operações com ações e ETFs americanos (*Backed Equities* `NVDAB`, `SPYB`, `SQQQB`, `TLT`, `TSLAB`) são executadas **estrita e exclusivamente no par direto com Dólar (`USDT <-> Ação`)**, eliminando intermediadores e bitributação cambial em Reais.
   * É obrigatório manter no mínimo **30% do saldo total de USDT em custódia retido no Simple Earn Flexível** como colchão defensivo de rendimento passivo (6,88% a.a.).
   * Os **70% restantes do saldo de USDT circulam livremente** entre os ativos não-cripto com lotes maximizados calibrados pelo LabAnalyst (NVDAB: 40 USDT, SPYB: 35 USDT, SQQQB: 25 USDT, TLT: 20 USDT).
   * **Garantia Anti-Bloqueio pelo Simple Earn:** O Simple Earn NUNCA deve impedir qualquer transação autorizada dentro do limite de 70%. O Gatekeeper `LabPolice` executa auto-resgate instantâneo para a conta Spot antes do envio de ordens de compra e auto-subscrição imediata após a realização de lucros sob a Trava 6.

---

## 📜 5. Matriz dos 15 Planos de Investimento (Versão 20.0 Oficial com Hedge US Binance e Plano Adeus, Perry)

| # | Estratégia | Par / Ativo | Alocação Base | Racional Quantitativo Intradiário | Status em Produção |
| :-: | :--- | :---: | :---: | :--- | :--- |
| **1** | **Plano Guiana Brasileira** | PAXG <-> BTC | 150 reais | Arbitragem intradiária de spread adaptativo PAXG/BTC com Z <= -0,50. | Ativo (Recalibrado) |
| **2** | ⭐ **Plano Escudo de Aquiles** | BRL -> BTC | 200 reais | Compra anti-pânico quando VIX >= 21 ou Z <= -1,8. DAS Fundo: 90,6%. | Ativo (+18,76 reais / DAS 90,6%) |
| **3** | **Plano Pátria Volátil** | BRL <-> USDT | 280 reais | Colchão no Simple Earn (6,88% a.a.) e Desova Escalonada de excedente (Não-Cripto/FX). | Ativo (Piso 280 reais / 0% Cripto) |
| **4** | 🇺🇸 ⭐ **Plano Titã do Silício** | USDT <-> NVDAB | 100 reais | Tech Alpha intradiário com Hilbert Wave em NVDABUSDT real Spot. | Ativo (Binance Backed Equity) |
| **5** | **Plano Gravidade Zero** | BTC -> SOL -> BRL | 180 reais | Dual-Scale Fourier ratio SOL/BTC com giro dinâmico de BTC (Modelo A: Hub de Alta Velocidade) e Teto SOL de 180 reais. | Ativo (Calibrado G500) |
| **6** | 🇺🇸 🛡️ **Plano Choque Energético** | USDT <-> XLE | 90 reais | Hedge de Petróleo/Energia com correlação negativa comprovada de -0,35 contra o Bitcoin. | Ativo (Hedge Macro) |
| **7** | ⭐ **Plano Duelo de Titãs** | BTC -> ETH -> BRL | 65 reais | Cointegração do ratio ETH/BTC com giro dinâmico de BTC (Modelo A: Hub de Alta Velocidade) e realização BRL. | Ativo (Calibrado G500) |
| **8** | ⭐ **Plano Flecha de Sagarana** | BRL <-> BTC | 220 reais | Dip hunter com Z <= -1,18 sigma e aceleração d2Z >= 0. DAS Fundo: 68,7%. | Ativo (+8,32 reais em 10 dias) |
| **9** | **Plano Cofre de Midas** | BRL -> USDT -> PAXG| 50 reais | Acumulação passiva de ouro via DCA a cada 5 dias. Ineficiência comprovada em candles 5m (-26,25 reais/mês; Simple Earn 0,01% a.a.). Ouro concentrado no Plano 1. | ⏸️ **Desativado pela Governança** |
| **10** | ⭐ **Plano Sentinela de Minas** | BRL <-> BNB | 90 reais | Mean-reversion 15m e economia perpétua de 25% em taxas Binance. | Ativo (+13,97 reais / DAS 79,5%) |
| **11** | 🇺🇸 🛡️ **Plano Escudo de Washington** | USDT <-> TLT | 80 reais | T-Bonds Soberanos de 20 anos. Flight to safety contra bear market cripto (Corr: -0,35). | Ativo (Hedge Soberano) |
| **12** | 🇺🇸 🐻 **Plano Sentinela Antifrágil** | USDT <-> SQQQB| 90 reais | ProShares UltraPro Short QQQ Spot. Lucro direto na queda dos mercados (Corr: -0,78). | Ativo (Binance Backed Inverse ETF) |
| **13** | 🦇 **Plano Bruce Wayne** | Altcoins -> BRL | 300 reais | Circuit breaker com Quarentena de 12h, janela macro 30d e VWAP. | ⏸️ **Desativado Temporariamente** |
| **14** | 🇺🇸 **Plano Sentinela Wall Street**| USDT <-> SPYB | 100 reais | S&P 500 ETF Trust em SPYBUSDT real Spot na Binance. | Ativo (Binance Backed S&P 500) |
| **15** | 🎩 🚪 **Plano Adeus, Perry** | Legados -> BRL/USDT | Custódia Total | Desova e liquidação cirúrgica de ativos legados (LINK, ADA, NEAR, AVAX) sob a Trava 6. | ⏸️ **Desativado Temporariamente** |

---

## 🌐 7. Governança, Segregação e Arquitetura dos Dashboards (MoneyLab vs Harmonicus SX)

O ecossistema opera com dois dashboards web totalmente distintos, com propósitos, níveis de segurança e requisitos de confidencialidade estritamente segregados. É terminantemente proibido confundir, mesclar ou unificar os seus arquivos:

### 1. MoneyLab Dashboard (`eng-guilsm/moneylab-dashboard`) — Radar Cinético Público Aberto
* **URL Pública:** `https://eng-guilsm.github.io/moneylab-dashboard/`
* **Finalidade:** Terminal público de física inercial de mercados, curvas multi-timeframe, derivadas ($dP/dt$, $d^2P/dt^2$), medidor de empuxo (*Thrust Gauge*), envelopes de Bollinger e banda Zero-Lag de Ehlers ($\pm 2\sigma$).
* **Regra de Ouro de Acesso:** **100% PÚBLICO E ABERTO.** É **TERMINANTEMENTE PROIBIDO** colocar senha, PIN, tela de login, modal de autenticação, `gatekeeper-lock.js` ou qualquer trava de proteção. O usuário/visitante deve abrir a página e ver o radar cinético imediatamente.
* **Blindagem de Sigilo (Zero Vazamento):** É **TERMINANTEMENTE PROIBIDO** subir ou referenciar arquivos de custódia, saldos reais, ordens executadas ou estratégias proprietárias. NUNCA incluir: `data/planos_data.js`, `data/harmonicus_sx_data.js`, `tactical-hud.js`, `harmonicus-sx.js` ou `audio-engine.js`.
* **Conteúdo Permitido:** Única e exclusivamente a **Página 3 Aberta** (`index.html` limpo), alimentada por `data/charts_data.js` (dados de cotações/indicadores) e renderizada por `charts-kinetics.js` e `style.css`.
* **Mecanismo de Deploy:** O repositório `moneylab-dashboard` é sincronizado via Git a partir do workspace local (`eng-guilsm/moneylab-dashboard.git`). A chave da VPS (`id_ed25519`) NÃO tem permissão de escrita nele; qualquer push para este repositório deve ser feito via credenciais locais do git.

---

### 2. Harmonicus SX (`eng-guilsm/harmonicus-sx`) — Suíte Tática & Analítica Privada
* **URL Privada:** `https://eng-guilsm.github.io/harmonicus-sx/`
* **Finalidade:** Terminal quantitativo completo com navegação por abas em 3 páginas:
  1. **Página 1: Radar Tático** (Monitoramento dos 8 motores, patrimônio consolidado, planos ativos, ordens recentes, termômetros e PnL).
  2. **Página 2: Sintetizador Espectral SX** (Sonificação harmônica WebAudio, coerência de Fourier, MST de Mantegna).
  3. **Página 3: Cinética & Ativos** (Curvas de mercado, derivadas e bandas).
* **Segurança e Proteção:** Protegido compulsoriamente por **Gatekeeper Lock (`gatekeeper-lock.js`) com senha/PIN de acesso privado**.
* **Conteúdo:** Suíte completa contendo `data/planos_data.js`, `data/harmonicus_sx_data.js`, `data/charts_data.js`, `gatekeeper-lock.js`, `tactical-hud.js`, `audio-engine.js`, `harmonicus-sx.js`, `charts-kinetics.js` e `style.css`.
* **Mecanismo de Deploy:** Sincronizado automaticamente pela VPS em tempo real via `harmonicus_deploy_daemon.py` utilizando a chave de deploy `id_ed25519` restrita ao repositório `harmonicus-sx`.

---

### 3. Protocolo Operacional para Atualizações Solicitadas pelo Usuário
* **Ao atualizar APENAS o `moneylab-dashboard`:** Modificar exclusivamente a Página 3 aberta, `charts-kinetics.js`, `data/charts_data.js` ou `index.html` público. **JAMAIS adicionar senha, PIN ou copiar arquivos confidenciais do Harmonicus.** Realizar o push no repositório `moneylab-dashboard`.
* **Ao atualizar APENAS o `harmonicus-sx`:** Atualizar os dados dos motores, planos, custódia e suíte completa no repositório `harmonicus-sx`, mantendo o `gatekeeper-lock.js` ativo. Realizar o push no repositório `harmonicus-sx`.
* **Ao atualizar AMBOS:** Sincronizar as melhorias do motor gráfico (`charts-kinetics.js`) e dados de mercado (`data/charts_data.js`) em ambos os repositórios, mas **preservar rigorosamente a segregação**: `moneylab-dashboard` permanece aberto e sem dados sigilosos; `harmonicus-sx` permanece fechado com PIN e suíte completa.
