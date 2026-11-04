
# =====================================================
# ANÁLISE DESCRITIVA E INFERENCIAL DOS JOVENS NEM-NEM (NEET)
# 
# MONOGRAFIA: ENTRE O TRABALHO PRODUTIVO E O REPRODUTIVO
#
# BASE DE DADOS: PNAD CONTÍNUA 2024 – 2º TRIMESTRE
# SUPLEMENTO EDUCAÇÃO
#
# FONTE: Instituto Brasileiro de Geografia e Estatística (IBGE).
# Microdados disponíveis no portal oficial: https://www.ibge.gov.br/
#
# AUTORA: Clarice Miranda do Amaral
#
# NOTA: Neste estudo, jovens NEET (Not in Education, Employment
# or Training) correspondem aos jovens nem-nem, definidos como
# aqueles que não estudam nem trabalham.
# =============================================================

# =====================================================
# 0. PREPARAÇÃO DO AMBIENTE
# =====================================================

rm(list = ls())
cat("\014")

library(PNADcIBGE)
library(survey)
library(srvyr)
library(dplyr)
library(tidyr)
library(openxlsx)

options(survey.lonely.psu = "adjust")

# =====================================================
# BANCO DE DADOS E VARIÁVEIS
# =====================================================

dados_pnad_2024 <- read_pnadc(
  microdata = "PNADC_2024_trimestre2.txt",
  input_txt = "PNAD EDUCAÇÃO 2024 - imput.txt"
)

# Renomear variáveis
dados_pnad_2024 <- dados_pnad_2024 %>%
  rename(
    trabalha                    = V4001,
    afastado_trabalho           = V4005,
    frequenta_escola            = V3002,
    idade                       = V2009,
    procura_trabalho            = V4071,
    uf                          = UF,
    situacao_domicilio          = V1022,
    sexo                        = V2007,
    raca                        = V2010,
    escolaridade                = VD3004,
    anos_de_estudo              = VD3005,
    renda_pc                    = VDI5011,
    faixa_renda_pc              = VDI5012,
    condicao_domicilio          = VD2002,
    motivo_nao_procura_trabalho = VD4030,
    motivo_nao_frequenta_escola = V3034C
  )

# Criação das variáveis derivadas 
dados_pnad_2024 <- dados_pnad_2024 %>%
  mutate(
    
    # --- Juventude e NEET (definição estrita) ---
    jovem = ifelse(idade >= 15 & idade <= 29, 1, 0),
    neet  = ifelse(idade >= 15 & idade <= 29 &
                     frequenta_escola == 2 & trabalha == 2, 1, 0),
    
    # --- 4 grupos (variável principal) ---
    situacao_jovem = case_when(
      neet == 1                                         ~ "NEET",
      neet == 0 & frequenta_escola == 1 & trabalha == 2 ~ "Só estuda",
      neet == 0 & frequenta_escola == 2 & trabalha == 1 ~ "Só trabalha",
      neet == 0 & frequenta_escola == 1 & trabalha == 1 ~ "Estuda e trabalha",
      TRUE ~ NA_character_
    ),
    situacao_jovem = factor(
      situacao_jovem,
      levels = c("NEET", "Só estuda", "Só trabalha", "Estuda e trabalha")
    ),
    
    # --- NEET inativo ---
    y_neet_inativo = ifelse(neet == 1 & procura_trabalho == 2, 1, 0),
    
    # --- Faixas etárias ---
    faixa_etaria_2 = case_when(
      idade >= 15 & idade <= 19 ~ "15 a 19 anos",
      idade >= 20 & idade <= 29 ~ "20 a 29 anos",
      TRUE ~ NA_character_
    ),
    faixa_etaria_3 = case_when(
      idade >= 15 & idade <= 19 ~ "15 a 19 anos",
      idade >= 20 & idade <= 24 ~ "20 a 24 anos",
      idade >= 25 & idade <= 29 ~ "25 a 29 anos",
      TRUE ~ NA_character_
    ),
    
    # --- Raça e grupo sexo + raça ---
    # (as análises interseccionais excluem raca_cat == "Outros")
    raca_cat = case_when(
      raca == 1        ~ "Branca",
      raca %in% c(2, 4) ~ "Negra",
      TRUE             ~ "Outros"
    ),
    grupo_sr = case_when(
      sexo == 1 & raca_cat == "Branca" ~ "Homem Branco",
      sexo == 2 & raca_cat == "Branca" ~ "Mulher Branca",
      sexo == 1 & raca_cat == "Negra"  ~ "Homem Negro",
      sexo == 2 & raca_cat == "Negra"  ~ "Mulher Negra",
      sexo == 1 & raca_cat == "Outros" ~ "Homem Outros",
      sexo == 2 & raca_cat == "Outros" ~ "Mulher Outros",
      TRUE ~ NA_character_
    ),
    
    # --- Região ---
    regiao = case_when(
      uf %in% c(11, 12, 13, 14, 15, 16, 17) ~ "Norte",
      uf %in% 21:29                         ~ "Nordeste",
      uf %in% c(31, 32, 33, 35)             ~ "Sudeste",
      uf %in% 41:43                         ~ "Sul",
      uf %in% 50:53                         ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    
    # --- Anos de estudo (numérico) e renda per capita limpa ---
    anos_de_estudo_num = as.numeric(as.character(anos_de_estudo)),
    renda_pc = ifelse(renda_pc >= 99999999 | renda_pc < 0, NA, renda_pc),
    
    # --- Escolaridade agrupada (19 a 29 anos) ---
    # Obs.: no original os limites "1 a 4" e "4 a 7" se sobrepunham no 4;
    # como case_when usa o primeiro match, o 4 ficava em "1 a 4". Mantido assim.
    escolaridade_grupo = case_when(
      escolaridade %in% 1:4 ~ "1 a 4",
      escolaridade %in% 5:7 ~ "4 a 7 (5 a 7)",
      TRUE ~ NA_character_
    ),
    
    # --- Condição no domicílio ---
    cond_dom_cat = case_when(
      condicao_domicilio %in% c("1", "01")             ~ "Pessoa responsável",
      condicao_domicilio %in% c("2", "02")             ~ "Cônjuge ou companheiro(a)",
      condicao_domicilio %in% c("3", "03", "4", "04")  ~ "Filho ou Enteado",
      condicao_domicilio %in% c("6", "06")             ~ "Pai, mãe, padrasto ou madrasta",
      TRUE ~ "Outros"
    ),
    
    # --- Motivo para não procurar trabalho ---
    motivo_cat = case_when(
      motivo_nao_procura_trabalho == 1 ~ "Afazeres domésticos/família",
      motivo_nao_procura_trabalho == 2 ~ "Estudando",
      motivo_nao_procura_trabalho == 3 ~ "Saúde ou gravidez",
      motivo_nao_procura_trabalho == 4 ~ "Muito jovem/idoso",
      motivo_nao_procura_trabalho == 5 ~ "Não quer trabalhar",
      motivo_nao_procura_trabalho == 6 ~ "Outro motivo",
      TRUE ~ NA_character_
    ),
    
    # --- Motivo para não frequentar escola ---
    motivo_nao_frequenta_cat = case_when(
      motivo_nao_frequenta_escola %in% c(1, "1", "01") ~
        "Trabalhava ou estava procurando trabalho",
      motivo_nao_frequenta_escola %in% c(2, 3, 4, 5, "02", "03", "04", "05") ~
        "Barreiras ao acesso educacional",
      motivo_nao_frequenta_escola %in% c(6, 7, "06", "07") ~
        "Gravidez ou trabalho de cuidado",
      motivo_nao_frequenta_escola %in% c(8, "08") ~
        "Estudando para concurso/vestibular/ENEM",
      motivo_nao_frequenta_escola %in% c(9, 11, "09", "11") ~
        "Não tem interesse/ já concluiu o nível de estudo desejado",
      motivo_nao_frequenta_escola %in% c(10, 12, "10", "12") ~
        "Problema de saúde permanente / Outro motivo",
      TRUE ~ NA_character_
    )
  )

# Desenho amostral 
pnad_design <- svydesign(
  id      = ~UPA,
  strata  = ~Estrato,
  weights = ~V1028,
  data    = dados_pnad_2024,
  nest    = TRUE
)

# =====================================================
# FUNÇÕES AUXILIARES
# =====================================================

# Subconjunto por faixa de idade
subset_idade <- function(design, idade_min, idade_max) {
  subset(design, idade >= idade_min & idade <= idade_max)
}

# Distribuição (%) de uma variável categórica: percentual, EP e CV
tabela_dist <- function(design, variavel) {
  est <- svymean(as.formula(paste0("~factor(", variavel, ")")),
                 design, na.rm = TRUE)
  data.frame(
    categoria  = sub(paste0("^factor\\(", variavel, "\\)"), "", names(coef(est))),
    percentual = round(coef(est) * 100, 2),
    ep         = round(SE(est) * 100, 2),
    cv         = round(cv(est) * 100, 2),
    row.names  = NULL
  )
}

# Distribuição (%) de uma variável categórica DENTRO de grupos (svyby)
# Retorna formato longo: grupos | categoria | Percentual | EP | CV
tabela_por_grupo <- function(design, variavel, por) {
  est <- svyby(
    as.formula(paste0("~factor(", variavel, ")")),
    reformulate(por),
    design, svymean, na.rm = TRUE, vartype = c("se", "cv")
  ) %>% as.data.frame()
  
  padrao <- paste0("^(se\\.|cv\\.)?factor\\(", variavel, "\\)(.*)$")
  
  est %>%
    pivot_longer(
      cols = -all_of(por),
      names_to = c("tipo", "categoria"),
      names_pattern = padrao,
      values_to = "valor"
    ) %>%
    mutate(
      tipo = case_when(tipo == "se." ~ "EP",
                       tipo == "cv." ~ "CV",
                       TRUE          ~ "Percentual"),
      valor = round(valor * 100, 2)
    ) %>%
    pivot_wider(names_from = tipo, values_from = valor)
}

# Média de uma variável numérica por grupo: média, EP e CV
media_por_grupo <- function(design, variavel, por) {
  est <- svyby(reformulate(variavel), reformulate(por),
               design, svymean, na.rm = TRUE, vartype = c("se", "cv"))
  data.frame(
    est[, por, drop = FALSE],
    media = round(est[[variavel]], 2),
    EP    = round(est$se, 2),
    CV    = round(est$cv * 100, 2),
    row.names = NULL
  )
}

# Aplica uma função a cada faixa de idade (lista nomeada: nome = c(min, max))
por_idade <- function(design, faixas, fun, ...) {
  lapply(faixas, function(f) fun(subset_idade(design, f[1], f[2]), ...))
}

# Aplica uma função a cada faixa etária (15-19 / 20-29) de um design
por_faixa_etaria <- function(design, fun, ...) {
  faixas <- c("15 a 19 anos", "20 a 29 anos")
  lapply(setNames(faixas, faixas),
         function(x) fun(subset(design, faixa_etaria_2 == x), ...))
}

# Ordenação dos grupos sexo-raça
ordem_sr <- c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
ordenar_sr <- function(df, niveis = ordem_sr) {
  df %>% mutate(grupo_sr = factor(grupo_sr, levels = niveis))
}

# Faixas de idade usadas em várias análises
faixas_15_29     <- list("15-29" = c(15, 29), "15-19" = c(15, 19), "20-29" = c(20, 29))
faixas_15_19_20_29 <- list("15-19" = c(15, 19), "20-29" = c(20, 29))

# Resultados 
resultados <- list()

# =====================================================
# SUBPOPULAÇÕES 
# =====================================================

d_jovens <- subset_idade(pnad_design, 15, 29)                 # juventude
d_neet   <- subset(d_jovens, situacao_jovem == "NEET")        # NEET

# Interseccional (sem "Outros" em raça)
d_neet_inativo <- subset(
  pnad_design,
  idade >= 15 & idade <= 29 &
    neet == 1 & y_neet_inativo == 1 &
    raca_cat != "Outros" & !is.na(grupo_sr)
)

# =====================================================
# 1. ANÁLISE DESCRITIVA - JOVENS NEM-NEM

# ======================================================

# -----------------------------------------------------
# Distribuição dos jovens por condição de ocupação
# (15-29, 15-19 e 20-29)
# -----------------------------------------------------

resultados$dist_situacao <- por_idade(
  pnad_design, faixas_15_29, tabela_dist, variavel = "situacao_jovem"
)
resultados$dist_situacao

# Média de idade por condição de ocupação
resultados$idade_media <- media_por_grupo(d_jovens, "idade", "situacao_jovem")
resultados$idade_media

# -----------------------------------------------------
# NEET x JUVENTUDE: idade, região e situação do domicílio
# -----------------------------------------------------

comparar_neet_juventude <- function(variavel) {
  list(
    NEET      = tabela_dist(d_neet,    variavel),
    Juventude = tabela_dist(d_jovens,  variavel)
  )
}

resultados$neet_faixa_etaria <- comparar_neet_juventude("faixa_etaria_3")
resultados$neet_regiao       <- comparar_neet_juventude("regiao")
resultados$neet_domicilio    <- comparar_neet_juventude("situacao_domicilio")

resultados$neet_faixa_etaria
resultados$neet_regiao
resultados$neet_domicilio

# -----------------------------------------------------
# Anos de estudo por condição de atividade
# -----------------------------------------------------

resultados$anos_estudo <- por_idade(
  pnad_design, faixas_15_19_20_29,
  media_por_grupo, variavel = "anos_de_estudo_num", por = "situacao_jovem"
)
resultados$anos_estudo

# -----------------------------------------------------
# Completou o ensino médio? (19 a 29 anos)
# Percentual DENTRO de cada condição de atividade
# -----------------------------------------------------

d_19_29 <- subset_idade(pnad_design, 19, 29)

resultados$escolaridade <- tabela_por_grupo(d_19_29, "escolaridade", "situacao_jovem")
resultados$escolaridade_grupo <- tabela_por_grupo(
  subset(d_19_29, !is.na(escolaridade_grupo)),
  "escolaridade_grupo", "situacao_jovem"
)
resultados$escolaridade
resultados$escolaridade_grupo

# -----------------------------------------------------
# Renda per capita média por condição de atividade
# -----------------------------------------------------

resultados$renda_media <- por_idade(
  pnad_design, faixas_15_29,
  media_por_grupo, variavel = "renda_pc", por = "situacao_jovem"
)
resultados$renda_media

# -----------------------------------------------------
# Distribuição das faixas de renda per capita (15-29)
# -----------------------------------------------------

d_jovens_renda <- subset(d_jovens, !is.na(faixa_renda_pc) & faixa_renda_pc != 9)

resultados$faixa_renda <- tabela_por_grupo(
  d_jovens_renda, "faixa_renda_pc", "situacao_jovem"
)
resultados$faixa_renda 
print(resultados$faixa_renda, n = Inf)

# -----------------------------------------------------
# Sexo por condição de atividade (+ total da juventude)
# -----------------------------------------------------

rotular_sexo <- function(df) {
  df %>% mutate(categoria = recode(categoria, "1" = "Homens", "2" = "Mulheres"))
}

resultados$sexo_situacao <- rotular_sexo(
  tabela_por_grupo(d_jovens, "sexo", "situacao_jovem")
)
resultados$sexo_jovens <- rotular_sexo(tabela_dist(d_jovens, "sexo"))
resultados$sexo_situacao
resultados$sexo_jovens

# -----------------------------------------------------
# Raça/cor por condição de atividade (+ total da juventude)
# -----------------------------------------------------

resultados$raca_situacao <- tabela_por_grupo(d_jovens, "raca_cat", "situacao_jovem")
resultados$raca_jovens   <- tabela_dist(d_jovens, "raca_cat")
resultados$raca_situacao
resultados$raca_jovens

# -----------------------------------------------------
# Sexo + raça: NEET x juventude
# -----------------------------------------------------

resultados$sr_neet  <- tabela_dist(d_neet,   "grupo_sr")
resultados$sr_total <- tabela_dist(d_jovens, "grupo_sr")
resultados$sr_neet
resultados$sr_total

# -----------------------------------------------------
# Desocupados x inativos dentro do grupo NEET
# (procura_trabalho: 1 = procura / 2 = não procura)
# -----------------------------------------------------

resultados$neet_desocupado_inativo <- tabela_dist(d_neet, "procura_trabalho")
resultados$neet_desocupado_inativo

# -----------------------------------------------------
# Sexo + raça entre NEET desocupados e NEET inativos
# -----------------------------------------------------

d_neet_desocupado <- subset(d_neet, procura_trabalho == 1 & !is.na(grupo_sr))
d_neet_inativo_sr <- subset(d_neet, procura_trabalho == 2 & !is.na(grupo_sr))

resultados$sr_desocupados <- tabela_dist(d_neet_desocupado, "grupo_sr")
resultados$sr_inativos    <- tabela_dist(d_neet_inativo_sr, "grupo_sr")
resultados$sr_desocupados
resultados$sr_inativos

# =====================================================
# 2 ANÁLISE DESCRITIVA INTERSECCIONAL
# (APENAS NEET INATIVOS - sexo x raça, sem "Outros")
# =====================================================

# N amostral por grupo sexo + raça
resultados$n_amostral <- table(d_neet_inativo$variables$grupo_sr, useNA = "ifany")
resultados$n_amostral

# -----------------------------------------------------
# Escolaridade por grupo sexo-raça (19 a 29 anos)
# -----------------------------------------------------

resultados$inter_escolaridade <- tabela_por_grupo(
  subset(d_neet_inativo, idade >= 19 & !is.na(escolaridade)),
  "escolaridade", "grupo_sr"
) %>%
  ordenar_sr() %>%
  arrange(categoria, grupo_sr)
print(resultados$inter_escolaridade, n = Inf)

# -----------------------------------------------------
# Anos de estudo (15-19 e 20-29)
# -----------------------------------------------------

resultados$inter_anos_estudo <- por_faixa_etaria(
  d_neet_inativo, media_por_grupo,
  variavel = "anos_de_estudo_num", por = "grupo_sr"
)
resultados$inter_anos_estudo

# -----------------------------------------------------
# Renda per capita média (grupo sexo-raça x faixa etária)
# -----------------------------------------------------

resultados$inter_renda_media <- media_por_grupo(
  d_neet_inativo, "renda_pc", c("grupo_sr", "faixa_etaria_2")
)
resultados$inter_renda_media

# -----------------------------------------------------
# Distribuição das faixas de renda per capita
# -----------------------------------------------------

resultados$inter_faixa_renda <- por_faixa_etaria(
  subset(d_neet_inativo, !is.na(faixa_renda_pc) & faixa_renda_pc != 9),
  tabela_por_grupo, variavel = "faixa_renda_pc", por = "grupo_sr"
)
resultados$inter_faixa_renda

# -----------------------------------------------------
# Situação do domicílio
# NEET inativos x toda a juventude (para comparação)
# -----------------------------------------------------

resultados$inter_domicilio_neet <- tabela_por_grupo(
  d_neet_inativo, "situacao_domicilio", c("grupo_sr", "faixa_etaria_2")
)

d_jovens_sr <- subset(
  d_jovens,
  raca_cat != "Outros" & !is.na(grupo_sr) &
    !is.na(faixa_etaria_2) & !is.na(situacao_domicilio)
)

resultados$inter_domicilio_jovens <- tabela_por_grupo(
  d_jovens_sr, "situacao_domicilio", c("grupo_sr", "faixa_etaria_2")
) %>%
  ordenar_sr(c("Mulher Negra", "Mulher Branca", "Homem Branco", "Homem Negro")) %>%
  arrange(faixa_etaria_2, grupo_sr, categoria)

resultados$inter_domicilio_neet
print(resultados$inter_domicilio_jovens, n = Inf, width = Inf)

# -----------------------------------------------------
# Condição no domicílio
# -----------------------------------------------------

resultados$inter_cond_dom <- tabela_por_grupo(
  d_neet_inativo, "cond_dom_cat", c("grupo_sr", "faixa_etaria_2")
)
resultados$inter_cond_dom
print(resultados$inter_cond_dom, n = Inf)

# -----------------------------------------------------
# Motivos para não buscar trabalho (15-19 e 20-29)
# -----------------------------------------------------

resultados$inter_motivo_trabalho <- por_faixa_etaria(
  d_neet_inativo, tabela_por_grupo,
  variavel = "motivo_cat", por = "grupo_sr"
)
resultados$inter_motivo_trabalho

# N amostral por grupo x motivo (para checar células pequenas)
resultados$n_motivo_trabalho <- lapply(
  c("15 a 19 anos", "20 a 29 anos"),
  function(x) with(
    subset(d_neet_inativo$variables, faixa_etaria_2 == x),
    table(grupo_sr, motivo_cat)
  )
)
names(resultados$n_motivo_trabalho) <- c("15 a 19 anos", "20 a 29 anos")

# -----------------------------------------------------
# Motivos para não estar estudando (15-19 e 20-29)
# -----------------------------------------------------

resultados$inter_motivo_escola <- por_faixa_etaria(
  d_neet_inativo, tabela_por_grupo,
  variavel = "motivo_nao_frequenta_cat", por = "grupo_sr"
)
resultados$inter_motivo_escola

# =====================================================
# 3. ANÁLISE INFERENCIAL
# REGRESSÃO LOGÍSTICA - NEET INATIVOS
# =====================================================

resultados_inf <- list()

# =====================================================
# FUNÇÕES AUXILIARES
# =====================================================
# -----------------------------------------------------
# Formata p-valor em notação científica (ex.: 1,23e-62)
# -----------------------------------------------------
formatar_p <- function(p) {
  sub("\\.", ",", formatC(p, format = "e", digits = 2))
}

# -----------------------------------------------------
# Tabela completa: coeficiente, OR, erro-padrão, t, p e IC 95%
# -----------------------------------------------------
extrair_or <- function(modelo) {
  coefs <- summary(modelo)$coefficients
  ic    <- confint(modelo)
  
  data.frame(
    Variavel  = rownames(coefs),
    Estimate  = coefs[, "Estimate"],      # coeficiente (log-odds)
    OR        = exp(coefs[, "Estimate"]), # razão de chances
    Std_Error = coefs[, "Std. Error"],    # erro-padrão
    t_value   = coefs[, "t value"],       # estatística de teste
    p_valor   = coefs[, "Pr(>|t|)"],      # valor-p exato, sem arredondar
    IC_inf_95 = exp(ic[, 1]),
    IC_sup_95 = exp(ic[, 2]),
    row.names = NULL
  ) %>%
    mutate(
      p_fmt          = formatar_p(p_valor),   # notação científica (texto)
      Estimate       = round(Estimate, 4),
      OR             = round(OR, 3),
      Std_Error      = round(Std_Error, 4),
      t_value        = round(t_value, 3),
      IC_inf_95      = round(IC_inf_95, 3),
      IC_sup_95      = round(IC_sup_95, 3),
      percentual     = round((OR - 1) * 100, 1),
      percentual_fmt = paste0(ifelse(percentual > 0, "+", ""), percentual, "%"),
      efeito         = ifelse(OR < 1, "Redução", "Aumento"),
      significativo  = ifelse(p_valor < 0.05, "Sim", "Não")
    )
}

# -----------------------------------------------------
# GVIF para modelos svyglm
# -----------------------------------------------------
calcular_gvif <- function(modelo) {
  
  coef_nomes <- setdiff(names(coef(modelo)), "(Intercept)")
  V <- vcov(modelo)[coef_nomes, coef_nomes, drop = FALSE]
  R <- cov2cor(V)
  
  termos <- attr(modelo$terms, "term.labels")
  
  bind_rows(lapply(termos, function(termo) {
    
    indices <- grep(paste0("^", termo), coef_nomes)
    if (length(indices) == 0) return(NULL)
    
    outros <- setdiff(seq_along(coef_nomes), indices)
    
    gvif <- if (length(outros) == 0) 1 else
      det(R[indices, indices, drop = FALSE]) *
      det(R[outros,  outros,  drop = FALSE]) / det(R)
    
    df <- length(indices)
    
    data.frame(
      Variavel      = termo,
      GVIF          = round(gvif, 3),
      GL            = df,
      GVIF_ajustado = round(gvif^(1 / (2 * df)), 3)
    )
  }))
}

# -----------------------------------------------------
# Rotulagem de escolaridade (usada para o jovem e o responsável)
# -----------------------------------------------------
niveis_escolaridade <- c("Sem instrução", "Fundamental incompleto",
                         "Fundamental completo", "Médio incompleto",
                         "Médio completo pra cima")

rotular_escolaridade <- function(x) {
  factor(
    case_when(
      x == 1       ~ "Sem instrução",
      x == 2       ~ "Fundamental incompleto",
      x == 3       ~ "Fundamental completo",
      x == 4       ~ "Médio incompleto",
      x %in% 5:7   ~ "Médio completo pra cima",
      TRUE         ~ NA_character_
    ),
    levels = niveis_escolaridade
  )
}

# Ajuste padrão: quasibinomial com desenho amostral
ajustar_modelo <- function(termos, design) {
  svyglm(reformulate(termos, response = "y_neet_inativo"),
         design = design, family = quasibinomial())
}

# =====================================================
#  RECODIFICAÇÃO DAS VARIÁVEIS (PÓS-LEITURA)
# =====================================================

salario_minimo_2024 <- 1412

niveis_renda_sm <- c("Até 1 salário mínimo",
                     "Mais de 1 até 2 salários mínimos",
                     "Mais de 2 até 3 salários mínimos",
                     "Mais de 3 até 5 salários mínimos",
                     "Mais de 5 salários mínimos")

# Variáveis que dependem apenas da própria linha
dados_pnad_2024 <- dados_pnad_2024 %>%
  mutate(
    # Sexo
    sexo_cat = factor(
      case_when(sexo == 1 ~ "Homem", sexo == 2 ~ "Mulher", TRUE ~ NA_character_),
      levels = c("Homem", "Mulher")
    ),
    
    # Escolaridade do jovem
    escolaridade_cat = rotular_escolaridade(escolaridade),
    
    # Situação do domicílio
    situacao_dom = factor(
      case_when(situacao_domicilio == 1 ~ "Urbano",
                situacao_domicilio == 2 ~ "Rural",
                TRUE ~ NA_character_),
      levels = c("Urbano", "Rural")
    ),
    
    # Condição no domicílio
    # (ATENÇÃO: sobrescreve cond_dom_cat do script descritivo; aqui
    #  "Pai, mãe, padrasto ou madrasta" entra em "Outros", como no original)
    cond_dom_cat = factor(
      case_when(
        condicao_domicilio %in% c("1", "01")            ~ "Pessoa responsável",
        condicao_domicilio %in% c("2", "02")            ~ "Cônjuge ou companheiro(a)",
        condicao_domicilio %in% c("3", "03", "4", "04") ~ "Filho ou Enteado",
        TRUE                                            ~ "Outros"
      ),
      levels = c("Pessoa responsável", "Cônjuge ou companheiro(a)",
                 "Filho ou Enteado", "Outros")
    ),
    
    # Faixa de renda per capita em salários mínimos
    faixa_renda_sm = factor(
      case_when(
        is.na(renda_pc)                      ~ NA_character_,
        renda_pc <= 1 * salario_minimo_2024  ~ niveis_renda_sm[1],
        renda_pc <= 2 * salario_minimo_2024  ~ niveis_renda_sm[2],
        renda_pc <= 3 * salario_minimo_2024  ~ niveis_renda_sm[3],
        renda_pc <= 5 * salario_minimo_2024  ~ niveis_renda_sm[4],
        TRUE                                 ~ niveis_renda_sm[5]
      ),
      levels = niveis_renda_sm
    ),
    
    # Faixa etária (reaproveita faixa_etaria_3 do script descritivo)
    faixa_etaria_3cat = factor(
      faixa_etaria_3,
      levels = c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos")
    ),
    
    # Categorias de referência (a 1ª de cada vetor é a referência)
    grupo_sr = factor(grupo_sr, levels = c("Homem Branco", "Mulher Branca",
                                           "Homem Negro", "Mulher Negra")),
    regiao   = factor(regiao, levels = c("Nordeste", "Sul", "Sudeste",
                                         "Norte", "Centro-Oeste")),
    raca_cat = factor(raca_cat, levels = c("Branca", "Negra"))
  )

# Variáveis que dependem do domicílio (agrupam por UPA + V1008)
dados_pnad_2024 <- dados_pnad_2024 %>%
  group_by(UPA, V1008) %>%
  mutate(
    # Presença de criança no domicílio
    tem_crianca = factor(
      ifelse(any(idade < 13, na.rm = TRUE), "Tem criança", "Não tem criança"),
      levels = c("Não tem criança", "Tem criança")
    ),
    # Escolaridade do responsável pelo domicílio
    escolaridade_responsavel =
      escolaridade[as.character(condicao_domicilio) %in% c("1", "01")][1]
  ) %>%
  ungroup() %>%
  mutate(
    escolaridade_responsavel_cat = rotular_escolaridade(escolaridade_responsavel)
  )

# =====================================================
# DESENHO AMOSTRAL (RECRIADO APÓS AS RECODIFICAÇÕES)
# =====================================================

pnad_reg <- svydesign(
  id      = ~UPA,
  strata  = ~Estrato,
  weights = ~V1028,
  data    = dados_pnad_2024,
  nest    = TRUE
)
# =====================================================
# MODELOS
# =====================================================

# Covariáveis comuns a todos os modelos
covariaveis <- c(
  "faixa_etaria_3cat",
  "escolaridade_cat",
  "escolaridade_responsavel_cat",
  "situacao_dom",
  "regiao",
  "cond_dom_cat",
  "tem_crianca",
  "faixa_renda_sm"
)

# MODELO 1 — Interseccionalidade (completo)
modelo <- ajustar_modelo(c("grupo_sr", covariaveis), pnad_reg)

# MODELO 1.2 — Bruto
modelo_bruto <- ajustar_modelo("grupo_sr", pnad_reg)

# MODELO 2 — Homens (raça/cor no lugar de grupo sexo-raça)
modelo_homens <- ajustar_modelo(
  c("raca_cat", covariaveis),
  subset(pnad_reg, sexo_cat == "Homem")
)

# MODELO 3 — Mulheres
modelo_mulheres <- ajustar_modelo(
  c("raca_cat", covariaveis),
  subset(pnad_reg, sexo_cat == "Mulher")
)

# Lista de modelos (facilita aplicar as mesmas funções a todos)
modelos <- list(
  completo = modelo,
  bruto    = modelo_bruto,
  homens   = modelo_homens,
  mulheres = modelo_mulheres
)

# Odds ratios (só DEPOIS de criar a lista "modelos")
resultados_inf$or <- lapply(modelos, extrair_or)

resultado_or          <- resultados_inf$or$completo
resultado_or_bruto    <- resultados_inf$or$bruto
resultado_or_homens   <- resultados_inf$or$homens
resultado_or_mulheres <- resultados_inf$or$mulheres

# Tabela final com p em notação científica
colunas_tabela <- c("Variavel", "Estimate", "OR", "Std_Error",
                    "t_value", "p_fmt", "IC_inf_95", "IC_sup_95")

resultado_or[, colunas_tabela]
resultado_or_bruto[, colunas_tabela]
resultado_or_homens[, colunas_tabela]
resultado_or_mulheres[, colunas_tabela]

# Conferir a categoria de referência de condição no domicílio
resultado_or_mulheres %>%
  filter(grepl("^cond_dom_cat", Variavel))


# =====================================================
# PRESSUPOSTOS
# =====================================================

# -----------------------------------------------------
# GVIF — multicolinearidade
# -----------------------------------------------------
resultados_inf$gvif <- lapply(
  modelos[c("completo", "homens", "mulheres")], calcular_gvif
)

gvif_modelo   <- resultados_inf$gvif$completo
gvif_homens   <- resultados_inf$gvif$homens
gvif_mulheres <- resultados_inf$gvif$mulheres

gvif_modelo
gvif_homens
gvif_mulheres

# -----------------------------------------------------
# Esparsidade — diagnóstico
# (casos sem ponderação por categoria de cada variável,
#  para identificar células pequenas)
# -----------------------------------------------------
diagnostico_esparsidade <- function(modelo) {
  dados_mod <- model.frame(modelo)
  y         <- dados_mod[[1]]
  termos    <- attr(modelo$terms, "term.labels")
  
  bind_rows(lapply(termos, function(t) {
    x <- dados_mod[[t]]
    data.frame(
      Variavel  = t,
      Categoria = levels(x),
      n         = as.numeric(table(x)),
      eventos   = as.numeric(tapply(y, x, sum)),
      row.names = NULL
    )
  })) %>%
    mutate(pct_eventos = round(eventos / n * 100, 1))
}

resultados_inf$esparsidade <- lapply(
  modelos[c("completo", "homens", "mulheres")], diagnostico_esparsidade
)
resultados_inf$esparsidade

# -----------------------------------------------------
# EFEITO DO DESENHO AMOSTRAL (DEFF / EPA)
# -----------------------------------------------------

# Obtém as linhas efetivamente utilizadas no modelo completo
dados_modelo_completo <- dados_pnad_2024[as.numeric(rownames(model.frame(modelo))), ]

# Normaliza os pesos para que somem o tamanho da amostra (n)
# Evita a explosão da escala de variância no glm tradicional
dados_modelo_completo$V1028_norm <- dados_modelo_completo$V1028 / mean(dados_modelo_completo$V1028, na.rm = TRUE)

# Ajusta o modelo equivalente ponderado (sem estratos e conglomerados)
modelo_pesos_norm <- glm(
  y_neet_inativo ~ grupo_sr + faixa_etaria_3cat + escolaridade_cat + 
    escolaridade_responsavel_cat + situacao_dom + regiao + 
    cond_dom_cat + tem_crianca + faixa_renda_sm,
  data = dados_modelo_completo,
  weights = V1028_norm,
  family = quasibinomial()
)

# Extrai os erros-padrão de ambos os modelos
se_plano  <- sqrt(diag(vcov(modelo)))
se_pesos  <- sqrt(diag(vcov(modelo_pesos_norm)))

# Calcula o DEFF para cada coeficiente: (SE_Complexo / SE_Pesos)^2
comparacao_desenho <- data.frame(
  Variavel            = names(coef(modelo)),
  OR_Plano            = round(exp(coef(modelo)), 3),
  SE_Plano_Complexo   = round(se_plano, 4),
  SE_Apenas_Pesos_Norm= round(se_pesos, 4)
) %>%
  mutate(
    DEFF                = round((SE_Plano_Complexo / SE_Apenas_Pesos_Norm)^2, 3),
    Aumento_SE_pct      = round((sqrt(DEFF) - 1) * 100, 1)
  )

# Salva nos resultados inferenciais
resultados_inf$estudo_desenho <- comparacao_desenho

# Exibe a tabela final limpa e interpretável
comparacao_desenho %>%
  select(Variavel, OR_Plano, SE_Plano_Complexo, SE_Apenas_Pesos_Norm, DEFF, Aumento_SE_pct)

# =====================================================
#EVENTOS POR PARÂMETRO (EPP / EPV) — TABELA A5
# =====================================================

calcular_epp <- function(modelo, nome_modelo) {
  # Obtém os dados utilizados no modelo
  dados_mod <- model.frame(modelo)
  y <- dados_mod[[1]]
  
  # Contagem não ponderada da amostra
  n_total  <- length(y)
  n_eventos <- sum(y == 1, na.rm = TRUE)
  n_zero    <- sum(y == 0, na.rm = TRUE)
  n_min     <- min(n_eventos, n_zero)
  
  # Número total de parâmetros (incluindo intercepto)
  n_parametros <- length(coef(modelo))
  
  # Razão Eventos por Parâmetro (EPP/EPV)
  epp <- n_min / n_parametros
  
  data.frame(
    Modelo          = nome_modelo,
    `N total`       = n_total,
    `Evento Y = 1`  = n_eventos,
    `Evento Y = 0`  = n_zero,
    `Nº de parâmetros` = n_parametros,
    `EPP/EPV`       = round(epp, 1),
    `Adequado`      = ifelse(epp >= 10, "Sim", "Não"),
    check.names     = FALSE
  )
}

# Aplica aos três modelos principais
tabela_a5 <- bind_rows(
  calcular_epp(modelo, "Modelo 1: Interseccional"),
  calcular_epp(modelo_homens, "Modelo 2: Homens"),
  calcular_epp(modelo_mulheres, "Modelo 3: Mulheres")
)

# Armazena nos resultados inferenciais
resultados_inf$tabela_a5_epp <- tabela_a5

# Exibe a Tabela A5
tabela_a5

# =====================================================
# PONTOS INFLUENTES
# =====================================================

diagnostico_influencia <- function(modelo, nome_modelo) {
  residuos_pad <- rstandard(modelo)
  
  hat_vals <- hatvalues(modelo)
  
  cook_d <- cooks.distance(modelo)
  
  n <- length(cook_d)
  limiar_cook <- 4 / n
  
  influentes <- which(cook_d > limiar_cook)
  
  data.frame(
    Modelo             = nome_modelo,
    Total_Observacoes  = n,
    Pontos_Acima_Limiar = length(influentes),
    Max_Cook_Distance  = round(max(cook_d, na.rm = TRUE), 5),
    Max_Hat_Value      = round(max(hat_vals, na.rm = TRUE), 5),
    Criterio_Limiar    = round(limiar_cook, 6)
  )
}

# Aplica aos modelos principais
resultados_inf$influencia <- bind_rows(
  diagnostico_influencia(modelo, "Modelo 1: Interseccional"),
  diagnostico_influencia(modelo_homens, "Modelo 2: Homens"),
  diagnostico_influencia(modelo_mulheres, "Modelo 3: Mulheres")
)

# Tabela de diagnóstico de influência
resultados_inf$influencia
