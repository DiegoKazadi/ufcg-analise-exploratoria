# =====================================================
# Pacotes
# =====================================================
library(tidyverse)
library(janitor)
library(readr)

# =====================================================
# Carregar tabelas
# =====================================================
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

carregar_tabelas <- function(pasta) {
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) stop("Nenhum arquivo CSV encontrado.")
  
  tabelas <- list()
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq))
    df <- read_delim(
      arq, delim = ";", show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
    tabelas[[nome]] <- df
  }
  return(tabelas)
}

tabelas <- carregar_tabelas(pasta_dados)
alunos_final <- tabelas[["alunos-final"]] %>% clean_names()

# =====================================================
# Filtragem — apenas evadidos (não graduados)
# =====================================================
evadidos <- alunos_final %>%
  filter(status == "INATIVO") %>% 
  filter(tipo_de_evasao != "GRADUADO") %>%
  filter(curriculo %in% c("1999", "2017")) %>%
  filter(!is.na(periodo_de_ingresso),
         !is.na(periodo_de_evasao))

# =====================================================
# Converter períodos para formato numérico
# =====================================================
converter_periodo <- function(x) {
  ano <- as.numeric(substr(x, 1, 4))
  sem <- as.numeric(substr(x, 6, 6))
  return(ano + (sem - 1) / 2)
}

evadidos <- evadidos %>%
  mutate(
    ingresso_num = converter_periodo(periodo_de_ingresso),
    deslig_num   = converter_periodo(periodo_de_evasao),
    tempo_ate_evasao = deslig_num - ingresso_num
  )

# =====================================================
# Tempo médio até evasão por currículo
# =====================================================
tme_curriculos <- evadidos %>%
  group_by(curriculo) %>%
  summarise(
    tempo_medio = round(mean(tempo_ate_evasao), 2),
    total_evadidos = n(),
    .groups = "drop"
  ) %>%
  mutate(curriculo = factor(curriculo,
                            levels = c("1999", "2017"),
                            labels = c("Currículo 1999", "Currículo 2017")))

# =====================================================
# Gráfico com cores claras e rótulos ajustados
# =====================================================

cores_tme <- c(
  "Currículo 1999" =  "#3A8BB7",   # azul claro
  "Currículo 2017" =  "#D98C4D"    # laranja claro
)
g_tme <- ggplot(tme_curriculos,
                aes(x = curriculo,
                    y = tempo_medio,
                    fill = curriculo)) +
  geom_col(width = 0.6, alpha = 0.95) +
  
  # texto sem negrito acima da barra + palavra períodos
  geom_text(aes(label = paste0(tempo_medio, " períodos")),
            vjust = -0.4,
            size = 4) +
  
  scale_fill_manual(values = cores_tme) +
  
  labs(
    title = "Tempo Médio até Evasão por Currículo",
    x = "Currículo",
    y = "Tempo Médio (em períodos)",
    fill = "Currículo"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_text(face = "plain"),
    legend.text  = element_text(face = "plain"),
    text = element_text(face = "plain")
  )

print(g_tme)

ggsave("tempo_medio_evasao_curriculos.png",
       g_tme, width = 10, height = 6, dpi = 300)

# =====================================================
# Exportar tabela
# =====================================================
write.csv(tme_curriculos,
          "tme_comparativo_curriculos.csv",
          row.names = FALSE)
# =====================================================
# Pacotes
# =====================================================
library(tidyverse)
library(janitor)
library(readr)

# =====================================================
# Carregar tabelas
# =====================================================
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

carregar_tabelas <- function(pasta) {
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) stop("Nenhum arquivo CSV encontrado.")
  
  tabelas <- list()
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq))
    df <- read_delim(
      arq, delim = ";", show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
    tabelas[[nome]] <- df
  }
  return(tabelas)
}

tabelas <- carregar_tabelas(pasta_dados)
alunos_final <- tabelas[["alunos-final"]] %>% clean_names()

# =====================================================
# Filtragem — apenas evadidos (não graduados)
# =====================================================
evadidos <- alunos_final %>%
  filter(status == "INATIVO") %>% 
  filter(tipo_de_evasao != "GRADUADO") %>%
  filter(curriculo %in% c("1999", "2017")) %>%
  filter(!is.na(periodo_de_ingresso),
         !is.na(periodo_de_evasao))

# =====================================================
# Converter períodos para formato numérico
# =====================================================
converter_periodo <- function(x) {
  ano <- as.numeric(substr(x, 1, 4))
  sem <- as.numeric(substr(x, 6, 6))
  return(ano + (sem - 1) / 2)
}

evadidos <- evadidos %>%
  mutate(
    ingresso_num = converter_periodo(periodo_de_ingresso),
    deslig_num   = converter_periodo(periodo_de_evasao),
    tempo_ate_evasao = deslig_num - ingresso_num
  )

# =====================================================
# Tempo médio até evasão por currículo
# =====================================================
tme_curriculos <- evadidos %>%
  group_by(curriculo) %>%
  summarise(
    tempo_medio = round(mean(tempo_ate_evasao), 2),
    total_evadidos = n(),
    .groups = "drop"
  ) %>%
  mutate(curriculo = factor(curriculo,
                            levels = c("1999", "2017"),
                            labels = c("Currículo 1999", "Currículo 2017")))

# =====================================================
# Gráfico com cores claras e rótulos ajustados
# =====================================================

cores_tme <- c(
  "Currículo 1999" =  "#3A8BB7",   # azul claro
  "Currículo 2017" =  "#D98C4D"    # laranja claro
)
g_tme <- ggplot(tme_curriculos,
                aes(x = curriculo,
                    y = tempo_medio,
                    fill = curriculo)) +
  geom_col(width = 0.6, alpha = 0.95) +
  
  # texto sem negrito acima da barra + palavra períodos
  geom_text(aes(label = paste0(tempo_medio, " períodos")),
            vjust = -0.4,
            size = 4) +
  
  scale_fill_manual(values = cores_tme) +
  
  labs(
    title = "Tempo Médio até Evasão por Currículo",
    x = "Currículo",
    y = "Tempo Médio (em períodos)",
    fill = "Currículo"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_text(face = "plain"),
    legend.text  = element_text(face = "plain"),
    text = element_text(face = "plain")
  )

print(g_tme)

ggsave("tempo_medio_evasao_curriculos.png",
       g_tme, width = 10, height = 6, dpi = 300)

# =====================================================
# Exportar tabela
# =====================================================
write.csv(tme_curriculos,
          "tme_comparativo_curriculos.csv",
          row.names = FALSE)
