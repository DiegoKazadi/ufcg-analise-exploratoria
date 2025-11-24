# =====================================================
# 1. Pacotes
# =====================================================
library(tidyverse)
library(janitor)
library(readr)

# =====================================================
# 2. Carregar tabelas
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
# 3. Filtrar Concluintes por Currículo (1999 vs 2017)
# =====================================================
concluintes_curriculo <- alunos_final %>%
  filter(tipo_de_evasao == "GRADUADO") %>%
  filter(as.numeric(substr(periodo_de_ingresso, 1, 4)) >= 2011) %>% 
  filter(as.numeric(substr(periodo_de_ingresso, 1, 4)) <= 2023) %>% 
  group_by(curriculo) %>%
  summarise(
    concluintes = n(),
    percentual = round(concluintes / sum(concluintes) * 100, 2),
    .groups = "drop"
  )

print(concluintes_curriculo)

# =====================================================
# 4. Gráfico — Concluintes por Currículo (com números)
# =====================================================
g1 <- ggplot(concluintes_curriculo,
             aes(x = factor(curriculo), y = concluintes, fill = curriculo)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_text(aes(label = concluintes),
            vjust = -0.3, size = 4) +
  labs(
    title = "Distribuição de Alunos Graduados por Currículo (2011–2023)",
    x = "Currículo",
    y = "Quantidade de Concluintes"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(g1)

ggsave("concluintes_por_curriculo.png", g1, width = 10, height = 6, dpi = 300)

# =====================================================
# 5. Distribuição dos Concluintes por Período de Ingresso (Figura 4.5)
# =====================================================
concluintes_ingresso <- alunos_final %>%
  filter(tipo_de_evasao == "GRADUADO") %>%
  filter(as.numeric(substr(periodo_de_ingresso, 1, 4)) >= 2011) %>% 
  filter(as.numeric(substr(periodo_de_ingresso, 1, 4)) <= 2023) %>%
  group_by(periodo_de_ingresso) %>%
  summarise(
    concluintes = n(),
    .groups = "drop"
  ) %>%
  arrange(periodo_de_ingresso)

print(concluintes_ingresso)

# =====================================================
# 6. Gráfico — Concluintes por Período de Ingresso (Figura 4.5)
# =====================================================
g2 <- ggplot(concluintes_ingresso,
             aes(x = periodo_de_ingresso, y = concluintes)) +
  geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.9) +
  geom_text(aes(label = concluintes),
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Distribuição dos Alunos Graduados por Período de Ingresso (2011–2023)",
    x = "Período de Ingresso",
    y = "Número de Concluintes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(g2)

ggsave("concluintes_por_periodo_ingresso.png", g2, width = 12, height = 6, dpi = 300)

