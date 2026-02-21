# Pacotes
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# Carregar tabelas

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

# Visualizar nomes das colunas
colnames(alunos_final)

# Visão geral da estrutura
glimpse(alunos_final)


# Filtragem temporal da amostra

dados_filtrados <- alunos_final %>%
  filter(
    periodo_de_ingresso >= 2011.1,
    periodo_de_ingresso <= 2023.2,
    curriculo %in% c(1999, 2017),
    !is.na(forma_de_ingresso)
  ) %>%
  mutate(
    curriculo = factor(curriculo,
                       levels = c(1999, 2017),
                       labels = c("Currículo 1999", "Currículo 2017")),
    periodo_de_ingresso = as.factor(periodo_de_ingresso),
    forma_de_ingresso = str_to_upper(str_trim(forma_de_ingresso))
  )

# Conferência do tamanho final da base
nrow(dados_filtrados)

# Tratamento da variável Cor/Raça

dados_cor <- dados_filtrados %>%
  filter(!is.na(cor), cor != "") %>%
  mutate(
    cor = str_to_title(str_trim(cor))
  )

# Contagem e cálculo de porcentagens

# Distribuição por cor/raça e currículo
dist_cor <- dados_cor %>%
  count(curriculo, cor, name = "n") %>%
  group_by(curriculo) %>%
  mutate(
    perc = round(100 * n / sum(n), 2),
    label = paste0(n, " (", perc, "%)")
  ) %>%
  ungroup()

# Gráfico – Cor/Raça Declarada por Currículo
library(ggplot2)

grafico_cor <- ggplot(
  dist_cor,
  aes(x = n, y = fct_reorder(cor, n), fill = curriculo)
) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.8),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("Currículo 1999" = "#1F4E79",
               "Currículo 2017" = "#C65911")
  ) +
  labs(
    title = "Distribuição dos Ingressantes por Cor/Raça Declarada",
    x = "Número de estudantes",
    y = "Cor/Raça declarada",
    fill = "Currículo"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "plain", hjust = 0.5),
    legend.position = "top",
    axis.text.y = element_text(size = 11)
  )

grafico_cor
