# Pacotes
library(tidyverse)
library(janitor)
library(readr)

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


# Padronização das formas de ingresso

dados_filtrados <- dados_filtrados %>%
  mutate(
    forma_de_ingresso = case_when(
      str_detect(forma_de_ingresso, "SISU") ~ "SISU",
      str_detect(forma_de_ingresso, "VESTIBULAR|ENEM") ~ "VESTIBULAR/ENEM",
      str_detect(forma_de_ingresso, "TRANSFER") ~ "TRANSFERÊNCIA",
      str_detect(forma_de_ingresso, "REOP") ~ "REOPÇÃO",
      str_detect(forma_de_ingresso, "CONVENIO|PEC") ~ "CONVÊNIO",
      str_detect(forma_de_ingresso, "GRADUAD") ~ "GRADUADO",
      TRUE ~ "OUTROS"
    )
  )

# Conferir categorias finais
table(dados_filtrados$forma_de_ingresso)


# Distribuição absoluta e percentual

ingresso_resumo <- dados_filtrados %>%
  group_by(curriculo, forma_de_ingresso) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(curriculo) %>%
  mutate(
    percentual = round((total / sum(total)) * 100, 2)
  )

ingresso_resumo


# Gráfico: Barras empilhadas por período e tipo de ingresso

g_ingresso_periodo <- ggplot(
  dados_filtrados,
  aes(
    x = periodo_de_ingresso,
    fill = forma_de_ingresso
  )
) +
  geom_bar(position = "stack") +
  facet_wrap(~ curriculo, scales = "free_x") +
  labs(
    title = "Distribuição dos Alunos por Período e Tipo de Ingresso",
    subtitle = "Análise exploratória – Currículos 1999 e 2017",
    x = "Período de ingresso",
    y = "Número de estudantes",
    fill = "Tipo de ingresso"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(g_ingresso_periodo)


# Gráfico percentual por currículo

g_ingresso_percentual <- ggplot(
  ingresso_resumo,
  aes(
    x = forma_de_ingresso,
    y = percentual,
    fill = forma_de_ingresso
  )
) +
  geom_col(width = 0.7) +
  facet_wrap(~ curriculo) +
  geom_text(
    aes(label = paste0(percentual, "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Distribuição Percentual por Forma de Ingresso",
    subtitle = "Análise exploratória da base filtrada (2011.1–2023.2)",
    x = "Forma de ingresso",
    y = "Percentual (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(g_ingresso_percentual)


# Distribuição por periodo de ingresso
g_ingresso_periodo <- ggplot(
  dados_filtrados,
  aes(
    x = periodo_de_ingresso,
    fill = forma_de_ingresso
  )
) +
  geom_bar(position = "stack") +
  facet_wrap(~ curriculo, scales = "free_x", ncol = 1) +
  labs(
    title = "Distribuição por Período de Ingresso",
    subtitle = "Análise exploratória – Currículos 1999 e 2017 (2011.1–2023.2)",
    x = "Período de ingresso",
    y = "Número de estudantes",
    fill = "Forma de ingresso"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")
  )

print(g_ingresso_periodo)


# Distribuição por sexo
 
# Filtrar e padronizar a variável sexo
dados_sexo <- dados_filtrados %>%
  filter(!is.na(sexo)) %>%
  mutate(
    sexo = str_to_upper(str_trim(sexo)),
    sexo = case_when(
      sexo %in% c("M", "MASCULINO") ~ "Masculino",
      sexo %in% c("F", "FEMININO") ~ "Feminino",
      TRUE ~ "Não informado"
    )
  )

sexo_resumo <- dados_sexo %>%
  group_by(curriculo, sexo) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(curriculo) %>%
  mutate(
    percentual = round((total / sum(total)) * 100, 2)
  )

sexo_resumo

# Gráfico
g_sexo <- ggplot(
  sexo_resumo,
  aes(
    x = sexo,
    y = total,
    fill = sexo
  )
) +
  geom_col(width = 0.6) +
  facet_wrap(~ curriculo) +
  geom_text(
    aes(label = paste0(total, " (", percentual, "%)")),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Distribuição dos Estudantes por Sexo",
    subtitle = "Análise exploratória – Currículos 1999 e 2017",
    x = "Sexo",
    y = "Número de estudantes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11)
  )

print(g_sexo)



# 
# Padronização do estado civil

dados_filtrados <- dados_filtrados %>%
  mutate(
    estado_civil = str_to_upper(str_trim(estado_civil)),
    estado_civil = case_when(
      str_detect(estado_civil, "SOLTEIR") ~ "SOLTEIRO(A)",
      str_detect(estado_civil, "CASAD") ~ "CASADO(A)",
      str_detect(estado_civil, "DIVOR") ~ "DIVORCIADO(A)",
      str_detect(estado_civil, "VIUV") ~ "VIÚVO(A)",
      TRUE ~ "OUTROS"
    )
  )

# Conferência das categorias
table(dados_filtrados$estado_civil)

# Resumo absoluto e percentual por currículo

estado_civil_resumo <- dados_filtrados %>%
  group_by(curriculo, estado_civil) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(curriculo) %>%
  mutate(
    percentual = round((total / sum(total)) * 100, 2)
  )

estado_civil_resumo

# Gráfico: Distribuição percentual por estado civil

g_estado_civil <- ggplot(
  estado_civil_resumo,
  aes(
    x = estado_civil,
    y = percentual,
    fill = estado_civil
  )
) +
  geom_col(width = 0.7) +
  facet_wrap(~ curriculo) +
  geom_text(
    aes(label = paste0(percentual, "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Distribuição dos Estudantes por Estado Civil",
    subtitle = "Análise exploratória – Currículos 1999 e 2017",
    x = "Estado civil",
    y = "Percentual (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(g_estado_civil)

















