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


# Preparação dos dados

# Garantir padronização dos rótulos
dados_ingresso <- alunos_final %>%
  filter(!is.na(forma_de_ingresso), !is.na(curriculo)) %>%
  mutate(
    curriculo = as.factor(curriculo),
    forma_de_ingresso = str_to_title(forma_de_ingresso)
  )


# Garantir padronização dos rótulos
dados_ingresso <- alunos_final %>%
  filter(!is.na(forma_de_ingresso), !is.na(curriculo)) %>%
  mutate(
    curriculo = as.factor(curriculo),
    forma_de_ingresso = str_to_title(forma_de_ingresso)
  )

# Remover o currículo 2023
dados_ingresso <- alunos_final %>%
  filter(
    !is.na(forma_de_ingresso),
    !is.na(curriculo),
    curriculo %in% c("1999", "2017")  # remove 2023
  ) %>%
  mutate(
    curriculo = factor(curriculo,
                       levels = c("1999", "2017"),
                       labels = c("Currículo 1999", "Currículo 2017")),
    forma_de_ingresso = str_to_title(forma_de_ingresso)
  )



# Cálculo de totais e percentuais por currículo

ingresso_resumo <- dados_ingresso %>%
  group_by(curriculo, forma_de_ingresso) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(curriculo) %>%
  mutate(
    percentual = round((total / sum(total)) * 100, 2)
  )

###
g_ingresso_facet <- ggplot(ingresso_resumo,
                           aes(x = forma_de_ingresso,
                               y = percentual,
                               fill = forma_de_ingresso)) +
  geom_col(width = 0.7) +
  facet_wrap(~ curriculo) +
  
  geom_text(
    aes(label = paste0(percentual, "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  
  labs(
    title = "Distribuição Percentual dos Alunos por Forma de Ingresso",
    subtitle = "Os Currículos 1999 e 2017",
    x = "Forma de Ingresso",
    y = "Percentual (%)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(g_ingresso_facet)


###
g_ingresso_facet <- ggplot(ingresso_resumo,
                           aes(x = forma_de_ingresso,
                               y = percentual,
                               fill = forma_de_ingresso)) +
  geom_col(width = 0.7) +
  facet_wrap(~ curriculo) +
  
  geom_text(
    aes(label = paste0(percentual, "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  
  labs(
    title = "Distribuição Percentual dos Alunos por Forma de Ingresso",
    subtitle = "Os Currículos 1999 e 2017",
    x = "Forma de Ingresso",
    y = "Percentual (%)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(g_ingresso_facet)


###
g_ingresso_facet <- ggplot(ingresso_resumo,
                           aes(x = forma_de_ingresso,
                               y = percentual,
                               fill = forma_de_ingresso)) +
  geom_col(width = 0.7) +
  facet_wrap(~ curriculo) +
  
  geom_text(
    aes(label = paste0(percentual, "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  
  labs(
    title = "Distribuição Percentual dos Alunos por Forma de Ingresso",
    subtitle = "Os Currículos 1999 e 2017",
    x = "Forma de Ingresso",
    y = "Percentual (%)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(g_ingresso_facet)


###
g_ingresso_lado <- ggplot(ingresso_resumo,
                          aes(x = forma_de_ingresso,
                              y = percentual,
                              fill = curriculo)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  
  geom_text(
    aes(label = paste0(percentual, "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 3
  ) +
  
  labs(
    title = "Comparação das Formas de Ingresso por Currículo",
    x = "Forma de Ingresso",
    y = "Percentual (%)",
    fill = "Currículo"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(g_ingresso_lado)


### Gráfico horizontal – Percentual
g_ingresso_lado_horizontal <- ggplot(ingresso_resumo,
                                     aes(x = forma_de_ingresso,
                                         y = percentual,
                                         fill = curriculo)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  
  geom_text(
    aes(label = paste0(percentual, "%")),
    position = position_dodge(width = 0.7),
    hjust = -0.15,
    size = 3
  ) +
  
  coord_flip() +
  
  labs(
    title = "Distribuição Percentual das Formas de Ingresso",
    x = "Forma de Ingresso",
    y = "Percentual (%)",
    fill = "Currículo"
  ) +
  
  theme_minimal(base_size = 13)

print(g_ingresso_lado_horizontal)


###
ingresso_total <- dados_ingresso %>%
  group_by(curriculo, forma_de_ingresso) %>%
  summarise(total = n(), .groups = "drop")

g_ingresso_total_horizontal <- ggplot(ingresso_total,
                                      aes(x = forma_de_ingresso,
                                          y = total,
                                          fill = curriculo)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  
  geom_text(
    aes(label = total),
    position = position_dodge(width = 0.7),
    hjust = -0.15,
    size = 3
  ) +
  
  coord_flip() +
  
  labs(
    title = "Número de Alunos por Forma de Ingresso e Currículo",
    x = "Forma de Ingresso",
    y = "Número de Alunos",
    fill = "Currículo"
  ) +
  
  theme_minimal(base_size = 13)

print(g_ingresso_total_horizontal)

### Tabela 
tabela_ingresso <- dados_ingresso %>%
  group_by(curriculo, forma_de_ingresso) %>%
  summarise(total = n(), .groups = "drop") %>%
  pivot_wider(
    names_from  = curriculo,
    values_from = total,
    values_fill = 0
  ) %>%
  arrange(desc(`Currículo 1999` + `Currículo 2017`))

tabela_ingresso


