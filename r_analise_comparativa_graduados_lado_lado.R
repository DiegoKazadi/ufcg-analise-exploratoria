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
# 3. Filtrar concluintes (graduados) por período e currículo
# =====================================================
concluintes_comparativo <- alunos_final %>%
  filter(tipo_de_evasao == "GRADUADO") %>%
  
  # filtrar somente ingressos de 2011 a 2023
  filter(as.numeric(substr(periodo_de_ingresso, 1, 4)) >= 2011,
         as.numeric(substr(periodo_de_ingresso, 1, 4)) <= 2023) %>%
  
  group_by(periodo_de_ingresso, curriculo) %>%
  summarise(
    concluintes = n(),
    .groups = "drop"
  ) %>%
  arrange(periodo_de_ingresso)

print(concluintes_comparativo)

# =====================================================
# 4. Gráfico Comparativo LADO A LADO — Currículos 1999 vs 2017
# =====================================================
g_comp <- ggplot(concluintes_comparativo,
                 aes(x = periodo_de_ingresso,
                     y = concluintes,
                     fill = factor(curriculo))) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           alpha = 0.85) +
  
  geom_text(aes(label = concluintes),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3.5) +
  
  labs(
    title = "Comparativo de Concluintes por Período de Ingresso\nCurrículos 1999 vs 2017 (2011–2023)",
    x = "Período de Ingresso",
    y = "Número de Concluintes",
    fill = "Currículo"
  ) +
  
  scale_fill_manual(values = c("1999" = "#2E86AB", "2017" = "#AA3E39")) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(face = "plain")
  )

print(g_comp)

ggsave("comparativo_concluintes_periodo_curriculo.png",
       g_comp, width = 14, height = 7, dpi = 300)
