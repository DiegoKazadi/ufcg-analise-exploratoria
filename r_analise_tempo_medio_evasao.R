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
# 3. Criar função para converter período para número decimal
# =====================================================

converter_periodo <- function(p) {
  ano <- as.numeric(substr(p, 1, 4))
  sem <- as.numeric(substr(p, 6, 6))
  return(ano + (sem - 1)/2)
}

# =====================================================
# 4. Filtrar EVADIDOS e calcular permanência
# =====================================================

evadidos <- alunos_final %>%
  filter(tipo_de_evasao != "GRADUADO",
         !is.na(periodo_de_evasao),
         !is.na(periodo_de_ingresso)) %>%
  mutate(
    ingresso_num = converter_periodo(periodo_de_ingresso),
    evasao_num = converter_periodo(periodo_de_evasao),
    tempo_ate_evasao = evasao_num - ingresso_num
  ) %>%
  filter(tempo_ate_evasao >= 0)

# =====================================================
# 5. Estatísticas por currículo
# =====================================================

tme_curriculo <- evadidos %>%
  group_by(curriculo) %>%
  summarise(
    TME = round(mean(tempo_ate_evasao), 2),
    n = n()
  )

print(tme_curriculo)

# =====================================================
# 6. Gráfico comparativo
# =====================================================

cores <- c("1999" = "#083C5A", "2017" = "#A14000")

g_tme <- ggplot(tme_curriculo, aes(x = factor(curriculo), y = TME, fill = factor(curriculo))) +
  geom_col(width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = cores) +
  geom_text(aes(label = TME), vjust = -0.4, size = 5) +
  labs(
    title = "Tempo Médio até Evasão por Currículo",
    x = "Currículo",
    y = "Tempo Médio (períodos)"
  ) +
  theme_minimal(base_size = 14)

print(g_tme)

ggsave("tempo_medio_evasao_curriculos.png",
       g_tme, width = 10, height = 6, dpi = 300)
