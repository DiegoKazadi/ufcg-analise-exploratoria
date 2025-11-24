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
# Filtrar apenas evadidos
# =====================================================
evadidos <- alunos_final %>%
  filter(status == "INATIVO") %>% 
  filter(tipo_de_evasao != "GRADUADO") %>%
  filter(curriculo %in% c("1999", "2017")) %>%
  filter(!is.na(idade_aproximada_no_ingresso),
         !is.na(periodo_de_ingresso),
         !is.na(periodo_de_evasao))

# =====================================================
# Converter períodos para numérico
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
    tempo_ate_evasao = deslig_num - ingresso_num,
    idade_evasao = idade_aproximada_no_ingresso + tempo_ate_evasao
  )

# =====================================================
# Média da idade no momento da evasão
# =====================================================
idade_media <- evadidos %>%
  group_by(curriculo) %>%
  summarise(
    idade_media_evasao = round(mean(idade_evasao), 2),
    .groups = "drop"
  ) %>%
  mutate(curriculo = factor(curriculo,
                            levels = c("1999","2017"),
                            labels = c("Currículo 1999","Currículo 2017")))

# =====================================================
# Gráfico
# =====================================================
cores_idade <- c("Currículo 1999" = "#6AA3C8",
                 "Currículo 2017" = "#E2A46F")

g_idade <- ggplot(idade_media,
                  aes(x = curriculo,
                      y = idade_media_evasao,
                      fill = curriculo)) +
  geom_col(width = 0.6, alpha = 0.95) +
  geom_text(aes(label = paste0(idade_media_evasao, " anos")),
            vjust = -0.4,
            size = 4) +
  scale_fill_manual(values = cores_idade) +
  labs(
    title = "Idade Média no Momento da Evasão por Currículo",
    x = "Currículo",
    y = "Idade Média (anos)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 13)

print(g_idade)

ggsave("idade_media_evasao_curriculos.png",
       g_idade, width = 10, height = 6, dpi = 300)
