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
# 3. Filtrar concluintes 2011–2023
# =====================================================
concluintes_comp <- alunos_final %>%
  filter(tipo_de_evasao == "GRADUADO") %>%
  filter(as.numeric(substr(periodo_de_ingresso, 1, 4)) >= 2011,
         as.numeric(substr(periodo_de_ingresso, 1, 4)) <= 2023) %>%
  group_by(periodo_de_ingresso, curriculo) %>%
  summarise(concluintes = n(), .groups = "drop")

# Ordenar períodos corretamente
concluintes_comp$periodo_de_ingresso <- factor(
  concluintes_comp$periodo_de_ingresso,
  levels = sort(unique(concluintes_comp$periodo_de_ingresso))
)

# =====================================================
# 4. Gráfico FACETADO — Currículo 1999 (azul) e 2017 (laranja)
# =====================================================

cores <- c("1999" = "#083C5A",    # azul escuro
           "2017" = "#A14000")    # laranja escuro

g_facet <- ggplot(concluintes_comp,
                  aes(x = periodo_de_ingresso, 
                      y = concluintes, 
                      fill = factor(curriculo))) +
  
  geom_bar(stat = "identity",
           width = 0.7,                # barras mais largas
           alpha = 0.9) +
  
  geom_text(aes(label = concluintes),
            vjust = -0.3,
            size = 3.5) +
  
  facet_wrap(~ curriculo, ncol = 1, scales = "free_y") +  # um em cima do outro
  
  scale_fill_manual(values = cores) +
  
  labs(
    title = "Distribuição de Alunos Graduados por Currículo",
    x = "Período de Ingresso",
    y = "Número de Concluintes",
    fill = "Currículo"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 14, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(face = "plain")
  )

print(g_facet)

ggsave("comparativo_concluintes_facetado.png",
       g_facet, width = 16, height = 10, dpi = 300)


# =====================================================
# Estatísticas gerais dos concluintes por currículo
# =====================================================
estatisticas_curriculo <- concluintes_comp %>%
  group_by(curriculo) %>%
  summarise(
    total_concluintes = sum(concluintes),
    media_por_periodo = round(mean(concluintes), 2),
    maximo = max(concluintes),
    minimo = min(concluintes),
    numero_de_periodos = n(),
    .groups = "drop"
  )

print(estatisticas_curriculo)

# =====================================================
# Estatísticas comparativas entre currículos
# =====================================================
estatisticas_comparativas <- estatisticas_curriculo %>%
  select(curriculo, total_concluintes) %>%
  tidyr::pivot_wider(
    names_from = curriculo,
    values_from = total_concluintes
  ) %>%
  mutate(
    diferenca_total = `2017` - `1999`,
    percentual_relativo = round((`2017` / `1999`) * 100, 2)
  )

print(estatisticas_comparativas)

# =====================================================
# Tabela final juntando tudo
# =====================================================
tabela_final_estatisticas <- estatisticas_curriculo %>%
  left_join(
    estatisticas_comparativas %>%
      pivot_longer(cols = c("1999", "2017"), names_to = "curr", values_to = "tot"),
    by = c("curriculo" = "curr")
  ) %>%
  select(
    Currículo = curriculo,
    Total_Concluintes = total_concluintes,
    Média_por_Período = media_por_periodo,
    Máximo = maximo,
    Mínimo = minimo
  )

print(tabela_final_estatisticas)

# SALVAR
write.csv(tabela_final_estatisticas,
          "estatisticas_concluintes_2011_2023.csv",
          row.names = FALSE)

