# ======================================================
# FIGURA — DISTRIBUIÇÃO DE ALUNOS ATIVOS POR PERÍODO
# ======================================================

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# ======================================================
# 1 — Garantir variável situacao_final
# ======================================================

base_analitica <- alunos_padrao %>%
  mutate(
    situacao_final = case_when(
      status == "ATIVO" ~ "ATIVO",
      tipo_de_evasao == "GRADUADO" ~ "CONCLUIDO",
      tipo_de_evasao %in% c(
        "CANCELAMENTO POR ABANDONO",
        "CANCELAMENTO P SOLICITACAO ALUNO"
      ) ~ "EVADIDO",
      TRUE ~ "OUTROS"
    ),
    grupo_curricular = case_when(
      curriculo == "1999" ~ "Currículo 1999",
      curriculo == "2017" ~ "Currículo 2017",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(grupo_curricular %in% c("Currículo 1999","Currículo 2017"))

# ======================================================
# 2 — Calcular proporção de ativos por período de ingresso
# ======================================================

ativos_periodo <- base_analitica %>%
  group_by(grupo_curricular, periodo_de_ingresso) %>%
  summarise(
    total_alunos = n(),
    ativos = sum(situacao_final == "ATIVO"),
    proporcao_ativos = ativos / total_alunos,
    .groups = "drop"
  )

# ======================================================
# 3 — Ordenar eixo X cronologicamente
# ======================================================

ativos_periodo <- ativos_periodo %>%
  mutate(
    ano  = as.numeric(str_sub(periodo_de_ingresso, 1, 4)),
    sem  = as.numeric(str_sub(periodo_de_ingresso, 6, 6))
  ) %>%
  arrange(ano, sem) %>%
  mutate(
    periodo_de_ingresso = factor(periodo_de_ingresso,
                                 levels = unique(periodo_de_ingresso))
  )

# ======================================================
# 4 — GRÁFICO FINAL (DUAS CURVAS)
# ======================================================

ggplot(ativos_periodo,
       aes(x = periodo_de_ingresso,
           y = proporcao_ativos,
           color = grupo_curricular,
           group = grupo_curricular)) +
  
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.8) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  labs(
    title = "Alunos ativos por período de ingresso",
    x = "Período de Ingresso",
    y = "Proporção de Alunos Ativos (%)",
    color = "Currículo"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
