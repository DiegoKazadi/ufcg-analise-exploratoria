# ============================================================================
# CAPÍTULO 4 - SCRIPT 1: PREPARAÇÃO E CARREGAMENTO DOS DADOS
# ============================================================================
# Descrição: Carrega e prepara os dados da população de estudo
# Filtros: Currículo 1999 (2011.1-2017.2) e Currículo 2017 (2018.1-2023.1)
# Total esperado: 1.440 alunos (795 + 645)
# ============================================================================

# Carregar bibliotecas necessárias
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Limpar ambiente
rm(list = ls())

# Carregar dados
alunos_final <- read_csv("alunos_final.csv")

# Função para extrair ano e semestre do período
extrair_ano_semestre <- function(periodo) {
  ano <- as.numeric(substr(periodo, 1, 4))
  semestre <- as.numeric(substr(periodo, 6, 6))
  list(ano = ano, semestre = semestre, periodo_num = ano * 10 + semestre)
}

# Preparar dados com filtros e campos calculados
cap4_alunos_filtrados <- alunos_final %>%
  mutate(
    # Extrair ano e semestre
    Ano_Ingresso = as.numeric(substr(Periodo_de_Ingresso, 1, 4)),
    Semestre_Ingresso = as.numeric(substr(Periodo_de_Ingresso, 6, 6)),
    Periodo_Numerico = Ano_Ingresso * 10 + Semestre_Ingresso,
    
    # Converter idade para numérico
    Idade = as.numeric(Idade_Aproximada_no_Ingresso),
    
    # Criar flag de escopo
    Escopo_Curr1999 = ifelse(Curriculo == "1999" & 
                               Periodo_Numerico >= 20111 & 
                               Periodo_Numerico <= 20172, 1, 0),
    Escopo_Curr2017 = ifelse(Curriculo == "2017" & 
                               Periodo_Numerico >= 20181 & 
                               Periodo_Numerico <= 20231, 1, 0),
    No_Escopo = ifelse(Escopo_Curr1999 == 1 | Escopo_Curr2017 == 1, 1, 0),
    
    # Criar faixa etária
    Faixa_Etaria = case_when(
      Idade <= 18 ~ "17-18",
      Idade <= 20 ~ "19-20",
      Idade <= 22 ~ "21-22",
      Idade <= 25 ~ "23-25",
      TRUE ~ "26+"
    ),
    
    # Agrupar formas de ingresso
    Forma_Agrupada = case_when(
      Forma_de_Ingresso == "SISU" ~ "SISU",
      Forma_de_Ingresso == "VESTIBULAR  ENEM" ~ "VESTIBULAR/ENEM",
      Forma_de_Ingresso == "TRANSFERENCIA" ~ "TRANSFERÊNCIA",
      Forma_de_Ingresso == "REOPCAO" ~ "REOPÇÃO",
      Forma_de_Ingresso == "GRADUADO" ~ "GRADUADO",
      TRUE ~ "OUTROS"
    ),
    
    # Flags binárias
    Eh_Graduado = ifelse(Tipo_de_Evasao == "GRADUADO", 1, 0),
    Eh_Evadido = ifelse(Status == "INATIVO" & Tipo_de_Evasao != "GRADUADO", 1, 0),
    
    # Tipo de cota
    Tipo_Cota = ifelse(Cota == "-", "Ampla Concorrência", "Cotista"),
    
    # Simplificar tipo de evasão
    Tipo_Evasao_Simples = case_when(
      Tipo_de_Evasao == "CANCELAMENTO POR ABANDONO" ~ "Abandono",
      Tipo_de_Evasao == "CANCELADO REPROVOU TODAS POR FALTAS" ~ "Reprovou por Faltas",
      Tipo_de_Evasao == "CANCELADO 3 REPROV MESMA DISCIPLINA" ~ "3 Reprov Mesma Disc",
      Tipo_de_Evasao == "CANCELAMENTO P SOLICITACAO ALUNO" ~ "Solicitação Aluno",
      Tipo_de_Evasao == "CANCELADO NOVO INGRESSO OUTRO CURSO" ~ "Outro Curso",
      Tipo_de_Evasao == "GRADUADO" ~ "GRADUADO",
      TRUE ~ "Outros"
    )
  ) %>%
  # Filtrar apenas alunos no escopo
  filter(No_Escopo == 1)

# Verificar totais
cat("\n=== VERIFICAÇÃO DOS DADOS FILTRADOS ===\n")
cat("Total de alunos no escopo:", nrow(cap4_alunos_filtrados), "\n\n")

totais_por_curriculo <- cap4_alunos_filtrados %>%
  group_by(Curriculo) %>%
  summarise(
    Total_Alunos = n(),
    Total_Graduados = sum(Eh_Graduado),
    Total_Evadidos = sum(Eh_Evadido),
    Taxa_Graduacao = round(sum(Eh_Graduado) / n() * 100, 1),
    Taxa_Evasao = round(sum(Eh_Evadido) / n() * 100, 1)
  )

print(totais_por_curriculo)

# Salvar dados preparados
write_csv(cap4_alunos_filtrados, "cap4_alunos_filtrados.csv")
saveRDS(cap4_alunos_filtrados, "cap4_alunos_filtrados.rds")

cat("\n✓ Dados preparados e salvos com sucesso!\n")