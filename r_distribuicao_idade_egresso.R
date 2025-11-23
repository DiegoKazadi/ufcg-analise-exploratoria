# =====================================================
# 1. Pacotes
# =====================================================

library(readr)
library(dplyr)
library(tibble)
library(janitor)
library(ggplot2)

# =====================================================
# 2. Função para carregar e visualizar tabelas CSV
# =====================================================

carregar_tabelas <- function(pasta) {
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo CSV encontrado na pasta.")
  }
  
  cat("Arquivos encontrados:\n")
  print(basename(arquivos))
  
  tabelas <- list()
  
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq))
    
    df <- read_delim(
      arq,
      delim = ";",
      show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
    
    tabelas[[nome]] <- df
    
    cat("\n=============================\n")
    cat("Tabela:", nome, "\n")
    cat("Dimensões:", dim(df)[1], "linhas x", dim(df)[2], "colunas\n")
    cat("Colunas:\n")
    print(colnames(df))
    cat("Visualização inicial:\n")
    print(head(df, 5))
    
    problemas <- problems(df)
    if (nrow(problemas) > 0) {
      cat("\n⚠️ Problemas encontrados (mostrando até 5):\n")
      print(head(problemas, 5))
    }
    cat("=============================\n")
  }
  
  return(tabelas)
}

# =====================================================
# 3. Carregamento dos dados
# =====================================================

pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"
tabelas <- carregar_tabelas(pasta_dados)

print("Tabelas carregadas:")
print(names(tabelas))

# Selecionar a tabela principal
alunos_final <- tabelas[["alunos-final"]]

# =====================================================
# 4. Tratamento de variáveis
# =====================================================

alunos_final <- alunos_final %>% janitor::clean_names()

alunos_final <- alunos_final %>%
  mutate(
    ano_ingresso = floor(periodo_de_ingresso),
    semestre_ingresso = ifelse(periodo_de_ingresso %% 1 == 0.1, 1, 2)
  )

# =====================================================
# 5. Verificação de inconsistências curriculares
# =====================================================

check_inconsistencias <- alunos_final %>%
  filter(
    (ano_ingresso < 2018 & curriculo == 2017) |
      (ano_ingresso >= 2018 & curriculo == 1999)
  ) %>%
  distinct(matricula, ano_ingresso, curriculo)

if (nrow(check_inconsistencias) > 0) {
  cat("⚠️ Inconsistências encontradas:\n")
  print(check_inconsistencias)
} else {
  cat("✅ Nenhuma inconsistência encontrada.\n")
}

# Filtrar apenas egressos (graduados)
egressos <- df %>%
  filter(status == "Inativo",
         tipo_evasao == "Graduado") %>%
  mutate(
    data_nascimento = as.Date(data_nascimento),
    data_conclusao = as.Date(data_conclusao),
    
    # Cálculo da idade de conclusão
    idade_conclusao = as.numeric(difftime(data_conclusao, data_nascimento, units = "days")) / 365.25
  )

# Gráfico
ggplot(egressos, aes(x = idade_conclusao)) +
  geom_histogram(color = "black", fill = "#1F77B4", bins = 20) +
  labs(
    title = "Distribuição da Idade de Conclusão dos Egressos",
    x = "Idade na Conclusão (anos)",
    y = "Frequência"
  ) +
  theme_minimal(base_size = 14)
