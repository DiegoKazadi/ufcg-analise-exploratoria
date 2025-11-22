# Pacotes
library(readr)  
library(dplyr)   
library(tibble) 

# Função para carregar e visualizar tabelas
carregar_tabelas <- function(pasta) {
  
  # Lista todos os arquivos CSV na pasta
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo CSV encontrado na pasta.")
  }
  
  cat("Arquivos encontrados:\n")
  print(basename(arquivos))
  
  # Cria lista vazia para armazenar as tabelas
  tabelas <- list()
  
  # Carrega cada arquivo
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq)) # nome do arquivo sem extensão
    
    # Ler CSV com separador ";"
    df <- read_delim(arq, delim = ";", show_col_types = FALSE,
                     locale = locale(decimal_mark = ".", grouping_mark = ",")) 
    
    # Guardar na lista
    tabelas[[nome]] <- df
    
    # Exibir resumo no console
    cat("\n=============================\n")
    cat("Tabela:", nome, "\n")
    cat("Dimensões:", dim(df)[1], "linhas x", dim(df)[2], "colunas\n")
    cat("Colunas:\n")
    print(colnames(df))
    cat("Visualização inicial:\n")
    print(head(df, 5))  # mostra as 5 primeiras linhas
    
    # Mostrar possíveis problemas de parsing
    problemas <- problems(df)
    if (nrow(problemas) > 0) {
      cat("\n⚠️ Problemas encontrados (mostrando até 5):\n")
      print(head(problemas, 5))
    }
    
    cat("=============================\n")
  }
  
  return(tabelas)
}

# Usando a função no seu caminho (Windows)
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

tabelas <- carregar_tabelas(pasta_dados)

# Função de pré-processamento coletivo
preprocess_coletivo <- function(df) {
  df %>%
    janitor::clean_names() %>%
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "byte") %>% trimws()
    ))
}


# Pré-processamento coletivo (já implementado)
tabelas <- lapply(tabelas, preprocess_coletivo)

# Mostrar nomes das tabelas carregadas
print(names(tabelas))

# Pré-processamento individual (se necessário em casos específicos)r
preprocess_individual <- function(df) {
  df %>%
    janitor::clean_names() %>% 
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "byte") %>% trimws()
    ))
}

# Carregar apenas a tabela de interesse
alunos_final <- tabelas[["alunos-final"]]


#
# Enriquecimento dos dados
# 

# Garantir ano e semestre separados
alunos_final <- alunos_final %>%
  mutate(
    ano_ingresso = floor(periodo_de_ingresso),
    semestre_ingresso = ifelse(periodo_de_ingresso %% 1 == 0.1, 1, 2)
  )

# =====================================================
# Verificação de integridade
# =====================================================

# Verificar se há currículos em anos incompatíveis
check_inconsistencias <- alunos_final %>%
  filter((ano_ingresso < 2018 & curriculo == 2017) |
           (ano_ingresso >= 2018 & curriculo == 1999)) %>%
  distinct(matricula, ano_ingresso, curriculo)

# Mostrar inconsistências encontradas
if (nrow(check_inconsistencias) > 0) {
  cat("️ Inconsistências encontradas:\n")
  print(check_inconsistencias)
} else {
  cat(" Nenhuma inconsistência encontrada.\n")
}
