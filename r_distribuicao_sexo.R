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
    
    # Exibir metadados da tabela
    cat("\n=============================\n")
    cat("Tabela:", nome, "\n")
    cat("Dimensões:", dim(df)[1], "linhas x", dim(df)[2], "colunas\n")
    cat("Colunas:\n")
    print(colnames(df))
    cat("Visualização inicial:\n")
    print(head(df, 5))
    
    # Verificar problemas na leitura
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

# Verificar nomes das tabelas carregadas
print("Tabelas carregadas:")
print(names(tabelas))

# Selecionar a tabela principal
alunos_final <- tabelas[["alunos-final"]]

# =====================================================
# 4. Tratamento de variáveis
# =====================================================

# Extrair ano e semestre de ingresso a partir de 'periodo_de_ingresso'
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

# =====================================================
# 6. Padronização e análise da variável SEXO
# =====================================================

# Padronizar valores da variável 'sexo'
alunos_final <- alunos_final %>%
  mutate(sexo = case_when(
    sexo %in% c("MASCULINO", "M") ~ "M",
    sexo %in% c("FEMININO", "F") ~ "F",
    TRUE ~ NA_character_
  ))

# Contagem por sexo
dist_sexo <- alunos_final %>%
  count(sexo, name = "quantidade")

print("Distribuição por sexo:")
print(dist_sexo)

# =====================================================
# 7. Visualização: Gráfico de barras – Figura 4.2
# =====================================================

grafico_sexo <- ggplot(dist_sexo, aes(x = sexo, y = quantidade, fill = sexo)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(
    values = c("M" = "#1F77B4", "F" = "#DAA520"),
    labels = c("M" = "Masculino", "F" = "Feminino")
  ) +
  labs(
    title = "Distribuição dos Estudantes por Sexo",
    x = "Sexo",
    y = "Quantidade de Estudantes",
    fill = "Legenda"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# Exibir o gráfico
print(grafico_sexo)