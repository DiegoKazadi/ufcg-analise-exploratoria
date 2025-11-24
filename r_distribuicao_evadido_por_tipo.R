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
  
  if (length(arquivos) == 0) stop("Nenhum arquivo CSV encontrado.")
  
  cat("Arquivos encontrados:\n")
  print(basename(arquivos))
  
  tabelas <- list()
  
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq))
    
    df <- read_delim(
      arq, delim = ";", show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
    
    tabelas[[nome]] <- df
    
    cat("\n=============================\n")
    cat("Tabela:", nome, "\n")
    cat("Dimensões:", nrow(df), "x", ncol(df), "\n")
    print(colnames(df))
    print(head(df, 5))
  }
  
  return(tabelas)
}

# =====================================================
# 3. Carregar e Padronizar
# =====================================================

pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

tabelas <- carregar_tabelas(pasta_dados)

alunos_final <- tabelas[["alunos-final"]] %>% clean_names()

print(colnames(alunos_final))

# =====================================================
# 4. Função auxiliar para gerar análise por idade
# =====================================================

analise_idade <- function(df, titulo_status) {
  
  distribuicao <- df %>%
    group_by(idade_aproximada_no_ingresso) %>%
    summarise(
      quantidade = n(),
      percentual = round((n() / nrow(df)) * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(idade_aproximada_no_ingresso) %>%
    rename(idade = idade_aproximada_no_ingresso)
  
  cat("\n=====================================================\n")
  cat("Distribuição de Idade —", titulo_status, "\n")
  cat("=====================================================\n")
  print(distribuicao)
  
  # Estatísticas
  cat("\nESTATÍSTICAS DESCRITIVAS —", titulo_status, "\n")
  cat("Total:", nrow(df), "\n")
  cat("Mínimo:", min(df$idade_aproximada_no_ingresso, na.rm = TRUE), "\n")
  cat("Máximo:", max(df$idade_aproximada_no_ingresso, na.rm = TRUE), "\n")
  cat("Média:", round(mean(df$idade_aproximada_no_ingresso, na.rm = TRUE), 2), "\n")
  cat("Mediana:", median(df$idade_aproximada_no_ingresso, na.rm = TRUE), "\n")
  
  pico <- distribuicao %>% filter(quantidade == max(quantidade)) %>% pull(idade)
  cat("Idade mais frequente:", pico, "\n")
  
  # Gráfico
  grafico <- ggplot(distribuicao, aes(x = factor(idade), y = quantidade)) +
    geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.8) +
    geom_text(aes(label = quantidade), vjust = -0.4, size = 3.5) +
    labs(
      title = paste("Distribuição de Idade —", titulo_status),
      x = "Idade no Ingresso",
      y = "Quantidade"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(grafico)
  
  ggsave(
    filename = paste0("idade_", gsub(" ", "_", tolower(titulo_status)), ".png"),
    plot = grafico,
    width = 12, height = 7, dpi = 300
  )
  
  return(distribuicao)
}

# =====================================================
# 5. Egressos
# =====================================================

egressos <- alunos_final %>%
  filter(status == "INATIVO", tipo_de_evasao == "GRADUADO") %>%
  select(cpf, matricula, idade_aproximada_no_ingresso)

dist_egressos <- analise_idade(egressos, "Egressos (Graduados)")

# =====================================================
# 6. Evadidos
# =====================================================

evadidos <- alunos_final %>%
  filter(status == "INATIVO", tipo_de_evasao != "GRADUADO") %>%
  select(cpf, matricula, idade_aproximada_no_ingresso)

dist_evadidos <- analise_idade(evadidos, "Evadidos")

# =====================================================
# 7. Ingressantes (todos da base)
# =====================================================

ingressantes <- alunos_final %>%
  select(cpf, matricula, idade_aproximada_no_ingresso)

dist_ingressantes <- analise_idade(ingressantes, "Todos os Ingressantes")

