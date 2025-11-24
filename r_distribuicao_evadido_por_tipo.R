# =====================================================
# 1. Pacotes
# =====================================================

library(readr)
library(dplyr)
library(tibble)
library(janitor)
library(ggplot2)

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
# 3. Função — distribuição por tipo de evasão
# =====================================================

analise_tipo_evasao <- function(df) {
  
  distribuicao <- df %>%
    group_by(tipo_de_evasao) %>%
    summarise(
      quantidade = n(),
      percentual = round((n() / nrow(df)) * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(desc(quantidade))
  
  print(distribuicao)
  
  # gráfico vertical
  g <- ggplot(distribuicao, aes(x = reorder(tipo_de_evasao, quantidade), 
                                y = quantidade)) +
    geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.9) +
    
    # Percentual dentro das barras
    geom_text(aes(label = paste0(percentual, "%")),
              vjust = 1.5, color = "white", size = 4) +
    
    labs(
      title = "Distribuição dos Inativos por Tipo de Evasão",
      x = "Tipo de Evasão",
      y = "Quantidade"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      text = element_text(face = "plain"),       # tira negrito
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(g)
  
  ggsave(
    filename = "distribuicao_tipo_evasao_inativos.png",
    plot = g,
    width = 12, height = 7, dpi = 300
  )
  
  return(distribuicao)
}

# =====================================================
# 4. Filtrar INATIVOS não graduados
# =====================================================

inativos_nao_graduados <- alunos_final %>%
  filter(status == "INATIVO", tipo_de_evasao != "GRADUADO")

# =====================================================
# 5. Gerar tabela e gráfico
# =====================================================

dist_tipo_evasao <- analise_tipo_evasao(inativos_nao_graduados)

