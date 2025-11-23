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
    cat("Colunas (originais):\n")
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
# 3. Carregamento e Padronização dos Dados
# =====================================================

pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"
tabelas <- carregar_tabelas(pasta_dados)

cat("Tabelas carregadas:\n")
print(names(tabelas))

# Carregar e padronizar nomes da tabela principal
alunos_final <- tabelas[["alunos-final"]] %>%
  clean_names()  # Converte "Tipo de Evasao" → "tipo_de_evasao", etc.

cat("\nNomes das colunas após padronização (snake_case):\n")
print(colnames(alunos_final))

# =====================================================
# 4. Filtrar Egressos (Graduados)
# =====================================================

egressos <- alunos_final %>%
  filter(status == "INATIVO", 
         tipo_de_evasao == "GRADUADO") %>%
  select(cpf, matricula, idade_aproximada_no_ingresso)

cat("Total de egressos graduados identificados:", nrow(egressos), "\n")

# =====================================================
# 5. Distribuição por Idade no Ingresso
# =====================================================

distribuicao_idade <- egressos %>%
  group_by(idade_aproximada_no_ingresso) %>%
  summarise(
    quantidade_egressos = n(),
    percentual = round((n() / nrow(egressos)) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(idade_aproximada_no_ingresso) %>%
  rename(idade = idade_aproximada_no_ingresso)

# =====================================================
# 6. Exibir Tabela Resumida
# =====================================================

cat("\nDistribuição de Idade dos Estudantes Egressos:\n")
cat("==============================================\n")
print(distribuicao_idade)

# =====================================================
# 7. Estatísticas Descritivas
# =====================================================

cat("\n\nESTATÍSTICAS DESCRITIVAS:\n")
cat("========================\n")
cat("Total de Egressos:", nrow(egressos), "\n")
cat("Idade Mínima:", min(egressos$idade_aproximada_no_ingresso, na.rm = TRUE), "\n")
cat("Idade Máxima:", max(egressos$idade_aproximada_no_ingresso, na.rm = TRUE), "\n")
cat("Idade Média:", round(mean(egressos$idade_aproximada_no_ingresso, na.rm = TRUE), 2), "\n")
cat("Idade Mediana:", median(egressos$idade_aproximada_no_ingresso, na.rm = TRUE), "\n")

# Idade com maior frequência (pico)
pico_idade <- distribuicao_idade %>%
  filter(quantidade_egressos == max(quantidade_egressos)) %>%
  pull(idade)

cat("Idade com Maior Frequência:", pico_idade, "\n")
cat("Quantidade no Pico:", 
    distribuicao_idade %>% filter(idade == pico_idade) %>% pull(quantidade_egressos), "\n")

# =====================================================
# 8. Gráfico de Barras
# =====================================================

grafico <- ggplot(distribuicao_idade, aes(x = factor(idade), y = quantidade_egressos)) +
  geom_bar(stat = "identity", fill = "#2E86AB", color = "#1A4D7A", alpha = 0.8) +
  geom_text(
    aes(label = paste0(quantidade_egressos, "\n(", percentual, "%)")), 
    vjust = -0.5, size = 3.5, color = "#333333"
  ) +
  labs(
    title = "Distribuição de Idade dos Estudantes Egressos",
    subtitle = "Estudantes com Status Inativo e Tipo de Evasão Graduado",
    x = "Idade no Ingresso",
    y = "Quantidade de Egressos",
    caption = "Fonte: Dados do Sistema Acadêmico - Ciência da Computação UFCG"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#666"),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "#EEEEEE"),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

print(grafico)

# =====================================================
# 9. Salvar Gráfico
# =====================================================

ggsave("distribuicao_idade_egressos.png", 
       plot = grafico, 
       width = 12, 
       height = 7, 
       dpi = 300, 
       units = "in")

cat("\n✓ Gráfico salvo como 'distribuicao_idade_egressos.png'\n")

# =====================================================
# 10. Análise Complementar – Faixa Etária Esperada
# =====================================================

cat("\n\nANÁLISE DE FAIXA ESPERADA:\n")
cat("==========================\n")
cat("Ingressantes típicos: 17-20 anos\n")
cat("Duração nominal do curso: 4,5 anos\n")
cat("Faixa esperada de conclusão: 21-24 anos\n")

egressos_faixa_esperada <- egressos %>%
  mutate(faixa = case_when(
    idade_aproximada_no_ingresso >= 17 & idade_aproximada_no_ingresso <= 20 ~ "17-20 (Esperado)",
    TRUE ~ "Fora da Faixa"
  )) %>%
  group_by(faixa) %>%
  summarise(
    quantidade = n(), 
    percentual = round((n() / nrow(egressos)) * 100, 2),
    .groups = "drop"
  )

print(egressos_faixa_esperada)