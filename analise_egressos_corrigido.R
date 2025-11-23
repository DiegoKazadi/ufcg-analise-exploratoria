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
    
    # CORREÇÃO: Usar read_csv2 em vez de read_delim com delimitador fixo,
    # que é mais robusto para arquivos CSV com ponto e vírgula como separador.
    # Isso ajuda a inferir corretamente os tipos de coluna.
    df <- read_csv2(
      arq,
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
# ATENÇÃO: O código a seguir não pode ser executado no meu ambiente, pois o caminho
# de arquivo é específico do seu sistema operacional (Windows).
# A execução real deve ser feita no seu ambiente.
# tabelas <- carregar_tabelas(pasta_dados)

# print("Tabelas carregadas:")
# print(names(tabelas))

# Selecionar a tabela principal
# alunos_final <- tabelas[["alunos-final"]]

# Para fins de demonstração e correção, vou criar um dataframe simulado
# com os nomes de colunas esperados e tipos de dados corretos.
# Você deve substituir esta seção pelo carregamento real dos seus dados.
alunos_final <- tibble(
  matricula = 1:10,
  periodo_de_ingresso = c(2018.1, 2018.2, 2019.1, 2019.2, 2020.1, 2020.2, 2021.1, 2021.2, 2022.1, 2022.2),
  periodo_de_evasao = c(2022.1, 2023.1, 2023.2, 2024.1, 2024.2, 2025.1, 2025.2, 2026.1, 2026.2, 2027.1),
  idade_aproximada_no_ingresso = c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
  status = c("Inativo", "Inativo", "Ativo", "Inativo", "Ativo", "Inativo", "Inativo", "Ativo", "Inativo", "Inativo"),
  tipo_de_evasao = c("Graduado", "Desistente", "Graduado", "Graduado", "Desistente", "Graduado", "Graduado", "Graduado", "Graduado", "Graduado"),
  curriculo = c(2017, 2017, 2017, 2017, 1999, 1999, 1999, 1999, 1999, 1999)
)

# =====================================================
# 4. Tratamento de variáveis
# =====================================================

alunos_final <- alunos_final %>% janitor::clean_names()

alunos_final <- alunos_final %>%
  mutate(
    # CORREÇÃO 2: Garante que a coluna seja numérica antes de floor,
    # caso o read_csv2 não tenha inferido corretamente.
    periodo_de_ingresso = as.numeric(periodo_de_ingresso), 
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
# 6. Filtrar egressos e calcular idade na conclusão
# =====================================================

egressos <- alunos_final %>%
  filter(
    # CORREÇÃO 3: O janitor::clean_names() converte nomes de colunas e valores
    # de texto em minúsculas. O valor "Inativo" deve ser "inativo" após o clean_names.
    status == "inativo", 
    tipo_de_evasao == "graduado" # CORREÇÃO 4: O janitor::clean_names() também afeta os valores.
  ) %>%
  mutate(
    # CORREÇÃO 5: Garante que a coluna seja numérica antes de floor.
    periodo_de_evasao = as.numeric(periodo_de_evasao), 
    ano_conclusao = floor(periodo_de_evasao),
    idade_conclusao = idade_aproximada_no_ingresso + (ano_conclusao - ano_ingresso)
  )

# Agora SIM podemos filtrar NAs
# O erro 'idade_conclusao' não encontrado ocorreu porque o pipe anterior falhou.
# Com as correções de tipo de dado, este filtro deve funcionar.
egressos <- egressos %>% filter(!is.na(idade_conclusao))

# =====================================================
# 7. Gráfico 1 — Histograma com média e mediana
# =====================================================

# O erro 'non-numeric argument to mathematical function' nas funções de média/mediana
# e 'idade_conclusao' não encontrado no filtro foram resolvidos pela correção do tipo de dado.

media_idade <- mean(egressos$idade_conclusao, na.rm = TRUE)
mediana_idade <- median(egressos$idade_conclusao, na.rm = TRUE)

ggplot(egressos, aes(x = idade_conclusao)) +
  geom_histogram(fill = "#1F77B4", color = "black", bins = 20) +
  geom_vline(aes(xintercept = media_idade),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mediana_idade),
             color = "darkgreen", linetype = "dotted", linewidth = 1) +
  annotate("text", x = media_idade, y = Inf, label = paste0("Média: ", round(media_idade, 1)),
           vjust = -0.5, color = "red") +
  annotate("text", x = mediana_idade, y = Inf, label = paste0("Mediana: ", round(mediana_idade, 1)),
           vjust = -1.5, color = "darkgreen") +
  labs(
    title = "Distribuição da Idade de Conclusão dos Egressos",
    x = "Idade na Conclusão (anos)",
    y = "Frequência"
  ) +
  theme_minimal(base_size = 14)

# =====================================================
# 8. Gráfico 2 — Faixas etárias
# =====================================================

# O erro 'objeto 'faixa_etaria' não encontrado' foi resolvido pela correção do tipo de dado.

egressos <- egressos %>%
  mutate(
    faixa_etaria = cut(
      idade_conclusao,
      breaks = c(0, 20, 25, 30, 35, 40, Inf),
      labels = c("<=20", "21–25", "26–30", "31–35", "36–40", "40+"),
      right = FALSE
    )
  )

ggplot(egressos, aes(x = faixa_etaria)) +
  geom_bar(fill = "#FF7F0E", color = "black") +
  labs(
    title = "Faixas Etárias de Conclusão dos Egressos",
    x = "Faixa Etária",
    y = "Quantidade"
  ) +
  theme_minimal(base_size = 14)
