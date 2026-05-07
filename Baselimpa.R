.rs.restartR()
library(readr)

dados_brutos <- read_csv(
  "artigos_com_autor_brasileiro.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

library(stringr)

cols_mojibake <- names(dados_brutos)[
  sapply(dados_brutos, function(col) {
    if (!is.character(col)) return(FALSE)
    any(str_detect(col, "Ã|Â|�"), na.rm = TRUE)
  })
]

cols_mojibake

library(stringi)

limpar_unicode_polido <- function(x) {
  x <- as.character(x)
  
  # Remove caracteres de controle Unicode
  x <- stri_replace_all_regex(x, "[\\p{Cc}\\p{Cf}]", " ")
  
  # Remove restos clássicos de mojibake
  x <- stri_replace_all_regex(x, "Ã|Â|�", " ")
  
  # Remove símbolos soltos
  x <- stri_replace_all_regex(x, "[¢©®§]", " ")
  
  # Normaliza espaços
  x <- stri_replace_all_regex(x, "\\s+", " ")
  
  trimws(x)
}

library(dplyr)

dados_corrigidos <- dados_brutos %>%
  mutate(across(all_of(cols_mojibake), limpar_unicode_polido))
any(grepl("Ã|Â|�", unlist(dados_corrigidos)))
write.csv(
  dados_corrigidos,
  "artigos_base_limpa_utf8.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

#triagem temática
library(dplyr)
library(stringr)
dados_corrigidos <- dados_corrigidos %>%
  mutate(
    texto_busca = str_to_lower(
      paste(
        Title,
        `Author Keywords`,
        Abstract,
        sep = " "
      )
    )
  )

termos_violencia <- c(
  # gerais
  "violence", "violent",
  "abuse", "abusive",
  "assault", "aggression",
  "maltreatment",
  
  # português
  "violência", "violento",
  "abuso", "abusivo",
  "agressão",
  "maus-tratos", "maus tratos",
  
  # tipos específicos
  "physical violence", "psychological violence",
  "sexual violence", "verbal violence",
  "violência física", "violência psicológica",
  "violência sexual", "violência verbal"
)
termos_domestico <- c(
  # inglês
  "domestic", "intrafamilial",
  "family", "familial",
  "household", "home",
  "partner", "intimate partner",
  "spouse", "marital",
  
  # português
  "doméstica", "doméstico",
  "intrafamiliar",
  "família", "familiar",
  "domicílio", "lar",
  "conjugal", "companheiro", "cônjuge",
  
  # vítimas em contexto doméstico
  "wife", "husband", "child", "children", "elderly",
  "mulher", "homem", "criança", "crianças", "idoso", "idosa"
)
dados_corrigidos <- dados_corrigidos %>%
  mutate(
    flag_violencia = str_detect(
      texto_busca,
      str_c(termos_violencia, collapse = "|")
    ),
    
    flag_domestico = str_detect(
      texto_busca,
      str_c(termos_domestico, collapse = "|")
    )
  )
dados_corrigidos <- dados_corrigidos %>%
  mutate(
    classificacao_tematica = case_when(
      flag_violencia & flag_domestico ~ "violência doméstica",
      flag_violencia & !flag_domestico ~ "violência fora do contexto doméstico",
      TRUE ~ "fora da temática de violência"
    )
  )
table(dados_corrigidos$classificacao_tematica)

library(dplyr)
library(openxlsx)

# =====================================================
# 1. SELECIONAR ARTIGOS PARA TRIAGEM MANUAL
# =====================================================
triagem_manual <- dados_corrigidos %>%
  filter(
    classificacao_tematica %in% c(
      "violência fora do contexto doméstico",
      "fora da temática de violência"
    )
  ) %>%
  select(
    classificacao_tematica,
    Title,
    `Author Keywords`,
    Abstract
  ) %>%
  arrange(classificacao_tematica) %>%
  mutate(
    decisao_manual = "",
    observacoes = ""
  )

# =====================================================
# 2. SALVAR EM EXCEL
# =====================================================
write.xlsx(
  triagem_manual,
  file = "triagem_manual_violencia_domestica.xlsx",
  overwrite = TRUE
)

library(readxl)
library(dplyr)

triagem_manual <- read_xlsx("C:/Users/rayan/OneDrive/Documentos/triagem_manual_violencia_domestica_final.xlsx")
triagem_manter <- triagem_manual %>%
  filter(tolower(decisao_manual) == "manter") %>%
  select(Title)
nrow(triagem_manter)
artigos_mantidos_manual <- dados_corrigidos %>%
  semi_join(triagem_manter, by = "Title")
artigos_automaticos <- dados_corrigidos %>%
  filter(classificacao_tematica == "violência doméstica")
corpus_final <- bind_rows(
  artigos_automaticos,
  artigos_mantidos_manual
) %>%
  distinct(Title, .keep_all = TRUE)
table(corpus_final$classificacao_tematica)
nrow(corpus_final)
#Salvar excel
library(openxlsx)

write.xlsx(
  corpus_final,
  "corpus_final_violencia_domestica.xlsx",
  overwrite = TRUE
)
write.csv(
  corpus_final,
  "corpus_final_violencia_domestica.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


#FIM!



library(readr)

dados <- read_csv(
  "artigos_com_autor_brasileiro.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)
library(stringi)

corrigir_mojibake <- function(x) {
  x <- as.character(x)
  
  # identifica mojibake, tratando NA
  tem_mojibake <- stringi::stri_detect_regex(
    x,
    "Ã.|Â.|â€™|â€œ|â€�|â€“|â€"
  )
  
  tem_mojibake[is.na(tem_mojibake)] <- FALSE
  
  x[tem_mojibake] <- iconv(
    x[tem_mojibake],
    from = "latin1",
    to   = "UTF-8"
  )
  
  return(x)
}

dados_corrigidos <- dados %>%
  mutate(across(where(is.character), corrigir_mojibake))
any(grepl("Ã|Â|�", unlist(dados_corrigidos)))

dados_corrigidos %>%
  select(where(is.character)) %>%
  slice(1:5)

view(dados_corrigidos)
colunas_com_mojibake <- sapply(dados_corrigidos, function(col) {
  if (!is.character(col)) return(FALSE)
  any(grepl("Ã|Â|�", col), na.rm = TRUE)
})

names(colunas_com_mojibake[colunas_com_mojibake])
cols_problema <- c(
  "Authors",
  "Author full names",
  "Title",
  "Affiliations",
  "Authors with affiliations",
  "Abstract",
  "Funding Details"
)
library(stringi)

corrigir_mojibake_forte <- function(x) {
  x <- as.character(x)
  
  tem_mojibake <- stri_detect_regex(
    x,
    "Ã.|Â.|â€™|â€œ|â€�|â€“|â€|�"
  )
  tem_mojibake[is.na(tem_mojibake)] <- FALSE
  
  # Tentativa 1: Latin-1
  x1 <- x
  x1[tem_mojibake] <- iconv(
    x1[tem_mojibake],
    from = "latin1",
    to   = "UTF-8"
  )
  
  # Tentativa 2: Windows-1252 (se ainda sobrar)
  sobra <- stri_detect_regex(x1, "Ã|Â|�")
  sobra[is.na(sobra)] <- FALSE
  
  x1[sobra] <- iconv(
    x1[sobra],
    from = "Windows-1252",
    to   = "UTF-8"
  )
  
  return(x1)
}
library(dplyr)

dados_corrigidos <- dados_corrigidos %>%
  mutate(across(all_of(cols_problema), corrigir_mojibake_forte))
any(grepl("Ã|Â|�", unlist(dados_corrigidos)))

dados_corrigidos %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = all_of(cols_problema),
    names_to = "coluna",
    values_to = "texto"
  ) %>%
  filter(grepl("Ã|Â|�", texto)) %>%
  select(row_id, coluna, texto) %>%
  slice(1:10)
limpar_replacement <- function(x) {
  x <- as.character(x)
  x <- gsub("\uFFFD", " ", x)
  x
}
dados_corrigidos <- dados_corrigidos %>%
  mutate(across(all_of(cols_problema), limpar_replacement))
any(grepl("Ã|Â|�", unlist(dados_corrigidos)))

library(stringi)

corrigir_mojibake_iterativo <- function(x, max_iter = 5) {
  x <- as.character(x)
  
  for (i in seq_len(max_iter)) {
    
    tem_mojibake <- stri_detect_regex(
      x,
      "Ã.|Â.|Ãƒ|Ã‚|â€™|â€œ|â€�|â€“|â€|�"
    )
    
    tem_mojibake[is.na(tem_mojibake)] <- FALSE
    
    if (!any(tem_mojibake)) break
    
    x[tem_mojibake] <- iconv(
      x[tem_mojibake],
      from = "latin1",
      to   = "UTF-8",
      sub  = " "
    )
  }
  
  # limpeza final de resíduos unicode inválidos
  x <- gsub("\uFFFD", " ", x)
  x <- stri_replace_all_regex(x, "\\s+", " ")
  
  trimws(x)
}
dados_corrigidos <- dados_corrigidos %>%
  mutate(across(all_of(cols_problema), corrigir_mojibake_iterativo))
any(grepl("Ã|Â|�", unlist(dados_corrigidos)))
dados_corrigidos %>%
  filter(row_number() %in% c(27, 120)) %>%
  select(Title)

library(stringi)

limpar_unicode_definitivo <- function(x) {
  x <- as.character(x)
  
  # Remove caracteres de controle Unicode
  x <- stri_replace_all_regex(x, "[\\p{Cc}\\p{Cf}]", " ")
  
  # Remove restos clássicos de mojibake
  x <- stri_replace_all_regex(x, "Ã|Â|�", " ")
  
  # Normaliza espaços
  x <- stri_replace_all_regex(x, "\\s+", " ")
  
  trimws(x)
}
dados_corrigidos <- dados_corrigidos %>%
  mutate(across(all_of(cols_problema), limpar_unicode_definitivo))
any(grepl("Ã|Â|�", unlist(dados_corrigidos)))
dados_corrigidos %>%
  filter(row_number() %in% c(27, 120)) %>%
  select(Title)

limpar_unicode_polido <- function(x) {
  x <- as.character(x)
  
  x <- stri_replace_all_regex(x, "[\\p{Cc}\\p{Cf}]", " ")
  x <- stri_replace_all_regex(x, "Ã|Â|�", " ")
  
  # remove símbolos soltos (moeda, copyright etc.)
  x <- stri_replace_all_regex(x, "[¢©®§]", " ")
  
  x <- stri_replace_all_regex(x, "\\s+", " ")
  trimws(x)
}
dados_corrigidos <- dados_corrigidos %>%
  mutate(across(all_of(cols_problema), limpar_unicode_polido))
any(grepl("Ã|Â|�", unlist(dados_corrigidos)))
dados_corrigidos %>%
  filter(row_number() %in% c(27, 120)) %>%
  select(Title)

write_csv(
  dados_corrigidos,
  "artigos_com_autor_brasileiro_utf8_final.csv"
)

dados_corrigidos <- dados_corrigidos %>%
  mutate(
    primeiro_autor = str_trim(word(Authors, 1))
  )

duplicados_composto <- dados_corrigidos %>%
  group_by(title_norm, Year, primeiro_autor) %>%
  filter(n() > 1) %>%
  ungroup()

n_distinct(duplicados_composto)

#Fora da temática
# =====================================================
# 1. PACOTES
# =====================================================
library(dplyr)
library(stringr)

# =====================================================
# 2. CRIAR TEXTO UNIFICADO (TITLE + KEYWORDS + ABSTRACT)
# =====================================================
dados_corrigidos <- dados_corrigidos %>%
  mutate(
    texto_busca = str_to_lower(
      paste(
        Title,
        `Author Keywords`,
        Abstract,
        sep = " "
      )
    )
  )

# =====================================================
# 3. TERMOS DE VIOLÊNCIA (núcleo)
# =====================================================
termos_violencia <- c(
  "violence", "violent",
  "abuse", "abusive",
  "assault", "aggression",
  "maltreatment",
  "violência", "violento",
  "abuso", "agressão",
  "maus-tratos", "maus tratos"
)

# =====================================================
# 4. TERMOS DE CONTEXTO DOMÉSTICO / FAMILIAR
# =====================================================
termos_domestico <- c(
  "domestic", "intrafamilial",
  "family", "familial",
  "household", "home",
  "partner", "intimate partner",
  "spouse", "marital",
  "doméstica", "doméstico",
  "intrafamiliar",
  "família", "familiar",
  "domicílio", "lar",
  "conjugal", "companheiro", "cônjuge"
)

# =====================================================
# 5. IDENTIFICAR PRESENÇA DOS TERMOS
# =====================================================
dados_corrigidos <- dados_corrigidos %>%
  mutate(
    flag_violencia = str_detect(
      texto_busca,
      str_c(termos_violencia, collapse = "|")
    ),
    
    flag_domestico = str_detect(
      texto_busca,
      str_c(termos_domestico, collapse = "|")
    )
  )

# =====================================================
# 6. CLASSIFICAÇÃO FINAL
# =====================================================
dados_corrigidos <- dados_corrigidos %>%
  mutate(
    classificacao_tematica = case_when(
      flag_violencia & flag_domestico ~ "violência doméstica",
      flag_violencia & !flag_domestico ~ "violência fora do contexto doméstico",
      TRUE ~ "fora da temática de violência"
    )
  )

# =====================================================
# 7. RESUMO RÁPIDO (CHECK)
# =====================================================
table(dados_corrigidos$classificacao_tematica)

fora_domestica <- dados_corrigidos %>%
  filter(classificacao_tematica == "violência fora do contexto doméstico") %>%
  select(Title, Abstract)
nrow(fora_domestica)
fora_total <- dados_corrigidos %>%
  filter(classificacao_tematica == "fora da temática de violência") %>%
  select(Title, Abstract)
nrow(fora_total)
fora_domestica %>% slice(1:10)
fora_total %>% slice(1:10)

# =====================================================
# 1. PACOTES
# =====================================================
library(dplyr)
library(writexl)

# =====================================================
# 2. SELECIONAR ARTIGOS FORA DA TEMÁTICA CENTRAL
# =====================================================
triagem_manual <- dados_corrigidos %>%
  filter(
    classificacao_tematica %in% c(
      "violência fora do contexto doméstico",
      "fora da temática de violência"
    )
  ) %>%
  select(
    classificacao_tematica,
    Title,
    `Author Keywords`,
    Abstract
  ) %>%
  arrange(classificacao_tematica)

# =====================================================
# 3. (OPCIONAL) CAMPO PARA DECISÃO MANUAL
# =====================================================
triagem_manual <- triagem_manual %>%
  mutate(
    decisao_manual = NA_character_,
    observacoes = NA_character_
  )

install.packages("openxlsx")
library(dplyr)
library(openxlsx)

# =====================================================
# SALVAR EM EXCEL
# =====================================================
write.xlsx(
  triagem_manual,
  file = "triagem_artigos_violencia_domestica.xlsx",
  overwrite = TRUE
)

