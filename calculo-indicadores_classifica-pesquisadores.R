#---------- Pacotes e Bibliotecas  ----------
# install.packages("dplyr", "dbplyr", "countrycode", )
# install.packages("tidyverse" ,)
# install.packages("corrplot" ,)
# install.packages("gt", )
# install.packages("knitr", )
# install.packages("kableExtra", )
# install.packages("openxlsx", )
# install.packages ("countrycode", )
# install.packages("writexl", )
# install.packages("readr", )
# install.packages(c("ggplot2", "gtable", "scales", "rlang"))

# library("dplyr")
# library("dbplyr")
# library("ggplot2")
# library("gtable")
# library("scales")
# library("rlang")
# library("countrycode")
# library("tidyverse")
# library("corrplot")
# library("gt")
# library("knitr")
# library("kableExtra")
# library("openxlsx")
# library("writexl")
# library("readr")
# library("RColorBrewer")


#---------- Carregar arquivos ------------------
df_orcid_doi<- read_csv("orcid_doi_sample_FINAL.csv")
colnames(df_orcid_doi) <- c("orcid", "doi")

df_profile <- read_csv("orcid_profile_sample_FINAL.csv")

df_profile <- df_profile %>%
  mutate(main_field = case_match(main_field,
                                 1 ~ "Social sciences and humanities",
                                 2 ~ "Biomedical and health sciences",
                                 3 ~ "Physical sciences and engineering",
                                 4 ~ "Life and earth sciences",
                                 5 ~ "Mathematics and computer science"
  ))


df_profile <- df_profile %>%
  mutate(region = countrycode(country, origin = "iso2c", destination = "continent"))


df_mention <-  read_delim("doi_mentions_mr_sample_FINAL.csv", delim = ",")
colnames(df_mention) <- c("doi", "x", "bsky", "fb", "n", "b", "pod", "rd", "v", "w", "pol", "pat", "f1000", "cg", "pr", "mr")

df_metrics <- read.csv ("orcid_metrics_sample_FINAL.csv")

#-- separa as fontes principais
df_unido <- df_orcid_doi %>%
  left_join(df_mention, by = "doi")

df_contagem <- df_unido %>%
  group_by(orcid) %>%
  summarise(
    pm_x   = sum(x > 0, na.rm = TRUE),
    pm_n   = sum(n > 0, na.rm = TRUE),
    pm_pat = sum(pat > 0, na.rm = TRUE),
    pm_mr  = sum(mr > 0, na.rm = TRUE),
    .groups = "drop"
  )

df_pm_4fontes <- df_contagem %>%
  left_join(select(df_metrics, orcid, p), by = "orcid") %>%
  select(orcid, p, pm_x, pm_n, pm_pat, pm_mr)


# ==============================================================================
# DESCRIÇÃO GERAL DA AMOSTRA
# ==============================================================================

# Frequência de Pesquisadores por regiões do mundo
tabela_frequencia_regiao <- df_profile %>%
  filter(!is.na(region)) %>% 
  count(region, sort = TRUE) %>% 
  mutate(percentual = round((n / sum(n)) * 100, 2))

write_excel_csv2(tabela_frequencia_regiao, "frequencia_regiao.csv")


#Frequência de Pesquisadores por país
tabela_frequencia_pais <- df_profile %>%
  filter(!is.na(country)) %>%  
  count(country, sort = TRUE) %>%  
  mutate(
    
    percentual_num = (n / sum(n)) * 100,
    
    resultado = paste0(n, " (", sprintf("%.2f", percentual_num), "%)")
  ) %>%
  select(country, resultado)


write_excel_csv2(tabela_frequencia_pais, "frequencia_pais_formatada.csv")



#Relacao País x Area
tabela_area_por_regiao <- df_profile %>%
  filter(!is.na(region), !is.na(main_field)) %>% 
  
  count(main_field, region) %>% 
  
  pivot_wider(names_from = region, values_from = n, values_fill = 0) %>%

  arrange(main_field)

write_excel_csv2(tabela_area_por_regiao, "area_regiao.csv")


#Tempo de atividade
df_profile <- df_profile %>%
  mutate(
    tempo_atividade = (2025 - f_pub_year) 
  )

carreira_area_full <- df_profile %>%
  group_by(main_field) %>%
  summarise(
    N = n(),
    Media_Anos = mean(tempo_atividade, na.rm = TRUE),
    Mediana_Anos = median(tempo_atividade, na.rm = TRUE),
    Min_Anos = min(tempo_atividade, na.rm = TRUE),
    Max_Anos = max(tempo_atividade, na.rm = TRUE)
  ) %>%
  arrange(desc(Media_Anos))

write_excel_csv2(carreira_area_full, "carreira_area_full.csv")



#Processando as faixas
df_frequencia_carreira <- df_profile %>%

  mutate(tempo_atividade = 2025 - f_pub_year) %>%

  mutate(faixa_carreira = cut(tempo_atividade, 
                              breaks = c(0, 10, 20, 30, 40, 50, Inf), 
                              labels = c("0-10 anos", "11-20 anos", "21-30 anos", 
                                         "31-40 anos", "41-50 anos", "Mais de 50 anos"),
                              right = TRUE,      # Inclui o limite superior na faixa
                              include.lowest = TRUE)) %>% 

  count(faixa_carreira, name = "n_pesquisadores") %>%

  mutate(porcentagem = (n_pesquisadores / sum(n_pesquisadores)) * 100)

write_excel_csv2(df_frequencia_carreira , "df_frequencia_carreira.csv")




# Estatistica descritiva Geral (apenas main source)
df_main_source_metrics <- df_profile %>%
  select(orcid, main_field) %>% 
  left_join(df_pm_4fontes, by = "orcid")


descritiva_main_source <- df_main_source_metrics %>%
  pivot_longer(
    cols = c(p, pm_x, pm_n, pm_pat, pm_mr), 
    names_to = "indicador", 
    values_to = "valor"
  ) %>%

    group_by(main_field, indicador) %>%
  summarise(
    `Mínimo`        = min(valor, na.rm = TRUE),
    `Média`         = round(mean(valor, na.rm = TRUE), 2),
    `Mediana`       = median(valor, na.rm = TRUE),
    `Máximo`        = max(valor, na.rm = TRUE),
    `Desvio Padrão` = round(sd(valor, na.rm = TRUE), 2),
    .groups = 'drop'
  )


descritiva_final <- descritiva_main_source %>%
  pivot_longer(
    cols = c(`Mínimo`, `Média`, `Mediana`, `Máximo`, `Desvio Padrão`), 
    names_to = "estatistica", 
    values_to = "valor"
  ) %>%
  mutate(
    estatistica = factor(
      estatistica, 
      levels = c("Mínimo", "Média", "Mediana", "Máximo", "Desvio Padrão")
    )
  ) %>%
  pivot_wider(
    names_from = main_field, 
    values_from = valor
  ) %>%
  arrange(indicador, estatistica)

write_excel_csv2(descritiva_final, "descritiva_final.csv")



#--- Pesquisadores sem menção ---#

#Frequência por área
total_por_area <- df_profile %>%
  group_by(main_field) %>%
  summarise(total_pesquisadores_area = n_distinct(orcid))

df_pm_zero_final <- df_metrics %>%
  filter(pm == 0) %>%
  inner_join(df_profile %>% select(orcid, main_field), by = "orcid") %>%
  group_by(main_field) %>%
  summarise(qtd_abs = n_distinct(orcid)) %>%
  left_join(total_por_area, by = "main_field") %>%
  mutate(
    porcentagem = (qtd_abs / total_pesquisadores_area) * 100,

        resultado_formatado = paste0(qtd_abs, " (", round(porcentagem, 2), "%)")
  ) %>%
  select(main_field, `Pesquisadores (pm=0)` = resultado_formatado) %>%
  arrange(desc(main_field))


write_excel_csv2(df_pm_zero_final, "df_pm_zero_final.csv")


#Frequência por Tempo de atividade
df_profile_faixas <- df_profile %>%
  mutate(tempo_atividade = 2025 - f_pub_year) %>%
  mutate(faixa_carreira = cut(tempo_atividade, 
                              breaks = c(0, 10, 20, 30, 40, 50, Inf), 
                              labels = c("0-10 anos", "11-20 anos", "21-30 anos", 
                                         "31-40 anos", "41-50 anos", "Mais de 50 anos"),
                              right = TRUE, 
                              include.lowest = TRUE))

df_pm_zero_por_carreira <- df_metrics %>%
  inner_join(df_profile_faixas %>% select(orcid, faixa_carreira), by = "orcid") %>%

  group_by(faixa_carreira) %>%
  
  summarise(
    total_na_faixa = n_distinct(orcid),
    qtd_pm0 = n_distinct(orcid[pm == 0])
  ) %>%
  
  mutate(
    porcentagem = (qtd_pm0 / total_na_faixa) * 100,
    resultado = paste0(qtd_pm0, " (", round(porcentagem, 2), "%)")
  ) %>%
  
  select(faixa_carreira, `Pesquisadores (pm=0)` = resultado)

write_excel_csv2(df_pm_zero_por_carreira, "pm_zero_por_faixa_carreira.csv")


#-- Fontes Altmétricas
df_mencoes_geral <- df_metrics %>%
  select(orcid, x, n, pat, mr)

resumo_geral <- df_mencoes_geral %>%

  pivot_longer(
    cols = c(x, n, pat, mr),
    names_to = "fonte",
    values_to = "mencoes"
  ) %>%

  group_by(fonte) %>%
  summarise(
    `Total de Menções`            = sum(mencoes, na.rm = TRUE),
    `Média por Pesquisador`       = round(mean(mencoes, na.rm = TRUE), 2),
    `Pesquisadores com Menção`    = sum(mencoes > 0, na.rm = TRUE),
    `Pesquisadores sem Menção`    = sum(mencoes == 0, na.rm = TRUE),
    .groups = "drop"
  )

resumo_geral_final <- resumo_geral %>%
  mutate(
    fonte = case_when(
      fonte == "x"   ~ "X",
      fonte == "n"   ~ "Notícias",
      fonte == "pat" ~ "Patentes",
      fonte == "mr"  ~ "Mendeley",
      TRUE           ~ fonte
    )
  )

write_excel_csv2(resumo_geral_final, "resumo_geral_mencoes.csv")

#Regiões do mundo
df_pm_zero_por_regiao <- df_metrics %>%
  inner_join(df_profile %>% select(orcid, region), by = "orcid") %>%
  
  group_by(region) %>%
  
  summarise(
    total_na_regiao = n_distinct(orcid),
    qtd_pm0         = n_distinct(orcid[pm == 0]),
    .groups         = "drop"
  ) %>%
  
  mutate(
    porcentagem = (qtd_pm0 / total_na_regiao) * 100,
    `Pesquisadores (pm=0)` = paste0(qtd_pm0, " (", round(porcentagem, 2), "%)")
  ) %>%
  
  select(region, `Pesquisadores (pm=0)`) %>%
  
  arrange(region)

write_excel_csv2(df_pm_zero_por_regiao, "pm_zero_por_regiao.csv")

# ==============================================================================


# =============================================================================#
# CALCULAR A COBERTURA (COV)
# =============================================================================

df_cobertura <- df_pm_4fontes %>%
  mutate(

    cov_x   = (pm_x / p) * 100,
    cov_n   = (pm_n / p) * 100,
    cov_pat = (pm_pat / p) * 100,
    cov_mr  = (pm_mr / p) * 100
  ) %>%
  select(orcid, cov_x, cov_n, cov_pat, cov_mr)


df_cobertura_area <- df_cobertura %>%
  left_join(select(df_profile, orcid, main_field), by = "orcid")


df_resumo_cov_por_area <- df_cobertura_area %>%
  group_by(main_field) %>%
  summarise(

    mediana_cov_x   = round(median(cov_x, na.rm = TRUE), 2),
    mediana_cov_n   = round(median(cov_n, na.rm = TRUE), 2),
    mediana_cov_pat = round(median(cov_pat, na.rm = TRUE), 2),
    mediana_cov_mr  = round(median(cov_mr, na.rm = TRUE), 2),
    
    .groups = "drop"
  )

write_excel_csv2(df_resumo_cov_por_area, "df_resumo_cov_por_area.csv")

#estatísitica descritiva
descritiva_cov_long <- df_cobertura_area %>%
  pivot_longer(
    cols = c(cov_x, cov_n, cov_pat, cov_mr),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  group_by(main_field, indicador) %>%
  summarise(
    `Mínimo`        = round(min(valor, na.rm = TRUE), 2),
    `Média`         = round(mean(valor, na.rm = TRUE), 2),
    `Mediana`       = round(median(valor, na.rm = TRUE), 2),
    `Máximo`        = round(max(valor, na.rm = TRUE), 2),
    `Desvio Padrão` = round(sd(valor, na.rm = TRUE), 2),
    .groups = "drop"
  )

descritiva_cov_final <- descritiva_cov_long %>%
  pivot_longer(
    cols = c(`Mínimo`, `Média`, `Mediana`, `Máximo`, `Desvio Padrão`),
    names_to = "estatistica",
    values_to = "valor"
  ) %>%
  mutate(
    estatistica = factor(
      estatistica,
      levels = c("Mínimo", "Média", "Mediana", "Máximo", "Desvio Padrão")
    )
  ) %>%
  pivot_wider(
    names_from = main_field,
    values_from = valor
  ) %>%
  arrange(indicador, estatistica)

write_excel_csv2(descritiva_cov_final, "descritiva_cov_final.csv")


# ==============================================================================
# CALCULAR A INTENSIDADE
# ==============================================================================

df_intensidade <- df_metrics %>%
  select(orcid, x, n, pat, mr) %>%
  left_join(
    select(df_pm_4fontes, orcid, pm_x, pm_n, pm_pat, pm_mr),
    by = "orcid"
  ) %>%
  mutate(

    int_x   = ifelse(pm_x > 0, x / pm_x, 0),
    int_n   = ifelse(pm_n > 0, n / pm_n, 0),
    int_pat = ifelse(pm_pat > 0, pat / pm_pat, 0),
    int_mr  = ifelse(pm_mr > 0, mr / pm_mr, 0)
  )

df_intensidade_area <- df_intensidade %>%
  left_join(select(df_profile, orcid, main_field), by = "orcid")

df_resumo_int_por_area <- df_intensidade_area %>%
  group_by(main_field) %>%
  summarise(

    mediana_int_x   = round(median(int_x, na.rm = TRUE), 2),
    mediana_int_n   = round(median(int_n, na.rm = TRUE), 2),
    mediana_int_pat = round(median(int_pat, na.rm = TRUE), 2),
    mediana_int_mr  = round(median(int_mr, na.rm = TRUE), 2),

    .groups = "drop"
  )


write_excel_csv2(df_resumo_int_por_area, "df_resumo_int_por_area.csv")



#estatísitica descritiva
descritiva_int_long <- df_intensidade_area %>%
  pivot_longer(
    cols = c(int_x, int_n, int_pat, int_mr),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  group_by(main_field, indicador) %>%
  summarise(
    `Mínimo`        = round(min(valor, na.rm = TRUE), 2),
    `Média`         = round(mean(valor, na.rm = TRUE), 2),
    `Mediana`       = round(median(valor, na.rm = TRUE), 2),
    `Máximo`        = round(max(valor, na.rm = TRUE), 2),
    `Desvio Padrão` = round(sd(valor, na.rm = TRUE), 2),
    .groups = "drop"
  )

descritiva_int_final <- descritiva_int_long %>%
  pivot_longer(
    cols = c(`Mínimo`, `Média`, `Mediana`, `Máximo`, `Desvio Padrão`),
    names_to = "estatistica",
    values_to = "valor"
  ) %>%
  mutate(
    estatistica = factor(
      estatistica,
      levels = c("Mínimo", "Média", "Mediana", "Máximo", "Desvio Padrão")
    )
  ) %>%
  pivot_wider(
    names_from = main_field,
    values_from = valor
  ) %>%
  arrange(indicador, estatistica)

write_excel_csv2(descritiva_int_final, "descritiva_int_final.csv")



# ==============================================================================
# MEDIANA COMPLEMENTAR - SOMENTE entre pesquisadores com pm > 0 
# ==============================================================================
df_cov_long <- df_cobertura_area %>%
  select(orcid, main_field, cov_x, cov_n, cov_pat, cov_mr) %>%
  pivot_longer(
    cols = starts_with("cov_"),
    names_to = "fonte",
    names_prefix = "cov_",
    values_to = "cov"
  )

df_int_long <- df_intensidade_area %>%
  select(orcid, int_x, int_n, int_pat, int_mr) %>%
  pivot_longer(
    cols = starts_with("int_"),
    names_to = "fonte",
    names_prefix = "int_",
    values_to = "int"
  )

df_pm_long <- df_pm_4fontes %>%
  select(orcid, pm_x, pm_n, pm_pat, pm_mr) %>%
  pivot_longer(
    cols = starts_with("pm_"),
    names_to = "fonte",
    names_prefix = "pm_",
    values_to = "pm"
  )

df_quadrantes_long <- df_cov_long %>%
  left_join(df_int_long, by = c("orcid", "fonte")) %>%
  left_join(df_pm_long, by = c("orcid", "fonte")) %>%
  mutate(
    fonte = case_when(
      fonte == "x"   ~ "X",
      fonte == "n"   ~ "Notícias",
      fonte == "pat" ~ "Patentes",
      fonte == "mr"  ~ "Mendeley"
    ),
    fonte = factor(fonte, levels = c("X", "Notícias", "Mendeley", "Patentes"))
  )


df_medianas_area_fonte <- df_quadrantes_long %>%
  filter(pm > 0) %>%
  group_by(main_field, fonte) %>%
  summarise(
    mediana_cov = median(cov, na.rm = TRUE),
    mediana_int = median(int, na.rm = TRUE),
    .groups = "drop"
  )

write_excel_csv2(df_medianas_area_fonte, "tabela_medianas_area_fonte.csv")

# ==============================================================================


# ==============================================================================
# CLASSIFICAÇÃO NOS QUADRANTES POR MEDIANA (Q2)
# ==============================================================================

df_quadrantes_long <- df_quadrantes_long %>%
  left_join(df_medianas_area_fonte, by = c("main_field", "fonte")) %>%
  mutate(
    cov_alto = if_else(mediana_cov == 0, cov > 0, cov >= mediana_cov),
    int_alto = if_else(mediana_int == 0, int > 0, int >= mediana_int),
    quadrante = case_when(
      pm == 0               ~ "Visibilidade Baixa ou Nula",
      cov_alto  & int_alto  ~ "Visibilidade Muito Alta (Elite)",
      !cov_alto & int_alto  ~ "Visibilidade Alta",
      cov_alto  & !int_alto ~ "Visibilidade Média",
      !cov_alto & !int_alto ~ "Visibilidade Baixa ou Nula"
    ),
    quadrante = factor(
      quadrante,
      levels = c(
        "Visibilidade Baixa ou Nula",
        "Visibilidade Média",
        "Visibilidade Alta",
        "Visibilidade Muito Alta (Elite)"
      )
    )
  ) %>%
  select(-cov_alto, -int_alto)

#Tabela de frequências: pesquisadores por área x fonte x quadrante
tabela_quadrantes_area_fonte <- df_quadrantes_long %>%
  filter(!is.na(quadrante)) %>%
  count(main_field, fonte, quadrante, name = "n_pesquisadores") %>%
  group_by(main_field, fonte) %>%
  mutate(percentual = round(n_pesquisadores / sum(n_pesquisadores) * 100, 2)) %>%
  ungroup() %>%
  arrange(main_field, fonte, quadrante)

write_excel_csv2(tabela_quadrantes_area_fonte, "tabela_quadrantes_area_fonte.csv")


tabela_quadrantes_fonte_area <- tabela_quadrantes_area_fonte %>%
  select(main_field, fonte, quadrante, n_pesquisadores) %>%
  pivot_wider(
    names_from = main_field,
    values_from = n_pesquisadores
  ) %>%
  arrange(fonte, quadrante)

write_excel_csv2(tabela_quadrantes_fonte_area, "tabela_quadrantes_fonte_area.csv")



#Plot: um plano bidimensional por área
cores_quadrantes <- c(
  "Visibilidade Baixa ou Nula"      = "#d62728",
  "Visibilidade Média"              = "#ff7f0e",
  "Visibilidade Alta"               = "#1f77b4",
  "Visibilidade Muito Alta (Elite)" = "#2ca02c"
)

plotar_quadrantes_area <- function(area_nome, escala_livre = TRUE) {
  
  dados_area <- df_quadrantes_long %>%
    filter(main_field == area_nome, !is.na(quadrante))
  
  linhas_mediana_area <- df_medianas_area_fonte %>%
    filter(main_field == area_nome)
  
  ggplot(dados_area, aes(x = cov, y = int, color = quadrante)) +
    geom_point(alpha = 0.6, size = 1.8) +
    geom_vline(
      data = linhas_mediana_area, aes(xintercept = mediana_cov),
      linetype = "dashed", color = "black", linewidth = 0.5
    ) +
    geom_hline(
      data = linhas_mediana_area, aes(yintercept = mediana_int),
      linetype = "dashed", color = "black", linewidth = 0.5
    ) +
    facet_wrap(~ fonte, ncol = 2, scales = if (escala_livre) "free" else "fixed") +
    scale_color_manual(values = cores_quadrantes, drop = FALSE) +
    labs(
      title = paste0("Perfis de visibilidade online - ", area_nome),
      x = "Nível de Cobertura (%)",
      y = "Nível de Intensidade",
      color = "Perfil de Visibilidade"
    ) +
    scale_color_manual(values = cores_quadrantes, drop = FALSE) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(
      title = paste0("Perfis de visibilidade online - ", area_nome),
      x = "Nível de Cobertura (%)",
      y = "Nível de Intensidade",
      color = "Perfil de Visibilidade"
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      strip.background = element_rect(fill = "grey90"),
      legend.position = "bottom",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.6, "cm"),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
    )
}

#Gerar e salvar um gráfico por área
areas <- c(
  "Biomedical and health sciences",
  "Life and earth sciences",
  "Mathematics and computer science",
  "Physical sciences and engineering",
  "Social sciences and humanities"
)

for (area in areas) {
  p <- plotar_quadrantes_area(area)
  print(p)
  
  nome_arquivo <- paste0("quadrantes_", gsub(" ", "_", tolower(area)), ".png")
  ggsave(nome_arquivo, plot = p, width = 10, height = 8, dpi = 300)
}
# ==============================================================================


# ==============================================================================
# CLASSIFICAÇÃO EM 10 CLASSES DE VISIBILIDADE ONLINE 
# ==============================================================================

df_perfil_pesquisador <- df_quadrantes_long %>%
  mutate(
    # código curto da fonte, só para nomear as colunas do pivot
    fonte_cod = case_when(
      fonte == "X"        ~ "x",
      fonte == "Notícias" ~ "n",
      fonte == "Mendeley" ~ "mr",
      fonte == "Patentes" ~ "pat"
    )
  ) %>%
  select(orcid, main_field, fonte_cod, quadrante) %>%
  pivot_wider(
    names_from = fonte_cod,
    values_from = quadrante,
    names_prefix = "perfil_"
  )

#Contar, por pesquisador, em quantas fontes ele caiu em cada nível
colunas_perfil <- c("perfil_x", "perfil_n", "perfil_mr", "perfil_pat")

contar_nivel <- function(df, nivel) {
  rowSums(sapply(df[colunas_perfil], `==`, nivel), na.rm = TRUE)
}

df_perfil_pesquisador <- df_perfil_pesquisador %>%
  mutate(
    n_elite = contar_nivel(., "Visibilidade Muito Alta (Elite)"),
    n_alta  = contar_nivel(., "Visibilidade Alta"),
    n_media = contar_nivel(., "Visibilidade Média"),
    n_baixa = contar_nivel(., "Visibilidade Baixa ou Nula")
  )

#Classificação hierárquica nas 10 classes
niveis_classe <- c(
  "Classe 1: 4 ou 3 Elite",
  "Classe 2: 2 Elite",
  "Classe 3: 1 Elite",
  "Classe 4: 4 ou 3 Alta",
  "Classe 5: 2 Alta",
  "Classe 6: 1 Alta + 1 Média",
  "Classe 7: 1 Alta (e o restante baixa)",
  "Classe 8: 2 Média",
  "Classe 9: 1 Média",
  "Classe 10: Visibilidade Nula"
)

df_perfil_pesquisador <- df_perfil_pesquisador %>%
  mutate(
    classe = case_when(
      n_elite >= 3                 ~ niveis_classe[1],
      n_elite == 2                 ~ niveis_classe[2],
      n_elite == 1                 ~ niveis_classe[3],
      n_alta  >= 3                 ~ niveis_classe[4],
      n_alta  == 2                 ~ niveis_classe[5],
      n_alta  == 1 & n_media >= 1  ~ niveis_classe[6],
      n_alta  == 1 & n_media == 0  ~ niveis_classe[7],
      n_media >= 2                 ~ niveis_classe[8],
      n_media == 1                 ~ niveis_classe[9],
      n_baixa == 4                 ~ niveis_classe[10]
    ),
    classe = factor(classe, levels = niveis_classe)
  )

#Tabelas de frequência: geral e por área
tabela_10_classes <- df_perfil_pesquisador %>%
  filter(!is.na(classe)) %>%
  count(classe, name = "n_pesquisadores") %>%
  mutate(percentual = round(n_pesquisadores / sum(n_pesquisadores) * 100, 2))

write_excel_csv2(tabela_10_classes, "tabela_10_classes.csv")

tabela_10_classes_area <- df_perfil_pesquisador %>%
  filter(!is.na(classe)) %>%
  count(main_field, classe, name = "n_pesquisadores") %>%
  group_by(main_field) %>%
  mutate(percentual = round(n_pesquisadores / sum(n_pesquisadores) * 100, 2)) %>%
  ungroup() %>%
  arrange(main_field, classe)

write_excel_csv2(tabela_10_classes_area, "tabela_10_classes_area.csv")


#Visualização: barras empilhadas
cores_10_classes <- setNames(
  rev(RColorBrewer::brewer.pal(10, "RdYlGn")),
  niveis_classe
)

# siglas das áreas, para caber na largura da página
siglas_area <- c(
  "Biomedical and health sciences"    = "BHS",
  "Life and earth sciences"           = "LES",
  "Mathematics and computer science"  = "MCS",
  "Physical sciences and engineering" = "PSE",
  "Social sciences and humanities"    = "SSH"
)

grafico_10_classes_area <- tabela_10_classes_area %>%
  mutate(area_sigla = factor(siglas_area[main_field], levels = siglas_area)) %>%
  ggplot(aes(x = area_sigla, y = percentual, fill = classe)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  coord_flip() +
  scale_fill_manual(values = cores_10_classes, name = "Classe de visibilidade") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
  labs(
    title = "Distribuição dos pesquisadores nas 10 classes de visibilidade online, por área",
    x = NULL,
    y = "Percentual de pesquisadores (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
  )

print(grafico_10_classes_area)
ggsave("grafico_10_classes_area.png", plot = grafico_10_classes_area, width = 7, height = 6.5, dpi = 300)
