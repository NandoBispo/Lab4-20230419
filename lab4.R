# PACOTES ----

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools,
               kableExtra, moments, ggpubr, formattable, gridExtra, 
               glue, corrplot, sessioninfo, readxl, writexl, ggthemes,
               patchwork,  plotly, lmtest, olsrr, gglm, ggplot2,
               tidymodels, GGally, skimr, qqplotr, performance)

## Identificação dos pacotes ----
# - tidyverse
# - janitor: Pacote para arrumação do conjunto dos dados e padronizar os nomes das variáveis com o comando "janitor::clean_names()"
# - skimr: Pacote que gera um mini relatório dos dados e identifica dados faltantes como comando "skimr::skim()".
# - qqplotr: Pacote que gera gráficos qq
# performance::check_model gera gráficos de análise de residuos para MRLS

# VERSIONAMENTO ----
# https://curso-r.githud.io/zen-do-r/git-githud.html
# gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()
# _________________________________________________

# DADOS ----
## Importação ----
dados <- read.csv("boston.csv")

dplyr::glimpse(dados)

## Arrumação ----
dados <- dados|>
  janitor::clean_names()

dados|> names()

dados <- dados|>
  mutate(
    chas = forcats::as_factor(chas),
    rad = forcats::as_factor(rad)
    )

## Tabela de Freq. VAs Categóricas ----

summarytools::st_options(freq.report.nas = F, headings = F)
# summarytools::st_options()

dados|>
  select(rad)|>
  summarytools::freq(
    plain.ascii = FALSE,
    style = "rmarkdown",
    round.digits = 2,
    # report.nas = F,
    # headings = F
    # order = "freq" #ordena pela frequencia
  )|> names()
kbl(
  caption = "Distribuição de Freqência do Índice de Acessibilidade às Rodovias",
  digits = 3,
  format.args=list(big.mark=".", decimal.mark=","),
  align = "c", row.names = T, booktabs = F
  # col.names = c("Freq", "%")
)|>
  kable_styling(
    full_width = F, position = 'center',
    latex_options = c("striped", "HOLD_position", "scale_down", "repeat_header")
  )|>
  column_spec(1, bold = T
  )

# Formatação Numero ----
# scales::number(accuracy = 0.0001, big.mark = ".", decimal.mark = ",")

## Gráfico VAs Categóricas ----
# p1 <- 
  dados|>
  ggplot(aes(x = rad, y = after_stat(count)/sum(after_stat(count))))+
  geom_bar(fill = "darkblue")+ 
  theme_minimal()+
  geom_text(
    aes(
      label = scales::percent((after_stat(count))/sum(after_stat(count)), big.mark = ".", decimal.mark = ","),
      y = after_stat(count)/sum(after_stat(count))),
    stat = "count", vjust = 0.4, size = 3.2, 
    angle = 90, color = "white", hjust = 1,
  ) +
  labs(
    title = "Índice de Acessibilidade às Rodovias",
    x = "Índice",
    y = "Frequência Relativa"
  )+scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(limits = c(1:25))
  # scale_x_continuous(limits = c(1:25))


# p1/gridExtra::tableGrob(dados|>
#   select(rad)|>
#   summarytools::freq(
#     plain.ascii = FALSE,
#     # style = "rmarkdown",
#     round.digits = 2,
#     report.nas = F,
#     headings = F
#     # order = "freq" #ordena pela frequencia
#   ), cols = colnames(c(1:3)), digits(2)
#   )


#dados|>
#   ggplot(aes(x = rad, y = after_stat(count)/sum(after_stat(count))))+
#   geom_bar(fill = "darkblue")+ 
#   geom_text(
#     aes(
#       label = scales::percent((after_stat(count))/sum(after_stat(count)))|>scales::number(accuracy = 0.0001, big.mark = ".", decimal.mark = ","),
#       y = after_stat(count)/sum(after_stat(count))),
#     stat = "count", vjust = -0.2, size = 2
#   ) +
#   labs(
#     title = "Índice de Acessibilidade às Rodovias",
#     x = "Índice",
#     y = "Frequência"
#   )+
#   scale_y_continuous(labels = scales::percent)+
#   theme_minimal()+
#   theme(
#     legend.position = "none", title = element_text(size = 7.5))


p2 <- dados %>% 
  count(chas) %>%
  mutate(
    # chas = forcats::fct_reorder(sex, n),
    tipo = case_when(
      chas == "0" ~ "Não Margeiam o rio",
      chas == "1" ~ "Margeiam o rio"),
    pct = round(prop.table(n)*100, 2), 
    rotulo = glue::glue('{tipo}\n{n} ({pct}%)')) %>% 
  ggpubr::ggdonutchart(., "pct", 
                       label = "rotulo", lab.pos = "out",
                       lab.font = c(4, "plain", "black"),
                       fill = "chas",  color = "white",
                       palette = c("darkblue", "skyblue"))+
                       # palette = c("#FC4E07", "#00AFBB"))+
  labs(
    title = "Figura 2: Imóveis que margeiam o Rio Charles"
    # x = "Sexo", y = "Frequência"
  )+
  theme(
    legend.position = "none", title = element_text(size = 10)
    # axis.title = element_text(hjust = 0)
  )

p1 + p2 + plot_layout(nrow = 2, widths = 3)

# Sem necessidade
# dados <- dados|>
#   mutate(
#     chas = forcats::as_factor(chas),
#     zn_m2 = round(zn*0.092903, 3))

dados|>skimr::skim() # Verificação de dados faltantes.


dplyr::glimpse(dados)




# AED ----

## Tab1 ----
summarytools::st_options(lang = "pt")

options(knitr.table.format = "latex")

dados|>
  rename(
    "Criminalidade" = crim,
    "Proporção de lote zoneado (sq.ft.)" = zn,
    "Proporção de negócios não varejistas" = indus,
    "Concentração de óxidos nítricos [partes/10M]" = nox,
    "N° médio de cômodos por habitação" = rm,
    "Proporção de unidades construídas antes de 1940" = age,
    "Distâncias ponderadas para centros de emprego de Boston" = dis,
    "Índice de acesso às rodovias"  = rad,
    "Valor total do imposto" = tax,
    "Proporção aluno-professor" = ptratio,
    "Resultado da equação B" = b,
    'População de "classe baixa"' = lstat,
    "Valor médio de residências ocupadas" = medv
    )|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv", "Skewness", "Kurtosis"),
    justify = "c",
    style = "rmarkdown",
    transpose = T
  )|>
  kbl(
    caption = "Medidas Resumo dos dados",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T, 
    booktabs = T
    # col.names =
    #   c("Min", "Q1", "Med", "Média", "Q3", "Max", "D.Padrão", "CV")
  )|>
  footnote(general = "Fonte: StatLib - Carnegie Mellon University") |>
  # kable_material(c(
  #   "striped", # listrado
  #   "hover", 
  #   "condensed"))|>
  kable_styling(
    full_width = F,
    position = 'center', 
    # latex_options = 'HOLD_position',
    latex_options = c("striped", "HOLD_position", "scale_down")
  )|>
  kable_material()

# Não apagar
# summarytools::st_options(lang = "pt")
# 
# dados|>
#   select(-b, - rad, -zn)|>
#   # rename(
#   # "Índice de Criminalidade" = crim,
#   # "Proporção de lote zoneado (sq.ft.)" = zn,
#   # "Proporção de negócios não varejistas" = indus,
#   # "Concentração de óxidos nítricos [partes/10 milhões]" = nox,
#   # "N° médio de cômodos por habitação" = rm,
#   # "Proporção de unidades construídas antes de 1940" = age,
#   # "Distâncias para centros de emprego" = dis,
#   # "Índice de acesso às rodovias"  = rad,
#   # "Valor total do imposto" = tax,
#   # "Proporção aluno-professor" = ptratio,
# # # "Resultado da equação B" = b,
# # 'População de "classe baixa"' = lstat,
# # "Valor médio de residências ocupadas" = medv
# # )|>
# summarytools::descr(
#   stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv", "Skewness", "Kurtosis"),
#   justify = "c",
#   style = "rmarkdown",
#   transpose = T
# )|>
#   kbl(
#     caption = "Medidas Resumo dos dados",
#     digits = 2,
#     format.args=list(big.mark=".", decimal.mark=","),
#     align = "c", row.names = T, booktabs = T
#   )|>
#   kable_styling(
#     full_width = F, position = 'center', 
#     latex_options = c("striped", "HOLD_position", "scale_down", "repeat_header")
#   )|>
#   column_spec(1, bold = F
#               # width = "15em", 
#   )|>
#   footnote(
#     general = "StatLib - Carnegie Mellon University",
#     general_title = "Fonte:",
#     footnote_as_chunk = T
#   )|>
#   kableExtra::add_footnote(c("Legenda:"), notation = "none")|>
#   kableExtra::add_footnote(c(
#     "age: Proporção de unidades próprias construídas antes de 1940.",
#     "crim: Índice de criminalidade per capita por bairro.",
#     "dis: Distâncias ponderadas para cinco centros de emprego de Boston.",
#     "indus: Proporção de hectares de negócios não varejistas por bairro.",
#     'lstat: Percentual da população de "classe baixa".',
#     "mdev: Valor médio de residências ocupadas pelo proprietário.",
#     "nox: Concentração de óxidos nítricos (partes por 10 milhões).",
#     "ptratio: Proporção aluno-professor por bairro.",
#     "rm: Número médio de cômodos por habitação.",
#     "tax: Valor total do imposto predial por $10.000."), 
#     notation = "none"
#   )|>
#   kable_material()

# Histograma ----
{
## g1 age ----
g1 <- dados|>
  ggplot() +
  aes(x = age) +
  geom_histogram(
    aes(y = ..density..),
    bins = 40,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Unidades constuídas antes \nde 1940",
    x = "Proporção",
    y = "Densidade"
  )

## g2 crim ----
g2 <- dados|>
  ggplot() +
  aes(x = crim) +
  geom_histogram(
    aes(y = ..density..),
    fill = "lightblue",
    colour = "darkblue",
    # binwidth = 2
    bins = 45
    ) +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Índice de Criminalidade",
    x = "Índice",
    y = "Densidade"
  )

## g3 dis ----
g3 <- dados|>
  ggplot() +
  aes(x = dis) +
  geom_histogram(
    aes(y = ..density..),
    bins = 45,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Distância para cinco centros \nde emprego.",
    x = "Distâncias Ponderadas",
    y = "Densidade"
  )

## g4 indus ----
g4 <- dados|>
  ggplot() +
  aes(x = indus) +
  geom_histogram(
    aes(y = ..density..),
    bins = 40,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Negócios não varejistas \npor bairro",
    x = "Proporção de hectares ocupados",
    y = "Densidade"
  )

## g5 lstat ----
g5 <- dados|>
  ggplot() +
  aes(x = lstat) +
  geom_histogram(
    aes(y = ..density..),
    bins = 40,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = 'População de "classe baixa"',
    x = 'Percentual',
    y = "Densidade"
  )

## g6 medv ----
g6 <- dados|>
  ggplot() +
  aes(x = medv) +
  geom_histogram(
    aes(y = ..density..),
    bins = 35,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Valor médio de residências ocupadas",
    x = "Valor Médio",
    y = "Densidade"
  )
## g7 nox ----
g7 <- dados|>
  ggplot() +
  aes(x = nox) +
  geom_histogram(
    aes(y = ..density..),
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Concentração de Óxidos \nNitricos (NO)",
    x = "Partes por 10 milhões",
    y = "Densidade"
  )

## g8 ptratio ----
g8 <- dados|>
  ggplot() +
  aes(x = ptratio) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Aluno/Professor por bairro",
    x = "Proporção",
    y = "Densidade"
  )

## g9 rm ----
g9 <- dados|>
  ggplot() +
  aes(x = rm) +
  geom_histogram(
    aes(y = ..density..),
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Número médio de cômodos por habitação",
    x = "Quantidade",
    y = "Densidade"
  )

## g10 tax ----
g10 <- dados|>
  ggplot() +
  aes(x = tax) +
  geom_histogram(
    aes(y = ..density..),
    bins = 35,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    title = "Taxa de imposto predial",
    x = "Valor por $10.000",
    y = "Densidade"
  )

g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9 + g10  +  
  plot_layout(ncol = 3) + 
  plot_annotation(
    title = "Figura 1: Histogramas das variáveis em análise.",
    caption = "Fonte: StatLib - Carnegie Mellon University",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":") &
  theme_minimal(base_size = 7) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 5, hjust = 0, vjust = -0.4))
}

# BoxPlot ----
{
  
## b1 age ----
b1 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = age)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Unidades constuídas \nantes de 1940",
    x = "Posição",
    y = "Proporção antes de 1940"
  )

## b2 crim ----
b2 <- dados|>
  mutate(
    chas = lvls_revalue(chas, c("Na Margem", "Afastado"))
  )|>
  ggplot(aes(x = chas, y = crim)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Índice de Criminalidade",
    x = "Posição",
    y = "Índice"
  )

## b3 dis ----
b3 <- dados|>
  mutate( chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = dis)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Distância para cinco \ncentros de emprego.",
    x = "Posição",
    y = "Distâncias Ponderadas"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))
      
## b4 indus ----
b4 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = indus)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Negócios não varejistas \npor bairro",
    x = "Proporção de hectares ocupados",
    y = "Densidade"
  )

## b5 lstat ----
b5 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = lstat)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'População de "classe baixa"',
    x = "Posição",
    y = "Percentual"
  )

## b6 medv ----
b6 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = medv)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Valor médio de residências ocupadas",
    x = "Posição",
    y = "Valor médio"
  )

## b7 nox ----
b7 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = nox)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Concentração de Óxidos \nNitricos (NO)",
    x = "Posição",
    y = "Partes por 10 milhões"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))

## b8 ptratio ----
b8 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = ptratio)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Aluno/Professor por bairro",
    x = "Posição",
    y = "Proporção"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))

## b9 rm ----
b9 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = rm)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Número médio de cômodos por habitação",
    x = "Posição",
    y = "Quantidade"
  )

## b10 tax ---- 
b10 <- dados|>
  mutate(chas = lvls_revalue(chas, c("Na Margem", "Afastado")))|>
  ggplot(aes(x = chas, y = tax)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Taxa de imposto predial",
    x = "Posição",
    y = "Valor por $10.000"
  )

b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10  +  
  plot_layout(ncol = 3) + 
  plot_annotation(
    title = "Figura 2: BoxPlot das variáveis em análise.",
    caption = "Fonte: StatLib - Carnegie Mellon University",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":") &
  theme_minimal(base_size = 7) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 5, hjust = 0, vjust = -0.4))
}


# Dispersão ----
{
  ## d1 age ----
  d1 <- dados|>
    ggplot(aes(y = medv, x = age)) +
    geom_point(color = "#234B6E")+
    labs(
      title = "Unidades constuídas antes de \n1940",
      y = 'Valor Médio (por $1.000)',
      x = 'Proporção antes de 1940'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
    # aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 5, label.y = 7.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)+
    scale_y_continuous(
      labels = scales::number_format(
        big.mark = ".",
        decimal.mark = ","
      ))
  # geom_smooth(method=lm, se=TRUE)+
  
  ## d2 crim ----
  d2 <- dados |>
    ggplot(aes(
      y = medv, 
      x = crim, color = crim)) +
    geom_point()+
    labs(
      title = 'Índice de Criminalidade',
      y = 'Valor Médio (por $1.000)',
      x = 'Índice'
    )+
    ggpubr::stat_cor(
      aes(label = ..rr.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 50, label.y = 60, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)
  
  ## d3 dis ----
  d3 <- dados |>
    ggplot(aes(
      y = medv, 
      x = dis, color = dis)) +
    geom_point()+
    labs(
      title = "Distância para cinco centros de \nemprego.",
      y = 'Valor Médio (por $1.000)',
      x = 'Distâncias Ponderadas'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 9, label.y = 7.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)+
    scale_x_continuous(
      labels = scales::number_format(
        big.mark = ".",
        decimal.mark = ","
      ))
  
  ## d4 indus ----
  d4 <- dados |>
    ggplot(aes(
      y = medv, 
      x = indus, color = indus)) +
    geom_point()+
    labs(
      title = "Negócios não varejistas por \nbairro",
      y = 'Valor Médio (por $1.000)',
      x = 'Hectares Ocupados'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 1.5, label.y = 7.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)
  # geom_smooth(method=lm, se=TRUE)
  
  ## d5 lstat ----
  d5 <- dados |>
    ggplot(aes(
      y = medv, 
      x = lstat, color = lstat)) +
    geom_point()+
    labs(
      title = 'População de "classe baixa"',
      y = 'Valor Médio (por $1.000)',
      x = 'Percentual'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 25, label.y = 47.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)
  
  ## d6 nox ----
  d6 <- dados |>
    ggplot(aes(
      y = medv, 
      x = nox, color = nox)) +
    geom_point()+
    labs(
      title = "Concentração de Óxidos \nNitricos (NO)",
      y = 'Valor Médio (por $1.000)',
      x = 'Partes por 10 milhões'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 0.75, label.y = 47.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)+
    scale_x_continuous(
      labels = scales::number_format(
        big.mark = ".",
        decimal.mark = ","
      ))
  
  ## d7 ptratio ----
  d7 <- dados |>
    ggplot(aes(
      y = medv, 
      x = ptratio, color = ptratio)) +
    geom_point()+
    labs(
      title = "Aluno/Professor por bairro",
      y = 'Valor Médio (por $1.000)',
      x = 'Proporção'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 13, label.y = 7.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)+
    scale_x_continuous(
      labels = scales::number_format(
        big.mark = ".",
        decimal.mark = ","
      ))
  
  ## d8 rm ----
  d8 <- dados |>
    ggplot(aes(
      y = medv, 
      x = rm, color = rm)) +
    geom_point()+
    labs(
      title = "Número médio de cômodos por \nhabitação",
      y = 'Valor Médio (por $1.000)',
      x = 'Quantidade'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 3.7, label.y = 47.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)
  
  ## d9 tax ----
  d9 <- dados |>
    ggplot(aes(
      x = tax, y = medv, color = tax)) +
    geom_point()+
    labs(
      title = "Taxa de imposto predial",
      y = 'Valor Médio (por $1.000)',
      x = 'Valor por $10.000'
    )+
    ggpubr::stat_cor(
      aes(label = ..r.label..),
      cor.coef.name = c("rho"),
      label.sep = "; ", geom = "text",
      color="red", method = "pearson", 
      label.x = 200, label.y = 7.5, show.legend = F,
      p.accuracy = 0.001, r.accuracy = 0.0001,
      size = 2)
  
  d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 +  
    plot_layout(ncol = 3) + 
    plot_annotation(
      title = "Figura 4: Relação entre o Valor médio dos imóveis e demais medições",
      caption = "Fonte: StatLib - Carnegie Mellon University",
      tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
      tag_suffix = ":") &
    theme_minimal(base_size = 7) &
    theme(
      legend.position = "none",
      plot.tag.position = c(0, 1),
      plot.tag = element_text(size = 5, hjust = 0, vjust = -0.4))
  
}

# Correlação 1 ----

cor.test(dados$medv, dados$crim)

dados %>% 
  select(-zn, -rad, -b, -chas) %>% 
  cor() %>% corrplot(method = "circle", type = "lower")

# Correlação 2----
corCrim <- round(cor(dados$medv, dados$crim), 4)
corZn <- round(cor(dados$medv, dados$zn), 4)
corIndus <- round(cor(dados$medv, dados$indus), 4)
corNox <- round(cor(dados$medv, dados$nox), 4)
corRm <- round(cor(dados$medv, dados$rm), 4)
corAge <- round(cor(dados$medv, dados$age), 4)
corDis <- round(cor(dados$medv, dados$dis), 4)
corTax <- round(cor(dados$medv, dados$tax), 4)
corPtratio <- round(cor(dados$medv, dados$ptratio), 4)
corB <- round(cor(dados$medv, dados$b), 4)
corLstat <- round(cor(dados$medv, dados$lstat), 4)

# Cor Test
cortestCrim <- stats::cor.test(dados$medv, dados$crim)
cortestZn <- stats::cor.test(dados$medv, dados$zn)
cortestIndus <- stats::cor.test(dados$medv, dados$indus)
cortestNox <- stats::cor.test(dados$medv, dados$nox)
cortestRm <- stats::cor.test(dados$medv, dados$rm)
cortestAge <- stats::cor.test(dados$medv, dados$age)
cortestDis <- stats::cor.test(dados$medv, dados$dis)
cortestTax <- stats::cor.test(dados$medv, dados$tax)
cortestPtratio <- stats::cor.test(dados$medv, dados$ptratio)
cortestB <- stats::cor.test(dados$medv, dados$b)
cortestLstat <- stats::cor.test(dados$medv, dados$lstat)

# Estatística t
resultados <- rbind(cortestCrim$statistic, 
           cortestZn$statistic, 
           cortestIndus$statistic,
            cortestNox$statistic,
            cortestRm$statistic,
            cortestAge$statistic,
            cortestDis$statistic,
            cortestTax$statistic,
            cortestPtratio$statistic,
            cortestB$statistic,
            cortestLstat$statistic)

# p-valor
aux <- rbind(cortestCrim$p.value,
cortestZn$p.value,
cortestIndus$p.value,
cortestNox$p.value,
cortestRm$p.value,
cortestAge$p.value,
cortestDis$p.value,
cortestTax$p.value,
cortestPtratio$p.value,
cortestB$p.value,
cortestLstat$p.value)

# aux <- rbind(cortestCrim$p.value|>round(5),
# cortestZn$p.value|>round(5),
# cortestIndus$p.value|>round(5),
# cortestNox$p.value|>round(5),
# cortestRm$p.value|>round(5),
# cortestAge$p.value|>round(5),
# cortestDis$p.value|>round(5),
# cortestTax$p.value|>round(5),
# cortestPtratio$p.value|>round(5),
# cortestB$p.value|>round(5),
# cortestLstat$p.value|>round(5))


resultados <- cbind(resultados, aux)

# IC
aux <- rbind(cortestCrim$conf.int[1:2],
             cortestZn$conf.int[1:2],
             cortestIndus$conf.int[1:2],
             cortestNox$conf.int[1:2],
             cortestRm$conf.int[1:2],
             cortestAge$conf.int[1:2],
             cortestDis$conf.int[1:2],
             cortestTax$conf.int[1:2],
             cortestPtratio$conf.int[1:2],
             cortestB$conf.int[1:2],
             cortestLstat$conf.int[1:2])

resultados <- cbind(resultados, aux)

rownames(resultados) <- c("Índice Criminalidade", "Prop. Terreno Zoneado", "Área Industrial", "Índice Oxido Nítrico", "N° Cômodos", "Idade do Imóvel", "Dist. Empregos", "Imposto Propriedade", "Prop. Prof.-Aluno", "Prop. Negros/bairro", "Pop. Classe Baixa")
colnames(resultados) <- c("t", "p-valor", "LI", "LS")

resultados|>
  kbl(
    caption = "Teste de Hipótese para Correlação",
    digits = 5,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", row.names = T, booktabs = T
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = T
  )|>
  footnote(
    general = "Teste realizado com 5% de significância",
    general_title = "Nota:",
    footnote_as_chunk = T
  )|>
  kable_material()


# Ajustes dos Modelos ----
(mLstat <- lm(medv~lstat, data = dados))

(lm(medv~lstat - 1, data = dados)) # Removendo beta0 - intercepto

# Exemplo de Modelo de Regressão Linear Multipla - MRLM
lm(medv ~ crim + indus + nox + rm + age + dis + tax + ptratio + lstat, data = dados)
# ________________________________________________

names(mLstat)

summary(mLstat)

mLstat$coefficients[1]
# ________________________________________________

mCrim <- lm(dados$medv~dados$crim)
mIndus <- lm(dados$medv~dados$indus)
mNox <- lm(dados$medv~dados$nox)
mRm <- lm(dados$medv~dados$rm)
mAge <- lm(dados$medv~dados$age)
mDis <- lm(dados$medv~dados$dis)
mTax <- lm(dados$medv~dados$tax)
mPtratio <- lm(dados$medv~dados$ptratio)
mLstat <- lm(dados$medv~dados$lstat)
mZn <- lm(dados$medv~dados$zn)
mB <- lm(dados$medv~dados$b)

# Calculando e armazenando o beta0 e erro padrão0
resultados <-  rbind(
  summary(mCrim)$coefficients[1,],
  summary(mIndus)$coefficients[1,],
  summary(mNox)$coefficients[1,],
  summary(mRm)$coefficients[1,],
  summary(mAge)$coefficients[1,],
  summary(mDis)$coefficients[1,],
  summary(mTax)$coefficients[1,],
  summary(mPtratio)$coefficients[1,],
  summary(mLstat)$coefficients[1,],
  summary(mZn)$coefficients[1,],
  summary(mB)$coefficients[1,])

# Removendo testes
resultados <-  resultados[, -c(3,4)]

# Calculando e armazenando o beta1 e erro padrão1
aux <-  rbind(
  summary(mCrim)$coefficients[2,],
  summary(mIndus)$coefficients[2,],
  summary(mNox)$coefficients[2,],
  summary(mRm)$coefficients[2,],
  summary(mAge)$coefficients[2,],
  summary(mDis)$coefficients[2,],
  summary(mTax)$coefficients[2,],
  summary(mPtratio)$coefficients[2,],
  summary(mLstat)$coefficients[2,],
  summary(mZn)$coefficients[2,],
  summary(mB)$coefficients[2,])

# Mantém apenas beta1 e o erro padrão
aux <- aux[, -c(3,4)]

resultados <- cbind(resultados, aux)

# Função para calcular o p-valor
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Calculando e armazenando o p-valor
aux <- rbind(
  lmp(mCrim), lmp(mIndus),
  lmp(mNox), lmp(mRm), lmp(mAge), 
  lmp(mDis), lmp(mTax), lmp(mPtratio),
  lmp(mLstat), lmp(mZn),lmp(mB)
)

resultados <- cbind(resultados, aux)

# Calculando e armazenando o Coeficiente de Correlação
aux <-  rbind(
  summary(mCrim)$r.squared,
  summary(mIndus)$r.squared,
  summary(mNox)$r.squared,
  summary(mRm)$r.squared,
  summary(mAge)$r.squared,
  summary(mDis)$r.squared,
  summary(mTax)$r.squared,
  summary(mPtratio)$r.squared,
  summary(mLstat)$r.squared,
  summary(mZn)$r.squared,
  summary(mB)$r.squared)

resultados <- cbind(resultados, aux)

# Inserindo o nome das variáveis (colunas)
rownames(resultados) <- c("Índice Criminalidade", "Área Industrial", "Índice Oxido Nítrico", "N° Cômodos", "Idade do Imóvel", "Dist. Empregos", "Imposto Propriedade", "Prop. Prof.-Aluno", "Pop. Classe Baixa", "Prop. Terreno Zoneado", "Prop. Negros/bairro")

# "Valor do Imóvel" = medv, "Acessibilidade Rodovias" = rad,  = zn,  = b

# Inserindo o nome das linhas
colnames(resultados) <- c("$\\beta_0$", "$\\sigma_0$", "$\\beta_1$", "$\\sigma_1$", "p-valor", "$R^2$")


resultados|>
  kbl(
    caption = "Sumarização dos Modelos Ajustados de Regressão Linear Simples - RLS.",
    format.args=list(big.mark=".", decimal.mark=","),
    digits = 3, align = "c", row.names = T, booktabs = T,
    escape = FALSE,
    col.names = c("Estimativa", "Erro Padrão", "Estatística t", "p-valor", "Estimativa", "Erro Padrão", "Estatística t", "p-valor", "$R^2$")
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header", "scale_down")
  )|>
  column_spec(1, bold = T
  )|>
  add_header_above(c(" " = 1, "$\\beta_0$" = 4, "$\\beta_1$" = 4, " " = 1))|>
  kable_material()


# Significância ANOVA ----

# fit_anova <- anova(mLstat)
# fit_sumario <- summary(mLstat)
# ic_parametros <- confint(mLstat)

# ic_parametros <- confint(mLstat)

ic_parametros[1,1]

resultados <- cbind(anova(mLstat), confint(mLstat))

rownames(resultados) <- c("$\\beta_0$", "$\\beta_1$")

resultados|>
  kbl(
    caption = "Análise de Variância (ANOVA) e Intervalos de Confiança para os parâmetros estimados no MRLS.",
    format.args=list(big.mark=".", decimal.mark=","),
    digits = 3, align = "c", row.names = T, booktabs = T,
    escape = F,
    col.names = c("GL", "Soma de Quadrados", "Quadrado Médio", "Estatística F-Snedecor", "p-valor", "$\\alpha$ = 2,5%", "(1 - $\\alpha$) = 97,5%")
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = T
  )|>
  add_header_above(c(" " = 1, "ANOVA" = 5, "Intervalos de Confiança" = 2))|>
  kable_material()

# fit_sumario[["coefficients"]] %>% tibble::as_tibble() %>% 
# teste %>% tibble::as_tibble() %>% 
#   kbl(
#     caption = "Sumarização do modelo ajustado.",
#     digits = 4,
#     format.args=list(big.mark=".", decimal.mark=","),
#     align = "c", 
#     row.names = T,
#     col.names =
#       c("Estimativa", "Erro Padrão", "Estatística t", "p-valor")
#   ) %>% 
#   footnote(
#     number = c("Linha 1: Dados referentes a β0", "Linha 2: Dados referentes a β1"),
#     number_title = "Legenda:",
#     footnote_as_chunk = F
#   )|>
#   kable_styling(
#     full_width = F, position = 'center', 
#     latex_options = c("striped", "HOLD_position", "repeat_header"))|>
#   column_spec(1, bold = F)|>
#   kable_material()

fit_anova %>%
  kbl(
    caption = "Resultados da ANOVA.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F,
    col.names =
      c("GL", "SQ", "QM", "Estatística", "p-valor")
  ) %>%
  footnote(
    number = c(
      "Linha 1: Dados referentes a β0", 
      "Linha 2: Dados referentes a β1",
      "GL: Graus de Liberdade", 
      "SQ: Soma de Quadrados", 
      "QM: Quadrado Médio", 
      "Estatística: F-Snedecor"
    ),
    number_title = "Legenda:",
    footnote_as_chunk = F
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = F
  )|>
  kable_material()

ic_parametros %>% 
  kbl(
    caption = "Intervalo de Confiança.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F,
    col.names =
      c("α/2 = 2,5%", "1-α/2 = 97,5%")
  ) %>%
  footnote(
    number = c("Linha 1: Dados referentes a β0", "Linha 2: Dados referentes a β1"),
    number_title = "Legenda:",
    footnote_as_chunk = F
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = F
  )|>
  kable_material()




# Ana. Resíduos ----
## Gráficos RBase ----
par(mfrow = c(2, 2))

plot(mLstat)

par(mfrow = c(1, 1))

# _____________________________________________

## Gráficos GGplot2 ----
dados_mLstat_resid <- broom::augment(mLstat)
dplyr::glimpse(dados_mLstat_resid)

# Gráfico de Resíduos contra Valor Médio
dados_mLstat_resid|>
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(color = "#234B6E") +
  geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_smooth(
    se = T, color = "tomato", method = 'loess', formula = 'y ~ x')+
  labs(
    x = "Valores Médios Ajustados",
    y = "Resíduos Ordinários",
    title = "Gráfico de Resíduos vs. Valores Ajustados"
  )+
  scale_x_continuous(breaks = seq(0,30,5))+
  theme_minimal(base_size = 7.5)+
  theme(legend.position = "none")

# seq(0,20,5)

## Gráfico de normalidade dos resíduos
dados_mLstat_resid %>% 
  ggplot(aes(sample = .std.resid)) + 
  qqplotr::stat_qq_band(alpha = 0.3) + # Plota a banda de confiança
  qqplotr::stat_qq_point(color = "#234B6E") + # Plota os pontos
  qqplotr::stat_qq_line(linetype = 2, size = 0.2) + # Plota a reta
  labs(
    x = "Quantil Teórico",
    y = "Quantil Amostral",
    title = "Gráfico quantil-quantil normal"
  )+
  scale_x_continuous(breaks = seq(-3,3,1))+
  theme_minimal(base_size = 7.5)

## Gráfico Homogeneidade de Variâncias (Locação-Escala)
dados_mLstat_resid %>% 
  ggplot(aes(x = .fitted, y = sqrt(abs(.std.resid)))) + 
  geom_point(color = "#234B6E") +
  # geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_smooth(
    se = T, color = "tomato", method = 'loess', formula = 'y ~ x')+
  # ylab("$\\sqrt(Resíduos Padronizados)$")+
  # ggtitle("Teste")+
  labs(
    x = "Valores Ajustados",
    y = "√|Resíduos Padronizados|",
    title = "Homogeneidade de Variâncias (Locação-Escala)"
  )+
  theme_minimal(base_size = 7.5)+
  theme(legend.position = "none")

# cores: 234B6E, 023047

## Teste pacote ----
pacman::p_load(performance, ggfortify)

performance::check_model(modelo, 
            check = c("linearity", "qq", "homogeneity", "outliers"))

performance::check_model(mLstat, 
            check = c("homogeneity", "outliers"))

ggfortify::autoplot(mLstat) # Não funcionou

autoplot(mLstat) # Não funcionou
# ----





# AnaRes Jeff
res <- mLstat$residuals

d1<- dados |>
  ggplot(aes(
    x = res, 
    y = mLstat$effects)) +
  geom_point(colour="tomato")+
  labs(
    title = '',
    y = 'Resíduos Ordinários',
    x = 'Valor Médio Ajustado'
  )+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))

d2 <- dados |>
  ggplot(aes(sample=res))+
  labs(
    title = '',
    y = 'Resíduos Studentizados',
    x = 'Quantis t-Student'
  )+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  stat_qq(colour="tomato") + stat_qq_line(col="blue") 



d1+d2 + plot_annotation(
  title = "Figura 5: Análise de resíduos do modelo de regressão da classe social com \n valor dos imóveis.") &
  theme_bw(base_size = 8) &
  theme(
    legend.position = "none",
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
  )



mLstat$residuals

dados_mLstat_resid$.resid

ks.test(res, "pnorm", mean(res), sd(res))

ks.test(dados_mLstat_resid$.resid, "pnorm", mean(dados_mLstat_resid$.resid), sd(dados_mLstat_resid$.resid))

devtools





# FIM ----



