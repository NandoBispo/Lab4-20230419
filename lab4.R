# PACOTES ----

if (!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools,
               kableExtra, moments, ggpubr, formattable, gridExtra, 
               glue, corrplot, sessioninfo, readxl, writexl, ggthemes,
               patchwork,  plotly, lmtest, olsrr, gglm, ggplot2,
               tidymodels, GGally, skimr, qqplotr)

## Identificação dos pacotes ----
# - tidyverse
# - janitor: Pacote para arrumação do conjunto dos dados e padronizar os nomes das variáveis com o comando "janitor::clean_names()"
# - skimr: Pacote que gera um mini relatório dos dados e identifica dados faltantes como comando "skimr::skim()".
# - qqplotr: Pacote que gera gráficos qq

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
    chas = forcats::as_factor(chas))

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
    ggplot(aes(y = medv, x = age, color = age)) +
    geom_point()+
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
      aes(label = ..r.label..),
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

# Correlação ----

cor.test(dados$medv, dados$crim)

dados %>% 
  select(-zn, -rad, -b, -chas) %>% 
  cor() %>% corrplot(method = "circle", type = "lower")

# Ajuste do modelo ----
(mFit <- lm(medv~crim, data = dados))

(lm(medv~crim - 1, data = dados)) # Removendo beta0 - intercepto

(lm(medv~crim, data = dados))

lm(medv ~ crim + indus + nox + rm + age + dis + tax + ptratio + lstat, data = dados)

names(mFit)

summary(mFit)

mFit$coefficients[1]

dados_mFit_resid <- broom::augment(mFit)
dados_mFit_resid


lm(medv~lstat, data = dados) %>% 
broom::augment() %>% 
  ggplot() + 
  geom_point(aes(x = .fitted, y = .resid, color = .resid)) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Valor Médio Ajustado",
    y = "Resíduoss",
    title = "Gráfico de Resíduos contra Valor Médio"
  )+
  theme(legend.position = "none")


# Ajuste do Modelo ----

mCrim <- lm(dados$medv~dados$crim)
mIndus <- lm(dados$medv~dados$indus)
mNox <- lm(dados$medv~dados$nox)
mRm <- lm(dados$medv~dados$rm)
mAge <- lm(dados$medv~dados$age)
mDis <- lm(dados$medv~dados$dis)
mTax <- lm(dados$medv~dados$tax)
mPtratio <- lm(dados$medv~dados$ptratio)
mLstat <- lm(dados$medv~dados$lstat)

# Calculando e armazenando o beta0 e erro padrão0
resultados <-  cbind(
  summary(mCrim)$coefficients[1,],
  summary(mIndus)$coefficients[1,],
  summary(mNox)$coefficients[1,],
  summary(mRm)$coefficients[1,],
  summary(mAge)$coefficients[1,],
  summary(mDis)$coefficients[1,],
  summary(mTax)$coefficients[1,],
  summary(mPtratio)$coefficients[1,],
  summary(mLstat)$coefficients[1,])

# Removendo testes
resultados <-  resultados[-c(3,4),]

# Calculando e armazenando o beta1 e erro padrão1
aux <-  cbind(
  summary(mCrim)$coefficients[2,],
  summary(mIndus)$coefficients[2,],
  summary(mNox)$coefficients[2,],
  summary(mRm)$coefficients[2,],
  summary(mAge)$coefficients[2,],
  summary(mDis)$coefficients[2,],
  summary(mTax)$coefficients[2,],
  summary(mPtratio)$coefficients[2,],
  summary(mLstat)$coefficients[2,])

# Mantém apenas beta1 e o erro padrão
aux <- aux[-c(3,4),]

resultados <- rbind(resultados, aux)

# Função para calcular o p-valor
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Calculando e armazenando o p-valor
aux <- cbind(
  lmp(mCrim), lmp(mIndus),
  lmp(mNox), lmp(mRm),
  lmp(mAge), lmp(mDis),
  lmp(mTax),lmp(mPtratio),lmp(mLstat))

resultados <- rbind(resultados, aux)

# Calculando e armazenando o Coeficiente de Correlação
aux <-  cbind(
  summary(mCrim)$r.squared,
  summary(mIndus)$r.squared,
  summary(mNox)$r.squared,
  summary(mRm)$r.squared,
  summary(mAge)$r.squared,
  summary(mDis)$r.squared,
  summary(mTax)$r.squared,
  summary(mPtratio)$r.squared,
  summary(mLstat)$r.squared)

resultados <- rbind(resultados, aux)

# Inserindo o nome das variáveis (colunas)
colnames(resultados) <- c("CRIM", "INDUS", "NOX", "RM", "AGE", "DIS", "TAX", "PTRATIO", "LSTAT")

# Inserindo o nome das linhas
rownames(resultados) <- c("$\\beta_0$", "$\\sigma_0$", "$\\beta_1$", "$\\sigma_1$", "p-valor", "$\\hat \\rho$")

resultados|>
  kbl(
    caption = "Valores dos modelos de regressão linear simples.",
    format.args=list(big.mark=".", decimal.mark=","),
    digits = 3, align = "c", row.names = T, booktabs = T,
    # format = "latex", 
    escape = FALSE,
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "scale_down", "repeat_header")
  )|>
  column_spec(1, bold = F
  )|>
  footnote(
    general = "StatLib - Carnegie Mellon University",
    general_title = "Fonte:",
    footnote_as_chunk = T
  )|>
  kableExtra::add_footnote(c("Legenda:"), notation = "none")|>
  kableExtra::add_footnote(c(
  "AGE: Proporção de unidades próprias construídas antes de 1940.",
  "CRIM: Índice de criminalidade per capita por bairro.",
  "DIS: Distâncias ponderadas para cinco centros de emprego de Boston.",
  "INDUS: Proporção de hectares de negócios não varejistas por bairro.",
  'LSTAT: Percentual da população de "classe baixa".',
  "NOX: Concentração de óxidos nítricos (partes por 10 milhões).",
  "PTRATIO: Proporção aluno-professor por bairro.",
  "RM: Número médio de cômodos por habitação.",
  "TAX: Valor total do imposto predial por $10.000."),
    notation = "none"
  )|>
  kable_material()



# ----

res <- mLstat$residuals

d1<- dados |>
  ggplot(aes(
    x = res, 
    y = mLstat$effects)) +
  geom_point(colour="tomato")+
  labs(
    title = '',
    y = 'Resíduos',
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

## Gráfico de resíduos padronizads vs preditos ----
dados_mFit_resid %>% 
  ggplot() + 
  geom_point(aes(x = .fitted, y = .std.resid, color = .std.resid)) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Valores Preditos",
    y = "Resíduos Padronizados",
    title = "Gráfico de Resíduos Padronizados contra Preditos"
  )+
  theme(legend.position = "none" )

## Gráfico de normalidade dos resíduos ----
dados_mFit_resid %>% 
  ggplot(aes(sample = .std.resid)) + 
  qqplotr::stat_qq_band() + # Plota a banda de confiança
  qqplotr::stat_qq_point() + # Plota os pontos
  qqplotr::stat_qq_line() + # Plota a reta
  labs(
    x = "Quantil Teórico",
    y = "Quantil Amostral",
    title = "Gráfico quantil-quantil normal"
  )


devtools





# FIM ----



