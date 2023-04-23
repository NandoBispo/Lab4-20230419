# PACOTES ----

if (!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools,
               kableExtra, moments, ggpubr, formattable, gridExtra, 
               glue, corrplot, sessioninfo, readxl, writexl, ggthemes,
               patchwork,  plotly, lmtest, olsrr, gglm, ggplot2,
               tidymodels, GGally, skimr)

## Identificação dos pacotes ----
# - tidyverse
# - janitor: Pacote para arrumação do conjunto dos dados e padronizar os nomes das variáveis com o comando "janitor::clean_names()"
# - skimr: Pacote que gera um mini relatório dos dados e identifica dados faltantes como comando "skimr::skim()".

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
    title = "Unidades constuídas \nantes de 1940",
    x = "Proporção",
    y = "Densidade"
  )

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
    title = "Distância para cinco \ncentros de emprego.",
    x = "Distâncias Ponderadas",
    y = "Densidade"
  )

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

## 5 ----

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
    title = "Valor total do imposto predial",
    x = "Valor por $10.000",
    y = "Densidade"
  )

(g1+g2) + plot_annotation(
  title = "Figura 1: Histogramas das variáveis em análise.",
  tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  tag_suffix = ":") &
  theme_bw(base_size = 9) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 7, hjust = 0, vjust = -0.2)
  )

(g3+g4) + plot_annotation(
  # title = "Figura 1: Histogramas das variáveis em análise.",
  tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  tag_suffix = ":") &
  theme_bw(base_size = 9) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 7, hjust = 0, vjust = -0.2)
  )

(g5+g6) + plot_annotation(
  # title = "Figura 1: Histogramas das variáveis em análise.",
  tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  tag_suffix = ":") &
  theme_bw(base_size = 9) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 7, hjust = 0, vjust = -0.2)
  )

(g7+g8) + plot_annotation(
  # title = "Figura 1: Histogramas das variáveis em análise.",
  tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  tag_suffix = ":") &
  theme_bw(base_size = 9) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 7, hjust = 0, vjust = -0.2)
  )

(g9+g10) + plot_annotation(
  caption = "Fonte: StatLib - Carnegie Mellon University",
  tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  tag_suffix = ":") &
  theme_bw(base_size = 9) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 7, hjust = 0, vjust = -0.2)
  )


(g13+g14)/(plot_spacer() + plot_spacer()) +
  plot_annotation(
    caption = "Fonte: StatLib - Carnegie Mellon University") & 
  theme_bw(base_size = 8) &
  theme(
    legend.position = "none",
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
  )

# BoxPlot ----


# FIM ----



