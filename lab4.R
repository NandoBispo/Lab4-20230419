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


dados <- dados |> 
  mutate(
    CRIM = as.numeric(CRIM),
    ZN = as.numeric(ZN),
    INDUS = as.numeric(INDUS),
    CHAS = as.numeric(CHAS),
    NOX = as.numeric(NOX),
    RM = as.numeric(RM),
    AGE = as.numeric(AGE),
    DIS = as.numeric(DIS),
    RAD = as.numeric(RAD),
    TAX = as.numeric(TAX),
    PTRATIO = as.numeric(PTRATIO),
    B = as.numeric(B),
    LSTAT = as.numeric(LSTAT),
    MEDV = as.numeric(MEDV)
  )
