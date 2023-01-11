# Setup do Ambiente
library(rio)
library(ggplot2)
library(scales)
library(dplyr)
library(rpart)
library(rpart.plot, help, pos = 2, lib.loc = NULL)
library(rattle)
library(crosstable)
options(digits=2)

# Base BANCALVO
site = "https://github.com/"
diretorio = "gustavomirapalheta/classes_datasets/raw/master/"
arquivo = "bancalvo.xlsx"
url = paste0(site, diretorio, arquivo); url
df = as_tibble(import(url, sheet="BANCALVO"))

# Dummy para UF = SP
df %>%
  mutate(novaUF = ifelse(UF=="SP","SP","NOSP")) %>%
  select(-UF) -> df2

# Reordena categorias de ESCOLARIDADE acrescentando "z." a "posgrad"
df2 %>% 
  mutate(novaESCOLARIDADE = ifelse(ESCOLARIDADE == 'posgrad',
                                   "z.posgrad", ESCOLARIDADE)) %>%
  select(-ESCOLARIDADE) -> df3

# Agrega categorias de ESTCIV "solt" e "outros" em "0.solt"
df3 %>% mutate(novoESTCIV = ifelse(ESTCIV == "solt" | ESTCIV == "outros",
                                   "0.solt", ESTCIV)) %>%
  select(-ESTCIV) -> df4

# Agrega categorias de REGIAO "B" e "C" em "BC", "D" e "E" em "DE"
mutate(df4, novaREGIAO = ifelse(REGIAO == "B" | REGIAO =="C",
                                "BC", REGIAO),
            novaREGIAO = ifelse(novaREGIAO == "D" | novaREGIAO == "E",
                                "DE", novaREGIAO)) %>%
  select(-REGIAO) -> df5

# Retira outliers de RENDA
filter(df5, RENDA>35, RENDA<35000) -> df6

# Preparar a variavel de saida com INAD = 1
mutate(df6, STATUSinad = ifelse(STATUS == 'inad',1,0)) %>% 
  select(-STATUS, -IDENTIDADE) -> df7

# Criação de modelo para conjunto de treino
df7 %>%
    mutate(STATUSinad = as.factor(STATUSinad)) %>%
    rpart(formula = STATUSinad ~ ., model = TRUE) -> modelo

fancyRpartPlot(modelo, yesno = 2)