# Aula de 2023/01/09 - Modelos em R

# Setup do Ambiente
library(rio)
library(ggplot2)
library(scales)
library(dplyr)
#install.packages("crosstable")
library(crosstable)
options(digits=2)

# Base BANCALVO
site = "https://github.com/"
diretorio = "gustavomirapalheta/classes_datasets/raw/master/"
arquivo = "bancalvo.xlsx"
url = paste0(site, diretorio, arquivo); url
df = as_tibble(import(url, sheet="BANCALVO")); df

url %>% import(sheet = "BANCALVO") %>% as_tibble() -> df; df

# Nomes das colunas
names(df)
View(df)

# Valores ausentes
sum(!complete.cases(df))
sum(complete.cases(df) == FALSE)

# Categorias da variável de saida
unique(df$STATUS)

# Contagem de STATUS (tabela e gráfico)
crosstable(df, STATUS)

ggplot(df, aes(x=STATUS)) + 
  geom_bar()

# Contagem de STATUS por SEXO
crosstable(df, SEXO, by=STATUS)
ggplot(df, aes(x=SEXO)) +
  geom_bar(aes(fill=STATUS),
           position='fill')

# Contagem de STATUS por UF
df %>%
  count(UF) %>%
  arrange(desc(n)) %>%
  mutate(p = n/sum(n)*100)

df %>%
  mutate(novaUF = ifelse(UF=="SP","SP","NOSP")) %>%
  select(-UF) -> df2; df2

crosstable(df2, novaUF, by=STATUS)
ggplot(df2, aes(x=novaUF)) +
  geom_bar(aes(fill=STATUS),
           position="fill")

# Escolaridade
crosstable(df2, ESCOLARIDADE, by=STATUS)

df2 %>% 
  mutate(novaESCOLARIDADE = ifelse(ESCOLARIDADE == 'posgrad',
                                   "z.posgrad", ESCOLARIDADE)) %>%
  select(-ESCOLARIDADE) -> df3; df3

crosstable(df3, novaESCOLARIDADE, by=STATUS)

ggplot(df3, aes(x=novaESCOLARIDADE)) +
  geom_bar(aes(fill=STATUS),
           position='fill')

# Estado Civil
crosstable(df3, ESTCIV, by=STATUS)

df3 %>% mutate(novoESTCIV = ifelse(ESTCIV == "solt" | ESTCIV == "outros",
                                   "0.solt", ESTCIV)) %>%
  select(-ESTCIV) -> df4; df4

crosstable(df4, novoESTCIV, by=STATUS)

df4 %>% ggplot(aes(x=novoESTCIV)) +
  geom_bar(aes(fill=STATUS),
           position="fill")

# NATUREZA
crosstable(df4, NATUREZA, by=STATUS)

# REGIAO
crosstable(df4, REGIAO, by=STATUS)

mutate(df4, novaREGIAO = ifelse(REGIAO == "B" | REGIAO =="C",
                                "BC", REGIAO),
            novaREGIAO = ifelse(novaREGIAO == "D" | novaREGIAO == "E",
                                "DE", novaREGIAO)) %>%
  select(-REGIAO) -> df5; df5

crosstable(df5, novaREGIAO, by=STATUS)

ggplot(df5, aes(x=novaREGIAO)) +
  geom_bar(aes(fill=STATUS),
           position='fill')

# interação entre Estado Civil e Sexo
df5 %>%
  group_by(novoESTCIV, SEXO, STATUS) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(STATUS == "inad") -> temp; temp

ggplot(temp, aes(x=SEXO, y=novoESTCIV)) +
  geom_tile(aes(fill=p),
            color='black') + 
  scale_fill_gradient(low = 'white',
                      high = 'steelblue')

library(tidyr)
select(temp, -STATUS, -n) %>%
  spread(key="SEXO", value="p")

# Idade (variável continua)
summary(df5$IDADE)
hist(df5$IDADE)

ggplot(df5, aes(x=IDADE)) + 
  geom_histogram(binwidth=2,
                 aes(fill=STATUS))

ggplot(df5, aes(x=STATUS, y=IDADE)) +
  geom_boxplot()

# RENDA
summary(df5$RENDA)

ggplot(df5, aes(x=RENDA)) + 
  geom_histogram()

quantile(df5$RENDA, c(0, 0.5/100, 99.5/100, 1))
cut(df5$RENDA, c(0, 35, 35000, 2e6)) %>% table()

filter(df5, RENDA>35, RENDA<35000) -> df6

ggplot(df6, aes(x=log10(RENDA))) + 
  geom_histogram(aes(fill=STATUS),
                 position="fill")

# Preparar a variavel de saida com INAD = 1
mutate(df6, STATUSinad = ifelse(STATUS == 'inad',1,0)) %>% 
  select(-STATUS, -IDENTIDADE) -> df7; df7
  
# Gerar o modelo
glm(df7, formula = STATUSinad ~ ., family='binomial') %>% summary()

