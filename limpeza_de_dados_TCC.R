library(tidyverse)
library(readxl)
library(knitr)
library(stargazer)
library(scales)
library(fixest)
library(modelsummary)
library(survival)
library(bife)
library(plm)
library(vtable)

# Renomeando os nomes das colunas para facilitar a concatenação
names(municipal_elections)[7] <- 'muni_code'

# filtrando os anos

municipal_elections <- municipal_elections %>%
  filter(year %in% c("2000", "2004", "2008", "2012"))

retro <- retro %>%
  filter(year %in% c("2000", "2004", "2008", "2012"))

# removendo anos duplicados em sequência
retro <- retro[!duplicated(retro[, 1:2])]

ano_2000_1 <- municipal_elections %>%
  filter(year %in% c("2000"))

ano_2000_2 <- retro %>%
  filter(year %in% c("2000"))

# concatenando as duas bases de dados para o ano 2000
mydata <- merge(ano_2000_1, ano_2000_2, by=c("muni_code"), all = TRUE)

ano_2004_1 <- municipal_elections %>%
  filter(year %in% c("2004"))

ano_2004_2 <- retro %>%
  filter(year %in% c("2004"))

# concatenando as duas bases de dados para o ano 2004
mydata2 <- merge(ano_2004_1, ano_2004_2, by=c("muni_code"), all = TRUE)

# concatenando os anos 2000 e 2004
df3 <- rbind(mydata, mydata2)

# reordenando códigos
df3 <- df3[order(df3$muni_code),]

ano_2008_1 <- municipal_elections %>%
  filter(year %in% c("2008"))

ano_2008_2 <- retro %>%
  filter(year %in% c("2008"))

# concatenando as duas bases de dados para o ano 2008
mydata4 <- merge(ano_2008_1, ano_2008_2, by=c("muni_code"), all = TRUE)

ano_2012_1 <- municipal_elections %>%
  filter(year %in% c("2012"))

ano_2012_2 <- retro %>%
  filter(year %in% c("2012"))

ano_2012_2$muni_code <- as.numeric(ano_2012_2$muni_code)

# concatenando as duas bases de dados para o ano 2012
mydata5 <- merge(ano_2012_1, ano_2012_2, by=c("muni_code"), all = TRUE)

# concatenando os 2008 e 2012
df4 <- rbind(mydata4, mydata5)

# reordenação dos códigos municipais
df4 <- df4[order(df4$muni_code),]

# junção de todos os anos 2000-2014
df5 <- rbind(df3, df4)
df5 <- df5[order(df5$muni_code),]

# base de dados limpa
save(df5, file = "revisada_base_dados_tcc.RData")


