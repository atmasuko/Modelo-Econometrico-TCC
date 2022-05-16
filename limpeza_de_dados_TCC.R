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

# Renomeando os nomes das colunas
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

mydata <- merge(ano_2000_1, ano_2000_2, by=c("muni_code"), all = TRUE)

ano_2004_1 <- municipal_elections %>%
  filter(year %in% c("2004"))

ano_2004_2 <- retro %>%
  filter(year %in% c("2004"))

mydata2 <- merge(ano_2004_1, ano_2004_2, by=c("muni_code"), all = TRUE)

df3 <- rbind(mydata, mydata2)

# ordenando códigos
df3 <- df3[order(df3$muni_code),]

ano_2008_1 <- municipal_elections %>%
  filter(year %in% c("2008"))

ano_2008_2 <- retro %>%
  filter(year %in% c("2008"))

mydata4 <- merge(ano_2008_1, ano_2008_2, by=c("muni_code"), all = TRUE)

ano_2012_1 <- municipal_elections %>%
  filter(year %in% c("2012"))

ano_2012_2 <- retro %>%
  filter(year %in% c("2012"))
ano_2012_2$muni_code <- as.numeric(ano_2012_2$muni_code)

mydata5 <- merge(ano_2012_1, ano_2012_2, by=c("muni_code"), all = TRUE)

df4 <- rbind(mydata4, mydata5)

df4 <- df4[order(df4$muni_code),]

df5 <- rbind(df3, df4)
df5 <- df5[order(df5$muni_code),]



