# Estratégia Empírica para Tese de Monografia

# carregando os pacotes 

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

# Cores para layout de gráficos
corTend <- c("#007E8E","#92D050","#7B9AA9","#3D4F59","#92D050","#18A711","#247E6F","#BEC0BE","#30885E","#D1E3D1")
Pd.Tend <- list(theme(text=element_text(size=10,family="Verdana"),
                    legend.position="bottom",legend.title = element_blank(), 
                    panel.background = element_blank(),axis.line=element_line(colour = "white", size=0.0)))


fixed_effect_logistic <- clogit(reelect ~ log(commodity_index) + interest_rate + fedalign + governoralign + left + right + Lgrgdp + strata(codigo_municipio), data = municipal_elections)
fe_logistic8 <- clogit(reelect ~ log(commodity_index) + interest_rate + fedalign + governoralign + left + right + Lgrgdp + IPCA + strata(codigo_municipio), data = municipal_elections)

stargazer(fixed_effect_logistic,
          logistic2,
          fe_logistic3,
          fe_logistic7,
          type = "html",
          out = "2_regressao_logistica_efeitos_fixos.html",
          title = "Regression Results")

# os 10 estados que mais exportam commodities
heavy_exporter <- municipal_elections %>%
  filter(state %in% c("SP", "RJ", "MG", "RS", "PA", "MT", "PR", "ES", "SC", "BA"))

# total - 10 estados mais relevantes
light_exporter <- municipal_elections %>%
  filter(!state %in% c("SP", "RJ", "MG", "RS", "PA", "MT", "PR", "ES", "SC", "BA"))

# Renomeando os nomes das colunas
names(municipal_elections)[7] <- 'muni_code'

# construindo os modelos com peso de cada estado

fixed_effect_logistic <- clogit(reelect ~ log(commodity_index) + interest_rate + fedalign + governoralign + left + right + Lgrgdp + log(bolsafamilia) + lnjobs_muni + lntotalexp + strata(codigo_municipio), data = municipal_elections)

fixed_effect_logistic_heavy <- clogit(reelect ~ log(commodity_index) + interest_rate + fedalign + governoralign + left + right + Lgrgdp + strata(codigo_municipio), data = heavy_exporter)

fixed_effect_logistic_light <- clogit(reelect ~ log(commodity_index) + interest_rate + fedalign + governoralign + left + right + Lgrgdp + strata(codigo_municipio), data = light_exporter)


stargazer(fixed_effect_logistic,
          fixed_effect_logistic_heavy,
          fixed_effect_logistic_light,
          type = "html",
          out = "__regressao_logistica_efeitos_fixos.html",
          title = "Regression Results")

# Fixed effects
probit <- bife(reelect ~ log(commodity_index) + lnjobs + Lgrgdp + fedalign + governoralign + bolsafamilia | id, municipal_elections, "probit")
logit <- bife(reelect ~ log(commodity_index) + lnjobs + Lgrgdp + fedalign + governoralign + bolsafamilia | id, municipal_elections, "probit")

# OLS
model <- lm(reelect ~ log(commodity_index) + lnjobs + Lgrgdp + log(bolsafamilia) + fedalign + governoralign, data = municipal_elections)


within <- plm(reelect ~ log(commodity_index) + lnjobs + Lgrgdp + log(bolsafamilia) + fedalign + governoralign, 
                    data = municipal_elections,
                    index = c("id", "year"), 
                    model = "within")

mqmo <- feols(reelect ~ commodity_index + lnjobs + Lgrgdp + fedalign + governoralign + bolsafamilia | id + year, municipal_elections)

fe2 <- plm(reelect ~ log(commodity_index) + lnjobs + Lgrgdp + fedalign + governoralign + priceindex + unemp,
            data = municipal_elections,
            index = c("id", "year"), 
            model = "within")


fe3 <- plm(reelect ~ log(commodity_index),
             data = municipal_elections,
             index = c("id", "year"), 
             model = "within")

fe4 <- plm(reelect ~ log(commodity_index) + lnjobs + Lgrgdp + fedalign + governoralign + priceindex + unemp + lnexppc,
           data = municipal_elections,
           index = c("id", "year"), 
           model = "within")

ols <- lm(reelect ~ log(commodity_index) + lnjobs + Lgrgdp + fedalign + governoralign + priceindex + unemp + lnexppc, data = municipal_elections)
ols2 <- lm(reelect ~ log(commodity_index) + fedalign + governoralign + priceindex + unemp + lnexppc, data = municipal_elections)

# salvando como modelo
stargazer(ols,
          ols2,
          fe2,
          fe4,
          type = "html",
          out = "modelos.html")

# gráficos de distribuição
ggplot(data = municipal_elections) +
geom_histogram(mapping = aes(x = lnexppc), fill = "steelblue", binwidth = .1) + theme_minimal()

# gráficos para checar possíveis outliers
ggplot(data = municipal_elections) +
  geom_point(mapping = aes(x = year, y = lnjobs)) + theme_minimal()

mix_ap <- ggplot(data = NFs_Mix_e_Nova_Luz_ajustado) +
  geom_histogram(mapping = aes(y = `valor mix ap`, fill = `Tipo de Combustível mix ap`, binwidth =.1)) + coord_flip() + Pd.Tend + labs(x = "Valor Total", y = "Contagem")

nova_luz <- ggplot(data = NFs_Mix_e_Nova_Luz_ajustado) +
  geom_histogram(mapping = aes(y = `valor novo luz`, fill = `Tipo de Combustível novo luz`, binwidth =.1)) + coord_flip() + Pd.Tend + labs(x = "Contagem", y = "Valor Total")

# pacote vtable
st(comparacao_posto4, file = 'comparada-tabela.html')

retro$id <- group_indices(retro, muni_code, year)

retro<-retro[!duplicated(retro[, 1:2])]

retro <- retro %>%
  filter(year!=2016)

municipal_elections <- municipal_elections %>%
  filter(year %in% c("2000", "2004", "2008", "2012"))


names(municipal_elections)[7] <- 'muni_code'

data <- merge(municipal_elections, retro, by = "muni_code")

data1 <- municipal_elections%>%
  select(year, muni_code, reelect)

data2 <- retro%>%
  select(year, muni_code, inflation)

data1 <- municipal_elections %>%
  filter(year %in% c("2000"))
data2 <- retro %>%
  filter(year %in% c("2000"))

data2000 <- merge(data1, data2, by = "muni_code")

data3 <- municipal_elections %>%
  filter(year %in% c("2004"))
data4 <- retro %>%
  filter(year %in% c("2004"))

data2004 <- merge(data3, data4, by = "muni_code")

df3 <- do.call(rbind.data.frame, list(data2000, data2004))

data5 <- municipal_elections %>%
  filter(year %in% c("2008"))
data6 <- retro %>%
  filter(year %in% c("2008"))

data2008 <- merge(data5, data6, by = "muni_code")

data7 <- municipal_elections %>%
  filter(year %in% c("2012"))
data8 <- retro %>%
  filter(year %in% c("2012"))

data2012 <- merge(data7, data8, by = "muni_code")

df4 <- do.call(rbind.data.frame, list(data2008, data2012))

df5 <- do.call(rbind.data.frame, list(df3, df4))

auou <- df5 %>%
  select(year.x, inflation, nm_mun_2014, reelect)

ao2<-auou[!duplicated(auou[, 1:2])]
df22<-df5[!duplicated(auou[, 1:2])]

bd <- df22 %>%
  select(nm_mun_2014, year.x, inflation, reelect, fedalign, governoralign, priceindex, unemp, lnexppc, elected, muni_code)

bd <- bd[order(bd$nm_mun_2014),]
names(bd)[2] <- "year"
names(bd)[1] <- "muni_name"
names(bd)[3] <- "commodities"

save(bd, file = "dados-limpos-tcc.RData")

bd <- data.frame(bd)
bd$year <- as.factor(bd$year)
bd$muni_name <- as.factor(bd$muni_name)

bd<-bd[!duplicated(bd[, 1:2])]

