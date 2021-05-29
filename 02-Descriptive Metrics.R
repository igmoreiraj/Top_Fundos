rm(list = ls())

library(reshape2)
library(xts)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(readr)
library(GetBCBData)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# get fund data
df_prices <- read_rds('data/base_fundos.rds')
fundos<-read_rds("data/base_xp.rds")
de_para<-read_rds("data/de_para.rds")
dados_crus<-read_rds('data/base_crua.rds')

#ajusta nomes df_prices
df_prices<-df_prices[,de_para$nomes_base]

names(df_prices)<-de_para$V3[match(colnames(df_prices),de_para$nomes_base)]

source("fcts/adjust_names.R")

#compute returns
df_price.rets<-diff(df_prices)/lag.xts(df_prices)
#accumulated returns
df_rets_cum <- apply(df_price.rets,2,function(x) cumprod(1+replace(x, is.na(x), 0)))

#filtra para fundos que possuem mais de 2 anos de historia
temp<-df_price.rets["2017/"]
temp<-temp[,sapply(temp,Negate(anyNA))]

df_rets_cum<-df_rets_cum[,names(temp)]

#Remove Claritas Investimento no Exterior Fund
#df_rets_cum<-df_rets_cum[,-45]

#Get return data to do analysis
df_price.rets<-df_price.rets[,names(temp)]

#Pega série CDI
cdi<-gbcbd_get_series(id=12, format.data = "wide", first.date = as.Date("2005-01-01"))
cdi.xts<-xts(cdi[,-1], order.by = cdi$ref.date)

#transformar em retornos diários
cdi.xts<-(1+cdi.xts)^(1/252)-1
names(cdi.xts)<-"CDI"


#sharpe<-SharpeRatio.annualized(df_price.rets, Rf=mean(cdi.xts, na.rm=T))

covmat<-covEstimation(df_price.rets["2017/"],control = list(type='ewma', lambda=0.99))
cor.mat<-cov2cor(covmat)

rds_out_covmat <- 'data/covmat.rds'
write_rds(covmat, rds_out_covmat)


rds_out_de_para <- 'data/de_para.rds'
write_rds(de_para, rds_out_de_para)

rds_out_rets <- 'data/rets.rds'
write_rds(df_price.rets, rds_out_rets)

rds_out_cdi <- 'data/cdi.rds'
write_rds(cdi.xts, rds_out_cdi)

rds_out_fundos <- 'data/fundos_prices.rds'
write_rds(df_prices, rds_out_fundos)

