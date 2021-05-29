rm(list = ls())

library(reshape2)
library(xts)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(readr)
library(GetBCBData)
library(BatchGetSymbols)
library(tidyr)
library(purrr)
library(broom)
library(lmtest)
library(sandwich)
library(RSQLite)

source("fcts/Gera_cov.R")
source("04-Get Factor Data.R")
source("fcts/regressions.R")
source("fcts/Get_cadastro.R")
source("fcts/filtra_fundos.R", encoding = "UTF-8")

# get fund data
rets <- read_rds('data/rets.rds')
fundos<-read_rds("data/base_xp.rds")
de_para<-read_rds("data/de_para.rds")
cdi<-read_rds('data/cdi.rds')
cadastro<-pega_cadastro(Sys.Date()-2)

#break the data into groups
de_para$classe<-fundos$classificationXp
de_para$Inv_Qualif<-cadastro$INVEST_QUALIF[match(de_para$consulta_cnpjs_2,cadastro$CNPJ_FUNDO)]

#filtra fundos
carteira_arrojada<-filtra_carteira(rets=rets, 
                                   fundos = fundos,
                                   de_para = de_para,
                                   factors = df_prices.wide,
                                   cdi = cdi, 
                                   carteira = "Arrojada",
                                   Alpha_thres = 7,
                                   Ret_thres = 8,
                                   Sharpe_thres = 0.7)


carteira_moderada<-filtra_carteira(rets=rets, 
                                   fundos = fundos,
                                   de_para = de_para,
                                   factors = df_prices.wide,
                                   cdi = cdi, 
                                   carteira = "Moderada",
                                   Alpha_thres = 7,
                                   Ret_thres = 6,
                                   Sharpe_thres = 1)


carteira_conservadora<-filtra_carteira(rets=rets, 
                                   fundos = fundos,
                                   de_para = de_para,
                                   factors = df_prices.wide,
                                   cdi = cdi, 
                                   carteira = "Conservadora",
                                   Alpha_thres = 2,
                                   Ret_thres = 5,
                                   Sharpe_thres = 1.2)


carteira = list("Conservadora" = carteira_conservadora, "Moderada" = carteira_moderada, "Arrojada" = carteira_arrojada)

#save outputs
rds_out_carteira <- 'carteiras/carteira.rds'
write_rds(carteira, rds_out_carteira)

rds_out_de_para <- 'data/de_para.rds'
write_rds(de_para, rds_out_de_para)



