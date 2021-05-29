library(readr)
library(GetBCBData)

#Pega série CDI
cdi<-gbcbd_get_series(id=12, format.data = "wide")
cdi.xts<-xts(cdi[,-1], order.by = cdi$ref.date)

#transformar em retornos diários
#cdi.xts<-(1+cdi.xts)^(1/252)-1
cdi.xts<-cdi.xts/100
names(cdi.xts)<-"CDI"

rds_out_cdi <- 'data/cdi.rds'
write_rds(cdi.xts, rds_out_cdi)

rm(rds_out_cdi);rm(cdi)