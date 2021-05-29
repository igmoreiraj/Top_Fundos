library(httr)
library(jsonlite)
library(xts)
library(tidyverse)
library(RSQLite)


temp<-fromJSON(txt="base_xp/fundos_xp_20200625.txt")

fundos_abertos<-temp[temp$fundingBlocked==F,]

consulta_cnpjs<-fundos_abertos$cnpj
consulta_cnpjs_2<-paste(substr(consulta_cnpjs,1,2), 
                          substr(consulta_cnpjs,3,5),
                          substr(consulta_cnpjs,6,14), sep = ".")
consulta_cnpjs_2<-paste(substr(consulta_cnpjs_2,1,10),substr(consulta_cnpjs_2,11,16),sep = "/")                        
consulta_cnpjs_2<-paste(substr(consulta_cnpjs_2,1,15),substr(consulta_cnpjs_2,16,17),sep = "-")


#converte datas para string para inserir na query
datas.vector.string = toString(sprintf("'%s'", consulta_cnpjs_2))
#cria query do sql com (%s) de  id para ser substituído depois pelas datas
sql_fmt<-"SELECT * from dataset_fundos where CNPJ_FUNDO in (%s)"
#aplica a substituição do id pelo vetor de datas em forma de string
sql<-sprintf(sql_fmt,datas.vector.string)

#Conectar ao BD
conn = dbConnect(dbDriver("SQLite"),"D:/Blog/Estudo FIAs/base_fundos.sqlite")
#Send SQL Query
dados.fundos_xp<-dbGetQuery(conn,sql)
#Disconectar do BD
dbDisconnect(conn)

#check if all data was imported
length(unique(dados.fundos_xp$CNPJ_FUNDO))==length(consulta_cnpjs_2)

#converte coluna de date para o formato Date
dados.fundos_xp$DT_COMPTC<-as.Date(dados.fundos_xp$DT_COMPTC)

dados.fundos_xp.adj<-dados.fundos_xp[!duplicated(dados.fundos_xp[,c(2,4,6)]),]

dados.fundos_xp_wide<-dcast(dados.fundos_xp.adj[,c(2,4,6)], DT_COMPTC~DENOM_SOCIAL, value.var ="VL_QUOTA")

base.xts<-xts(dados.fundos_xp_wide[,-1],order.by=dados.fundos_xp_wide[,1])

de_para<-as.data.frame(cbind(consulta_cnpjs, consulta_cnpjs_2, fundos_abertos$name))

nomes_base<-dados.fundos_xp[match(de_para$consulta_cnpjs_2,dados.fundos_xp$CNPJ_FUNDO),"DENOM_SOCIAL"]

de_para<-cbind(de_para,nomes_base)
#names(base.xts)<-de_para$V3

rds_out <- 'data/base_fundos.rds'
write_rds(base.xts, rds_out)

rds_out_xp <- 'data/base_xp.rds'
write_rds(fundos_abertos, rds_out_xp)

rds_out_cnpj <- 'data/cnpj.rds'
write_rds(consulta_cnpjs_2, rds_out_cnpj)

rds_out_base_crua <- 'data/base_crua.rds'
write_rds(dados.fundos_xp, rds_out_base_crua)

rds_out_de_para <- 'data/de_para.rds'
write_rds(de_para, rds_out_de_para)

