
library(tidyverse)
library(lubridate)
library(readr)
library(sandwich)
library(lmtest)
library(purrr)

#Load library
source('00_Scripts/04-Get Factor Data.R')

#carrega dados
fundos<-read_rds("00_Data/base_xp.rds")
de_para<-read_rds("00_Data/de_para.rds")
cdi<-read_rds('00_Data/cdi.rds')
df_prices <- read_rds('00_Data/fundos_prices.rds')
#pega base de cadastro cvm
cad_fi <- read_delim("http://dados.cvm.gov.br/dados/FI/CAD/DADOS/cad_fi.csv", 
                     ";", escape_double = FALSE, col_types = cols(CLASSE = col_character(), 
                                                                  INVEST_QUALIF = col_character()), 
                     trim_ws = TRUE)

# constroi de_para
de_para_adj <- de_para %>% 
  left_join(cad_fi %>% select(CNPJ_FUNDO, INVEST_QUALIF, CLASSE), 
            by = c("consulta_cnpjs" = "CNPJ_FUNDO"))

#cria nomes_fundos
nomes_fundos_filtrados<-fundos %>% 
  filter(classificationCvm == "Ações") %>% 
  select(name)

#Gera base Fatores
Fatores <- df_prices.wide %>% 
  as_tibble() %>% 
  mutate(Date = index(df_prices.wide)) %>% 
  relocate(Date) %>% 
  setNames(c("Date", "Ibovespa")) %>% 
  mutate(Date = as.Date(Date)) %>% 
  left_join(cdi %>% as_tibble() %>% mutate("Date" = index(cdi) %>% setNames(c("Date", "Cdi"))))


base_rets<-df_prices %>%
  as_tibble() %>% 
  mutate("Date" = index(df_prices)) %>% 
  pivot_longer(
    cols = -Date,
    names_to = "Fundos",
    values_to = "Prices"
  ) %>% 
  group_by(Fundos) %>% 
  mutate(Retornos = Prices / dplyr::lag(Prices)-1) %>% 
  filter(Fundos %in% nomes_fundos_filtrados$name) %>% 
  left_join(Fatores, by="Date") %>% 
  mutate("Retorno_sem_rf" = Retornos - CDI,
         "Ibovespa_rf" = Ibovespa - CDI) %>% 
  drop_na() %>% 
  nest(-Fundos) %>% 
  mutate("model_lm" = map(data, ~ lm(Retorno_sem_rf ~ Ibovespa_rf, data=.)),
         "tidy_lm" = map(model_lm, ~broom::tidy(coeftest(.x, vcov. = NeweyWest(.x))))) %>% 
  select(-model_lm) %>% 
  unnest(tidy_lm) %>% 
  filter(term=="(Intercept)") %>% 
  select(Fundos, data, statistic) %>% 
  mutate("Retorno_Ann" = map_dbl(data, .f= ~ .x %>% 
                                   filter(Date>as.Date("2020-01-01")) %>%
                                   summarise(prod(1 + Retornos)^(252/n())-1) %>% 
                                   as.numeric()),
         "Vol" = map_dbl(data, .f= ~ .x %>% 
                           filter(Date>as.Date("2020-01-01")) %>%
                           summarise(sd(Retornos, na.rm = T)*sqrt(252)) %>% 
                           as.numeric()),
         "MaxDrawDown" = map_dbl(data, .f= ~ .x %>% 
                                   filter(Date>as.Date("2020-01-01")) %>%
                                   na.omit() %>% 
                                   summarise(tseries:: maxdrawdown(Retornos)$maxdrawdown) %>% 
                                   as.numeric()),
         "Dias" = map_dbl(data, .f = ~nrow(.x)),
         "Sharpe" = Retorno_Ann/Vol,
         "Retorno_5Y" = map_dbl(data, .f= ~ .x %>% 
                                  filter(Date>as.Date("2016-01-01")) %>%
                                  summarise(prod(1 + Retornos)^(252/n())-1) %>% 
                                  as.numeric()),
         "Vol_5Y" = map_dbl(data, .f= ~ .x %>% 
                              filter(Date>as.Date("2016-01-01")) %>%
                              summarise(sd(Retornos, na.rm = T)*sqrt(252)) %>% 
                              as.numeric()),
         "Sharpe_5Y" = Retorno_5Y/Vol_5Y)
          

  

  lista_fundos <- base_rets %>%
    select(-data) %>% 
    ungroup() %>% 
    mutate(across(where(is.numeric), ~scale(.x, center = T, scale = T))) %>% 
    mutate(nota = statistic + Retorno_Ann + Sharpe + Retorno_5Y + Sharpe_5Y - Vol_5Y - Vol - MaxDrawDown) %>% 
    arrange(nota) %>% 
    slice_max(n=10, order_by = nota)
  
  #Salva Lista top Fundos
  write_rds(lista_fundos, "00_Data/lista_final_top10_fundos.rds")

  