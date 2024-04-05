# Teste de splines
library(ggplot2)
library(dplyr)
library(Runuran)

# load("data/nautilus_occ.Rdata")

lota_naut_2$month = case_when(lota_naut_2$MES %in% c('10', '11', '12') ~ lota_naut_2$MES,
                              T ~ paste0("0",lota_naut_2$MES))

lota_naut_2 = lota_naut_2 %>% 
  mutate(week = lubridate::week(DATA))

lota_naut_2$pesos = 1/
((lota_naut_2$peso_am_caixa / lota_naut_2$peso_total_caixa) *
  (lota_naut_2$peso_amostrado_dom / lota_naut_2$land_kg)) 

hist(lota_naut_2$pesos)



?loess

ajuste = loess(peso_total ~ week,
               data = lota_naut_2,
               degree = 2)

lota_naut_2 %>% 
  # group_by(MES) %>% 
  ggplot +
  geom_point(aes(x = week, y = peso_total)) +
  geom_line(aes(x = week, y = fitted(ajuste), group = 1), col = 'purple') +
  geom_point(aes(x = week, y = fitted(ajuste)), col = 'red') +
  theme_bw()

