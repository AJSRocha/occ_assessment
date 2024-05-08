# Teste de splines
library(ggplot2)
library(dplyr)
library(Runuran)

# load("data/nautilus_occ.Rdata")

lota_naut_2$month = case_when(lota_naut_2$MES %in% c('10', '11', '12') ~ lota_naut_2$MES,
                              T ~ paste0("0",lota_naut_2$MES))

lota_naut_2 = lota_naut_2 %>% 
<<<<<<< HEAD
  mutate(semana = lubridate::isoweek(DATA))

lota_naut_2$pesos = 1/
((lota_naut_2$peso_am_caixa / lota_naut_2$peso_total_caixa) *
  (lota_naut_2$peso_amostrado_dom / lota_naut_2$land_kg)) 
=======
  filter(REGIAO == '27.9.a.s.a') %>% 
  mutate(week = lubridate::week(DATA))

# lota_naut_2$pesos = 1/
# ((lota_naut_2$peso_am_caixa / lota_naut_2$peso_total_caixa) *
#   (lota_naut_2$peso_amostrado_dom / lota_naut_2$land_kg)) 
>>>>>>> b9ba00bf2183f611c807f6793023984f3af008dd

hist(lota_naut_2$pesos)



# ?loess

<<<<<<< HEAD
ajuste = loess(peso_total ~ semana,
=======
ajuste = loess(peso_total ~ week,
>>>>>>> b9ba00bf2183f611c807f6793023984f3af008dd
               data = lota_naut_2,
               degree = 2)

lota_naut_2 %>% 
  # group_by(MES) %>% 
  ggplot +
<<<<<<< HEAD
  geom_point(aes(x = semana, y = peso_total)) +
  geom_line(aes(x = semana, y = fitted(ajuste), group = 1), col = 'purple') +
  geom_point(aes(x = semana, y = fitted(ajuste)), col = 'red') +
=======
  geom_point(aes(x = week, y = peso_total)) +
  geom_line(aes(x = week, y = fitted(ajuste), group = 1), col = 'purple') +
  geom_point(aes(x = week, y = fitted(ajuste)), col = 'red') +
>>>>>>> b9ba00bf2183f611c807f6793023984f3af008dd
  theme_bw()

