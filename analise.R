library(dplyr)

# Sintese das vendas
load("C:/repos/occ_assessment/data/initial_data_occ_sumario.Rdata")

# Sintetiza o dataframe
df_effort =
  vd %>%
  # mutate(predicoes = pred) %>% 
  group_by(year_sale, week, EGRUPART) %>% 
  summarise(catch = sum(QVENDA),
            effort = n_distinct(IEMBARCA))


# Usa concurrent sampling para criar modelo de splines
source('data_import.R')

lota_naut_2$month = case_when(lota_naut_2$MES %in% c('10', '11', '12') ~ lota_naut_2$MES,
                              T ~ paste0("0",lota_naut_2$MES))

lota_naut_2 = lota_naut_2 %>%
  filter(REGIAO == '27.9.a.s.a') %>% 
  mutate(week = lubridate::isoweek(DATA))

ajuste = loess(peso_total ~ week,
               data = lota_naut_2,
               degree =2)


# acrescenta semanas que faltam
cobaia = data.frame(semana = c(1:53), res = predict(ajuste, newdata = c(1:53)))
cobaia$mbw = imputeTS::na_interpolation(cobaia$res)

# Gera série de estimativas para TS com +- 2 Standard errors

# adiciona estimativa de mbw no dataframe principal
df_effort = df_effort %>% 
  left_join(.,
            cobaia,
            by = c('week' = 'semana'))

teste = df_effort %>% 
  mutate(mbw_rand = urnorm(1, mean = mbw, sd = 2*ajuste$s, lb = 0))

teste %>% 
  ggplot() +
  geom_line(aes(x = week,
                y = mbw),
            group = 1) +
  geom_line(aes(x = week,
                y = mbw_rand),
            group = 1,
            color = 'red') + 
  facet_wrap(year_sale ~.) + 
  theme_bw()


lota_naut_2 %>% 
  # group_by(MES) %>% 
  ggplot +
  # geom_point(aes(x = week, y = peso_total)) +
  geom_line(aes(x = week, y = fitted(ajuste), group = 1), col = 'purple') +
  geom_point(aes(x = week, y = fitted(ajuste)), col = 'red') +
  geom_line(data = cobaia, aes(x = semana,
                               y = res,
                               group = 1),
            color = 'green') +
  theme_bw()





















contabilidade =
  lota_naut_2 %>%
  mutate(week = lubridate::isoweek(DATA)) %>% 
  group_by(id_caixa,week, ANO) %>% 
  summarise(peso_am = unique(peso_am_caixa),
            n_am = sum(n_nao_observados_tot, na.rm =T)) %>% 
  group_by(week,ANO) %>% 
  summarise(peso = sum(peso_am),
            n = sum(n_am),
            mean_weight = peso/n)



contabilidade_w$ANO = factor(contabilidade_w$ANO)
contabilidade_w$ANO = droplevels(contabilidade_w$ANO) 
contabilidade_w = complete(contabilidade_w, ANO)
contabilidade_w = contabilidade_w %>%
  arrange(ANO, week) %>%
  mutate(time =as.numeric(as.character(ANO))+as.numeric(week)/52)
contabilidade_w$mean_weight_estim = na_interpolation(contabilidade_w$mean_weight)


library(CatDyn)

naut_peso2 =
  naut_peso %>%
  mutate(regiao = case_when(as.character(REGIAO) %in% 
                              c('27.9.a.c.s', '27.9.a.c.n') ~ 'Western Coast',
                            as.character(REGIAO) == '27.9.a.s.a' ~ 'Southern Coast',
                            T ~ as.character(REGIAO)),
         # mutate(id_caixa = case_when(is.na(id_caixa) ~ paste0(id_viagem, cat_com),
         #                             T ~ as.character(id_caixa)),
         semana = lubridate::isoweek(DATA))

contabilidade_s =
  naut_peso2 %>%
  filter(regiao == 'Southern Coast') %>% 
  group_by(id_caixa,semana, ANO) %>% 
  summarise(peso_am = unique(peso_am_caixa),
            n_am = sum(n_nao_observados)) %>% 
  group_by(semana,ANO) %>% 
  summarise(peso = sum(peso_am),
            n = sum(n_am),
            mean_weight = peso/n)
contabilidade_s$ANO = factor(contabilidade_s$ANO)
contabilidade_s$ANO = droplevels(contabilidade_s$ANO) 
contabilidade_s = complete(contabilidade_s, ANO)
contabilidade_s = contabilidade_s %>%
  arrange(ANO, semana) %>%
  mutate(time =as.numeric(as.character(ANO))+as.numeric(semana)/52)
contabilidade_s$mean_weight_estim = imputeTS::na_interpolation(contabilidade_s$mean_weight)


cat_w_22 = as.CatDynData(x=df_w_22,
                         step="week",
                         fleet.name="Polyvalent-W-2022",
                         coleff=7,
                         colcat=5,
                         colmbw=15,
                         unitseff="trips",
                         unitscat="kg",
                         unitsmbw="kg",
                         nmult="thou",
                         season.dates=c(as.Date("2022-01-01"),
                                        as.Date("2022-12-25")))



