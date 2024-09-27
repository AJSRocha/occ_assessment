library(dplyr)
library(CatDyn)
library(ggplot2)
library(Runuran)
set.seed(123)

# Sintese das vendas
load("C:/repos/occ_assessment/data/initial_data_occ_sumario.Rdata")

# Sintetiza o dataframe
# df_effort =
#   vd %>%
#   # mutate(week = case_when(week == 53 ~ 52,
#   #                         T ~ week)) %>%
#   group_by(year_sale, month_sale, IEMBARCA) %>%
#   summarise(catch_i = sum(QVENDA),
#             effort_i = n_distinct(IDATVEND)) %>%
#   # mutate(predicoes = pred) %>%
#   group_by(year_sale, month_sale) %>%
#   summarise(catch = sum(catch_i, na.rm =T),
#             effort = sum(effort_i, na.rm =T))
# 
# 
# save(df_effort, file = 'data/df_effort_m.Rdata')
load('data/df_effort_m.Rdata')

source('data_import.R')

lota_naut_2$month = case_when(lota_naut_2$MES %in% c('10', '11', '12') ~ lota_naut_2$MES,
                              T ~ paste0("0",lota_naut_2$MES))

lota_naut_2 = lota_naut_2 %>%
  filter(REGIAO == '27.9.a.s.a') %>% 
  mutate(week = lubridate::isoweek(DATA),
         peso_total = peso_total/1000,
         week = case_when(week == 53 ~ 52,
                          T ~ week))

ajuste = loess(peso_total ~ MES,
               data = lota_naut_2,
               degree =2)


# acrescenta semanas que faltam
predicos = predict(ajuste, newdata = c(1:12), se = T)

cobaia = data.frame(mes = c(1:12), 
                    res = predicos$fit,
                    res.se = predicos$se.fit)
cobaia = cobaia %>% 
  mutate(mes = stringr::str_pad(mes, 2, pad = "0"))

# cobaia$mbw = imputeTS::na_interpolation(cobaia$res)
# cobaia$se = imputeTS::na_interpolation(cobaia$res.se)

# Gera série de estimativas para TS com +- 2 Standard errors

#adiciona estimativa de mbw no dataframe principal
df_effort = df_effort %>%
  left_join(.,
            cobaia,
            by = c('month_sale' = 'mes'))

df_effort = df_effort %>% 
  rowwise() %>% 
  mutate(mbw_rand = urnorm(1, mean = mbw, sd = 6*res.se)) %>% 
  ungroup()

df_effort %>% 
  ggplot() +
  geom_line(aes(x = month_sale,
                y = mbw),
            group = 1) +
  geom_line(aes(x = month_sale,
                y = mbw_rand),
            group = 1,
            color = 'red') + 
  facet_wrap(year_sale ~.) + 
  theme_bw()


# Funcoes catdyn
source('funcoes_catdyn.R')


# Começando
cat_df = as.CatDynData(x=df_effort,
                       step="month",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=8,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("1995-01-01"),
                                      # as.Date("1995-12-24")))
                                      last_date_of_week(2023, 52)-1))

plot.CatDynData(cat_df,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_null = 
  trialer(cat_df,
          p = 0,
          M = 0.05,
          N0.ini = 10000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.001,
          alpha.ini = 1,
          beta.ini  = 1,
          distr = 'gamma',
          method = 'spg',
          itnmax = 100000,
          disp = list(50))
