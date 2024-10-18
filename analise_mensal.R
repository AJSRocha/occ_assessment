library(dplyr)
library(CatDyn)
library(ggplot2)
library(Runuran)
library(wesanderson)
set.seed(123)

# Sintese das vendas
# load("C:/repos/occ_assessment/data/initial_data_occ_sumario_otb.Rdata")

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


# save(df_effort, file = 'data/df_effort_m_otb.Rdata')
# load('data/df_effort_m_otb.Rdata')

# source('data_import.R')

# lota_naut_2$month = case_when(lota_naut_2$MES %in% c('10', '11', '12') ~ lota_naut_2$MES,
#                               T ~ paste0("0",lota_naut_2$MES))
# 
# lota_naut_2 = lota_naut_2 %>%
#   filter(REGIAO == '27.9.a.s.a') %>%
#   mutate(week = lubridate::isoweek(DATA),
#          peso_total = peso_total/1000,
#          week = case_when(week == 53 ~ 52,
#                           T ~ week))
# 
# ajuste = loess(peso_total ~ MES,
#                data = lota_naut_2,
#                degree =2)
# 
# 
# # acrescenta semanas que faltam
# predicos = predict(ajuste, newdata = c(1:12), se = T)
# 
# cobaia = data.frame(mes = c(1:12),
#                     res = predicos$fit,
#                     res.se = predicos$se.fit)
# cobaia = cobaia %>%
#   mutate(mes = stringr::str_pad(mes, 2, pad = "0"))
# 
# # cobaia$mbw = imputeTS::na_interpolation(cobaia$res)
# # cobaia$se = imputeTS::na_interpolation(cobaia$res.se)
# 
# # Gera série de estimativas para TS com +- 2 Standard errors
# 
# #adiciona estimativa de mbw no dataframe principal
# df_effort = df_effort %>%
#   left_join(.,
#             cobaia,
#             by = c('month_sale' = 'mes'))
# 
# df_effort = df_effort %>%
#   rowwise() %>%
#   mutate(mbw_rand = urnorm(1, mean = res, sd = 6*res.se)) %>%
#   ungroup()
# 
# df_effort %>%
#   ggplot() +
#   geom_line(aes(x = month_sale,
#                 y = res),
#             group = 1) +
#   geom_line(aes(x = month_sale,
#                 y = mbw_rand),
#             group = 1,
#             color = 'red') +
#   facet_wrap(year_sale ~.) +
#   theme_bw()
# 
# save(df_effort, file = 'data/df_effort_m_mbw_otb.Rdata')
load('data/df_effort_m_mbw_otb.Rdata')

# Funcoes catdyn
source('funcoes_catdyn.R')


# Começando
cat_df = as.CatDynData(x=df_effort %>% 
                         filter(as.numeric(
                           as.character(year_sale)) %in% c(1999:2023)),
                       step="month",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=7,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("1999-01-01"),
                                      # as.Date("1995-12-24")))
                                      last_date_of_week(2023, 52)-1))

plot.CatDynData(cat_df,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')


#deteccao dos catch spikes

detectados =
cat_df$Data$`Polyvalent-S` %>% 
  mutate(year = ((time.step -1) %/% 12)) %>% 
  group_by(year) %>% 
  summarise(recrutamento = which.max(spikecat)) %>% 
  mutate(indice = (year)*12 + recrutamento)

indice = list()
for(i in 1:nrow(detectados)){
  indice[[i]] = detectados$indice[i]
}

indice_manual = 
  list(
    12,12,12,12,12,
       12,10,10,11,12,
       12,12,10,11,12,
       11,12,10,12,11,
       12,12,12,12,10)

for(i in 0:(length(indice_manual)-1)){
  indice_manual[[i+1]] = 12*i + indice_manual[[i+1]]
}

unlist(indice_manual)

cat_df$Data$`Polyvalent-S` %>% 
  mutate(year = ((time.step -1) %/% 12),
         x2 = rep(1:12,25)) %>% 
  ggplot() + 
  geom_line(aes(color = factor(year))) + 
  aes(y = spikecat,
      x = x2) + 
  facet_wrap(year ~.) + 
  scale_color_manual(values = colorRampPalette(wes_palette('Zissou1'))(25)) + 
  theme_bw() + 
  theme(legend.position = 'none')



pars.ini = log(c(M,
                 N0,
                 unlist(P.ini), # estimativa de amplitude da perturbacao
                 k,
                 alpha,
                 beta,
                 unlist(disp)))

pre_fit =
catdynexp(x=cat_df,
          p=25,
          par=pars.ini,
          dates=c(head(cat_df$Data[[1]]$time.step,1),
                  unlist(indice_manual), #estimativa do timing da perturbacao
                  tail(cat_df$Data[[1]]$time.step,1)),
          distr='gamma')

# View(pre_fit$Model$Results)
plot(pre_fit$Model$Results$Predicted.Catch.kg ~ pre_fit$Model$Results$Observed.Catch.kg)
abline(a=0, b=1)


#'Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B',
#' 'nlm', 'nlminb', 'spg', 'ucminf', 'newuoa',
#'  'bobyqa', 'nmkb', 'hjkb', 'Rcgmin', or 'Rvmmin'.

# fit_null = 
#   trialer(cat_df,
#           p = 25,
#           M = 0.05976,
#           N0.ini = 14020, #millions, as in nmult
#           P = indice_manual,
#           P.ini = list(rep(15000, 25)), #2 elementos porque sao 2 perturbacaoes
#           k.ini = 0.0003546,
#           alpha.ini = 1.1455,
#           beta.ini  = 0.6134,
#           distr = 'aplnormal',
#           method = 'spg',
#           itnmax = 10000,
#           disp = list(1000))


fit_null =
  trialer(cat_df,
          p = 25,
          M = 0.05976,
          N0.ini = 30000, #millions, as in nmult
          P = indice_manual,
          P.ini  = list(20000,20000,20000,40000,20000,
                       20000,20000,20000,100000,20000,
                       20000,20000,20000,50000,20000,
                       20000,20000,20000,20000,20000,
                       20000,20000,40000,20000,20000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.00005,
          alpha.ini = 0.9,
          beta.ini  = 0.9,
          distr = 'aplnormal',
          method = 'CG',
          itnmax = 10000,
          disp = list(5000))


fit_null$Model$spg$AIC



# 
CatDynFit(x = cat_df,
          p = p,
          par = pars.ini,
          dates = c(head(cat_df$Data[[1]]$time.step,1),
                    unlist(indice_manual), #estimativa do timing da perturbacao
                    tail(cat_df$Data[[1]]$time.step,1)),
          distr = 'normal',
          method = 'spg',
          itnmax = 100)

fit_null$Model$spg$AIC

CatDynPred(fit_null,'')
