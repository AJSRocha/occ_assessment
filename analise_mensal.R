library(dplyr)
library(CatDyn)
library(ggplot2)
library(Runuran)
library(wesanderson)
library(cuttlefish.model)
set.seed(123)

# Funcoes catdyn
source('.scripts/funcoes_catdyn.R')

# Sintese das vendas
# load("C:/repos/occ_assessment/.data/initial_data_occ_sumario_otb.Rdata")
# 
# # Sintetiza o dataframe e prepara para standardizacao 
# df_effort =
#   vd %>%
#   group_by(year_sale, month_sale, IEMBARCA, PORTO) %>%
#   summarise(Power.Main.raw = mean(Power.Main, na.rm = T),
#             Power.main = trunc(Power.Main.raw/50) * 50,
#             catch_i = sum(QVENDA[EGRUPART == 'MIS_MIS']),
#             catch_i_otb = sum(QVENDA),
#             effort_i = n_distinct(IDATVEND[EGRUPART == 'MIS_MIS']),
#             effort_i_otb = n_distinct(IDATVEND))
# 
# std_otb = 
# df_effort %>% 
#   transmute(year = year_sale,
#             fishing.season = year_sale,
#             month = month_sale,
#             rectangle = PORTO,
#             power.class = Power.main,
#             catch_i_otb = catch_i_otb,
#             effort_i_otb = effort_i_otb,
#             lpue = catch_i_otb/effort_i_otb)
# 
# std_mis = 
#   df_effort %>% 
#   transmute(year = year_sale,
#             fishing.season = year_sale,
#             month = month_sale,
#             rectangle = PORTO,
#             power.class = Power.main,
#             catch_i = catch_i,
#             effort_i = effort_i,
#             lpue = catch_i/effort_i) %>% 
#   mutate(lpue = case_when(is.nan(lpue) ~ 0,
#                           T ~ lpue))
# 
# std_otb_temp = custom_delta_glm(std_otb)
# std_mis_temp = custom_delta_glm(std_mis)
# 
# 
# df_nominal = df_effort %>% 
#   mutate(Power.main = as.character(Power.main)) %>%
#   left_join(.,
#             std_otb_temp$predicted.lpue,
#             by = c('month_sale' = 'month',
#                    'year_sale' = 'fishing.season',
#                    'Power.main' = 'power.class',
#                    'PORTO' = 'rectangle')) %>% 
#   rename(st.lpue_otb = st.lpue) %>% 
#   left_join(.,
#             std_mis_temp$predicted.lpue,
#             by = c('month_sale' = 'month',
#                    'year_sale' = 'fishing.season',
#                    'Power.main' = 'power.class',
#                    'PORTO' = 'rectangle')) 
# 
# df_effort = df_effort %>% 
#   group_by(year_sale, month_sale) %>%
#   summarise(catch = sum(catch_i, na.rm =T),
#             effort = sum(effort_i, na.rm =T),
#             catch_otb = sum(catch_i_otb, na.rm = T),
#             effort_otb = sum(effort_i_otb, na.rm =T))
# 
# 
# save(std_mis_temp, 
#      std_otb_temp,
#      df_nominal,
#      df_effort,
#      file = '.data/df_effort_m_otb.Rdata')

# load('data/df_effort_m_otb.Rdata')

# source('.scripts/data_import.R')
# 
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

# save(df_effort, file = '.data/df_effort_m_mbw_otb.Rdata')
# save(predicos, file = '.data/mbw_model.Rdata')
load('.data/mbw_model.Rdata')
load('.data/df_effort_m_mbw_otb.Rdata')




# Começando com frota completa (mis + otb)
cat_df = as.CatDynData(x=df_effort %>% 
                         filter(as.numeric(
                           as.character(year_sale)) %in% c(2009:2023)),
                       step="month",
                       fleet.name="MIS+OTB-S",
                       coleff=6,
                       colcat=5,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2009-01-01"),
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
    # 12,12,12,12,12,
    #    12,10,10,11,12,
    12,12,10,11,12,
    11,12,10,12,11,
    12,12,12,12,10)

for(i in 0:(length(indice_manual)-1)){
  indice_manual[[i+1]] = 12*i + indice_manual[[i+1]]
}

unlist(indice_manual)

cat_df$Data$`Polyvalent-S` %>% 
  mutate(year = ((time.step -1) %/% 12),
         x2 = rep(1:12,5)) %>% 
  ggplot() + 
  geom_line(aes(color = factor(year))) + 
  aes(y = spikecat,
      x = x2) + 
  facet_wrap(year ~.) + 
  scale_color_manual(values = colorRampPalette(wes_palette('Zissou1'))(25)) + 
  theme_bw() + 
  theme(legend.position = 'none')


# View(pre_fit$Model$Results)
# plot(pre_fit$Model$Results$Predicted.Catch.kg ~ pre_fit$Model$Results$Observed.Catch.kg)
# abline(a=0, b=1)


#'Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B',
#' 'nlm', 'nlminb', 'spg', 'ucminf', 'newuoa',
#'  'bobyqa', 'nmkb', 'hjkb', 'Rcgmin', or 'Rvmmin'.


# recuando de 2019 a 2015: N0 = 30000
#

fit_null =
  trialer(cat_df,
          p = 15,
          M = 0.01,
          N0.ini = 60000, #millions, as in nmult
          P = indice_manual,
          P.ini  = list(
            # 20000,20000,20000,40000,20000,
            #            20000,20000,20000,100000,20000,
            #
            20000, 20000, 20000, 50000,20000,       
            20000, 20000, 20000, 20000, 20000,
            20000, 20000,40000,20000,20000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.00005,
          alpha.ini = 0.85,
          beta.ini  = 0.85,
          distr = 'aplnormal',
          method = 'CG',
          itnmax = 10000,
          disp = list(100))


fit_null$fit$Model$CG$AIC


plotador(cat_df, fit_null, pre = F,
         post1 = T,
         post2 = T)


# Métricas para exportar

# Biomassa anual
annual_biomass =
CatDynBSD(fit_null$fit,
          method = 'CG',
          multi = T,
          mbw.sd = predicos$se.fit)

annual_biomass %>% 
  ggplot() + 
  geom_line(aes(x = TimeStep,
                y = B.ton,
                group = 1),
            size = 1) +
  geom_line(aes(x = TimeStep,
                y = B.ton + 2*B.ton.SE,
                group = 1),
            linetype = 2,
            size = 1) +
  geom_line(aes(x = TimeStep,
                y = B.ton - 2*B.ton.SE,
                group = 1),
            linetype = 2,
            size = 1) + 
  theme_bw()

# Fishing Mortality
results = fit_null$pred$Model$Results

natural_mortality = fit_null$fit$Model$CG$bt.par$M
natural_mortality_sd = fit_null$fit$Model$CG$bt.stdev[['M']]

results %>% 
  ggplot() + 
  geom_line(aes(x = Period.month,
                y = `Observed.F.1/month`),
            col = 'tomato',
            size = 1) +
  geom_line(aes(x = Period.month,
                y = `Predicted.F.1/month`),
            col = 'darkred',
            size = 1,
            linetype = 2) + 
  geom_hline(yintercept = natural_mortality,
            col = 'darkgreen',
            size = 1,
            linetype = 1) +
  geom_hline(yintercept = natural_mortality + 2*natural_mortality_sd,
             col = 'darkgreen',
             size = 1,
             linetype = 2) +
  geom_hline(yintercept = natural_mortality - 2*natural_mortality_sd,
             col = 'darkgreen',
             size = 1,
             linetype = 2) +
  theme_bw()

# Exploitation Rate

results %>% 
  ggplot() + 
  geom_line(aes(x = Period.month,
                y = `Observed.Explotrate`),
            col = 'tomato',
            size = 1) +
  geom_line(aes(x = Period.month,
                y = `Observed.Explotrate`*20),
            col = 'blue',
            size = 1) +
  geom_line(aes(x = Period.month,
                y = `Predicted.Explotrate`),
            col = 'darkred',
            size = 1,
            linetype = 2) + 
  geom_line(aes(x = Period.month,
                y = `Observed.F.1/month`/(`Observed.F.1/month`+natural_mortality)),
            col = 'purple',
            size = 1) +
  geom_hline(yintercept = 0.4,
             col = 'darkgreen',
             size = 1,
             linetype = 1) +
  theme_bw()


results %>% 
  summarise(pilas = `Observed.F.1/month`/(`Observed.F.1/month`+natural_mortality),
            expo = Observed.Explotrate) %>% 
  mutate(teste = pilas/expo)
