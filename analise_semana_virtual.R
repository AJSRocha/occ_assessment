library(dplyr)
library(CatDyn)
library(ggplot2)
library(Runuran)

# Sintese das vendas
load("C:/repos/occ_assessment/data/initial_data_occ_sumario.Rdata")

# Sintetiza o dataframe
# df_effort =
#   vd %>%
#   mutate(week = case_when(week == 53 ~ 52,
#                           T ~ week)) %>% 
#   group_by(year_sale, week, IEMBARCA) %>% 
#   summarise(catch_i = sum(QVENDA),
#             effort_i = n_distinct(IDATVEND)) %>% 
#   # mutate(predicoes = pred) %>% 
#   group_by(year_sale, week) %>% 
#   summarise(catch = sum(catch_i, na.rm =T),
#             effort = sum(effort_i, na.rm =T))
# 
# 
# save(df_effort, file = 'data/df_effort.Rdata')
load('data/df_effort.Rdata')

# Elimina semana 53


# Usa concurrent sampling para criar modelo de splines
source('data_import.R')

lota_naut_2$month = case_when(lota_naut_2$MES %in% c('10', '11', '12') ~ lota_naut_2$MES,
                              T ~ paste0("0",lota_naut_2$MES))

lota_naut_2 = lota_naut_2 %>%
  filter(REGIAO == '27.9.a.s.a') %>% 
  mutate(week = lubridate::isoweek(DATA),
         peso_total = peso_total/1000,
         week = case_when(week == 53 ~ 52,
                          T ~ week))

ajuste = loess(peso_total ~ week,
               data = lota_naut_2,
               degree =2)


# acrescenta semanas que faltam
predicos = predict(ajuste, newdata = c(1:52), se = T)

cobaia = data.frame(semana = c(1:52), 
                    res = predicos$fit,
                    res.se = predicos$se.fit)
cobaia$mbw = imputeTS::na_interpolation(cobaia$res)
cobaia$se = imputeTS::na_interpolation(cobaia$res.se)

# Gera série de estimativas para TS com +- 2 Standard errors

#adiciona estimativa de mbw no dataframe principal
df_effort = df_effort %>%
  left_join(.,
            cobaia,
            by = c('week' = 'semana'))

# df_effort = df_effort %>% 
#   mutate(mbw = predict(ajuste, newdata = mbw, se = T)$fit +
#            )

df_effort = df_effort %>% 
  rowwise() %>% 
  mutate(mbw_rand = urnorm(1, mean = mbw, sd = 2*se)) %>% 
  ungroup()

df_effort %>% 
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


####

# funcoes para fittar catdyn

trialer = function(data, p, M, N0.ini, P.ini, k.ini,
                   alpha.ini, beta.ini, P,
                   distr, method, itnmax, disp = list()){
  
  pars.ini = log(c(M,
                   N0.ini,
                   unlist(P.ini), # estimativa de amplitude da perturbacao
                   k.ini,
                   alpha.ini,
                   beta.ini,
                   unlist(disp)))
  
  dates = c(head(data$Data[[1]]$time.step,1),
            unlist(P), #estimativa do timing da perturbacao
            tail(data$Data[[1]]$time.step,1))
  
  res = list()
  
  res$pre_fit = catdynexp(x=data,
                          p=p,
                          par=pars.ini,
                          dates=dates,
                          distr=distr)
  
  res$fit = CatDynFit(x = data,
                      p = p,
                      par = pars.ini,
                      dates = dates,
                      distr = distr,
                      method = method,
                      itnmax = itnmax)
  
  res$pred = CatDynPred(res$fit,method)
  
  return(res)
}

plotador = function(data, model, pre = T, post1 = T, post2 = T){
  if(pre){
    plot.CatDynData(data,
                    mark = T,
                    offset = c(0,1,10),
                    hem = 'N')}
  if(post1){
    plot(x=model$pre_fit,
         leg.pos="topright",
         Biom.tstep=7,
         Cat.tstep=120,
         Biom.xpos=0.4,
         Biom.ypos=0,
         Cat.xpos=0.4,
         Cat.ypos=0.1)}
  
  if(post2){
    plot(x=model$pred,
         leg.pos="topright",
         Biom.tstep=7,
         Cat.tstep=10,
         Biom.xpos=0.18,
         Biom.ypos=0.1,
         Cat.xpos=0.18,
         Cat.ypos=0.2)}
  
}

# funcao que tapa buracos de defeso
defeso = function(df){
  semanas =c(1:52)[!(c(1:52) %in% unique(df$week))]
  ref = df_effort %>% filter(year_sale == 2000)
  
  for(i in semanas){
    linha = data.frame("year_sale" = df$year_sale[1],
                       "week" = i,
                       "catch" = 0,
                       "effort" = 0,
                       "res" = NA,
                       "res.se" = NA,
                       "mbw" = ref[ref$week == i,]$mbw,
                       "se" = ref[ref$week == i,]$se,
                       "mbw_rand" = ref[ref$week == i,]$mbw_rand)
    
    df = rbind(data.frame(df), linha) %>% 
      arrange(week)
  }
  return(df)}

#Funcao que determina ultimo dia da semana
last_date_of_week = function(year, week){strptime(paste(year, week, 1), format = "%Y %W %u")}


# Prepara dataframes para catdtn

cat_95 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(1995)),
                         step="week",
                         fleet.name="Polyvalent-S",
                         coleff=4,
                         colcat=3,
                         colmbw=9,
                         unitseff="trips",
                         unitscat="kg",
                         unitsmbw="kg",
                         nmult="thou",
                         season.dates=c(as.Date("1995-01-01"),
                                        # as.Date("1995-12-24")))
                                        last_date_of_week(1995, 52)-1))
                                        
last_date_of_week(1995, 52)-1


plot.CatDynData(cat_95,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_95_1 = 
  trialer(cat_95,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(48, 49),
          distr = 'gamma',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

CatDynPar(fit_95_1$fit, 'spg')
pred = CatDynPred(fit_95_1$fit,"spg")


dev.off()
plotador(cat_95,fit_95_1)

dev.off()
ggplot() + 
  # geom_line(aes(x = pred$Model$Results$Period.week,
  #               y = pred$Model$Results$Predicted.Biomass.tonnes),
  #           group = 1,
  #           col = 'red') +
  geom_line(aes(x = pred$Model$Results$Period.week,
                y = pred$Model$Results$Predicted.Abundance.thou),
            group = 1,
            col = 'darkgreen') + 
  theme_bw() 
  # xlim(0,40) + 
  # ylim(300,350)


  


# Teste Maratona
cat_96 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(1996)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("1996-01-01"),
                                      # as.Date("1996-12-29"))),
                                      last_date_of_week(1996, 52)-0))
                                      


plot.CatDynData(cat_96,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_96_1 = 
  trialer(cat_96,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000, 10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(4,5),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_96,fit_96_1)


# 1997
cat_97 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(1997)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("1997-01-01"),
                                      # as.Date("1997-12-28")))
                                      last_date_of_week(1997, 52)-1))


plot.CatDynData(cat_97,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_97_1 = 
  trialer(cat_97,
          p = 3,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000, 10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(2, 45,47),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_97,fit_97_1)


# 1998
cat_98 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(1998)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("1998-01-01"),
                                      # as.Date("1998-12-30")))
                                      last_date_of_week(1998, 52)-1))


plot.CatDynData(cat_98,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_98_1 = 
  trialer(cat_98,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(2,7),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_98,fit_98_1)


# 1999
cat_99 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(1999)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("1999-01-01"),
                                      # as.Date("1999-12-30")))
                                      last_date_of_week(1999, 52)-1))


plot.CatDynData(cat_99,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_99_1 = 
  trialer(cat_99,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(48),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_99,fit_99_1)

# 2000
cat_00 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2000)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2000-01-01"),
                                      # as.Date("2000-12-30")))
                                      last_date_of_week(2000, 52)-1))


plot.CatDynData(cat_00,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_00_1 = 
  trialer(cat_00,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(9),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_00,fit_00_1)


# 2001
cat_01 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2001)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2001-01-01"),
                                      # as.Date("2001-12-31")))
                                      last_date_of_week(2001, 52)-0))


plot.CatDynData(cat_01,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_01_1 = 
  trialer(cat_01,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(10,44),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_01,fit_01_1)


# 2002
cat_02 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2002)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2002-01-01"),
                                      # as.Date("2002-12-30")))
                                      last_date_of_week(2002, 52)-1))


plot.CatDynData(cat_02,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_02_1 = 
  trialer(cat_02,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_02,fit_02_1)

# 2003
cat_03 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2003)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2003-01-01"),
                                      # as.Date("2003-12-30")))
                                      last_date_of_week(2003, 52)-1))


plot.CatDynData(cat_03,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_03_1 = 
  trialer(cat_03,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(43,44),
          distr = 'normal',
          method = 'CG',
          itnmax = 100000,
          disp = 50)

plotador(cat_03,fit_03_1)


# 2004
cat_04 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2004)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2004-01-01"),
                                      # as.Date("2004-12-30")))
                                      last_date_of_week(2004, 52)-1))


plot.CatDynData(cat_04,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_04_1 = 
  trialer(cat_04,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(50),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_04,fit_04_1)

# 2005
cat_05 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2005)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2005-01-01"),
                                      # as.Date("2005-12-30")))
                                      last_date_of_week(2005, 52)-1))


plot.CatDynData(cat_05,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_05_1 = 
  trialer(cat_05,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(14,47),
          distr = 'gamma',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_05,fit_05_1)

# 2006
cat_06 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2006)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2006-01-01"),
                                      # as.Date("2006-12-24")))
                                      last_date_of_week(2006, 52)-1))


plot.CatDynData(cat_06,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_06_1 = 
  trialer(cat_06,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(4,48),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_06,fit_06_1)

# 2007
cat_07 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2007)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2007-01-01"),
                                      # as.Date("2007-12-31")))
                                      last_date_of_week(2007, 52)-0))


plot.CatDynData(cat_07,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_07_1 = 
  trialer(cat_07,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(45,47),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_07,fit_07_1)


# 2008
cat_08 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2008)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2008-01-01"),
                                      # as.Date("2008-12-30")))
                                      last_date_of_week(2008, 52)-1))


plot.CatDynData(cat_08,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_08_1 = 
  trialer(cat_08,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0,1),
          distr = 'negbin',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_08,fit_08_1)


# 2009
cat_09 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2009)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2009-01-01"),
                                      # as.Date("2009-12-30")))
                                      last_date_of_week(2009, 52)-1))


plot.CatDynData(cat_09,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_09_1 = 
  trialer(cat_09,
          p = 5,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000,10000,10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0,5,41,43,46),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_09,fit_09_1)

# 2010
cat_10 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2010)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2010-01-01"),
                                      # as.Date("2010-12-30")))
                                      last_date_of_week(2010, 52)-1))


plot.CatDynData(cat_10,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_10_1 = 
  trialer(cat_10,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(8,9),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_10,fit_10_1)


# 2011
cat_11 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2011)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2011-01-01"),
                                      # as.Date("2011-12-30")))
                                      last_date_of_week(2011, 52)-1))


plot.CatDynData(cat_11,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_11_1 = 
  trialer(cat_11,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0,1),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_11,fit_11_1)

# 2012
cat_12 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2012)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2012-01-01"),
                                      # as.Date("2012-12-30")))
                                      last_date_of_week(2012, 52)-1))


plot.CatDynData(cat_12,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_12_1 = 
  trialer(cat_12,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(46),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_12,fit_12_1)

# 2013
cat_13 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2013)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2013-01-01"),
                                      # as.Date("2013-12-30")))
                                      last_date_of_week(2013, 52)-1))


plot.CatDynData(cat_13,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_13_1 = 
  trialer(cat_13,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(1),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_13,fit_13_1)

# 2014
cat_14 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2014)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2014-01-01"),
                                      # as.Date("2014-12-30")))
                                      last_date_of_week(2014, 52)-1))


plot.CatDynData(cat_14,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_14_1 = 
  trialer(cat_14,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(1,46),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_14,fit_14_1)


# 2015
cat_15 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2015)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2015-01-01"),
                                      # as.Date("2015-12-30")))
                                      last_date_of_week(2015, 52)-1))


plot.CatDynData(cat_15,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_15_1 = 
  trialer(cat_15,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0,1),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_15,fit_15_1)

# 2016
cat_16 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2016)) %>% 
                         defeso,
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2016-01-01"),
                                      # as.Date("2016-12-31")))
                                      last_date_of_week(2016, 52)-1))


plot.CatDynData(cat_16,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_16_1 = 
  trialer(cat_16,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(49,50),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_16,fit_16_1)

# 2017
cat_17 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2017)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2017-01-01"),
                                      # as.Date("2017-12-30")))
                                      last_date_of_week(2017, 52)-1))


plot.CatDynData(cat_17,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_17_1 = 
  trialer(cat_17,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(1),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_17,fit_17_1)

# 2018
cat_18 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2018)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2018-01-01"),
                                      # as.Date("2018-12-31")))
                                      last_date_of_week(2018, 52)-0))


plot.CatDynData(cat_18,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_18_1 = 
  trialer(cat_18,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(43,49),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_18,fit_18_1)

# 2019
cat_19 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2019)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2019-01-01"),
                                      # as.Date("2019-12-30")))
                                      last_date_of_week(2019, 52)-1))


plot.CatDynData(cat_19,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_19_1 = 
  trialer(cat_19,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(1,2),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_19,fit_19_1)

# 2020
cat_20 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2020)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2020-01-01"),
                                      # as.Date("2020-12-30")))
                                      last_date_of_week(2020, 52)-1))


plot.CatDynData(cat_20,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_20_1 = 
  trialer(cat_20,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(49,50),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_20,fit_20_1)

# 2021
cat_21 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2021)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2021-01-01"),
                                      # as.Date("2021-12-30")))
                                      last_date_of_week(2021, 52)-1))


plot.CatDynData(cat_21,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_21_1 = 
  trialer(cat_21,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0,41),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_21,fit_21_1)

# 2022
cat_22 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2022)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2022-01-01"),
                                      # as.Date("2022-12-30")))
                                      last_date_of_week(2022, 52)-1))


plot.CatDynData(cat_22,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_22_1 = 
  trialer(cat_22,
          p = 2,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000,10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0,1),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_22,fit_22_1)

# 2023
cat_23 = as.CatDynData(x=df_effort %>% filter(year_sale %in% c(2023)),
                       step="week",
                       fleet.name="Polyvalent-S",
                       coleff=4,
                       colcat=3,
                       colmbw=9,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="thou",
                       season.dates=c(as.Date("2023-01-01"),
                                      # as.Date("2023-12-30")))
                                      last_date_of_week(2023, 52)-1))


plot.CatDynData(cat_23,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

fit_23_1 = 
  trialer(cat_23,
          p = 1,
          M = 1/52,
          N0.ini = 20000, #millions, as in nmult
          P.ini = list(10000), #2 elementos porque sao 2 perturbacaoes
          k.ini = 0.01,
          alpha.ini = 0.5,
          beta.ini  = 0.5,
          P = list(0),
          distr = 'normal',
          method = 'spg',
          itnmax = 100000,
          disp = 50)

plotador(cat_23,fit_23_1)

inp_s = list(fit_95_1$fit, fit_96_1$fit, fit_97_1$fit,
             fit_98_1$fit, fit_99_1$fit, fit_00_1$fit,
             fit_01_1$fit, fit_02_1$fit, fit_03_1$fit,
             fit_04_1$fit, fit_05_1$fit, fit_06_1$fit,
             fit_07_1$fit, fit_08_1$fit, fit_09_1$fit,
             fit_10_1$fit, fit_11_1$fit, fit_12_1$fit,
             fit_13_1$fit, fit_14_1$fit, fit_15_1$fit,
             fit_16_1$fit, fit_17_1$fit, fit_18_1$fit,
             fit_19_1$fit, fit_20_1$fit, fit_21_1$fit,
             fit_22_1$fit, fit_23_1$fit)

temp = df_effort %>% 
  ungroup %>% 
  # filter(year_sale %in% c(1995:2009)) %>%
  group_by(year_sale) %>% 
  summarise(mbw = mean(mbw_rand, na.rm =T),
            se = sd(mbw_rand, na.rm =T)) %>% 
  as.data.frame()


compila =
CatDynBSD(inp_s,
          multi = F,
          mbw.sd = temp,
          method = c('spg','spg','spg',
                     'spg','spg','spg',
                     'spg','spg','CG',
                     'spg','spg','spg',
                     'spg','spg','spg',
                     'spg','spg','spg',
                     'spg','spg','spg',
                     'spg','spg','spg',
                     'spg','spg','spg',
                     'spg','spg'))

base = df_effort %>%
  group_by(year_sale) %>%
  summarise(qvenda = sum(catch)/1000)
# 
base2 = df_effort %>%
  group_by(year_sale, week) %>%
  summarise(qvenda = sum(catch),
            effort = sum(effort))
# 
base2 %>%
  # filter(year_sale %in% c(2009:2011)) %>%
  # filter(week %in% c(25,53)) %>%
  ggplot +
  geom_line(aes(
    x = paste(year_sale, week),
            y = qvenda,
    group = 1))

# base2 %>% 
#   # filter(year_sale %in% c(2009:2011)) %>%
#   # filter(week %in% c(25,53)) %>% 
#   ggplot + 
#   geom_line(aes(
#     x = paste(year_sale, week),
#     y = effort,
#     group = 1)) 
# 
# 
# 
# 
# 
compila %>%
ggplot() +
  geom_line(aes(x = factor(Year),
                y = unlist(B0Tot.ton)),
            col = 'red',
            group = 1) +
  # geom_line(aes(x = Year,
  #               y =  unlist(B0Tot.ton) + unlist(B0Tot.ton.SE)),
  #           col = 'green',
  #           group = 1) +
  geom_line(data = base,
            aes(x = year_sale,
                y = qvenda),
            col = 'blue',
            group =1) +
  theme_bw()
# 
# 

compila %>%
  ggplot() +
  geom_line(aes(x = factor(Year),
                y = unlist(N0Tot.thou)),
            col = 'red',
            group = 1) +
  # geom_line(aes(x = Year,
  #               y =  unlist(B0Tot.ton) + unlist(B0Tot.ton.SE)),
  #           col = 'green',
  #           group = 1) +
  geom_line(data = base,
            aes(x = year_sale,
                y = qvenda),
            col = 'blue',
            group =1) +
  theme_bw()


tempo = c()
for(i in 1995:2023){
  j = 1
  while(j<53){
    tempo[length(tempo)+1] = paste(i,
                                   sprintf("%02d",j),
                                   sep = '_') 
    j = j+1
  }
}




compila %>% mutate(CV = B0Tot.ton.SE/B0Tot.ton) %>% View

sumario = data.frame(
  year_sale = tempo,
  observed = c(fit_95_1$pred$Model$Results$Observed.Catch.kg,
               fit_96_1$pred$Model$Results$Observed.Catch.kg,
               fit_97_1$pred$Model$Results$Observed.Catch.kg,
               fit_98_1$pred$Model$Results$Observed.Catch.kg,
               fit_99_1$pred$Model$Results$Observed.Catch.kg,
               fit_00_1$pred$Model$Results$Observed.Catch.kg,
               fit_01_1$pred$Model$Results$Observed.Catch.kg,
               fit_02_1$pred$Model$Results$Observed.Catch.kg,
               fit_03_1$pred$Model$Results$Observed.Catch.kg,
               fit_04_1$pred$Model$Results$Observed.Catch.kg,
               fit_05_1$pred$Model$Results$Observed.Catch.kg,
               fit_06_1$pred$Model$Results$Observed.Catch.kg,
               fit_07_1$pred$Model$Results$Observed.Catch.kg,
               fit_08_1$pred$Model$Results$Observed.Catch.kg,
               fit_09_1$pred$Model$Results$Observed.Catch.kg,
               fit_10_1$pred$Model$Results$Observed.Catch.kg,
               fit_11_1$pred$Model$Results$Observed.Catch.kg,
               fit_12_1$pred$Model$Results$Observed.Catch.kg,
               fit_13_1$pred$Model$Results$Observed.Catch.kg,
               fit_14_1$pred$Model$Results$Observed.Catch.kg,
               fit_15_1$pred$Model$Results$Observed.Catch.kg,
               fit_16_1$pred$Model$Results$Observed.Catch.kg,
               fit_17_1$pred$Model$Results$Observed.Catch.kg,
               fit_18_1$pred$Model$Results$Observed.Catch.kg,
               fit_19_1$pred$Model$Results$Observed.Catch.kg,
               fit_20_1$pred$Model$Results$Observed.Catch.kg,
               fit_21_1$pred$Model$Results$Observed.Catch.kg,
               fit_22_1$pred$Model$Results$Observed.Catch.kg,
               fit_23_1$pred$Model$Results$Observed.Catch.kg),
  predicted = c(fit_95_1$pred$Model$Results$Predicted.Catch.kg,
                fit_96_1$pred$Model$Results$Predicted.Catch.kg,
                fit_97_1$pred$Model$Results$Predicted.Catch.kg,
                fit_98_1$pred$Model$Results$Predicted.Catch.kg,
                fit_99_1$pred$Model$Results$Predicted.Catch.kg,
                fit_00_1$pred$Model$Results$Predicted.Catch.kg,
                fit_01_1$pred$Model$Results$Predicted.Catch.kg,
                fit_02_1$pred$Model$Results$Predicted.Catch.kg,
                fit_03_1$pred$Model$Results$Predicted.Catch.kg,
                fit_04_1$pred$Model$Results$Predicted.Catch.kg,
                fit_05_1$pred$Model$Results$Predicted.Catch.kg,
                fit_06_1$pred$Model$Results$Predicted.Catch.kg,
                fit_07_1$pred$Model$Results$Predicted.Catch.kg,
                fit_08_1$pred$Model$Results$Predicted.Catch.kg,
                fit_09_1$pred$Model$Results$Predicted.Catch.kg,
                fit_10_1$pred$Model$Results$Predicted.Catch.kg,
                fit_11_1$pred$Model$Results$Predicted.Catch.kg,
                fit_12_1$pred$Model$Results$Predicted.Catch.kg,
                fit_13_1$pred$Model$Results$Predicted.Catch.kg,
                fit_14_1$pred$Model$Results$Predicted.Catch.kg,
                fit_15_1$pred$Model$Results$Predicted.Catch.kg,
                fit_16_1$pred$Model$Results$Predicted.Catch.kg,
                fit_17_1$pred$Model$Results$Predicted.Catch.kg,
                fit_18_1$pred$Model$Results$Predicted.Catch.kg,
                fit_19_1$pred$Model$Results$Predicted.Catch.kg,
                fit_20_1$pred$Model$Results$Predicted.Catch.kg,
                fit_21_1$pred$Model$Results$Predicted.Catch.kg,
                fit_22_1$pred$Model$Results$Predicted.Catch.kg,
                fit_23_1$pred$Model$Results$Predicted.Catch.kg)
)

sumario %>% 
  reshape::melt() %>% 
  ggplot() + 
  geom_line(aes(x = year_sale,
                y = value,
                group = variable,
                col = variable)) +
  theme_classic() 
  facet_grid(variable ~.)


# SPiCT

library(spict)

timeC = base$year_sale %>% as.character %>% as.numeric()
timeI = base$year_sale %>% as.character %>% as.numeric()



obsC = compila$B0Tot.ton
obsI = df_effort %>% 
  group_by(year_sale) %>% 
  summarise(
    cat = sum(catch, na.rm =T),
    eff = sum(effort, na.rm = T),
    lpue = cat/eff) %>% 
  pull(lpue)

modelo_spict = list(timeC = timeC,
                    timeI = timeC,
                    obsC = obsC,
                    obsI = obsI)

modelo_spict$priors$logbkfrac <- c(log(0.8),0.5,1)
modelo_spict$ini$logn <- log(2) #adjust production curve to Shaefer
modelo_spict$phases$logn <- -1
modelo_spict$priors$logalpha <- c(1, 1, 0)
modelo_spict$priors$logbeta <- c(1, 1, 0)

res_spict = fit.spict(modelo_spict)
retro_res = retro(res_spict)

par(mfrow = c(2,2))
plotspict.bbmsy(res_spict, qlegend = F)
plotspict.ffmsy(res_spict, qlegend = F)
plotspict.fb(res_spict)
plotspict.production(res_spict, n.plotyears = 40)

plotspict.retro(retro_res)
plotspict.diagnostic(calc.osa.resid(res_spict), qlegend = F)

  rbind(
    sumspict.parest(res_spict),
    sumspict.srefpoints(res_spict)[,1:4])

  