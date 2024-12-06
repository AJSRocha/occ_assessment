# Correr depois do catdyn

library(spict)

head(df_nominal)

# df_nominal %>% 
#   mutate(x = paste(year_sale,
#                    month_sale,
#                    sep = '_')) %>% 
#   ggplot() + 
#   geom_boxplot(aes(x = x,
#                 y = st.lpue)) + 
#   theme_bw()


df_spict = df_nominal %>% 
  mutate(st.lpue = ifelse(is.na(st.lpue), 0, st.lpue),
         st.lpue_otb = ifelse(is.na(st.lpue_otb), 0, st.lpue_otb)) %>%  
  group_by(year_sale, month_sale) %>% 
  summarise(catch = sum(catch_i),
            catch_otb = sum(catch_i_otb),
            lpue = median(st.lpue),
            lpue_otb = median(st.lpue_otb)) %>% 
  mutate(time_step = paste(year_sale,
                           month_sale,
                           sep = '_'))

frac_to_month <- function (x) {x/12} 


timeC = seq(1995,2023+11/12,1/12)
timeI =seq(1995,2023+11/12,1/12)
# timeC = as.numeric(as.character(df_effort$year_sale)) + rep(frac_to_month(0:11),29)
# timeI = as.numeric(df_effort$year_sale) + rep(frac_to_month(0:11),29)
obsC = df_spict$catch_otb
obsI = df_spict$lpue_otb

modelo_spict = list(timeC = timeC,
                    timeI = timeC,
                    obsC = obsC,
                    obsI = obsI)

check.inp(modelo_spict)

modelo_spict$priors$logbkfrac <- c(log(0.8),0.5,1)
modelo_spict$ini$logn <- log(2) #adjust production curve to Shaefer
modelo_spict$phases$logn <- -1
modelo_spict$priors$logalpha <- c(1, 1, 0)
modelo_spict$priors$logbeta <- c(1, 1, 0)
modelo_spict$dtc = 1/12
modelo_spict$dte = 1/12
modelo_spict$catchunit = 'kg'
modelo_spict$nseasons = 4

check.inp(modelo_spict)$dtc


res_spict = fit.spict(modelo_spict)
retro_res = retro(res_spict)
 # res_spict$inp$dteuler = res_spict$inp$dteuler/2 

save(df_spict, modelo_spict, res_spict,
     file = '.data/spict_time_corrigido.Rdata')


plot(res_spict)
dev.off()
plotspict.data(res_spict, qlegend = F)
plotspict.fb(res_spict)
plotspict.diagnostic(calc.osa.resid(res_spict), qlegend = F)

plotspict.f(res_spict)
plotspict.biomass(res_spict)

res_spict$value


res_spict$value %>% View

plotspict.biomass(res_spict, plot = T)


##################


plotspict.biomass(res_spict)


