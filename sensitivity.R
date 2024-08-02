load('data/df_effort.Rdata')

devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")

library(ggplot2)
library(dplyr)

library(JABBA)
library(gplots)
library(coda)
library(rjags)
library(R2jags)
library("fitdistrplus")
library(reshape)

library(VAST)
library(wesanderson)

df = df_effort %>% 
  group_by(year_sale) %>% 
  summarise(catch = sum(catch),
            effort = sum(effort))

col_mis_w  = wes_palette('Zissou1',4)[1]

# VAST
#https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization
# JABBA

input = df 

jbinput = build_jabba(catch = data.frame('Year' = input$year_sale %>% as.numeric(),
                                           'mis' = input$catch),
                        cpue = data.frame('Year' = input$year_sale %>% as.numeric(),
                                          'mis' = input$effort),
                        scenario = 'TestRun',
                        model.type = 'Fox',
                        sigma.est = F,
                        fixed.obsE = 0.01)


bet_total = fit_jabba(jbinput,
                        ni = 30000, #number of iterations 
                        nt = 5, # thinning interval of saved iterations
                        nb = 5000, #burn-in
                        nc = 2, #number of mcmc chains initial values
                        init.values = FALSE, # 
                        init.K = NULL,
                        init.r = 0.81,
                        init.q = NULL,
                        peels = NULL, # NULL, # retro peel option
                        do.ppc = TRUE, # conducts and saves posterior predictive checks
                        save.trj = TRUE, # adds posteriors of stock, harvest and bk trajectories
                        save.all = FALSE, # add complete posteriors to fitted object
                        save.jabba = FALSE, # saves jabba fit as rdata object
                        save.csvs = FALSE, # option to write csv outputs
                        output.dir = 'sens_analysis//jabba',
                        quickmcmc = TRUE,
                        seed = 123,
                        jagsdir = NULL,
                        verbose = TRUE)


jbplot_ppdist(bet_total, mfrow = c(2,3))

dev.new()
# png('plots/res_jabba_w.png', height = 10, width = 10, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(5, 5, 5, 5))
jbplot_trj(bet_total,type="BBmsy", add = T)
jbplot_trj(bet_total,type="FFmsy", add = T)
jbplot_kobe(bet_total, add = T)
jbplot_spphase(bet_total, add = T)
dev.off()

# Como obter estimativa de K?
bet_total$estimates

# Impacto de r no MSY e no R final

range_k = seq(max(df$catch)*2,max(df$catch)*20, max(df$catch))

res = data.frame(r = numeric(), r_lci = numeric(), r_hci = numeric(),
                 msy = numeric(), msy_lci = numeric(), msy_hci = numeric())
j = 0

for(i in range_r){
  j = j+1
  modelo = fit_jabba(jbinput,
                  ni = 30000, #number of iterations 
                  nt = 5, # thinning interval of saved iterations
                  nb = 5000, #burn-in
                  nc = 2, #number of mcmc chains initial values
                  init.values = FALSE, # 
                  init.K = i,
                  init.r = 0.81,
                  init.q = NULL,
                  peels = NULL, # NULL, # retro peel option
                  do.ppc = TRUE, # conducts and saves posterior predictive checks
                  save.trj = TRUE, # adds posteriors of stock, harvest and bk trajectories
                  save.all = FALSE, # add complete posteriors to fitted object
                  save.jabba = FALSE, # saves jabba fit as rdata object
                  save.csvs = FALSE, # option to write csv outputs
                  output.dir = 'sens_analysis//jabba',
                  quickmcmc = TRUE,
                  seed = 123,
                  jagsdir = NULL,
                  verbose = TRUE)
  
  res[j,] = c(modelo$estimates[1,1], modelo$estimates[1,2], modelo$estimates[1,3],
              modelo$estimates[8,1], modelo$estimates[8,1], modelo$estimates[8,1])
}

