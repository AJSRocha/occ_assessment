# funcoes para fittar catdyn

trialer = function(data, p, M, N0.ini, P.ini, k.ini,
                   alpha.ini, beta.ini, P,
                   distr, method, itnmax, disp = list()){
  
  # psi.ini   = 0.33*sd(log(data$Data$`Polyvalent-S`$obscat.thou))^2
  
  
  if(p>0){
    pars.ini = log(c(M,
                     N0.ini,
                     unlist(P.ini), # estimativa de amplitude da perturbacao
                     k.ini,
                     alpha.ini,
                     beta.ini))
    
    dates = c(head(data$Data[[1]]$time.step,1),
              unlist(P), #estimativa do timing da perturbacao
              tail(data$Data[[1]]$time.step,1))}
  else{
    pars.ini = log(c(M,
                     N0.ini,
                     # unlist(P.ini), # estimativa de amplitude da perturbacao
                     k.ini,
                     alpha.ini,
                     beta.ini))
    
    
    # negative binomial, normal, lognormal, gamma, robust lognormal or gumbel distribution  
    
    
    
    dates = c(head(data$Data[[1]]$time.step,1),
              # unlist(P), #estimativa do timing da perturbacao
              tail(data$Data[[1]]$time.step,1))
  }
  
  if(distr %in% c('negbin','normal','lognormal','gamma')){
    pars.ini = c(pars.ini, log(unlist(disp)))
  }  
  
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

# funcoes para simular catdyn
catch_module = function(index,M){ #index é o t na formulacao original
  res = 0
  for(i in 2:index-1){
    res = res + df$Ct[i]*exp(-M*(index-i-1))
  }
  return(res)
}

recruit_module = function(index, M){ #index é o t na formulacao original
  contador = 1
  res = 0
  while(perturbacoes$timing[contador] <= index){
    res = res + perturbacoes$R[contador] * exp(-M*(index-perturbacoes$timing[contador]))
    if(contador == nrow(perturbacoes)){break()}
    contador = contador + 1
  }
  return(res)
}

simulador = function(data, k, alpha,
                     beta, M, N0){
  res = c()
  for(i in 1:nrow(df)){
    if(i == 1){# t0
      res[i] = k * df$obseff.trips[i]^ alpha * exp(-M/2) * (
        N0 * exp(-M*i) -
          exp(-M/2) * (0) # catch
        + 0# recrutamento
      )^beta 
    }
    else{
      res[i] = k * df$obseff.trips[i]^ alpha * exp(-M/2) * (
        N0 * exp(-M*i) -
          exp(-M/2) * catch_module(i,M) # catch
        + recruit_module(i,M)# recrutamento
      )^beta
    }
  }
  
  return(res)
}
