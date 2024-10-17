df = cat_df$Data$`Polyvalent-S`

head(df_effort %>% filter(year_sale == 1999))

p = 25
M = 0.05
N0 = 15000 #millions, as in nmult
P = indice_manual
P.ini = list(rep(200000, 25)) #2 elementos porque sao 2 perturbacaoes
k = 0.00005
alpha = 1
beta  = 0.5
disp = list(50)

#C_t=〖kE〗_t^α N_t^β=〖kE〗_t^α (N_0 e^(-Mt)-e^((-M)⁄2) (∑▒C_i  e^(-M(t-i-1) ) )+∑▒R_j  e^(-M(t-p_j ) ) )^β e^((-M)⁄2)

P.ini = list(rep(200000, 25))
indice_manual

perturbacoes = data.frame(
  year = 1999:2023,
  R = unlist(P.ini),
  timing = unlist(indice_manual)
)


catch_module = function(index,M){ #index é o t na formulacao original
  res = 0
  for(i in 2:index-1){
    res = res + df$Ct[i]*exp(-M*(index-i-1))
  }
  return(res)
}

recruit_module = function(index){ #index é o t na formulacao original
  contador = 1
  res = 0
  while(perturbacoes$timing[contador] <= index){
    res = res + perturbacoes$R[contador] * exp(-M*(index-perturbacoes$timing[contador]))
    if(contador == nrow(perturbacoes)){break()}
    contador = contador + 1
  }
  return(res)
}





df$Ct = 0

for(i in 1:nrow(df)){
  if(i == 1){# t0
    df$Ct[i] = k * df$obseff.trips[i]^ alpha * exp(-M/2) * (
      N0 * exp(-M*i) -
    exp(-M/2) * (0) # catch
                 + 0# recrutamento
                 )^beta 
  }
  else{
    df$Ct[i] = k * df$obseff.trips[i]^ alpha * exp(-M/2) * (
      N0 * exp(-M*i) -
                exp(-M/2) * catch_module(i,M) # catch
            + recruit_module(i)# recrutamento
    )^beta
  }
}



df %>% 
# mutate(year = ((time.step -1) %/% 12),
#        x2 = rep(1:12,25)) %>% 
  ggplot() + 
  geom_line(aes(y = obscat.thou,
      x = time.step)) + 
  geom_line(aes(y = Ct,
                x = time.step),
            linetype = 2,
            color = 'orange') + 
  # facet_wrap(year ~.) + 
  scale_color_manual(values = colorRampPalette(wes_palette('Zissou1'))(25)) + 
  theme_bw() + 
  theme(legend.position = 'none')
