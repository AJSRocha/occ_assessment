df = cat_df$Data$`Polyvalent-S`
df$Ct = 0 # funcao depende disso

# Funcoes catdyn
source('funcoes_catdyn.R')

P.ini = list(20000,20000,20000,40000,20000,
             20000,20000,20000,100000,20000,
             20000,20000,20000,50000,20000,
             20000,20000,20000,20000,20000,
             20000,20000,40000,20000,20000) #2 elementos porque sao 2 perturbacaoes

disp = list(50)


perturbacoes = data.frame(
  year = 1999:2023,
  R = unlist(P.ini),
  timing = unlist(indice_manual)
)


teste=
  simulador(data = df,
            k = 0.00002,
            alpha = .8,
            beta = .8,
            M = 0.05,
            N0 = 30000)

ensaio = list()
intervalo = c(0.00005,0.00001,
              0.000005,0.000001,
              0.0000005,0.0000001,
              0.00000005,0.00000001)
for(j in 1:length(intervalo)){
  ensaio[[j]] = simulador(data = df,
                                k = 0.00005,
                                alpha = 0.8,
                                beta = 0.8,
                                M = 0.25,
                                N0 = 30000)
  
}

resultados = data.frame(parametro = rep(intervalo, each = length(ensaio[[1]])),
                        time.step = rep(df$time.step, times = length(intervalo)),
                        C_t = unlist(ensaio))


# df %>% 
# mutate(Ct = C_t) %>%
  ggplot() + 
 
  # geom_line(aes(y = Ct,
  #               x = time.step),
  #           linetype = 2,
  #           color = 'orange') + 
  geom_line(aes(x = resultados$time.step,
                y = resultados$C_t,
                group = resultados$parametro,
                color = resultados$parametro %>% factor),
            size = 1) +
    geom_line(aes(y = df$obscat.thou,
                  x = df$time.step),
              size = 1, linetype = 2) +
  geom_vline(xintercept = unlist(indice_manual),
             linetype = 2) + 
  # facet_wrap(year ~.) + 
  scale_color_manual(values = colorRampPalette(wes_palette('Zissou1'))(length(unique(resultados$parametro)))) + 
  theme_bw() + 
  theme(legend.position = 'right') + 
    labs(color = '', title = '')

