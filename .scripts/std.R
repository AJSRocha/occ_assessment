# Teste de standardizacao

# Identificar embarcacao de referencia:

## Mais de 5000 viagens ao todo
## Cobre os anos todos
## Viagens bem distribuidas
## Maximo polvo

emb_ref = 
  vd %>% 
  group_by(IEMBARCA) %>% 
  summarise(init = min(IDATVEND),
            retir = max(IDATVEND),
            freq = n_distinct(IDATVEND),
            polvo = sum(QVENDA))

selector = 
  vd %>% 
  filter(IEMBARCA %in% c('PRT000000454','PRT000019305',
                         'PRT000018790','PRT000019307',
                         'PRT000000957','PRT000019686')) 

table(droplevels(selector$IEMBARCA), selector$year_sale)

# Escolhido: PRT000000957  

emb_957 = vd %>% 
  filter(IEMBARCA == 'PRT000000957')

extractor = function(embarcaco){
  df = vd %>% filter(IEMBARCA == embarcaco)
  periodo = emb_957 %>% filter(IDATVEND >= min(df$IDATVEND) &
                                     IDATVEND <= max(df$IDATVEND))
  res = periodo %>% 
    ungroup %>% 
    summarise(catch = sum(QVENDA),
              effort = n_distinct(IDATVEND))
  return(res)
}

emb1 = extractor(unique(vd$IEMBARCA)[1])


# Método de Beverton-Holt

bev_holt =
  vd %>% 
  group_by(IEMBARCA) %>% 
  summarise(catch = sum(QVENDA),
            effort = n_distinct(IDATVEND)) 

ref = list()
for(i in bev_holt$IEMBARCA){
  ref[[length(ref)+1]] = extractor(i)
}




  mutate(catch_ref = extractor(IEMBARCA)$catch,
         effort_ref = extactor(IEMBARCA)$effort)



## GLM
mod_glm =
  vd %>%
  mutate(week = case_when(week == 53 ~ 52,
                          T ~ week),
         Loa = case_when(is.na(Loa) ~ 0,
                         T ~ Loa)) %>% 
  group_by(IEMBARCA, PORTO, year_sale, month_sale, quarter_sale, week, Loa) %>% 
  summarise(catch = sum(QVENDA),
            effort = n_distinct(IDATVEND),
            lpue = catch/effort) 
    

mod1 = glm(lpue ~ PORTO + year_sale + week + Loa,
           family = gaussian(link = "identity"),
           data = mod_glm)

mod1
ggplot() + 
  geom_point(aes(x = mod_glm$lpue,
                  y = predict(mod1)))

res = c()
for(i in 1: nrow(mod_glm)){
  res[length(res)+1] = predict(mod1, newdata = mod_glm[i,])
}
    



df_effort =
  vd %>%
  mutate(week = case_when(week == 53 ~ 52,
                          T ~ week)) %>%
  group_by(year_sale, week, IEMBARCA) %>%
  summarise(catch_i = sum(QVENDA),
            effort_i = n_distinct(IDATVEND))

















  # mutate(predicoes = pred) %>%
  group_by(year_sale, week) %>%
  summarise(catch = sum(catch_i, na.rm =T),
            effort = sum(effort_i, na.rm =T))
