
# Sintese das vendas
load("C:/repos/occ_assessment/data/initial_data_occ_sumario.Rdata")

df_effort =
  vd %>%
  # mutate(predicoes = pred) %>% 
  group_by(year_sale, week, EGRUPART) %>% 
  summarise(catch = sum(QVENDA),
            effort = n_distinct(IEMBARCA))





contabilidade =
  lota_naut_2 %>%
  group_by(id_caixa,week, ANO) %>% 
  summarise(peso_am = unique(peso_am_caixa),
            n_am = sum(n_nao_observados)) %>% 
  group_by(week,ANO) %>% 
  summarise(peso = sum(peso_am),
            n = sum(n_am),
            mean_weight = peso/n)



contabilidade_w$ANO = factor(contabilidade_w$ANO)
contabilidade_w$ANO = droplevels(contabilidade_w$ANO) 
contabilidade_w = complete(contabilidade_w, ANO)
contabilidade_w = contabilidade_w %>%
  arrange(ANO, week) %>%
  mutate(time =as.numeric(as.character(ANO))+as.numeric(week)/52)
contabilidade_w$mean_weight_estim = na_interpolation(contabilidade_w$mean_weight)




