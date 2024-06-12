library(wesanderson)
library(gridExtra)

cores = wes_palette('Zissou1')
#>  [1] "BottleRocket1"  "BottleRocket2"  "Rushmore1"      "Rushmore"      
#>  [5] "Royal1"         "Royal2"         "Zissou1"        "Darjeeling1"   
#>  [9] "Darjeeling2"    "Chevalier1"     "FantasticFox1"  "Moonrise1"     
#> [13] "Moonrise2"      "Moonrise3"      "Cavalcanti1"    "GrandBudapest1"
#> [17] "GrandBudapest2" "IsleofDogs1"    "IsleofDogs2"    "FrenchDispatch"
#> [21] "AsteroidCity1"  "AsteroidCity2"  "AsteroidCity3"

cores2 =  
  c(wes_palette('Zissou1'),
    wes_palette('BottleRocket1'),
    wes_palette('AsteroidCity1'),
    wes_palette('Moonrise1'),
    wes_palette('Cavalcanti1'),
    wes_palette('GrandBudapest1'))


# Plot de desembarques - serie global

fig1 = 
df_effort %>% 
  group_by(year_sale) %>% 
  summarise(catch = sum(catch)) %>% 
  ggplot() + 
  geom_line(aes(x = year_sale, y = catch, group = 1),
            color = cores[1]) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))
ggsave(fig1, device = 'eps', dpi = 300, width = 40, height = 20, units = 'cm', filename = 'figures/land_global.eps')


# Plot de desembarques - todas as seasons
fig2 = 
df_effort %>% 
  ggplot() + 
  geom_line(aes(x = week, y = catch, group = year_sale, color = year_sale)) +
  geom_vline(xintercept = 13) +
  geom_vline(xintercept = 26) +
  geom_vline(xintercept = 39) +
  theme_bw() + 
  # scale_color_manual(values = cores[sample(c(1:5), 29, replace  =T)]) + 
  scale_color_manual(values = cores2) + 
  theme(legend.position = 'none')
ggsave(fig2, device = 'eps', dpi = 300, width = 40, height = 20, units = 'cm', filename = 'figures/land_by_year.eps')


# Plot do modelo de pesos medios

fig3 = 
grid.arrange(

lota_naut_2 %>% 
  # group_by(MES) %>% 
  ggplot +
  geom_point(aes(x = week, y = peso_total)) +
  geom_line(aes(x = week, y = fitted(ajuste), group = 1), col = 'red', linewidth = 2.5) +
  # geom_point(aes(x = week, y = fitted(ajuste)), col = 'red') +
  # geom_line(data = cobaia, aes(x = semana,
  # y = res,
  # group = 1),
  # color = 'green') +
  theme_bw() + 
  labs(y = 'mean body weight (kg)')
,
df_effort %>% 
  ggplot() +
  geom_line(aes(x = week,
                y = mbw,
                group = year_sale)) +
  geom_line(aes(x = week,
                y = mbw_rand,
                group = year_sale,
                color = year_sale)) + 
  scale_color_manual(values = cores2) +
  # facet_wrap(year_sale ~.) + 
  theme_bw() + 
  theme(legend.position = 'none') +
  labs(y = 'mean body weight (kg)')
,
nrow = 1)
ggsave(fig3, device = 'eps', dpi = 300, width = 40, height = 20, units = 'cm', filename = 'figures/mbw.eps')


# esforco de amostragem
fig4 = 
lota_naut_2 %>% 
  group_by(ANO, week) %>% 
  summarise(samples = n()) %>% 
  ggplot() + 
  geom_bar(aes(x = 1:260,
               y = samples),
           stat = 'identity',
           fill = cores[1]) + 
  labs(x = 'week',
       y = 'sampled octopus') + 
  theme_bw()
ggsave(fig4, device = 'eps', dpi = 300, width = 40, height = 20, units = 'cm', filename = 'figures/sample_effort.eps')

