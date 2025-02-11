
fig1 = 
df_effort %>% 
  ggplot() + 
  geom_bar(aes(x = year_sale,
               y = catch_otb/1000),
           stat = 'identity',
           fill = 'black') + 
    geom_bar(aes(x = year_sale,
               y = catch/1000),
           stat = 'identity',
           fill = gray.colors(4)[2]) + 
  theme_bw() + 
  labs(x = '',
       y = 'catch (tons)') + 
  theme(axis.text.x = element_text(angle = 90))


ggsave(fig1, dpi = 300, units = 'cm',
       height = 10, width = 10, filename = 'figures/landings.png')
