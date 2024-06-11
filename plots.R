library(wesanderson)



# Plot de desembarques - serie global

df_effort %>% 
  ggplot() + 
  geom_line(aes(x = rownames(df_effort), y = catch, group = 1)) + theme_bw()


# Plot de desembarques - todas as seasons
df_effort %>% 
  ggplot() + 
  geom_line(aes(x = week, y = catch, group = year_sale)) + theme_bw()

            