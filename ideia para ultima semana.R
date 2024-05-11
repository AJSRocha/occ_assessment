last_date_of_week <- function(year, week){strptime(paste(year, week, 1), format = "%Y %W %u") }


df_effort %>% 
  mutate(fake_week = week-1,
         dia = last_date_of_week(year_sale, fake_week)) %>% 
  View
