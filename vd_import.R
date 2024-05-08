for(i in c(1995:2023)){
  load(paste0('C:/puta/vd_', i, '.RData'))
  assign(paste0('vd_', i),
         mget(ls(pattern = paste0('vd_*', i)))[[1]] %>%
           filter(!is.na(EESPECIE)) %>% 
           # filter(EESPECIE %in% c('OCC', 'OCT'))) %>% 
           group_by(IEMBARCA, IPORTO, PORTO, zona, IDATVEND,
                    EGRUPART, EARTE, ESUBARTE,
                    year_sale, month_sale, quarter_sale,
                    id_venda, CFR, Event.Start.Date, Event.End.Date,
                    Vessel.Name, Construction.Year, Loa, Ton.Gt,
                    Gear.Main.Code, Gear.Sec.Code, Gear.3rd.Code,
                    Gear.4th.Code, Gear.5th.Code, Gear.6th.Code,
                    Power.Main) %>% 
           summarise(QVENDA_total = sum(QVENDA),
                     OCC = sum(QVENDA[EESPECIE %in% c('OCC', 'OCT')])))}

vd = do.call("rbind", mget(ls(pattern = "^vd_*")))
rm(list = ls(pattern = 'vd_'))

names(vd)[grepl('OCC', names(vd))] = 'QVENDA'
names(vd)

vd = vd %>% 
  filter(EGRUPART == 'MIS_MIS' & 
           zona == '27.9.a.s.a')

vd = vd %>% 
  mutate(week = lubridate::week(IDATVEND))

save(vd, file = 'data/initial_data_occ_sumario.Rdata')
