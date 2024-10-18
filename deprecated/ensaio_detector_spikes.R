
cat_09$Data$`Polyvalent-S`$spikecat

which.max(cat_03$Data$`Polyvalent-S`$spikecat)


detector = function(dados){
  res = list()
  res$P = list(which(dados$Data$`Polyvalent-S`$spikecat>=0))
  res$p = length(P)
  
  return(res)
}


batata = detector(cat_09)
batata$P

