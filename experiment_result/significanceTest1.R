args<-commandArgs(TRUE)

library(data.table,quietly=T,warn.conflicts=F)
library(dplyr,quietly=T,warn.conflicts=F)
library(MASS,quietly=T,warn.conflicts=F)

x = list.files(args[1], full.names=TRUE) %>%
  as.list %>% 
 lapply(function(file) { 
   fread(file,sep=" ") %>% 
   mutate(set=file)
   }) %>% 
 rbindlist %>%
 group_by(set) %>%
 mutate(setTime = sum(V2)) %>%
 ungroup() %>%
 filter(V1 == 29)
 
 y = list.files(args[2], full.names=TRUE) %>% 
 as.list %>% 
 lapply(function(file) { 
   fread(file,sep=" ") %>% 
   mutate(set=file)
   }) %>% 
 rbindlist %>%
 group_by(set) %>%
 mutate(setTime = sum(V2+V3)) %>%
 ungroup() %>%
 filter(V1 == 29) 
  
fitnessTestLess = wilcox.test(x$V3, y$V6, paired = TRUE, alt="less", conf.int=T, conf.level=0.95)
speedTestLess = wilcox.test(x$setTime, y$setTime, paired = TRUE, alt="less", conf.int=T, conf.level=0.95)
fitnessTestGreater = wilcox.test(x$V3, y$V6, paired = TRUE, alt="greater", conf.int=T, conf.level=0.95)
speedTestGreater = wilcox.test(x$setTime, y$setTime, paired = TRUE, alt="greater", conf.int=T, conf.level=0.95)

sprintf("%s, %s\n", args[1], args[2]) %>% cat
sprintf("P-fitness (2nd < 1st): %f, P-time (2nd < 1st): %f\n",fitnessTestLess$p.value,speedTestLess$p.value) %>% cat
sprintf("P-fitness (2nd > 1st): %f, P-time (2nd > 1st): %f\n",fitnessTestGreater$p.value,speedTestGreater$p.value) %>% cat
sprintf("------------\n") %>% cat
