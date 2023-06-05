rm(list=ls())

library(dplyr)
library(tidyr)

csv = list.files()

df_1 = read.csv(csv[1]) %>% janitor::clean_names()
df_2 = read.csv(csv[2]) %>% janitor::clean_names()

df = bind_rows(df_1,df_2) %>% 
     select(-lbh,-lba,-lbd,-ftr,-htr,-referee) %>% 
     select(-div,-date,-home_team,-away_team) %>% 
     mutate(target = ifelse(fthg > ftag,1,0)) %>% 
     select(-fthg,-ftag) %>% 
     select(-hthg,-htag,-as,-ast,-af,-ac,-ay,-ar,-b365a,-bwa,-iwa,-psa,-wha,-vca,
            -bb_mx_a,-bb_av_a,-psca,-bb1x2)

df$target %>% table()

df_pisah = df %>% group_split(target)

df_pisah[[1]] %>% nrow()
df_pisah[[2]] %>% nrow()

id_ambil = sample(354,300)

train_0 = df_pisah[[1]][id_ambil,]
train_1 = df_pisah[[2]][id_ambil,]

train_df = rbind(train_0,train_1)
train_df_final = train_df[sample(600,600),]


test_0 = df_pisah[[1]][-id_ambil,]
test_1 = df_pisah[[2]][-id_ambil,]

test_df = rbind(test_0,test_1)
test_df = test_df[sample(160,160),]
jawab   = test_df %>% select(target)
test_df = test_df %>% select(-target)

write.csv(train_df_final,"train.csv")
write.csv(jawab,"jawab.csv")
write.csv(test_df,"test.csv")
