
TC <- read.csv('file:///C:/107下/107下 資料探索與資訊視覺化/107下 Final/data and code/Taichung.csv')
library(tidyverse)
library(ggplot2)
TC <- na.omit(TC)
TC1 <- mutate(TC,土地移轉總面積平方公尺
              ,土地移轉總面積坪=土地移轉總面積平方公尺*0.3025) %>%
  mutate(建物移轉總面積坪=建物移轉總面積平方公尺*0.3025) %>%
  mutate(車位移轉總面積坪=車位移轉總面積平方公尺*0.3025) %>%
  mutate(每坪單價=單價元平方公尺*3.305) %>%
  select(-單價元平方公尺,-土地移轉總面積平方公尺,-建物移轉總面積平方公尺,-車位移轉總面積平方公尺)

ggplot(data=TC1)+
  geom_bar(mapping=aes(x=主要用途,fill=主要用途))


TC1 %>% filter(每坪單價<=800000) %>%
  ggplot()+ geom_histogram(aes(x=每坪單價, fill=交易標的),bins=15) +
  scale_fill_brewer(palette='Dark2')



TC2 <- separate(TC1,交易年月日,sep=c(-4,-2)
                ,into=c("交易年","交易月","交易日")
                ,convert=T) %>%
  select(-交易日) %>% #or: select(交易年,交易月) 
  filter(交易年==108) %>%
  group_by(交易月) %>%
  summarise(n(),mean(每坪單價),median(每坪單價))

TC1 %>% filter(總價元<=50000000,每坪單價<=1500000) %>%
  ggplot()+
  geom_point(aes(x=總價元,y=每坪單價,color=有無管理組織))

