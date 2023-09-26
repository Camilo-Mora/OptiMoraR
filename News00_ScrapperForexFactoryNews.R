library(Stocks)
library(dplyr)
setwd("D:/Scrips/Trading/OptiMoraR/")
News=GetNewsForexFactory(From = "2021/01/01", To ="2023/12/31")

#NZD=News%>%filter(currency=="NZD")
#CHF=News%>%filter(currency=="CHF")
#News=rbind(NZD,CHF)

write.csv(News,"ForexNewsData.csv")

