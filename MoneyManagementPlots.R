library(plotly)
library(rpivotTable)
library( htmltools )
Data=read.csv("D:/Scrips/Trading/OptiMoraR/MoneyManagementData.csv")

  
  setwd("D:/Scrips/Trading/OptiMoraR")
#As plot
  #Data$Var <- do.call(paste, c(Data[c("ProfitTake", "ProfitTakeF", "FractionCapital", "ReEntry", "AdjustAfterNMartingale")], sep = " "))
  #fig <- Data |plot_ly(x = ~PercentDecline, y = ~CummulativeLost , split = ~Var)
  #fig <- fig %>% add_lines()
  
  

Pivot=rpivotTable(Data, rows = c("AdjustAfterNMartingale" ,"ReEntry"), cols = c("PercentDecline"), aggregatorName = "Maximum", vals = "CummulativeLost",  inclusions = list(
                     "ProfitTake" = list("0.15"),
                     "ProfitTakeF" = list("0.1"),
                     "FractionCapital" = list("5")))

save_html(Pivot, "MoneyManagementData.html", background = "white", libdir = "lib", lang = "en")
