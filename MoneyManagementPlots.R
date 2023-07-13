
Data=read.csv("D:/Scrips/Trading/Strategies/MoneyManagementData.csv")

Data$FractionCapital = sprintf("%e", Data$FractionCapital)

Data$Var <- do.call(paste, c(Data[c("ProfitTake", "ProfitTakeF", "FractionCapital", "ReEntry", "AdjustAfterNMartingale")], sep = " "))
fig <- Data |> plot_ly(x = ~PercentDecline, y = ~CummulativeLost , split = ~Var)

fig <- fig %>% add_lines()
rpivotTable(Data, rows = c("PercentDecline"), cols = c("n_MG"), aggregatorName = "Max", vals = "CummulativeLost")