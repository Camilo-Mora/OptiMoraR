#Matching critical entries to news
#library(sqldf)
library(dplyr)
TickerI="XAUUSD"
MoneyToStart=30000
MinDDWanted=2.5 #percent of account wanted to be at maximum DD for this pair. 
DesireDropD=(MoneyToStart* (MinDDWanted/100))

#results from each likely entry
  RUN=readRDS("C:/Users/Camilo/Documents/TestBot.RData") # a simple robot of entries based on reversion from BB bands and exiting at 0.05% profit
  
  Signals=RUN[[1]]
  Signals[,c("X","X.1")]=NULL
  EachEntry=RUN[[4]]
  Dates=Signals[,c("ID", "Date")]
  Dates$Date=as.Date(Dates$Date,  format ="%Y-%m-%d ")

  
  
  #Finds statiics for entries
  CriticalHiddenEntries=EachEntry %>% group_by(StartingSignalNum) %>% summarise(DD = sum(DD_Money), EntryCandle=min(EntryRawID), ExitCandle=max(ExitRawID), Type= first(EntryId ))
  CriticalHiddenEntries$Type <- ifelse(grepl("S",CriticalHiddenEntries$Type),'Short','Long')
  CriticalHiddenEntries= CriticalHiddenEntries %>% filter(abs(DD) >=DesireDropD)

  
  
  #critical regions... regions at which the DD was larger than the desired DD, considering overlaping entry that caused the DD
  #for Longs
      Longs=CriticalHiddenEntries %>% filter(Type=="Long")
 
      CriticalLongEntries=Longs
      
      Longs=merge(Longs, Dates, by.x="EntryCandle", by.y="ID", all.x=TRUE)
      Longs <- Longs %>%  rename("EntryDate"="Date" )

      Longs=merge(Longs, Dates, by.x="ExitCandle", by.y="ID", all.x=TRUE)
      Longs <- Longs %>%  rename("ExitDate"="Date" )
   
  #for short
      Shorts=CriticalHiddenEntries %>% filter(Type=="Short")
      CriticalShortEntries=Shorts
      
      Shorts=merge(Shorts, Dates, by.x="EntryCandle", by.y="ID", all.x=TRUE)
      Shorts <- Shorts %>%  rename("EntryDate"="Date" )
      
      Shorts=merge(Shorts, Dates, by.x="ExitCandle", by.y="ID", all.x=TRUE)
      Shorts <- Shorts %>%  rename("ExitDate"="Date" )
      
      
      
      
#reset Entry and exit candle to the first and last candle of the day...this broadens the critical period to the day it happended
      Dates=Dates %>% group_by(Date) %>% summarise(FirstDayCandle=first(ID),LastDayCandle=last(ID))
      #For longs
      Longs=merge(Longs, Dates, by.x="EntryDate", by.y="Date", all.x=TRUE)
      Longs$LastDayCandle=NULL
      Longs=merge(Longs, Dates, by.x="ExitDate", by.y="Date", all.x=TRUE)
      Longs$FirstDayCandle.y=NULL
      Longs <- Longs %>%  rename("FirstDayCandle"="FirstDayCandle.x" )
      
      
      #Cluster overlaping critical regions
      Longs$CriticalREgionID=NA
      Longs$CriticalREgionID[1]=1
      CrticalRegion=1
      for (Li in 2:nrow(Longs)){
        if(Longs$LastDayCandle[Li-1] <= Longs$FirstDayCandle[Li]) {CrticalRegion=CrticalRegion+1; Longs$CriticalREgionID[Li]=CrticalRegion} else {Longs$CriticalREgionID[Li]=CrticalRegion}
      }
      Longs=Longs %>% group_by(CriticalREgionID) %>% summarise(DD = min(DD), FirstDayCandle=min(FirstDayCandle), LastDayCandle=max(LastDayCandle))
      

      #For shortd
      Shorts=merge(Shorts, Dates, by.x="EntryDate", by.y="Date", all.x=TRUE)
      Shorts$LastDayCandle=NULL
      Shorts=merge(Shorts, Dates, by.x="ExitDate", by.y="Date", all.x=TRUE)
      Shorts$FirstDayCandle.y=NULL
      Shorts <- Shorts %>%  rename("FirstDayCandle"="FirstDayCandle.x" )
      
      
      #Cluster overlaping critical regions
      Shorts$CriticalREgionID=NA
      Shorts$CriticalREgionID[1]=1
      CrticalRegion=1
      for (Li in 2:nrow(Shorts)){
        if(Shorts$LastDayCandle[Li-1] <= Shorts$FirstDayCandle[Li]) {CrticalRegion=CrticalRegion+1; Shorts$CriticalREgionID[Li]=CrticalRegion} else {Shorts$CriticalREgionID[Li]=CrticalRegion}
      }
      Shorts=Shorts %>% group_by(CriticalREgionID) %>% summarise(DD = min(DD), FirstDayCandle=min(FirstDayCandle), LastDayCandle=max(LastDayCandle))
      
 
  
#news
  News=read.csv("D:/Scrips/Trading/OptiMoraR/ForexNewsData.csv")
  News1=News %>% filter(currency == substr(TickerI, 1, 3))
  News2=News %>% filter(currency == substr(TickerI, 4, 6))

  News=rbind(News1,News2)
  News$Date=as.Date(News$DateI,  format ="%b%d.%Y")
  

  #Add candleID

  News=merge(News,Dates, by="Date", all.x=TRUE)
  
#Merge critical entries with news

  LongsResult= inner_join( News, Longs, join_by(between(FirstDayCandle, FirstDayCandle, LastDayCandle))) %>% select(CriticalREgionID, DD , event)
  ShortsResult= inner_join( News, Shorts, join_by(between(FirstDayCandle, FirstDayCandle, LastDayCandle))) %>% select(CriticalREgionID, DD , event)

#Summary by news type
  BadLongs =LongsResult %>% group_by(event) %>% summarise(DD = min(DD), NCritical=n())
  BadShorts=ShortsResult %>% group_by(event) %>% summarise(DD = min(DD), NCritical=n())
  
  
  Bads=rbind(LongsResult,ShortsResult)
  Bads=Bads %>% group_by(event) %>% summarise(DD = min(DD), NCritical=n())
  
  
  
  
  #plot
  PlotSignals=dygraphs::dygraph(Signals[,c("ID", "c", "Entry_UpperBB", "Entry_LowerBB", "Exit_UpperBB", "Exit_LowerBB")], group="ID", width = "1500" ,height=400)%>%   dyLegend(width = "1200") # %>% dyCandlestick()
  PlotSignals=dyAxis(PlotSignals,"x",drawGrid = FALSE)
  PlotSignals=PlotSignals%>%
    dySeries("Entry_UpperBB", stepPlot = FALSE, fillGraph = FALSE, color = adjustcolor( "green", alpha.f = 0.5))%>%
    dySeries("Entry_LowerBB", stepPlot = FALSE, fillGraph = FALSE, color = adjustcolor( "green", alpha.f = 0.5))%>%
    dySeries("Exit_UpperBB", stepPlot = FALSE, fillGraph = FALSE, color = adjustcolor( "orange", alpha.f = 0.5))%>%
    dySeries("Exit_LowerBB", stepPlot = FALSE, fillGraph = FALSE, color= adjustcolor( "orange", alpha.f = 0.5))%>%
    dySeries("c", stepPlot = FALSE, fillGraph = FALSE, color= "black")
  
  
 # PlotSignals=PlotSignals %>% dyEvent(CriticalShortEntries$EntryCandle, labelLoc = "bottom", strokePattern ="dotted",color ="red")
 # PlotSignals=PlotSignals %>% dyEvent(CriticalLongEntries$EntryCandle, labelLoc = "bottom", strokePattern ="solid",color ="blue")
  

  #Loop over entries to add shading for crtical entry days
  for (i in 1:nrow(Longs)) {
    PlotSignals <- dygraphs::dyShading( PlotSignals, from  =Longs$FirstDayCandle[i], to    = Longs$LastDayCandle[i], color = adjustcolor( "blue", alpha.f = 0.3))
  }
  
  for (i in 1:nrow(Shorts)) {
    PlotSignals <- dygraphs::dyShading( PlotSignals, from  =Shorts$FirstDayCandle[i], to    = Shorts$LastDayCandle[i], color = adjustcolor( "orange", alpha.f = 0.3))
  }
  
  PlotSignals=PlotSignals %>% dyEvent(News$FirstDayCandle, labelLoc = "bottom", strokePattern ="dotted",color = adjustcolor( "pink", alpha.f = 0.5))

  
  
#Calculate return ...daily volativility
  Signals$Date=as.Date(Signals$Date,  format ="%Y-%m-%d ")
  Return=Signals %>% group_by(Date) %>% summarise(Max = max(h), Min = min(l))
  Return$ReturnPerc=(Return$Max-Return$Min)/Return$Min*100
  CriticalReturn= Return %>% filter(abs(ReturnPerc) >=MinDDWanted)
