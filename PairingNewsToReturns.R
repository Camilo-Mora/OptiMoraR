
#Matching critical entries to news
#library(sqldf)
library(purrr)
library(dplyr)
library(ggplot2)
library(plotly)
library(Stocks)
library(zoo)

MinDDWanted=3 #percent of account wanted to be at maximum DD for this pair. 
ZigZagDDWated=2 #largest etesion in price used for sqing selection...used smaller than the returns DD wated..
PerCorrection=0.25 #Percent of correction tested. This is used to calcualte zigzag of this size of large. Basically, times when market extended before having a correction of this size

ForexData=read.csv("D:/Scrips/Trading/ForexPairs2YrsAll.csv")

TickerI="XAUUSD"
TickerForDat=ForexData %>% filter(TickerNam==TickerI)

DaylyTickerForDat=TickerForDat %>% filter(TF=="1 day")

DaylyTickerForDat$Return=abs(DaylyTickerForDat$h-DaylyTickerForDat$l)/DaylyTickerForDat$l*100 #Calcaulte returns as percentahe 
DaylyTickerForDat$Date=as.Date(DaylyTickerForDat$Date,  format ="%Y-%m-%d") #Convert to date




 
  
#news
  News=read.csv("D:/Scrips/Trading/OptiMoraR/ForexNewsData.csv")
  News$X=NULL
  News1=News %>% filter(currency == substr(TickerI, 1, 3))
  News2=News %>% filter(currency == substr(TickerI, 4, 6))

  News=rbind(News1,News2) #Concantenate news from both pairs
  News$Date=as.Date(News$DateUTC,  format ="%Y-%m-%d")
  

#Merge
  ReturnNews=merge(DaylyTickerForDat,News, by="Date", all.x=TRUE)
  
#Returns by news type...resume the variation in return when each news is reported
  ReturnByNews =ReturnNews %>% group_by(event) %>% summarise(MeanReturn = mean(Return, na.rm=T),MinReturn = min(Return, na.rm=T),MaxReturn = max(Return, na.rm=T))
  ReturnByNews=ReturnByNews %>% arrange(desc(MaxReturn))
  ReturnByNews$Rank=(1:nrow(ReturnByNews))
  ReturnByNews$Critical=ifelse(abs(ReturnByNews$MaxReturn)>=MinDDWanted, "Yes","No")
  
  ReturnRangeByNews = plot_ly(ReturnByNews, x = ~Rank, xend = ~Rank, y = ~MinReturn, yend = ~MaxReturn, 
          text = ~event, hoverinfo = "event") %>% 
    add_segments(color = ~Critical,
                 colors = c("Yes" = "red", "No" = "blue")) %>% 
          layout(
           showlegend = F,
           plot_bgcolor="rgba(0, 0, 0, 0)",
           xaxis = list(
             title ="",
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = "rgba(0, 0, 0, 0)",
             tickvals = ReturnByNews$Rank, ticktext = ReturnByNews$event,
             tickfont = list(size = 7) 
           ),
           yaxis = list(
             title ="Range of Daily Returns (%)",
             zerolinecolor = '#ffff',
             zerolinewidth = 2)
          )%>%
        add_segments(x = 0, xend = max(ReturnByNews$Rank), y = 3, yend = 3, color=I("red"), line = list(dash = 'dot')) 

  
  
#################################################################################################################################
###----------------------------------------------Critical Returns-------------------------------------------------------------###
#-------------------------------------Returns that excedd the desire dropdown-------------------------------------------------###
#################################################################################################################################
  CriticalReturns=ReturnNews %>% filter(Return >=MinDDWanted)
  NewsEvents=unique(CriticalReturns$event)
  
CritLeftAfterSequaltialRemval=data.frame(Eventname="Total",CriticalLeft=length(unique(CriticalReturns$Date)))

#count number of critical entries left after removing days of given news.
Count=length(NewsEvents)
while (Count!=0){
TemLeft=data.frame(Eventname=as.character(),CriticalLeft=as.numeric())
NewsEvents=unique(CriticalReturns$event)
NewsEvents <- NewsEvents[!is.na(NewsEvents)]
for (NewsI in NewsEvents){
  CriticalRetLeft = CriticalReturns %>% filter(event != NewsI | (is.na(event)))
  NCriticalRetLeft=length(unique(CriticalRetLeft$Date))
  Vals=c(NewsI, NCriticalRetLeft)
  TemLeft=rbind(TemLeft,Vals)
}

colnames(TemLeft)=c("Eventname", "CriticalLeft")
LeftCri=TemLeft %>% filter(CriticalLeft ==min(CriticalLeft))

#removve sequentia news that rank even
EvenNews=unique(LeftCri$Eventname)

for (EvenNewsI in EvenNews){
  CriticalReturns = CriticalReturns %>% filter(event != EvenNewsI | (is.na(event)))
  FinalLeft=length(unique(CriticalReturns$Date))
  FinalVals=c(EvenNewsI, FinalLeft)
  CritLeftAfterSequaltialRemval=rbind(CritLeftAfterSequaltialRemval,FinalVals)
}
Count=nrow(CriticalReturns)
NAs=sum(is.na(CriticalReturns$event))
if(NAs==Count){Count=0}
}

NonFilterCriticalReturn=CriticalReturns[,c("Date", "Return")] #list of criticl returns that were not filtered

assign(paste0("NonFiltereReturns_",TickerI),NonFilterCriticalReturn)

#count days left for working after news days are removed
DaystAfterSequaltialRemval=data.frame(Eventname="Total",DaysLeft=length(unique(ReturnNews$Date)), MaximumDDLeft=round(max(ReturnNews$Return, na.rm=T),1))
ImportantNews=CritLeftAfterSequaltialRemval$Eventname

DaysLeftDatabase=ReturnNews
FinalDaysLeft=c()
for (ImporantNewsI in ImportantNews){
  DaysLeftDatabase=DaysLeftDatabase %>% filter(event !=ImporantNewsI | (is.na(event))) #Exclude dates with the news and include days with no news...to cound days left after removing days with the given news
  MaxDDLeft=round(max(DaysLeftDatabase$Return,na.rm=TRUE),1)
  nDaysLeft=length(unique(DaysLeftDatabase$Date))
  ResltDayLeft=c(ImporantNewsI,nDaysLeft, MaxDDLeft)
  FinalDaysLeft=rbind(FinalDaysLeft,ResltDayLeft)
}

FinalDaysLeft=data.frame(FinalDaysLeft[-c(1), ]  )
colnames(FinalDaysLeft)=c("Eventname", "DaysLeft", "MaximumDDLeft")
FinalDaysLeft=rbind(DaystAfterSequaltialRemval,FinalDaysLeft)


FinalData=merge(CritLeftAfterSequaltialRemval, FinalDaysLeft,by="Eventname")

FinalData$CriticalLeft=as.numeric(FinalData$CriticalLeft)
FinalData$DaysLeft=as.numeric(FinalData$DaysLeft)
FinalData$MaximumDDLeft=as.numeric(FinalData$MaximumDDLeft)
FinalData=FinalData %>% arrange(desc(CriticalLeft))
FinalData$Rank=1:nrow(FinalData)

#plot relationship bwetween news and retruns
fig <- plot_ly()

fig <- fig %>% add_trace(x = ~FinalData$Rank, y = ~FinalData$CriticalLeft, name = "CriticalDaysLeft", mode = "lines+markers", type = "scatter", line = list(color = 'red'), marker = list(color ="red"))


y2 <- list( color ="orange",
           showline = T,
           ticks='outside',
           showgrid = F, 
           tickfont = list(color = "orange"), 
           titlefont = list(color = "orange"), 
           overlaying = "y",  side = "left", 
           anchor="free",  
           position=0.01, 
           title = "Maximum DropDown Left")
fig <- fig %>% add_trace(x = FinalData$Rank, y = ~FinalData$MaximumDDLeft, name = "MaximumDDLeft", yaxis = "y2", mode = "lines+markers", type = "scatter", line = list(color = 'orange'), marker = list(color ="orange"))


y3 <- list( color ="blue",
            showline = T,
            ticks='outside',
           showgrid = F, 
           tickfont = list(color = "blue"),  
           titlefont = list(color = "blue"), 
           overlaying = "y", side = "right", 
           title = "Days Left for Trading")
fig <- fig %>% add_trace(x = ~FinalData$Rank, y = ~FinalData$DaysLeft, name = "GoodDaysLeft", yaxis = "y3", mode = "lines+markers", type = "scatter", line = list(color = 'blue'), marker = list(color ="blue"))




fig <- fig %>% layout(
  yaxis2 = y2, yaxis3 = y3, 
  xaxis = list(title = '', domain = c(0.1, 0.9)),
  yaxis = list(title="CriticalDaysLeft",
               tickfont = list(color = "red"),
               titlefont = list(color = "red")
  )
  
)%>%
  
  layout(showlegend = F,
         plot_bgcolor="rgba(0, 0, 0, 0)",
         xaxis = list( 
           ticks='outside',
           zerolinecolor = "rgba(0, 0, 0, 0)",
           zerolinewidth = 2,
           gridcolor = "rgba(0, 0, 0, 0)",
           showline = T,
           color="black",
         tickvals = FinalData$Rank, ticktext = FinalData$Eventname),
         yaxis = list(
           color ="red",
           showline = T,
           ticks='outside',
           zerolinecolor = "rgba(0, 0, 0, 0)",
           zerolinewidth = 2,
           gridcolor = "rgba(0, 0, 0, 0)")
  )




assign(paste0("CriticalReturns_",TickerI),fig)



#################################################################################################################################
###-----------------------------------------------Critical Swings-------------------------------------------------------------###
#-------------------------------------Zigzags that excedd the desire dropdown for zigzas--------------------------------------###
#################################################################################################################################


###do filtering by size of swings
MineTickrForDt=TickerForDat %>% filter(TF=="1 min")
Date=MineTickrForDt[,c("X", "Date")]
SizeSwins=ZigZag_Frequency.Swings(MineTickrForDt$c,change=PerCorrection)
Swings=data.frame(Swings=ZigZag_Swings.Points(MineTickrForDt$c,change=PerCorrection))
Swings$X=MineTickrForDt$X
Swings=na.omit(Swings) 
Swings=Swings[1:(nrow(Swings)-1),]
Swings$SizeSwins=SizeSwins

Swings=merge(Swings,Date, by="X", all=TRUE)
Swings$Date=as.Date(Swings$Date,  format ="%Y-%m-%d")
Swings$Correction=PerCorrection
#Merge with news
SwingsNews=merge(Swings,News, by="Date", all.x=TRUE)

CriticalSwingsNews=SwingsNews %>% filter(abs(SizeSwins) >=ZigZagDDWated)
CriticalSwingsNews = rename(CriticalSwingsNews, Price = Swings)



nCriticalSwin=length(unique(CriticalSwingsNews$X))


CriticalSwinNews=unique(CriticalSwingsNews$event)
CriticalSwinNews <- CriticalSwinNews[!is.na(CriticalSwinNews)]

CountCSwin=nrow(CriticalSwingsNews)
CritSwinLeftAfterSequaltialRemval=data.frame(Eventname="Total",CriticalSwingLeft=nCriticalSwin)


while (CountCSwin!=0){
  TemLeft=data.frame(Eventname=as.character(),CriticalSwingLeft=as.numeric())
  CriticalSwinNews=unique(CriticalSwingsNews$event)
  CriticalSwinNews <- CriticalSwinNews[!is.na(CriticalSwinNews)]
  for (NewsI in CriticalSwinNews){
    CriticalRetLeft = CriticalSwingsNews %>% filter(event != NewsI | (is.na(event)) )
    NCriticalRetLeft=length(unique(CriticalRetLeft$X))
    Vals=c(NewsI, NCriticalRetLeft)
    TemLeft=rbind(TemLeft,Vals)
  }
  
  colnames(TemLeft)=c("Eventname", "CriticalSwingLeft")
  LeftCri=TemLeft %>% filter(CriticalSwingLeft ==min(CriticalSwingLeft))
  
  #removve sequentia news that rank even
  EvenNews=unique(LeftCri$Eventname)
  
  for (EvenNewsI in EvenNews){
    CriticalSwingsNews = CriticalSwingsNews %>% filter(event != EvenNewsI | (is.na(event)))
    FinalLeft=length(unique((CriticalSwingsNews$X)))
    FinalVals=c(EvenNewsI, FinalLeft)
    CritSwinLeftAfterSequaltialRemval=rbind(CritSwinLeftAfterSequaltialRemval,FinalVals)
  }
  CountCSwin=nrow(CriticalSwingsNews)
  NAs=sum(is.na(CriticalSwingsNews$event))
if(NAs==CountCSwin){CountCSwin=0}
}

NonFilterCriticalSwing=CriticalSwingsNews[,c("Date", "SizeSwins")] #list of criticl swings that were not filtered


assign(paste0("NonFiltereSwings_",TickerI),NonFilterCriticalSwing)

#count days left for working after news days are removed

SwinFinalDaysLeft=c()

ImportantNews=unique(CritSwinLeftAfterSequaltialRemval$Eventname)
for (ImporantNewsI in ImportantNews){
  SwingsNews=SwingsNews %>% filter(event !=ImporantNewsI | (is.na(event)))
  nDaysLeft=length(unique(SwingsNews$Date))
  SwinMaxDDLeft=round(max(abs(SwingsNews$SizeSwins),na.rm=TRUE),1)
  ResltDayLeft=c(ImporantNewsI,nDaysLeft, SwinMaxDDLeft)
  SwinFinalDaysLeft=rbind(SwinFinalDaysLeft,ResltDayLeft)
}

SwinFinalDaysLeft=data.frame(SwinFinalDaysLeft )
colnames(SwinFinalDaysLeft)=c("Eventname", "SwinDaysLeft", "SwinMaxDDLeft")
rownames(SwinFinalDaysLeft)=NULL


SwinFinalData=merge(CritSwinLeftAfterSequaltialRemval, SwinFinalDaysLeft,by="Eventname")

SwinFinalData$CriticalSwingLeft =as.numeric(SwinFinalData$CriticalSwingLeft )
SwinFinalData$SwinDaysLeft=as.numeric(SwinFinalData$SwinDaysLeft)
SwinFinalData=SwinFinalData %>% arrange(desc(CriticalSwingLeft ))
SwinFinalData$Rank=1:nrow(SwinFinalData)


#plot relationship bwetween news and retruns
fig <- plot_ly()

fig <- fig %>% add_trace(x = ~SwinFinalData$Rank, y = ~SwinFinalData$CriticalSwingLeft, name = "CriticalDaysLeft", mode = "lines+markers", type = "scatter", line = list(color = 'red'), marker = list(color ="red"))


y2 <- list( color ="orange",
            showline = T,
            ticks='outside',
            showgrid = F, 
            tickfont = list(color = "orange"), 
            titlefont = list(color = "orange"), 
            overlaying = "y",  side = "left", 
            anchor="free",  
            position=0.01, 
            title = "Maximum DropDown Left")
fig <- fig %>% add_trace(x = SwinFinalData$Rank, y = ~SwinFinalData$SwinMaxDDLeft, name = "MaximumDDLeft", yaxis = "y2", mode = "lines+markers", type = "scatter", line = list(color = 'orange'), marker = list(color ="orange"))


y3 <- list( color ="blue",
            showline = T,
            ticks='outside',
            showgrid = F, 
            tickfont = list(color = "blue"),  
            titlefont = list(color = "blue"), 
            overlaying = "y", side = "right", 
            title = "Days Left for Trading")
fig <- fig %>% add_trace(x = ~SwinFinalData$Rank, y = ~SwinFinalData$SwinDaysLeft, name = "GoodDaysLeft", yaxis = "y3", mode = "lines+markers", type = "scatter", line = list(color = 'blue'), marker = list(color ="blue"))




fig <- fig %>% layout(
  yaxis2 = y2, yaxis3 = y3, 
  xaxis = list(title = '', domain = c(0.1, 0.9)),
  yaxis = list(title="CriticalDaysLeft",
               tickfont = list(color = "red"),
               titlefont = list(color = "red")
  )
  
)%>%
  
  layout(showlegend = F,
         plot_bgcolor="rgba(0, 0, 0, 0)",
         xaxis = list( 
           ticks='outside',
           zerolinecolor = "rgba(0, 0, 0, 0)",
           zerolinewidth = 2,
           gridcolor = "rgba(0, 0, 0, 0)",
           showline = T,
           color="black",
           tickvals = SwinFinalData$Rank, ticktext = SwinFinalData$Eventname),
         yaxis = list(
           color ="red",
           showline = T,
           ticks='outside',
           zerolinecolor = "rgba(0, 0, 0, 0)",
           zerolinewidth = 2,
           gridcolor = "rgba(0, 0, 0, 0)")
  )

assign(paste0("CriticalSwings_",TickerI),fig)



