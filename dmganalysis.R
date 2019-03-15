library(data.table)
library(dplyr)


storm<-fread("repdata_data_StormData.csv",sep=",",header=TRUE)
storm<-tbl_df(storm)
summary(storm)
names(storm)
stormpop<-subset(storm,select = c("EVTYPE","FATALITIES","INJURIES"))
fatalities<-aggregate(data=stormpop,FATALITIES~EVTYPE,FUN = sum)
fatalities<-fatalities[order(fatalities$FATALITIES,decreasing = TRUE),]
injuries<-aggregate(data=stormpop,INJURIES~EVTYPE,FUN=sum)
injuries<-injuries[order(injuries$INJURIES,decreasing = TRUE),]

topfatalities<-fatalities[1:20,]
topinjuries<-injuries[1:20,]
par(mfrow = c(1, 2), las = 2,cex=0.7,font.lab=2,mar=c(8,4,1,1))
barplot(topfatalities$FATALITIES,names.arg = topfatalities$EVTYPE,col="red",legend.text = "FATALITIES")
barplot(topinjuries$INJURIES,names.arg = topinjuries$EVTYPE,col="pink",legend.text = "INJURIES")

healthdmg<-merge(fatalities,injuries,by="EVTYPE")
healthdmg$dmg<-healthdmg$FATALITIES+healthdmg$INJURIES
healthdmg<-healthdmg[order(healthdmg$dmg,decreasing = TRUE),]
tophealthdmg<-healthdmg[1:20,]
par(mfrow=c(1,1),mar=c(12,8,3,3),cex=0.6)
barplot(tophealthdmg$dmg,names.arg = tophealthdmg$EVTYPE,col="orange",legend.text = "Total Health Damage")


stormprop<-subset(storm,select=c("EVTYPE","PROPDMG","CROPDMG"))
propdmg<-aggregate(data=stormprop,PROPDMG~EVTYPE,FUN=sum)
cropdmg<-aggregate(data=stormprop,CROPDMG~EVTYPE,FUN=sum)
propdmg<-propdmg[order(propdmg$PROPDMG,decreasing = TRUE),]
cropdmg<-cropdmg[order(cropdmg$CROPDMG,decreasing=TRUE),]

topprop<-propdmg[1:20,]
topcrop<-cropdmg[1:20,]
par(mfrow = c(1, 2), las = 3,cex=0.6,font.lab=2,mar=c(10,4,1,1))
barplot(topprop$PROPDMG,names.arg = topprop$EVTYPE,col="blue",legend.text = "Property DMG")
barplot(topcrop$CROPDMG,names.arg = topcrop$EVTYPE,col="green",legend.text = "Crop DMG")

economicdmg<-merge(propdmg,cropdmg,by="EVTYPE")
economicdmg$ecodmg<- economicdmg$PROPDMG+economicdmg$CROPDMG
economicdmg<-economicdmg[order(economicdmg$ecodmg,decreasing = TRUE),]
topecodmg<-economicdmg[1:20,]
par(mfrow=c(1,1),mar=c(12,8,3,3),cex=0.6)
barplot(topecodmg$ecodmg,names.arg=topecodmg$EVTYPE,col="brown",legend.text = "Total Economic Damage")
