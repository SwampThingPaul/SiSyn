#use after running HJA_WRTDS_Si - requires the output from that script to run this code

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS")

#extract all files of continuous Si data
csvContList<-list.files(pattern = "_Si_Cont.csv")

#create time series of Si concentration for each site
pdf("Si_TIme_Series_HJA.pdf")

for (i in 1:length(csvContList)) {
  
  data<-read.csv(csvContList[i])
  
  title<-unlist(strsplit(csvContList[i], "\\_"))
  
  data$Date<-as.Date(data$Date)
  
  p1<-ggplot(data)+geom_line(aes(Date, ConcDay))+ggtitle(title[2])
  
  print(p1)
}

dev.off()

#plots annual variation for each site. WY are differentiated on each plot by color
pdf("MontlyChange_Si.pdf")

for (i in 1:length(csvContList)) {
  
  data<-read.csv(csvContList[i])
  
  dataagg<-aggregate(data, by=list(data$Month, data$waterYear), mean)
  
  title<-unlist(strsplit(csvContList[i], "\\_"))
  
  p1<-ggplot(dataagg)+geom_line(aes(Month, ConcDay, color=factor(waterYear)))+ggtitle(title[2])
  
  print(p1)
}

dev.off()


#combine all dataframes from ContList (continuous data files)
data<-read.csv(csvContList[1])

title<-unlist(strsplit(csvContList[1], "\\_"))

data$Site<-paste(title[2])

for (i in 2:length(csvContList)) {
  
  data2<-read.csv(csvContList[i])
  
  title<-unlist(strsplit(csvContList[i], "\\_"))
  
  data2$Site<-paste(title[2])
  
  data<-bind_rows(data, data2)
  
}

contMaster<-data

contMaster$Month<-as.numeric(contMaster$Month)

unique(contMaster$Site)

monthsList<-seq(1, 12, by=1)

monthsNames<-month.abb[monthsList]

#use dataframe of all files to plot Si concentration by month. Sites are differentiaed by colors.
pdf("Monthly_ChangebySite.pdf")

for (i in 1:length(monthsList)) {
  
  contMonth<-contMaster[which(contMaster$Month==monthsList[i]),]
  
  dataagg<-aggregate(contMonth, by=list(contMonth$waterYear, contMonth$Site), mean)
  
  p1<-ggplot(dataagg)+geom_line(aes(Group.1, ConcDay, color=Group.2))+ggtitle(paste(monthsNames[i]))
  
  print(p1)
  
}

dev.off()

