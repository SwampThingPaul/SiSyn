##random forest for si data
install.packages("randomForest")
install.packages("caret")
install.packages("BAMMtools")
install.packages("tree")
require(randomForest)
require(caret)
require(plyr)
require(dplyr)
require(BAMMtools)
require(tree)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in files
site_chars<-read.csv("PCA_WRTDS_093021.csv")
RF_data<-read.csv("UMR_Data_RF_011022.csv")
names(RF_data)[1]<-"SITE"

climate<-read.csv("MAP_MAT_byStream.csv")

#merge
RF_data<-merge(RF_data, site_chars, by="SITE")
RF_data<-merge(RF_data, climate, by="SITE")

#keep important columns
RF_data<-RF_data[,c(3:8, 12,16,22,25:27,31, 36,37)]

RF_data<-RF_data[complete.cases(RF_data),]

hist(RF_data$Si_Conc)

pdf("RandomForestOutput_v4.pdf")

ind<-sample(2, nrow(RF_data), replace = TRUE, prob = c(0.7, 0.3))

train <- RF_data[ind==1,]

test <- RF_data[ind==2,]

rf <- randomForest(Si_Conc~., data=train, proximity=TRUE, ntree=2000, mtry=5)

plot(rf)

p1 <- predict(rf, train)

p2<-predict(rf, test)

lm<-lm(p2~test$Si_Conc)
lm_sum<-summary(lm)

rf_test_output<-lm_sum$coefficients[1]+lm_sum$coefficients[2]*p2

r2<-round(lm_sum$r.squared, 2)

plot(test$Si_Conc, p2, xlab = "Observed Si", ylab = "Modeled Si")
lines(p2, rf_test_output)
text(x=3.2, y=6.5, paste0("R2=",r2))

tr<-tree(rf, test)

plot(tr); text(tr)

varImpPlot(rf,
           sort = T,
           main = "Variable Importance")

dev.off()


