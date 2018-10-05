library(readr)
Disabilities <- read_csv("Documents/Homeless analysis/CoC/Disabilities.csv")
length(unique(Disabilities$PersonalID))
# 25157
sum(is.na(Disabilities$PersonalID))

Dcount <- Disabilities%>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts = length(PersonalID))

table(Disabilities$DisabilityType)

#Type5
Distype5 <- Disabilities[Disabilities$DisabilityType==5,]
length(unique(Distype5$PersonalID))

#PID 296790
table(Distype5$DisabilityResponse)
attach(Distype5)
Distype5$DisabilityResponse[DisabilityResponse==8] <- NA
Distype5$DisabilityResponse[DisabilityResponse==9] <- NA
Distype5$DisabilityResponse[DisabilityResponse==99] <- NA
Distype5$DisabilityResponse[DisabilityResponse==1] <- 1
Distype5$DisabilityResponse[DisabilityResponse==0] <- 0
detach(Distype5)
sum(!is.na(Distype5$DisabilityResponse))
Distype5 <- Distype5[!is.na(Distype5$DisabilityResponse),] 

Distype5 <- Distype5%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType5=sum(DisabilityResponse))

Distype5$DisType5<- ifelse(Distype5$DisType5==0,0,1)
length(unique(Distype5$PersonalID))



#Type6
Distype6 <- Disabilities[Disabilities$DisabilityType==6,]
length(unique(Distype6$PersonalID))

#PID 296790
table(Distype6$DisabilityResponse)
attach(Distype6)
Distype6$DisabilityResponse[DisabilityResponse==8] <- NA
Distype6$DisabilityResponse[DisabilityResponse==9] <- NA
Distype6$DisabilityResponse[DisabilityResponse==99] <- NA
Distype6$DisabilityResponse[DisabilityResponse==1] <- 1
Distype6$DisabilityResponse[DisabilityResponse==0] <- 0
detach(Distype6)
sum(!is.na(Distype6$DisabilityResponse))
Distype6 <- Distype6[!is.na(Distype6$DisabilityResponse),] 

Distype6 <- Distype6%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType6=sum(DisabilityResponse))

Distype6$DisType6<- ifelse(Distype6$DisType6==0,0,1)
length(unique(Distype6$PersonalID))



#Type7
Distype7 <- Disabilities[Disabilities$DisabilityType==7,]
length(unique(Distype7$PersonalID))

#PID 296790
table(Distype7$DisabilityResponse)
attach(Distype7)
Distype7$DisabilityResponse[DisabilityResponse==8] <- NA
Distype7$DisabilityResponse[DisabilityResponse==9] <- NA
Distype7$DisabilityResponse[DisabilityResponse==99] <- NA
Distype7$DisabilityResponse[DisabilityResponse==1] <- 1
Distype7$DisabilityResponse[DisabilityResponse==0] <- 0
detach(Distype7)
sum(!is.na(Distype7$DisabilityResponse))
Distype7 <- Distype7[!is.na(Distype7$DisabilityResponse),] 

Distype7 <- Distype7%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType7=sum(DisabilityResponse))

Distype7$DisType7<- ifelse(Distype7$DisType7==0,0,1)
length(unique(Distype7$PersonalID))



#Type8
Distype8 <- Disabilities[Disabilities$DisabilityType==8,]
length(unique(Distype8$PersonalID))

#PID 296790
table(Distype8$DisabilityResponse)
attach(Distype8)
Distype8$DisabilityResponse[DisabilityResponse==8] <- NA
Distype8$DisabilityResponse[DisabilityResponse==9] <- NA
Distype8$DisabilityResponse[DisabilityResponse==99] <- NA
Distype8$DisabilityResponse[DisabilityResponse==1] <- 1
Distype8$DisabilityResponse[DisabilityResponse==0] <- 0
detach(Distype8)
sum(!is.na(Distype8$DisabilityResponse))
Distype8 <- Distype8[!is.na(Distype8$DisabilityResponse),] 

Distype8 <- Distype8%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType8=sum(DisabilityResponse))

Distype8$DisType8<- ifelse(Distype8$DisType8==0,0,1)
length(unique(Distype8$PersonalID))



#Type9
Distype9 <- Disabilities[Disabilities$DisabilityType==9,]
length(unique(Distype9$PersonalID))

#PID 296790
table(Distype9$DisabilityResponse)
attach(Distype9)
Distype9$DisabilityResponse[DisabilityResponse==8] <- NA
Distype9$DisabilityResponse[DisabilityResponse==9] <- NA
Distype9$DisabilityResponse[DisabilityResponse==99] <- NA
Distype9$DisabilityResponse[DisabilityResponse==1] <- 1
Distype9$DisabilityResponse[DisabilityResponse==0] <- 0
detach(Distype9)
sum(!is.na(Distype9$DisabilityResponse))
Distype9 <- Distype9[!is.na(Distype9$DisabilityResponse),] 

Distype9 <- Distype9%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType9=sum(DisabilityResponse))

Distype9$DisType9<- ifelse(Distype9$DisType9==0,0,1)
length(unique(Distype9$PersonalID))


#Type10
Distype10 <- Disabilities[Disabilities$DisabilityType==10,]
length(unique(Distype10$PersonalID))

#PID 296790
table(Distype10$DisabilityResponse)
attach(Distype10)
Distype10$DisabilityResponse[DisabilityResponse==8] <- NA
Distype10$DisabilityResponse[DisabilityResponse==9] <- NA
Distype10$DisabilityResponse[DisabilityResponse==99] <- NA
Distype10$DisabilityResponse[DisabilityResponse==3] <- 3
Distype10$DisabilityResponse[DisabilityResponse==2] <- 2
Distype10$DisabilityResponse[DisabilityResponse==1] <- 1
Distype10$DisabilityResponse[DisabilityResponse==0] <- 0
detach(Distype10)
sum(!is.na(Distype10$DisabilityResponse))
Distype10 <- Distype10[!is.na(Distype10$DisabilityResponse),] 

Distype10 <- Distype10%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType10=max(DisabilityResponse))

length(unique(Distype10$PersonalID))


Distype <- merge(Distype5,Distype6, by="PersonalID")
Distype <- merge(Distype,Distype7, by="PersonalID")
Distype <- merge(Distype,Distype8, by="PersonalID")
Distype <- merge(Distype,Distype9, by="PersonalID")
Distype <- merge(Distype,Distype10, by="PersonalID")

testdistype <- merge(Distype,testdf,by="PersonalID")
testdistype <- merge(testdistype,Income.info,by="PersonalID")
testdistype <- merge(testdistype,Enroll.info,by="PersonalID")
table(testdistype$chronicity)

testdistype.2<-testdistype[,c("age","Disable1","income1","Gender1","VeteranStatus1","chronicity",
                              "AmIndAKNative","Asian","BlackAfAmerican","NativeHIOtherPacific","White",
                              "DisType5","DisType6","DisType7","DisType8","DisType9","DisType10")]
matrix1 <-model.matrix(~.,data=testdistype.2)[,-1]
library('corrplot')
corrplot(cor(matrix1),method = "circle")

#split dataset into train/test sets
set.seed(1234)###For reproducible results
train=sample(1:nrow(testdistype), 7919)
testdistype.train=testdistype[train,]
testdistype.testdf=testdistype[-train,]
table(testdistype.train$chronicity)

logisticModel <-glm(chronicity~age+income1+Gender1+VeteranStatus1+
      AmIndAKNative+Asian+
      BlackAfAmerican+NativeHIOtherPacific+
        DisType5+DisType6+DisType7+
        DisType8+DisType9+DisType10
    , data = testdistype.train, family = "binomial")
summary(logisticModel)
logisticModel <-glm(chronicity~age+income1+VeteranStatus1+
                      BlackAfAmerican+
                      DisType6+DisType7+
                      DisType8+DisType9+DisType10
                    , data = testdistype.train, family = "binomial")
summary(logisticModel)
logisticModel <-glm(chronicity~age+income1+VeteranStatus1+
                      BlackAfAmerican+
                      DisType6+DisType7+
                      DisType9+DisType10
                    , data = testdistype.train, family = "binomial")
summary(logisticModel)
#AUC 0.6527 not better than only "Disable1"
# type 7, 9 and 10 more significant


library(lmtest)#likelihood ratio test
modelnull <- glm(chronicity~1 , family=binomial, data=testdistype.train)
summary(modelnull)
lrtest (modelnull, logisticModel)#Chisq is test statistic,pr(>Chisq) is p-value

prob=predict(logisticModel,testdistype.testdf,type=c("response"))
prob
library(pROC) # calculate AUC
g <- roc(chronicity ~ prob, data = testdistype.testdf)
plot(g)
auc(g)



library(ROCR)
roc_pred <- prediction(prob, testdistype.testdf$chronicity)
roc_pred
performance(roc_pred, measure="tpr", x.measure="fpr")
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE)
#LIFT
plot(performance(roc_pred, measure="lift", x.measure="rpp"), colorize=TRUE)

##Sensitivity/specificity curve and precision/recall curve:
#address specialty
plot(performance(roc_pred, measure="sens", x.measure="spec"), colorize=TRUE)
plot(performance(roc_pred, measure="prec", x.measure="rec"), colorize=TRUE)



###Goodness of Fit: Hosmer-Lemeshow Test
library(ResourceSelection)
trainobserved1 <-as.character(testdistype.train$chronicity)
trainobserved <- as.numeric(trainobserved1)
hoslem.test(trainobserved, fitted(logisticModel), g=10)#good fit 0.05