library(readr)
Client <- read_csv("Documents/Homeless analysis/CoC/Client.csv")
str(Client)
#39457 obs. of  30 variables
colnames(Client)
#[1] "PersonalID"           "DOB"                  "DOBDataQuality"       "AmIndAKNative"       
#[5] "Asian"                "BlackAfAmerican"      "NativeHIOtherPacific" "White"               
#[9] "RaceNone"             "Ethnicity"            "Gender"               "OtherGender"         
#[13] "VeteranStatus"        "YearEnteredService"   "YearSeparated"        "WorldWarII"          
#[17] "KoreanWar"            "VietnamWar"           "DesertStorm"          "AfghanistanOEF"      
#[21] "IraqOIF"              "IraqOND"              "OtherTheater"         "MilitaryBranch"      
#[25] "DischargeStatus"      "DateCreated"          "DateUpdated"          "UserID"              
#[29] "DateDeleted"          "ExportID"            
sum(is.na(Client$PersonalID))
length(unique(Client$PersonalID))
#28680

library(dplyr)
counts <-Client %>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts=length(PersonalID))



class(Client$DateCreated)
max(Client$DateCreated) # should be 2018
min(Client$DateCreated)

class(Client$DateUpdated)
max(Client$DateUpdated) #should be 2018
min(Client$DateUpdated)

Client$DateUpdated <- as.Date(Client$DateUpdated,format="%m/%d/%Y %H:%M")
Client$DateCreated <- as.Date(Client$DateCreated,format="%m/%d/%Y %H:%M")

class(Client$DateCreated)
max(Client$DateCreated) # should be 2018
min(Client$DateCreated)

class(Client$DateUpdated)
max(Client$DateUpdated) #should be 2018
min(Client$DateUpdated)

library(dplyr)
# select those rows with the latest updated date in multiple records
clientunique <- Client%>%
  select(PersonalID,DateUpdated)%>%
  group_by(PersonalID)%>%
  summarize(DateUpdated=max(DateUpdated))

clientdf <-merge(clientunique,Client,by=c("PersonalID","DateUpdated"))
length(unique(clientdf$PersonalID))


dup <- clientdf[duplicated(clientdf$PersonalID), ]
#142 records have the same combination of PersonalID and DateUpdated 
# with one record in the test1 dataset below

# test1<-clientdf[!duplicated(clientdf$PersonalID), ]
alldup <-clientdf[clientdf$PersonalID %in% dup$PersonalID,]

alldup1 <- alldup[,c(1,2,27)]

alldupcount <- alldup1%>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts=length(PersonalID)) 
#140 PersonalID has duplicated records,some have two duplicates,some have three
p <- alldupcount$PersonalID[alldupcount$counts >2]


notdup <-clientdf[!clientdf$PersonalID %in% dup$PersonalID,] #28680-140=28540
#select from alldup, those rows with later created
clientunique1 <- alldup%>%
  select(PersonalID,DateCreated)%>%
  group_by(PersonalID)%>%
  summarize(DateCreated=max(DateCreated))

clientdf1 <-merge(clientunique1,alldup,by=c("PersonalID","DateCreated")) 
dup1 <- clientdf1[duplicated(clientdf1$PersonalID), ] 
# 58 have the same combination of PersonalID, Datecreated, Dateupdated with one record in rest dataset
needadd <-clientdf1[!duplicated(clientdf1$PersonalID), ]

clientfinal <-rbind(notdup,needadd) #final client data without any duplicates
length(unique(clientfinal$PersonalID))


#age
sum(is.na(clientfinal$DOB))#660
table(is.na(clientfinal$DOB))
client.info = clientfinal[!is.na(clientfinal$DOB),]
table(is.na(client.info$DOB))
client.info$age <- (as.Date(Sys.Date(),format = "%m/%d/%Y")-as.Date(client.info$DOB,format = "%m/%d/%Y"))/365.25
table(is.na(client.info$age))
client.info$age <-as.numeric(client.info$age)
hist(client.info$age)

#gender
table(Client$Gender)
table(client.info$Gender)
attach(client.info)
client.info$Gender1[Gender==8] <- NA
client.info$Gender1[Gender==9] <- NA
client.info$Gender1[Gender==99] <- NA
client.info$Gender1[Gender==4] <- NA
client.info$Gender1[Gender==0] <- 0
client.info$Gender1[Gender==1] <- 1
client.info$Gender1[Gender==2] <- 0
client.info$Gender1[Gender==3] <- 1
detach(client.info)
table(client.info$Gender1)
table(is.na(client.info$Gender1))
client.info <- client.info[!is.na(client.info$Gender1),]

#Veteran
table(client.info$VeteranStatus)
attach(client.info)
client.info$VeteranStatus1[VeteranStatus==8] <- NA
client.info$VeteranStatus1[VeteranStatus==9] <- NA
client.info$VeteranStatus1[VeteranStatus==99] <- NA
client.info$VeteranStatus1[VeteranStatus==1] <- 1
client.info$VeteranStatus1[VeteranStatus==0] <- 0
detach(client.info)
table(client.info$VeteranStatus1)
table(is.na(client.info$VeteranStatus1))
client.info <- client.info[!is.na(client.info$VeteranStatus1),]

colSums(is.na(client.info))

library(readr)
Enrollment <- read_csv("Documents/Homeless analysis/CoC/Enrollment.csv")
Exit <- read_csv("Documents/Homeless analysis/CoC/Exit.csv")
length(unique(Enrollment$EnrollmentID)) #no duplicates
length(unique(Exit$EnrollmentID)) #60098
#duplicate exits for only one enroll,e.g. 10878,10888,10899

mydata <- merge(Enrollment,Exit, by=c("EnrollmentID","PersonalID"))#60446
mydata <- mydata[,c("EnrollmentID","PersonalID","EntryDate","ExitDate", "ProjectID")]
length(unique(mydata$PersonalID))

class(mydata$EntryDate)
class(mydata$ExitDate)
# mydata$EntryDate <- as.Date(mydata$EntryDate,format="%m-%d-%Y")
# mydata$ExitDate <- as.Date(mydata$ExitDate,format="%m-%d-%Y")

mydatadup <- mydata[duplicated(mydata$EnrollmentID),]#348
mydataalldup <-mydata[mydata$EnrollmentID %in% mydatadup$EnrollmentID,]#696

mydatanotdup <-mydata[!mydata$EnrollmentID %in% mydatadup$EnrollmentID,]#60446-696=59750
library(dplyr)
mydataunique <- mydataalldup%>%
  select(EnrollmentID,EntryDate)%>%
  group_by(EnrollmentID)%>%
  summarize(EntryDate=min(EntryDate))
mydata1 <-merge(mydataalldup,mydataunique,by = c("EnrollmentID","EntryDate"))

mydataunique1 <- mydata1%>%
  select(EnrollmentID,ExitDate)%>%
  group_by(EnrollmentID)%>%
  summarize(ExitDate=max(ExitDate))

mydata2 <-merge(mydataalldup,mydataunique1,by = c("EnrollmentID","ExitDate"))
mydatadup1 <- mydata2[duplicated(mydata2$EnrollmentID),]
mydataneedadd <-mydata2[!duplicated(mydata2$EnrollmentID),]
mydatafinal <- rbind(mydatanotdup, mydataneedadd) #60098
length(unique(mydata$PersonalID))
length(unique(mydata$EnrollmentID))


# https://www.statmethods.net/management/sorting.html
mydata <- mydatafinal[order(mydatafinal$PersonalID,mydatafinal$EntryDate),]

#test1
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$ExitDate[i+1])<(mydata$ExitDate[i])){
      print((mydata$PersonalID[i+1]))
    } 
  }
} 
#e.g. 333197,334263,607965
result = list()
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$ExitDate[i+1])<(mydata$ExitDate[i])){
      result=rbind(mydata$PersonalID[i+1],result)
    } 
  }
}
a = c()
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$ExitDate[i+1])<(mydata$ExitDate[i])){
      a=c(a,mydata$PersonalID[i+1])
    } 
  }
}
#ExitDate
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$ExitDate[i+1])<(mydata$ExitDate[i])){
      mydata$ExitDate[i+1] <- mydata$ExitDate[i]
    } 
  } 
}  

#test2
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$EntryDate[i+1]) < (mydata$ExitDate[i])){
      print((mydata$PersonalID[i+1]))
    } 
  }
}  
# e.g. PID:452889,EID:797411 entrydate:2018-01-05 exitdate:2018-03-11
#                 EID:797779 entrydate:2018-01-17 exitdate:2018-02-11
# 452891 pic
# 333197

result1 = list()
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$EntryDate[i+1]) < (mydata$ExitDate[i])){
      result1=rbind(result1,mydata$PersonalID[i+1])
    } 
  }
} 
b=c()
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$EntryDate[i+1]) < (mydata$ExitDate[i])){
      b=c(b,mydata$PersonalID[i+1])
    } 
  }
}
#EntryDate
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$EntryDate[i+1])<(mydata$ExitDate[i])){
      mydata$EntryDate[i+1] <- mydata$ExitDate[i]
    } 
  } 
}  

#calculate LoH
mydata$EntryYear <- format(as.Date(mydata$EntryDate, format= "%m/%d/%Y"),"%Y")
mydata$ExitYear <- format(as.Date(mydata$ExitDate, format= "%m/%d/%Y"),"%Y")

#count unique persons
length(unique(mydata$PersonalID))
# 22693

#count entrants each year
table(mydata$EntryYear)
table(mydata$ExitYear)

#calculate length of homeless of each episode
mydata$lengthhomeless <-mydata$ExitDate-mydata$EntryDate
range(mydata$lengthhomeless)
mydata$PersonalID[mydata$lengthhomeless==5030]
# PID 317436

#calculate total length of homeless, years in the system

library(dplyr)
loh <- mydata%>%
  select(PersonalID, lengthhomeless, ExitYear,EntryYear)%>%
  group_by(PersonalID)%>%
  summarise(lengthhomelesstotal = sum(lengthhomeless), years =max(as.numeric(ExitYear))-min(as.numeric(EntryYear)))
View(loh)

#calculate calendar years in the system
loh$years <- loh$years+1

#average length of homeless per year
loh$averagelength <-as.numeric(loh$lengthhomelesstotal)/as.numeric(loh$years)
range(loh$averagelength)
mean(loh$averagelength)
loh$PersonalID[loh$averagelength==0]
loh$PersonalID[loh$averagelength==max(loh$averagelength)]
# PID 683560
hist(loh$averagelength)
table(loh$averagelength*3>=365)



#demographic & LoH
testdf <- merge(client.info,loh, by="PersonalID")

colSums(is.na(testdf))
plot(testdf$age,testdf$averagelength)
cor(testdf$age,testdf$averagelength) #the correlation is very weak
cor.test(testdf$age,testdf$averagelength)

library("ggplot2")
ggplot(testdf,
       aes(x= testdf$age,y=testdf$averagelength))+geom_smooth()+geom_point()

testmodel <- lm(testdf$averagelength ~ testdf$age)
# http://r-statistics.co/Assumptions-of-Linear-Regression.html
mean(testmodel$residuals) #The mean of residuals is zero
summary(testmodel)

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(testmodel) # Homoscedasticity of residuals or equal variance does not hold

cor.test(testdf$age, testmodel$residuals) # p-value = 1 not reject H0 (not correlated)

ggplot(testdf,
       aes(x= testdf$Gender1,y=testdf$averagelength))+geom_smooth()+geom_point()
chisq.test(testdf$Gender1,testdf$averagelength)
cor.test(testdf$Gender1,testdf$averagelength)
####the assumptions of linear regression do not hold####


testdf$chronicity <-ifelse(testdf$averagelength*3>=365,1,0)

table(testdf$chronicity)
cor.test(testdf$chronicity,testdf$age)
chisq.test(testdf$chronicity,testdf$Gender1)
chisq.test(testdf$chronicity,testdf$VeteranStatus1)

par(mfrow=c(1,1))
# dev.off()
plot(testdf$chronicity,testdf$age)
plot(testdf$chronicity,testdf$Gender1)
table(testdf$chronicity,testdf$Gender1)

# independent samples test 
# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor
t.test(testdf$age~testdf$chronicity) #mean ages for chronicity or not are significantly different.
t.test(testdf$Gender1~testdf$chronicity)# frequency of gender is s~ different
t.test(testdf$VeteranStatus1~testdf$chronicity) #veteran is s~ different

#enrollment disabiling condition
sum(is.na(Enrollment$DisablingCondition))#1365
table(Enrollment$DisablingCondition)
attach(Enrollment)
Enrollment$Disability[DisablingCondition==8] <- NA
Enrollment$Disability[DisablingCondition==9] <- NA
Enrollment$Disability[DisablingCondition==99] <- NA
Enrollment$Disability[DisablingCondition==1] <- 1
Enrollment$Disability[DisablingCondition==0] <- 0
detach(Enrollment)
sum(!is.na(Enrollment$Disability))
Enrolldf <- Enrollment[!is.na(Enrollment$Disability),]#52071

length(unique(Enrolldf$PersonalID)) 
Enroll.info <- Enrolldf%>%
  select(PersonalID, Disability)%>%
  group_by(PersonalID)%>%
  summarise(Disable = sum(as.numeric(Disability)))
View(Enroll.info)
Enroll.info$Disable1 <- ifelse(Enroll.info$Disable==0,0,1)

testdf1 <-merge(testdf,Enroll.info,by="PersonalID")
table(testdf1$chronicity)
chisq.test(testdf1$chronicity,testdf1$Disable1)
table(testdf1$chronicity,testdf1$Disable1)
t.test(testdf1$Disable1~testdf1$chronicity)#significantly different

#income info
library(readr)
IncomeBenefits <- read_csv("Documents/Homeless analysis/CoC/IncomeBenefits.csv")

attach(IncomeBenefits)
IncomeBenefits$INCOME[IncomeFromAnySource==8] <- NA
IncomeBenefits$INCOME[IncomeFromAnySource==9] <- NA
IncomeBenefits$INCOME[IncomeFromAnySource==99] <- NA
IncomeBenefits$INCOME[IncomeFromAnySource==1] <- 1
IncomeBenefits$INCOME[IncomeFromAnySource==0] <- 0
detach(IncomeBenefits)
sum(!is.na(IncomeBenefits$INCOME))
Incomedf <- IncomeBenefits[!is.na(IncomeBenefits$INCOME),]
length(unique(Incomedf$PersonalID)) 

library(dplyr)
Income.info <- Incomedf%>%
  select(PersonalID, INCOME)%>%
  group_by(PersonalID)%>%
  summarise(income = sum(as.numeric(INCOME)))
View(Income.info)
Income.info$income1 <- ifelse(Income.info$income==0,0,1)
table(Income.info$income1)
# 0     1 
# 10612 10115 

testdf2 <-merge(testdf1,Income.info,by="PersonalID")
table(testdf2$chronicity)

chisq.test(testdf2$chronicity,testdf2$income1)
table(testdf2$chronicity,testdf2$income1)
t.test(testdf2$income1~testdf2$chronicity)#significantly different


#build LOGISTIC Regression model
testdf2$chronicity <-factor(testdf2$chronicity)
testdf2$Gender1 <-factor(testdf2$Gender1)
testdf2$VeteranStatus1 <-factor(testdf2$VeteranStatus1)
testdf2$Disable1<-factor(testdf2$Disable1)
testdf2$income1<-factor(testdf2$income1)
testdf2$age <-as.numeric(testdf2$age)
testdf2$AmIndAKNative <-factor(testdf2$AmIndAKNative)
testdf2$Asian <-factor(testdf2$Asian)
testdf2$BlackAfAmerican <-factor(testdf2$BlackAfAmerican)
testdf2$NativeHIOtherPacific <-factor(testdf2$NativeHIOtherPacific)
testdf2$White <-factor(testdf2$White)

class(testdf2$Disable1)
class(testdf2$income1)

testdf2.2<-testdf2[,c("age","Disable1","income1","Gender1","VeteranStatus1","chronicity",
                      "AmIndAKNative","Asian","BlackAfAmerican","NativeHIOtherPacific","White")]  


matrix1 <-model.matrix(~.,data=testdf2.2)[,-1]
library('corrplot')
corrplot(cor(matrix1),method = "circle")

#split dataset into train/test sets
set.seed(1234)###For reproducible results
train=sample(1:nrow(testdf2), 8341)
testdf2.train=testdf2[train,]
testdf2.testdf=testdf2[-train,]


logisticModeltest <- glm(chronicity~age+Disable1+income1+Gender1+VeteranStatus1+
                           AmIndAKNative+Asian+
                           BlackAfAmerican+NativeHIOtherPacific
                         , data = testdf2.train, family = "binomial")
summary(logisticModeltest)
logisticModeltest <- glm(chronicity~age+Disable1+income1+Gender1+VeteranStatus1+
                           AmIndAKNative+Asian+
                           NativeHIOtherPacific+
                           White, data = testdf2.train, family = "binomial")
summary(logisticModeltest) 

#BlackAmerican 0.6693 (better)
logisticModel <- glm(chronicity~age+Disable1+income1+
                       VeteranStatus1+BlackAfAmerican, 
                     data = testdf2.train, family = "binomial")
summary(logisticModel)

library(lmtest)#likelihood ratio test
modelnull <- glm(chronicity~1 , family=binomial, data=testdf2.train)
summary(modelnull)
lrtest (modelnull, logisticModel)#Chisq is test statistic,pr(>Chisq) is p-value

prob=predict(logisticModel,testdf2.testdf,type=c("response"))
prob
library(pROC) # calculate AUC
g <- roc(chronicity ~ prob, data = testdf2.testdf)
plot(g)
auc(g)



library(ROCR)
roc_pred <- prediction(prob, testdf2.testdf$chronicity)
roc_pred
performance(roc_pred, measure="tpr", x.measure="fpr")
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE)
#LIFT
plot(performance(roc_pred, measure="lift", x.measure="rpp"), colorize=TRUE)

##Sensitivity/specificity curve and precision/recall curve:
#address specialty
plot(performance(roc_pred, measure="sens", x.measure="spec"), colorize=TRUE)
plot(performance(roc_pred, measure="prec", x.measure="rec"), colorize=TRUE)
sort(prob,decreasing = TRUE)

testdf2.2<-testdf2[,c("age","Disable1","income1","Gender1","VeteranStatus1","chronicity",
                      "AmIndAKNative","Asian","BlackAfAmerican","NativeHIOtherPacific","White")]  


matrix1 <-model.matrix(~.,data=testdf2.2)[,-1]
library('corrplot')
corrplot(cor(matrix1),method = "circle")


###Goodness of Fit: Hosmer-Lemeshow Test
library(ResourceSelection)
testdf2.train$chronicity
trainobserved1 <-as.character(testdf2.train$chronicity)
trainobserved <- as.numeric(trainobserved1)
hoslem.test(trainobserved, fitted(logisticModel), g=10)#good fit




logisticModel1 <- glm(chronicity~age+Disable1+income1+
                       VeteranStatus1+White, 
                     data = testdf2.train, family = "binomial")
summary(logisticModel1)

library(lmtest)#likelihood ratio test
modelnull <- glm(chronicity~1 , family=binomial, data=testdf2.train)
summary(modelnull)
lrtest (modelnull, logisticModel1)#Chisq is test statistic,pr(>Chisq) is p-value

prob1=predict(logisticModel1,testdf2.testdf,type=c("response"))
prob1
library(pROC) # calculate AUC
h <- roc(chronicity ~ prob1, data = testdf2.testdf)
plot(h)
auc(h)
