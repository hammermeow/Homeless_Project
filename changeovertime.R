library(readr)
Enrollment <- read_csv("A_HOMELESS/COC/Enrollment.csv")
Exit <- read_csv("A_HOMELESS/COC/Exit.csv")
length(unique(Enrollment$EnrollmentID)) #no duplicates
length(unique(Exit$EnrollmentID)) #60098
#duplicate exits for only one enroll,e.g. 10878,10888,10899

Exitunique <- Exit%>%
  select(EnrollmentID,ExitDate)%>%
  group_by(EnrollmentID)%>%
  summarize(ExitDate=max(ExitDate))
ExitUpdate <- merge(Exit, Exitunique, by=c("EnrollmentID","ExitDate"))
ExitUpdate <- Exit[!duplicated(Exit$EnrollmentID),]

mydata <- merge(Enrollment,ExitUpdate, by=c("EnrollmentID","PersonalID"))#60098
mydata <- mydata[,c("EnrollmentID","PersonalID","EntryDate","ExitDate", "ProjectID")]
length(unique(mydata$PersonalID))

class(mydata$EntryDate)
class(mydata$ExitDate)
# mydata$EntryDate <- as.Date(mydata$EntryDate,format="%m-%d-%Y")
# mydata$ExitDate <- as.Date(mydata$ExitDate,format="%m-%d-%Y")


# https://www.statmethods.net/management/sorting.html

range(mydata$ExitDate) #"2014-01-01" "2018-05-27"#
enroll2014<-mydata[mydata$EntryDate<=as.Date('12-31-2014',format="%m-%d-%Y"),]
#entered before 2014/12/31
enroll2015<-mydata[mydata$EntryDate<=as.Date('12-31-2015',format="%m-%d-%Y") & mydata$ExitDate>=as.Date('12-31-2014',format="%m-%d-%Y"),]
#entered before 2015/12/31 and not exit in 2014
exit2014 <-mydata[mydata$ExitDate<=as.Date('12-31-2014',format="%m-%d-%Y"),]
enroll2016<-mydata[mydata$EntryDate<=as.Date('12-31-2016',format="%m-%d-%Y") & mydata$ExitDate>=as.Date('12-31-2015',format="%m-%d-%Y"),]
enroll2017<-mydata[mydata$EntryDate<=as.Date('12-31-2017',format="%m-%d-%Y") & mydata$ExitDate>=as.Date('12-31-2016',format="%m-%d-%Y"),]
enroll2018<-mydata[mydata$EntryDate<=as.Date('12-31-2018',format="%m-%d-%Y") & mydata$ExitDate>=as.Date('12-31-2017',format="%m-%d-%Y"),]
length(unique(enroll2014$PersonalID))
length(unique(enroll2015$PersonalID))
length(unique(enroll2016$PersonalID))
length(unique(enroll2017$PersonalID))
length(unique(enroll2018$PersonalID))




library(readr)
Client <- read_csv("A_HOMELESS/COC/Client.csv")
str(Client)

colnames(Client)
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
# 142 records have the same combination of PersonalID and DateUpdated with one record 
#in the test1 dataset below
# test1<-clientdf[!duplicated(clientdf$PersonalID), ]
alldup <-clientdf[clientdf$PersonalID %in% dup$PersonalID,]

alldup1 <- alldup[,c(1,2,27)]

alldupcount <- alldup1%>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts=length(PersonalID)) 
#140 PersonalID has duplicated records,some have two duplicates,some have three
p <- alldupcount$PersonalID[alldupcount$counts >2] #have three duplicates


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

#############################clientfinal#######################################
clientfinal <-rbind(notdup,needadd) #final client data without any duplicates
length(unique(clientfinal$PersonalID))
#############################clientfinal#######################################



#Veteran
table(clientfinal$VeteranStatus)
client.veteran <- clientfinal
attach(client.veteran)
client.veteran$VeteranStatus[VeteranStatus==8] <- NA
client.veteran$VeteranStatus[VeteranStatus==9] <- NA
client.veteran$VeteranStatus[VeteranStatus==99] <- NA
detach(client.veteran)
table(client.veteran$VeteranStatus)
table(is.na(client.veteran$VeteranStatus))
client.veteran <- client.veteran[!is.na(client.veteran$VeteranStatus),]#23870
barplot(table(client.veteran$VeteranStatus),names.arg = c("No","Yes"))
###############################################################################################
client.veteran<-client.veteran[client.veteran$VeteranStatus==1,]
client.veteran<-client.veteran[!is.na(client.veteran$VeteranStatus),]



veteran2014 <-enroll2014[enroll2014$PersonalID %in% client.veteran$PersonalID,]
length(unique(veteran2014$PersonalID))

veteran2015 <-enroll2015[enroll2015$PersonalID %in% client.veteran$PersonalID,]
length(unique(veteran2015$PersonalID))

veteran2016 <-enroll2016[enroll2016$PersonalID %in% client.veteran$PersonalID,]
length(unique(veteran2016$PersonalID))

veteran2017 <-enroll2017[enroll2017$PersonalID %in% client.veteran$PersonalID,]
length(unique(veteran2017$PersonalID))

veteran2018 <-enroll2018[enroll2018$PersonalID %in% client.veteran$PersonalID,]
length(unique(veteran2018$PersonalID))


##############################mydata############################################################
mydata <- mydata[order(mydata$PersonalID,mydata$EntryDate),]

###################################################################################################
#e.g. 333197,334263,607965
#ExitDate
for (i in 1:(nrow(mydata)-1)){
  if ((mydata$PersonalID[i]) == (mydata$PersonalID[i+1])){
    if ((mydata$ExitDate[i+1])<(mydata$ExitDate[i])){
      mydata$ExitDate[i+1] <- mydata$ExitDate[i]
    } 
  } 
}  


# e.g. PID:452889,EID:797411 entrydate:2018-01-05 exitdate:2018-03-11
#                 EID:797779 entrydate:2018-01-17 exitdate:2018-02-11
# 452891 pic
# 333197
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
hist(loh$averagelength,main = "Distribution of Average LoH",xlab = "Average LoH")
table(loh$averagelength*3>=365)
barplot((table(loh$averagelength*3>=365)), 
        names.arg = c("Less than 1 years","greater than 1 years"),ylim = range(0,20000),
        main = "average Loh in 3 years",ylab = "the number of individuals")