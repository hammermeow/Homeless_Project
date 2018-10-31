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
table(client.veteran$Gender)
table(client.veteran$AmIndAKNative)
table(client.veteran$Asian)
table(client.veteran$BlackAfAmerican)
table(client.veteran$NativeHIOtherPacific)
table(client.veteran$White)

client.veteran$noraceresponse <-client.veteran$AmIndAKNative+client.veteran$Asian+
  client.veteran$BlackAfAmerican+client.veteran$NativeHIOtherPacific+
  client.veteran$White
table(client.veteran$noraceresponse)


table(client.veteran$Ethnicity) #8,9,99 ---3167
attach(client.veteran)
client.veteran$Ethnicity[Ethnicity==8] <- NA
client.veteran$Ethnicity[Ethnicity==9] <- NA
client.veteran$Ethnicity[Ethnicity==99] <- NA
detach(client.veteran)
table(client.veteran$Ethnicity)
table(is.na(client.veteran$Ethnicity))

##############################Age at the most recent enrollment#############################
library(readr)
Enrollment <- read_csv("A_HOMELESS/COC/Enrollment.csv")
enrollment<-Enrollment[Enrollment$PersonalID %in% client.veteran$PersonalID,]
length(unique(enrollment$PersonalID)) 
enrolldate <- enrollment %>%
  select(PersonalID,EntryDate) %>%
  group_by(PersonalID) %>%
  summarise(date=max(EntryDate))
agerecentenroll <-merge(enrolldate,client.veteran,by="PersonalID")
agerecentenroll$age <- (as.Date(agerecentenroll$date,format = "%m/%d/%Y")-as.Date(agerecentenroll$DOB,
                                                                                  format = "%m/%d/%Y"))/365.25

table(is.na(agerecentenroll$age))
agerecentenroll <-agerecentenroll[!is.na(agerecentenroll$age),]
agerecentenroll$age<-as.numeric(agerecentenroll$age)
range(agerecentenroll$age)
length(agerecentenroll$age[agerecentenroll$age<18])
length(agerecentenroll$age[agerecentenroll$age<24 &agerecentenroll$age>18])
length(agerecentenroll$age[agerecentenroll$age>24])

# enrollment
sum(is.na(enrollment$DisablingCondition))
table(enrollment$DisablingCondition)
attach(enrollment)
enrollment$DisablingCondition[DisablingCondition==8] <- NA
enrollment$DisablingCondition[DisablingCondition==9] <- NA
enrollment$DisablingCondition[DisablingCondition==99] <- NA
detach(enrollment)
sum(is.na(enrollment$DisablingCondition))
sum(!is.na(enrollment$DisablingCondition))
DisablingCondition <- enrollment[!is.na(enrollment$DisablingCondition),]

length(unique(DisablingCondition$PersonalID)) 
DisablingCondition.info <- DisablingCondition%>%
  select(PersonalID, DisablingCondition)%>%
  group_by(PersonalID)%>%
  summarise(Disable = max(as.numeric(DisablingCondition)))
View(DisablingCondition.info)
table(DisablingCondition.info$Disable)

#################### disability type #########################################################
library(readr)
Disabilities <- read_csv("A_HOMELESS/COC/Disabilities.csv")
Disabilities <-Disabilities[Disabilities$EnrollmentID %in% enrollment$EnrollmentID,]
Disabilities <-Disabilities[Disabilities$PersonalID %in% client.veteran$PersonalID,]
length(unique(Disabilities$PersonalID))

Dcount <- Disabilities%>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts = length(PersonalID))

attach(Disabilities)
Disabilities$DisabilityResponse[DisabilityResponse==8] <- NA
Disabilities$DisabilityResponse[DisabilityResponse==9] <- NA
Disabilities$DisabilityResponse[DisabilityResponse==99] <- NA
detach(Disabilities)

sum(!is.na(Disabilities$DisabilityResponse))
Disabilities <- Disabilities[!is.na(Disabilities$DisabilityResponse),] 
length(unique(Disabilities$PersonalID))
table(Disabilities$DisabilityResponse)

#Type5
Distype5 <- Disabilities[Disabilities$DisabilityType==5,]
table(Distype5$DisabilityResponse)

Distype5 <- Distype5%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType5=max(DisabilityResponse))

table(Distype5$DisType5)

#Type6
Distype6 <- Disabilities[Disabilities$DisabilityType==6,]
table(Distype6$DisabilityResponse)

Distype6 <- Distype6%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType6=max(DisabilityResponse))

table(Distype6$DisType6)

#Type7
Distype7 <- Disabilities[Disabilities$DisabilityType==7,]
table(Distype7$DisabilityResponse)

Distype7 <- Distype7%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType7=max(DisabilityResponse))

table(Distype7$DisType7)

#Type8
Distype8 <- Disabilities[Disabilities$DisabilityType==8,]
table(Distype8$DisabilityResponse)

Distype8 <- Distype8%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType8=max(DisabilityResponse))

table(Distype8$DisType8)

#Type9
Distype9 <- Disabilities[Disabilities$DisabilityType==9,]
table(Distype9$DisabilityResponse)

Distype9 <- Distype9%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType9=max(DisabilityResponse))

table(Distype9$DisType9)

#Type10
Distype10 <- Disabilities[Disabilities$DisabilityType==10,]
table(Distype10$DisabilityResponse)

Distype10 <- Distype10%>%
  select(PersonalID,DisabilityResponse)%>%
  group_by(PersonalID)%>%
  summarise(DisType10=max(DisabilityResponse))

table(Distype10$DisType10)
##############################################################################################
veteranincome<- Income.info[Income.info$PersonalID %in% client.veteran$PersonalID,]
table(veteranincome$income1)



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

##############################mydata######################################################
mydata <- mydata[order(mydata$PersonalID,mydata$EntryDate),]


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
loh.veteran <- loh[loh$PersonalID %in% client.veteran$PersonalID,]
loh.veteran$averagelength <-as.numeric(loh.veteran$averagelength)
range(loh.veteran$averagelength)
mean(loh.veteran$averagelength)
hist(loh.veteran$averagelength,main = "Distribution of Average LoH for Veterans",xlab = "Average LoH for veterans")
