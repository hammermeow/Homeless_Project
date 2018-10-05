library(readr)
Client <- read_csv("A_HOMELESS/COC/Client.csv")
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
# 142 records have the same combination of PersonalID and DateUpdated 
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


#############################clientfinal#######################################
clientfinal <-rbind(notdup,needadd) #final client data without any duplicates
length(unique(clientfinal$PersonalID))


#age1
sum(is.na(clientfinal$DOB))#660
table(is.na(clientfinal$DOB))
client.age = clientfinal[!is.na(clientfinal$DOB),]
table(is.na(client.age$DOB))
client.age$age <- (as.Date(Sys.Date(),format = "%m/%d/%Y")-as.Date(client.age$DOB,
                                                                   format = "%m/%d/%Y"))/365.25
table(is.na(client.age$age))
client.age$age <-as.numeric(client.age$age)
hist(client.age$age,xlim = range(0,100),ylim =range(0,7000),xlab = "age",ylab = "the number of clients", 
     main = "Histogram of Age")

#gender
sum(is.na(clientfinal$Gender))
table(clientfinal$Gender)
client.gender <- clientfinal
attach(client.gender)
client.gender$Gender1[Gender==8] <- NA
client.gender$Gender1[Gender==9] <- NA
client.gender$Gender1[Gender==99] <- NA
client.gender$Gender1[Gender==4] <- NA
client.gender$Gender1[Gender==0] <- 0
client.gender$Gender1[Gender==1] <- 1
client.gender$Gender1[Gender==2] <- 0
client.gender$Gender1[Gender==3] <- 1
detach(client.gender)
table(client.gender$Gender1)
table(is.na(client.gender$Gender1))
client.gender <- client.gender[!is.na(client.gender$Gender1),]#27976
barplot(table(client.gender$Gender1),names.arg = c("Female","Male"))

#Veteran
table(clientfinal$VeteranStatus)
client.veteran <- clientfinal
attach(client.veteran)
client.veteran$VeteranStatus1[VeteranStatus==8] <- NA
client.veteran$VeteranStatus1[VeteranStatus==9] <- NA
client.veteran$VeteranStatus1[VeteranStatus==99] <- NA
client.veteran$VeteranStatus1[VeteranStatus==1] <- 1
client.veteran$VeteranStatus1[VeteranStatus==0] <- 0
detach(client.veteran)
table(client.veteran$VeteranStatus1)
table(is.na(client.veteran$VeteranStatus1))
client.veteran <- client.veteran[!is.na(client.veteran$VeteranStatus1),]#23870
barplot(table(client.veteran$VeteranStatus1),names.arg = c("No","Yes"))

#race
table(clientfinal$AmIndAKNative)
table(clientfinal$Asian)
table(clientfinal$BlackAfAmerican)
table(clientfinal$NativeHIOtherPacific)
table(clientfinal$White)
table(clientfinal$Ethnicity)#missing value, drop Ethnicity
barplot(height = c(281,415,10447,326,13988,5049),
        names.arg = c("AmIndAKNative","Asian","BlackAfAmerican",
                      "NativeHIOtherPacific","White","Ethnicity(Hispanic/Latino)"),
        ylim = range(0,14000))
client.ethnicity<-clientfinal
attach(client.ethnicity)
client.ethnicity$Ethnicity[Ethnicity==8] <- NA
client.ethnicity$Ethnicity[Ethnicity==9] <- NA
client.ethnicity$Ethnicity[Ethnicity==99] <- NA
client.ethnicity$Ethnicity[Ethnicity==0] <- 0
client.ethnicity$Ethnicity[Ethnicity==1] <- 1
detach(client.ethnicity)
table(client.ethnicity$Ethnicity)
table(is.na(client.ethnicity$Ethnicity))
client.ethnicity <- client.ethnicity[!is.na(client.ethnicity$Ethnicity),]#25513
barplot(table(client.ethnicity$Ethnicity),names.arg = c("No","Yes"))



##merge
age <-client.age[,c("PersonalID","age")]
gender <-client.gender[,c("PersonalID","Gender1")]
veteran <-client.veteran[,c("PersonalID","VeteranStatus1")]
ethnicity <-client.ethnicity[,c("PersonalID","Ethnicity")]

client.info <- merge(age,gender, by="PersonalID")#27666
client.info <- merge(client.info,veteran,by="PersonalID")##23556##
client.info <- merge(client.info,ethnicity,by="PersonalID")#22866
client.info <- merge(client.info,clientfinal,by="PersonalID")#22866
#######client.info######
colSums(is.na(client.info))#####Ethnicity.x 


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




#Disability
#enrollment disabiling condition
sum(is.na(Enrollment$DisablingCondition))#1365
table(Enrollment$DisablingCondition)
Enrollment <- Enrollment[Enrollment$EnrollmentID %in% mydata$EnrollmentID,]
table(Enrollment$DisablingCondition)
attach(Enrollment)
Enrollment$Disability[DisablingCondition==8] <- NA
Enrollment$Disability[DisablingCondition==9] <- NA
Enrollment$Disability[DisablingCondition==99] <- NA
Enrollment$Disability[DisablingCondition==1] <- 1
Enrollment$Disability[DisablingCondition==0] <- 0
detach(Enrollment)
sum(!is.na(Enrollment$Disability))
Enrolldf <- Enrollment[!is.na(Enrollment$Disability),]#46361

length(unique(Enrolldf$PersonalID)) #18970
Enroll.info <- Enrolldf%>%
  select(PersonalID, Disability)%>%
  group_by(PersonalID)%>%
  summarise(Disable = sum(as.numeric(Disability)))
View(Enroll.info)
Enroll.info$Disable1 <- ifelse(Enroll.info$Disable==0,0,1)
#######Enroll.info########
table(Enroll.info$Disable1)
barplot(table(Enroll.info$Disable1),names.arg = c("No","Yes"))


library(readr)
Disabilities <- read_csv("A_HOMELESS/COC/Disabilities.csv")
length(unique(Disabilities$PersonalID))
# 25157
sum(is.na(Disabilities$PersonalID))

Dcount <- Disabilities%>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts = length(PersonalID))

table(Disabilities$DisabilityType)

Disabilities <-Disabilities[Disabilities$EnrollmentID %in% mydata$EnrollmentID,]

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
length(unique(Distype$PersonalID))
colnames(Distype)
table(Distype$DisType5)
table(Distype$DisType6)
table(Distype$DisType7)
table(Distype$DisType8)
table(Distype$DisType9)
table(Distype$DisType10)

barplot(height=c(5519,2512,6118,505,8990,8845),
        names.arg = c("DisType5","DisType6","DisType7","DisType8","DisType9","DisType10"))




#education level and employment history
library(readr)
EduEmp <- read_csv("A_HOMELESS/COC/EmploymentEducation.csv")
EduEmp <- EduEmp[EduEmp$EnrollmentID %in% mydata$EnrollmentID,]
table(EduEmp$LastGradeCompleted)
attach(EduEmp)
EduEmp$LastGradeCompleted[LastGradeCompleted==8] <- NA
EduEmp$LastGradeCompleted[LastGradeCompleted==9] <- NA
EduEmp$LastGradeCompleted[LastGradeCompleted==99] <- NA
detach(EduEmp)
table(EduEmp$LastGradeCompleted)
Education <- EduEmp[!is.na(EduEmp$LastGradeCompleted),]
length(unique(Education$PersonalID))#6326


library(dplyr)
edu <-Education%>%
  select(PersonalID,LastGradeCompleted)%>%
  group_by(PersonalID)%>%
  summarise(LastGradeCompleted=max(LastGradeCompleted))
table(edu$LastGradeCompleted)
#####edu######
barplot(table(edu$LastGradeCompleted))

table(EduEmp$Employed)
attach(EduEmp)
EduEmp$Employed[Employed==8] <- NA
EduEmp$Employed[Employed==9] <- NA
EduEmp$Employed[Employed==99] <- NA
detach(EduEmp)
table(EduEmp$Employed)
Employment <- EduEmp[!is.na(EduEmp$Employed),]
length(unique(Employment$PersonalID))#10784

library(dplyr)
emp <-Employment%>%
  select(PersonalID,Employed)%>%
  group_by(PersonalID)%>%
  summarise(Employed=max(Employed))
table(emp$Employed)
#####emp####
barplot(table(emp$Employed),names.arg = c("No","Yes"),ylim = range(0,10000))






library(readr)
HealthAndDV <- read_csv("A_HOMELESS/COC/HealthAndDV.csv")
colSums(is.na(HealthAndDV))
table(HealthAndDV$PregnancyStatus)
table(HealthAndDV$DomesticViolenceVictim)
length(unique(HealthAndDV$PersonalID))
table(HealthAndDV$GeneralHealthStatus)  
HealthAndDV <- HealthAndDV[HealthAndDV$EnrollmentID %in% mydata$EnrollmentID,]

table(HealthAndDV$DomesticViolenceVictim)
attach(HealthAndDV)
HealthAndDV$DomesticViolenceVictim[DomesticViolenceVictim==8] <- NA
HealthAndDV$DomesticViolenceVictim[DomesticViolenceVictim==9] <- NA
HealthAndDV$DomesticViolenceVictim[DomesticViolenceVictim==99] <- NA
detach(HealthAndDV)
table(HealthAndDV$DomesticViolenceVictim)
DVdf <- HealthAndDV[!is.na(HealthAndDV$DomesticViolenceVictim),]
length(unique(DVdf$PersonalID))#19194

library(dplyr)
DV <-DVdf%>%
  select(PersonalID,DomesticViolenceVictim)%>%
  group_by(PersonalID)%>%
  summarise(DomesticViolenceVictim=max(DomesticViolenceVictim))
table(DV$DomesticViolenceVictim)
#####DV######
barplot(table(DV$DomesticViolenceVictim),names.arg = c("No","Yes"),ylim = range(0,20000),
        main = "DV")


table(HealthAndDV$GeneralHealthStatus) 
attach(HealthAndDV)
HealthAndDV$GeneralHealthStatus[GeneralHealthStatus==8] <- NA
HealthAndDV$GeneralHealthStatus[GeneralHealthStatus==9] <- NA
HealthAndDV$GeneralHealthStatus[GeneralHealthStatus==99] <- NA
detach(HealthAndDV)
table(HealthAndDV$GeneralHealthStatus)
GHdf <- HealthAndDV[!is.na(HealthAndDV$GeneralHealthStatus),]
length(unique(GHdf$PersonalID))#11532

library(dplyr)
GH <-GHdf%>%
  select(PersonalID,GeneralHealthStatus)%>%
  group_by(PersonalID)%>%
  summarise(GeneralHealthStatus=mean(GeneralHealthStatus))
hist(GH$GeneralHealthStatus,breaks = 5,xlab = "General Health Status", 
     main = "General Health")
####GH#####






#income info
library(readr)
IncomeBenefits <- read_csv("A_HOMELESS/COC/IncomeBenefits.csv")


#income amount
colnames(IncomeBenefits)
class(IncomeBenefits$TotalMonthlyIncome)
range(IncomeBenefits$TotalMonthlyIncome,na.rm = TRUE)
sum(is.na(IncomeBenefits$IncomeFromAnySource))

incomeamounts1 <-IncomeBenefits[,c(1:36)]#118991
incomeamounts1 <- incomeamounts1[incomeamounts1$EnrollmentID %in% mydata$EnrollmentID,]#109227

incomeamounts<-incomeamounts1
incomeamounts$TotalMonthlyIncome[is.na(incomeamounts$TotalMonthlyIncome)]<-0
range(incomeamounts$TotalMonthlyIncome,na.rm = TRUE)
incomeamounts$PersonalID[incomeamounts$TotalMonthlyIncome==290683]

incomeamounts[is.na(incomeamounts)]<-0
class(incomeamounts$VADisabilityNonServiceAmount)
incomeamounts$VADisabilityNonServiceAmount<-as.numeric(incomeamounts$VADisabilityNonServiceAmount)
class(incomeamounts$PrivateDisabilityAmount)
incomeamounts$PrivateDisabilityAmount <-as.numeric(incomeamounts$PrivateDisabilityAmount)
class(incomeamounts$WorkersCompAmount)
incomeamounts$WorkersCompAmount <-as.numeric(incomeamounts$WorkersCompAmount)
class(incomeamounts$AlimonyAmount)
incomeamounts$AlimonyAmount <- as.numeric(incomeamounts$AlimonyAmount)

incomeamounts$amount <- incomeamounts$EarnedAmount+incomeamounts$UnemploymentAmount+incomeamounts$SSIAmount+
  incomeamounts$SSDIAmount+incomeamounts$VADisabilityServiceAmount+incomeamounts$VADisabilityNonServiceAmount+
  incomeamounts$PrivateDisabilityAmount+incomeamounts$WorkersCompAmount+incomeamounts$TANFAmount+
  incomeamounts$GAAmount+incomeamounts$SocSecRetirementAmount+incomeamounts$PensionAmount+
  incomeamounts$ChildSupportAmount+incomeamounts$AlimonyAmount+incomeamounts$OtherIncomeAmount

incomeamounts <-incomeamounts[,37]

incomeamounts <-cbind(incomeamounts1,incomeamounts)
table(incomeamounts$IncomeFromAnySource)
incomeamounts <- incomeamounts[,c(1:6,37)]


#-100 as missing value
incomeamounts$TotalMonthlyIncome[is.na(incomeamounts$TotalMonthlyIncome)]<--100
#dealing with missing value in TMI
df<-incomeamounts[incomeamounts$TotalMonthlyIncome==-100,]
df <- df[df$amount>0,]#585 has value in calculated income amount but missing in TMI

df1<-incomeamounts[incomeamounts$TotalMonthlyIncome==0,]
df1 <- df1[df1$amount>0,]#2 has cal amount but original amount is 0

#original amount is not equal to calculated amount
d <- incomeamounts[!incomeamounts$TotalMonthlyIncome==incomeamounts$amount,]#33067
e <- incomeamounts[incomeamounts$TotalMonthlyIncome==incomeamounts$amount,]#76160


#fix missing values in TotalMonthlyIncome
norow<-nrow(incomeamounts)
for (i in 1:norow){
  if (incomeamounts$TotalMonthlyIncome[i]==-100){
    if(incomeamounts$amount[i]>0){
      incomeamounts$TotalMonthlyIncome[i]=incomeamounts$amount[i]
    }
  }
}
# IBID 206023 973065

for (i in 1:norow){
  if (incomeamounts$TotalMonthlyIncome[i]==0){
    if(incomeamounts$amount[i]>0){
      incomeamounts$TotalMonthlyIncome[i]=incomeamounts$amount[i]
    }
  }
}
# IBID 60472 777242

d1 <- incomeamounts[!incomeamounts$TotalMonthlyIncome==incomeamounts$amount,]#32480
d2 <-incomeamounts[incomeamounts$TotalMonthlyIncome > incomeamounts$amount,]#197
d3 <-incomeamounts[incomeamounts$TotalMonthlyIncome < incomeamounts$amount,]#32283

for (i in 1:norow){
  if (incomeamounts$TotalMonthlyIncome[i] < incomeamounts$amount[i]){
    if(incomeamounts$amount[i]>0){
      incomeamounts$TotalMonthlyIncome[i]=incomeamounts$amount[i]
    }
  }
}

d4 <-incomeamounts[incomeamounts$TotalMonthlyIncome < incomeamounts$amount,]#32282





#fix discrepency between binary and amounts
withincome <-incomeamounts[incomeamounts$IncomeFromAnySource==1,]#34170
#labeld as with income, but the amount is 0
b <-withincome[withincome$TotalMonthlyIncome==0,]


#fix amount is 0 but labeled as with income (1)/8/9/99, change the original label to 0
for (i in 1:norow) {
  if (incomeamounts$TotalMonthlyIncome[i]==0){
    incomeamounts$IncomeFromAnySource[i]=0
  }
}

withincome1 <-incomeamounts[incomeamounts$IncomeFromAnySource==1,] #33145
table(incomeamounts$IncomeFromAnySource)



noincome <-incomeamounts[incomeamounts$IncomeFromAnySource==0,]#59598

# has income but labeled as no income
a <- noincome[noincome$TotalMonthlyIncome > 0,]#113

#fix acutually have income but labeled as no income(0) or labeled as 8/9/99
for (i in 1:norow) {
  if (incomeamounts$TotalMonthlyIncome[i]>0){
    incomeamounts$IncomeFromAnySource[i]=1
  }
}

table(incomeamounts$IncomeFromAnySource)
noincome1 <-incomeamounts[incomeamounts$IncomeFromAnySource==0,] #59485=59598-113
#difference between noincome and noincome1 is 113
a1 <- noincome1[noincome1$TotalMonthlyIncome > 0,] #0

withincome2 <-incomeamounts[incomeamounts$IncomeFromAnySource==1,] #33263


attach(incomeamounts)
incomeamounts$INCOME[IncomeFromAnySource==8] <- NA
incomeamounts$INCOME[IncomeFromAnySource==9] <- NA
incomeamounts$INCOME[IncomeFromAnySource==99] <- NA
incomeamounts$INCOME[IncomeFromAnySource==1] <- 1
incomeamounts$INCOME[IncomeFromAnySource==0] <- 0
detach(incomeamounts)
sum(!is.na(incomeamounts$INCOME))
Incomedf <- incomeamounts[!is.na(incomeamounts$INCOME),]
length(unique(Incomedf$PersonalID)) 

library(dplyr)
Income.info <- Incomedf%>%
  select(PersonalID, INCOME)%>%
  group_by(PersonalID)%>%
  summarise(income = sum(as.numeric(INCOME)))
View(Income.info)
Income.info$income1 <- ifelse(Income.info$income==0,0,1)

table(Income.info$income1)
#    0     1 
# 10704  8862
barplot(table(Income.info$income1),names.arg = c("No","Yes"),
        main = "income",ylim = range(0,15000))


attach(incomeamounts)
incomeamounts$TotalMonthlyIncome[TotalMonthlyIncome==-100] <- NA
detach(incomeamounts)
sum(!is.na(incomeamounts$TotalMonthlyIncome))
Incomeamountdf <- incomeamounts[!is.na(incomeamounts$TotalMonthlyIncome),]
length(unique(Incomeamountdf$PersonalID)) 

#check 10878
library(dplyr)
Incomeamount.info <- Incomeamountdf%>%
  select(EnrollmentID, PersonalID,TotalMonthlyIncome)%>%
  group_by(EnrollmentID)%>%
  summarise(TotalMonthlyIncome = max(as.numeric(TotalMonthlyIncome)),PersonalID=min(PersonalID))
View(Incomeamount.info)

Incomeamount.info <- Incomeamount.info%>%
  select(PersonalID, TotalMonthlyIncome)%>%
  group_by(PersonalID)%>%
  summarise(TotalMonthlyIncome = mean(as.numeric(TotalMonthlyIncome)))
View(Incomeamount.info)

reasonableic<- Incomeamount.info[Incomeamount.info$TotalMonthlyIncome<5000,]
hist(reasonableic$TotalMonthlyIncome)



###chronic####
chronic <-loh
chronic$chronicity <-ifelse(chronic$averagelength*3>=365,1,0)


###test####
#demographic & LoH
testage <- merge(age,chronic, by="PersonalID")

colSums(is.na(testage))
plot(testage$age,testage$averagelength)
cor(testage$age,testage$averagelength) #the correlation is very weak
cor.test(testage$age,testage$averagelength)

library("ggplot2")
ggplot(testage,
       aes(x= testage$age,y=testage$averagelength))+geom_smooth()+geom_point()

testgender <- merge(gender,chronic, by="PersonalID")
cor.test(testgender$Gender1,testgender$averagelength)

testveteran <-merge(veteran,chronic, by="PersonalID")
cor.test(testveteran$VeteranStatus1,testveteran$averagelength)


# independent samples test 
# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor
t.test(testage$age~testage$chronicity) #mean ages for chronicity or not are significantly different.
t.test(testgender$Gender1~testgender$chronicity)# frequency of gender is s~ different
t.test(testveteran$VeteranStatus1~testveteran$chronicity) #veteran is s~ different

testdisable <- merge(Enroll.info,chronic, by="PersonalID")
t.test(testdisable$Disable1~testdisable$chronicity)
t.test(chronic$averagelength~chronic$chronicity)


#association test
cor.test(testage$chronicity,testage$age)
chisq.test(testgender$chronicity,testgender$Gender1)
chisq.test(testveteran$chronicity,testveteran$VeteranStatus1)
chisq.test(testdisable$chronicity,testdisable$Disable1)
cor.test(testdisable$chronicity,testdisable$Disable1)

testclient<-merge(client.info, chronic,by="PersonalID")
str(testclient)
testclient.1 <-testclient[,c(2:5,9:13,38)]
matrix1 <-model.matrix(~.,data=testclient.1)[,-1]
library('corrplot')
corrplot(cor(matrix1),method = "circle")


logisticModeltest <- glm(chronicity~age+Gender1+VeteranStatus1+
                           AmIndAKNative+Asian+
                           BlackAfAmerican+NativeHIOtherPacific+White+Ethnicity.x
                         , data = testclient, family = "binomial")

logisticModeltest <- glm(chronicity~age+VeteranStatus1+BlackAfAmerican
                         , data = testclient, family = "binomial")
summary(logisticModeltest)

library(lmtest)#likelihood ratio test
modelnull <- glm(chronicity~1 , family=binomial, data=testclient)
summary(modelnull)
lrtest (modelnull, logisticModeltest)#Chisq is test statistic,pr(>Chisq) is p-value

prob=predict(logisticModeltest,testclient,type=c("response"))
prob
library(pROC) # calculate AUC
g <- roc(chronicity ~ prob, data = testclient)
plot(g)
auc(g)

