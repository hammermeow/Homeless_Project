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



##############################gender#############################
sum(is.na(clientfinal$Gender))
table(clientfinal$Gender)
client.gender <- clientfinal
attach(client.gender)
client.gender$Gender[Gender==8] <- NA
client.gender$Gender[Gender==9] <- NA
client.gender$Gender[Gender==99] <- NA
detach(client.gender)
table(client.gender$Gender)
prop.table(table(client.gender$Gender)) 
table(is.na(client.gender$Gender))


##############################race#############################
colSums(is.na(clientfinal))
client.race <- clientfinal

table(client.race$AmIndAKNative)
table(client.race$Asian)
table(client.race$BlackAfAmerican)
table(client.race$NativeHIOtherPacific)
table(client.race$White)
table(client.race$RaceNone)# 8,9,99?
barplot(height = c(281,415,10447,326,13988),
        names.arg = c("AmIndAKNative","Asian","BlackAfAmerican",
                      "NativeHIOtherPacific","White"),
        ylim = range(0,14000))

281+415+10447+326+13988 #25457

client.race$noraceresponse <-client.race$AmIndAKNative+client.race$Asian+
        client.race$BlackAfAmerican+client.race$NativeHIOtherPacific+
        client.race$White
table(client.race$noraceresponse)

#   0     1     2     3     5 
#3466 24984   219    10     1 

24984+ 219*2 + 10*3 +  1*5 #25457

##############################Ethnicity#############################
table(client.race$Ethnicity) #8,9,99 ---3167
attach(client.race)
client.race$Ethnicity[Ethnicity==8] <- NA
client.race$Ethnicity[Ethnicity==9] <- NA
client.race$Ethnicity[Ethnicity==99] <- NA
detach(client.race)
table(client.race$Ethnicity)
table(is.na(client.race$Ethnicity))

##############################Age at the most recent enrollment#############################
library(readr)
Enrollment <- read_csv("A_HOMELESS/COC/Enrollment.csv")
enrollment<-Enrollment[Enrollment$PersonalID %in% clientfinal$PersonalID,]
length(unique(enrollment$PersonalID)) #28678
enrolldate <- enrollment %>%
  select(PersonalID,EntryDate) %>%
  group_by(PersonalID) %>%
  summarise(date=max(EntryDate))
agerecentenroll <-merge(enrolldate,clientfinal,by="PersonalID")
agerecentenroll$age <- (as.Date(agerecentenroll$date,format = "%m/%d/%Y")-as.Date(agerecentenroll$DOB,
                                                                      format = "%m/%d/%Y"))/365.25

table(is.na(agerecentenroll$age))
agerecentenroll <-agerecentenroll[!is.na(agerecentenroll$age),]
agerecentenroll$age<-as.numeric(agerecentenroll$age)
range(agerecentenroll$age)
length(agerecentenroll$age[agerecentenroll$age<18])
length(agerecentenroll$age[agerecentenroll$age<24 &agerecentenroll$age>18])
length(agerecentenroll$age[agerecentenroll$age>24])

#################### disabiling condition##################################

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
Disabilities <-Disabilities[Disabilities$PersonalID %in% clientfinal$PersonalID,]
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

library(readr)
HealthAndDV <- read_csv("A_HOMELESS/COC/HealthAndDV.csv")
HealthAndDV <-HealthAndDV[HealthAndDV$EnrollmentID %in% enrollment$EnrollmentID,]
HealthAndDV <-HealthAndDV[HealthAndDV$PersonalID %in% clientfinal$PersonalID,]

table(HealthAndDV$DomesticViolenceVictim)
attach(HealthAndDV)
HealthAndDV$DomesticViolenceVictim[DomesticViolenceVictim==8] <- NA
HealthAndDV$DomesticViolenceVictim[DomesticViolenceVictim==9] <- NA
HealthAndDV$DomesticViolenceVictim[DomesticViolenceVictim==99] <- NA
detach(HealthAndDV)
table(HealthAndDV$DomesticViolenceVictim)
DVdf <- HealthAndDV[!is.na(HealthAndDV$DomesticViolenceVictim),]
length(unique(DVdf$PersonalID))

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
length(unique(GHdf$PersonalID))

library(dplyr)
GH <-GHdf%>%
  select(PersonalID,GeneralHealthStatus)%>%
  group_by(PersonalID)%>%
  summarise(GeneralHealthStatus=max(GeneralHealthStatus))

####GH#####
table(GH$GeneralHealthStatus)
#freq <-hist(GH$GeneralHealthStatus,breaks = 5,xlab = "General Health Status", ylim = range(0,6000),
#     main = "General Health")
#freq


############################################
#education level and employment history
library(readr)
EduEmp <- read_csv("A_HOMELESS/COC/EmploymentEducation.csv")
EduEmp <-EduEmp[EduEmp$EnrollmentID %in% enrollment$EnrollmentID,]
EduEmp <-EduEmp[EduEmp$PersonalID %in% clientfinal$PersonalID,]
table(EduEmp$LastGradeCompleted)
attach(EduEmp)
EduEmp$LastGradeCompleted[LastGradeCompleted==8] <- NA
EduEmp$LastGradeCompleted[LastGradeCompleted==9] <- NA
EduEmp$LastGradeCompleted[LastGradeCompleted==99] <- NA
detach(EduEmp)
table(EduEmp$LastGradeCompleted)
Education <- EduEmp[!is.na(EduEmp$LastGradeCompleted),]
length(unique(Education$PersonalID))

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
length(unique(Employment$PersonalID))

library(dplyr)
emp <-Employment%>%
  select(PersonalID,Employed)%>%
  group_by(PersonalID)%>%
  summarise(Employed=max(Employed))
table(emp$Employed)
#####emp####
barplot(table(emp$Employed),names.arg = c("No","Yes"),ylim = range(0,10000))

###############################################################################################
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
# 12466  9989
barplot(table(Income.info$income1),names.arg = c("No","Yes"),
        main = "income",ylim = range(0,15000))
##############################################################################################