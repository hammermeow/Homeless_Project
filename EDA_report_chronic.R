
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
table(loh$averagelength>=365/3)
chronic<-loh[loh$averagelength>=365/3,]
range(chronic$averagelength)
mean(chronic$averagelength)
hist(chronic$averagelength,main = "Distribution of Average LoH for Chronically homeless individuals",xlab = "Average LoH",
     ylim = c(0,1000),xlim = c(100,400))


client.chronic <- clientfinal[clientfinal$PersonalID %in% chronic$PersonalID, ]
###############################################################################

##############################gender#############################

attach(client.chronic)
client.chronic$Gender[Gender==8] <- NA
client.chronic$Gender[Gender==9] <- NA
client.chronic$Gender[Gender==99] <- NA
detach(client.chronic)
table(client.chronic$Gender)
prop.table(table(client.chronic$Gender)) 
table(is.na(client.chronic$Gender))

##############################race#############################
table(client.chronic$AmIndAKNative)
table(client.chronic$Asian)
table(client.chronic$BlackAfAmerican)
table(client.chronic$NativeHIOtherPacific)
table(client.chronic$White)
client.chronic$noraceresponse <-client.chronic$AmIndAKNative+client.chronic$Asian+
  client.chronic$BlackAfAmerican+client.chronic$NativeHIOtherPacific+
  client.chronic$White
table(client.chronic$noraceresponse)

##############################Ethnicity#############################
table(client.chronic$Ethnicity) #8,9,99 ---3167
attach(client.chronic)
client.chronic$Ethnicity[Ethnicity==8] <- NA
client.chronic$Ethnicity[Ethnicity==9] <- NA
client.chronic$Ethnicity[Ethnicity==99] <- NA
detach(client.chronic)
table(client.chronic$Ethnicity)
table(is.na(client.chronic$Ethnicity))

##############################Age at the most recent enrollment#############################
library(readr)
Enrollment <- read_csv("A_HOMELESS/COC/Enrollment.csv")
enrollment<-Enrollment[Enrollment$PersonalID %in% client.chronic$PersonalID,]
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
Disabilities <-Disabilities[Disabilities$PersonalID %in% client.chronic$PersonalID,]
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
chronicincome<- Income.info[Income.info$PersonalID %in% client.chronic$PersonalID,]
table(chronicincome$income1)

##############################################################################################
library(readr)
HealthAndDV <- read_csv("A_HOMELESS/COC/HealthAndDV.csv")
HealthAndDV <-HealthAndDV[HealthAndDV$EnrollmentID %in% enrollment$EnrollmentID,]
HealthAndDV <-HealthAndDV[HealthAndDV$PersonalID %in% client.chronic$PersonalID,]

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