library(readr)
Enrollment <- read_csv("A_HOMELESS/COC/Enrollment.csv")
View(Enrollment)

length(unique(Enrollment$EnrollmentID))#no duplicates in terms of EnrollmentID
colSums(is.na(Enrollment))

sum(is.na(Enrollment$DisablingCondition))
table(Enrollment$DisablingCondition)

Enrollment$DisablingCondition[Enrollment$DisablingCondition==99] <- NA

sum(is.na(Enrollment$DisablingCondition)) #20382/74564
Diswvalue <-Enrollment[!is.na(Enrollment$DisablingCondition),]



library(readr)
EmploymentEducation <- read_csv("A_HOMELESS/COC/EmploymentEducation.csv")
View(EmploymentEducation)
length(unique(EmploymentEducation$EnrollmentID))#65788

sum(is.na(EmploymentEducation$Employed))
table(EmploymentEducation$Employed)
EmploymentEducation$Employed[EmploymentEducation$Employed==99] <- NA
sum(is.na(EmploymentEducation$Employed))

EmploymentEducation$Employed[is.na(EmploymentEducation$Employed)]<--100 #-100 means missing

library(dplyr)
emp <-EmploymentEducation%>%
  select(EnrollmentID,Employed)%>%
  group_by(EnrollmentID)%>%
  summarise(Employed=max(Employed))
table(emp$Employed)
length(unique(emp$EnrollmentID))#65788 no duplicates

sum(is.na(EmploymentEducation$NotEmployedReason))
table(EmploymentEducation$NotEmployedReason)
EmploymentEducation$NotEmployedReason[is.na(EmploymentEducation$NotEmployedReason)]<--100 #-100 means missing

library(dplyr)
empreason <-EmploymentEducation%>%
  select(EnrollmentID,NotEmployedReason)%>%
  group_by(EnrollmentID)%>%
  summarise(NotEmployedReason=max(NotEmployedReason))
table(empreason$NotEmployedReason)
length(unique(empreason$EnrollmentID))#65788 no duplicates



#in Diswvalue how many clients without employee info?
DiswvalueEm <- merge(Diswvalue, emp ,by="EnrollmentID" )
sum(is.na(DiswvalueEm$DisablingCondition)) #no missing in disableconditioning
table(DiswvalueEm$Employed)#28461 missing in employed
#28461/53696, 0.5300395 have no employed info.

#in Diswvalue how many clients without notemloyed reason info?
DiswvalueRe <- merge(Diswvalue, empreason ,by="EnrollmentID" )
sum(is.na(DiswvalueRe$DisablingCondition)) #no missing in disableconditioning
table(DiswvalueRe$NotEmployedReason)#31200 missing in notemployedreason
#31200/53696, 0.5810489 have no notemployedreason info.