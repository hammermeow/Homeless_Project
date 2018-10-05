#clientfinal
#mydata
library(dplyr)
exitdate_for_age <-mydata%>%
  select(PersonalID,ExitDate)%>%
  group_by(PersonalID)%>%
  summarise(latestexit=max(ExitDate))

age <- merge(clientfinal,exitdate_for_age, by="PersonalID")

sum(is.na(age$DOB))
table(is.na(age$DOB))
age = age[!is.na(age$DOB),]
table(is.na(age$DOB))
age$ageatexit <- (as.Date(age$latestexit,format = "%m/%d/%Y")-as.Date(age$DOB,
                                                                   format = "%m/%d/%Y"))/365.25
table(is.na(age$ageatexit))
age$ageatexit <-as.numeric(age$ageatexit)
range(age$ageatexit) #date of birth issues need to be discuss

hist(age$ageatexit,xlim = range(0,120),ylim =range(0,7000),xlab = "age",ylab = "the number of clients", 
     main = "Histogram of Age at Exit")
