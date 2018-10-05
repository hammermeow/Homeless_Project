library(dplyr)
counts <-Client %>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts=length(PersonalID))

counts1 <-Enrollment %>%
  select(PersonalID)%>%
  group_by(PersonalID)%>%
  summarise(counts=length(PersonalID))

counts2 <-mydata %>%
  select(EnrollmentID)%>%
  group_by(EnrollmentID)%>%
  summarise(counts=length(EnrollmentID))
