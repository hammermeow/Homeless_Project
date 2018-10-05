library(readr)
EduEmp <- read_csv("Documents/Homeless analysis/CoC/EmploymentEducation.csv")
table(EduEmp$LastGradeCompleted)
attach(EduEmp)
EduEmp$LastGradeCompleted[LastGradeCompleted==8] <- NA
EduEmp$LastGradeCompleted[LastGradeCompleted==9] <- NA
EduEmp$LastGradeCompleted[LastGradeCompleted==99] <- NA
detach(EduEmp)
table(EduEmp$LastGradeCompleted)
Education <- EduEmp[!is.na(EduEmp$LastGradeCompleted),]


table(EduEmp$Employed)
attach(EduEmp)
EduEmp$Employed[Employed==8] <- NA
EduEmp$Employed[Employed==9] <- NA
EduEmp$Employed[Employed==99] <- NA
detach(EduEmp)
table(EduEmp$Employed)
Employment <- EduEmp[!is.na(EduEmp$Employed),]

EducationEmployment <- Education[!is.na(Education$Employed),]
