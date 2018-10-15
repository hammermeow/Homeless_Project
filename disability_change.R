library(readr)
Disabilities <- read_csv("A_HOMELESS/COC/Disabilities.csv")
View(Disabilities)
dis<-Disabilities[Disabilities$DataCollectionStage==1,]
