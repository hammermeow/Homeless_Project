#income info
library(readr)
IncomeBenefits <- read_csv("Documents/Homeless analysis/CoC/IncomeBenefits.csv")


#income amount
colnames(IncomeBenefits)
class(IncomeBenefits$TotalMonthlyIncome)
range(IncomeBenefits$TotalMonthlyIncome,na.rm = TRUE)
sum(is.na(IncomeBenefits$IncomeFromAnySource))

incomeamounts1 <-IncomeBenefits[,c(1:36)]

incomeamounts<-incomeamounts1
incomeamounts$TotalMonthlyIncome[is.na(incomeamounts$TotalMonthlyIncome)]<-0
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
d <- incomeamounts[!incomeamounts$TotalMonthlyIncome==incomeamounts$amount,]#39541
e <- incomeamounts[incomeamounts$TotalMonthlyIncome==incomeamounts$amount,]#79450


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

d1 <- incomeamounts[!incomeamounts$TotalMonthlyIncome==incomeamounts$amount,]#38954
d2 <-incomeamounts[incomeamounts$TotalMonthlyIncome > incomeamounts$amount,]#200
d3 <-incomeamounts[incomeamounts$TotalMonthlyIncome < incomeamounts$amount,]#38754

for (i in 1:norow){
  if (incomeamounts$TotalMonthlyIncome[i] < incomeamounts$amount[i]){
    if(incomeamounts$amount[i]>0){
      incomeamounts$TotalMonthlyIncome[i]=incomeamounts$amount[i]
    }
  }
}

d4 <-incomeamounts[incomeamounts$TotalMonthlyIncome < incomeamounts$amount,]#38753


#fix discrepency between binary and amounts


withincome <-incomeamounts[incomeamounts$IncomeFromAnySource==1,]#36897
#labeld as with income, but the amount is 0
b <-withincome[withincome$TotalMonthlyIncome==0,]
b<-b[!is.na(b$TotalMonthlyIncome),]#1083

#fix amount is 0 but labeled as with income (1)/8/9/99, change the original label to 0
for (i in 1:norow) {
  if (incomeamounts$TotalMonthlyIncome[i]==0){
    incomeamounts$IncomeFromAnySource[i]=0
  }
}

withincome1 <-incomeamounts[incomeamounts$IncomeFromAnySource==1,] #36897-1083=35814
table(incomeamounts$IncomeFromAnySource)



noincome <-incomeamounts[incomeamounts$IncomeFromAnySource==0,]#64238
sum(is.na(incomeamounts$IncomeFromAnySource))
# has income but labeled as no income
a <- noincome[noincome$TotalMonthlyIncome > 0,]#115

#fix acutually have income but labeled as no income(0) or labeled as 8/9/99
for (i in 1:norow) {
  if (incomeamounts$TotalMonthlyIncome[i]>0){
    incomeamounts$IncomeFromAnySource[i]=1
  }
}

table(incomeamounts$IncomeFromAnySource)
noincome1 <-incomeamounts[incomeamounts$IncomeFromAnySource==0,] #64123=64238-115
#difference between noincome and noincome1 is 115
a1 <- noincome1[noincome1$TotalMonthlyIncome > 0,] #0

withincome2 <-incomeamounts[incomeamounts$IncomeFromAnySource==1,] #35935


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
#   0     1 
# 12466  9989 



