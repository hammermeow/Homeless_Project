###chronic####
chronic <-loh
chronic$chronicity <-ifelse(chronic$averagelength*3>=365,1,0)
##DisType
distypechronic <- merge(chronic,Distype,by="PersonalID")
t.test(y~x) # where y is numeric and x is a binary factor
t.test(distypechronic$DisType5~distypechronic$chronicity)
t.test(distypechronic$DisType6~distypechronic$chronicity)
t.test(distypechronic$DisType7~distypechronic$chronicity)
t.test(distypechronic$DisType8~distypechronic$chronicity)
t.test(distypechronic$DisType9~distypechronic$chronicity)
distype10chronic<-distypechronic[distypechronic$chronicity==1,]
table(distype10chronic$DisType10)
table(chronic$chronicity)#4566
table(distypechronic$chronicity)#among 4566 with distype info 3865
distype10notchronic<-distypechronic[distypechronic$chronicity==0,]
table(distype10notchronic$DisType10)


educhronic <- merge(edu,chronic,by="PersonalID")
t.test(educhronic$LastGradeCompleted~educhronic$chronicity)
empchronic <- merge(emp,chronic,by="PersonalID")
t.test(empchronic$Employed~empchronic$chronicity)

dvchronic <- merge(DV,chronic,by="PersonalID")
t.test(dvchronic$DomesticViolenceVictim~dvchronic$chronicity)

ghchronic <- merge(GH,chronic,by="PersonalID")
t.test(ghchronic$GeneralHealthStatus~ghchronic$chronicity)


icchronic <- merge(Income.info,chronic,by="PersonalID")
t.test(icchronic$income1~icchronic$chronicity)




#modeling##################################################################################
###chronic####
chronic <-loh
chronic$chronicity <-ifelse(chronic$averagelength*3>=365,1,0)
##DisType
chronic1 <- merge(chronic,Distype,by="PersonalID")
chronic1 <- merge(edu,chronic1,by="PersonalID")
chronic1 <- merge(emp,chronic1,by="PersonalID")
chronic1 <- merge(DV,chronic1,by="PersonalID")
chronic1 <- merge(GH,chronic1,by="PersonalID")
chronic1 <- merge(Income.info,chronic1,by="PersonalID")
colnames(chronic1)

logisticModeltest <- glm(chronicity~income1+GeneralHealthStatus+DomesticViolenceVictim+
                           Employed+
                           LastGradeCompleted+DisType5+DisType6+DisType7+DisType8+DisType9+DisType10
                         , data = chronic1, family = "binomial")

logisticModeltest <- glm(chronicity~income1+DomesticViolenceVictim+
                           DisType5+DisType7+DisType9+DisType10
                         , data = chronic1, family = "binomial")
summary(logisticModeltest)

library(lmtest)#likelihood ratio test
modelnull <- glm(chronicity~1 , family=binomial, data=chronic1)
summary(modelnull)
lrtest (modelnull, logisticModeltest)#Chisq is test statistic,pr(>Chisq) is p-value

prob=predict(logisticModeltest,chronic1,type=c("response"))
prob
library(pROC) # calculate AUC
g <- roc(chronicity ~ prob, data = chronic1)
plot(g)
auc(g)
