DVgender <- DV[DV$DomesticViolenceVictim==1,]#3092
DVgender <- merge(DVgender,gender,by="PersonalID")#3082
table(DVgender$Gender1)
table(DV$DomesticViolenceVictim)
