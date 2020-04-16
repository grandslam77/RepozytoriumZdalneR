literki<-function(){
zobowiazaniaEPLN<-zobowiazania
#attach(zobowiazania)
zobowiazaniaEPLN<-subset(zobowiazania,is.na(Rozlicz.))
zobowiazaniaEPLN<-subset(zobowiazaniaEPLN,Wal.=="PLN")
zobowiazaniaEPLN<-subset(zobowiazaniaEPLN,Rodzaj!="BO")
zobowiazaniaEPLN<-zobowiazaniaEPLN[,c(1,2,3,5,6,9,10,11,12,13,14,15,16,17,21)]

zobowiazaniaEPLN<-subset(zobowiazaniaEPLN,is.na(FP) | is.na(TypB))
zobowiazaniaEPLN<-subset(zobowiazaniaEPLN,Rodzaj!="CE"& Rodzaj!="PW" & Rodzaj!="PX" & Rodzaj!="SA" & Rodzaj!="KA" & Rodzaj!="PK")

zobowiazaniaEPLN<-zobowiazaniaEPLN[order(zobowiazaniaEPLN$Konto),]
zobowiazaniaEPLN$Konto<-as.numeric(zobowiazaniaEPLN$Konto)
sesta<-left_join(zobowiazaniaEPLN,partnerzy,by="Konto")
write.csv2(sesta,file = "~/sesta.csv")

View(sesta)


}

