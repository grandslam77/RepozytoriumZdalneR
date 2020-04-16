literkiEUR<-function(){
zobowiazaniaE<-zobowiazania
#attach(zobowiazania)
zobowiazaniaE<-subset(zobowiazania,is.na(Rozlicz.))
zobowiazaniaE<-subset(zobowiazaniaE,Wal.!="PLN")
zobowiazaniaE<-subset(zobowiazaniaE,Rodzaj!="BO")
zobowiazaniaE<-zobowiazaniaE[,c(1,2,3,5,6,9,10,11,12,13,14,15,16,17,21)]

zobowiazaniaE<-subset(zobowiazaniaE,is.na(FP) | is.na(TypB))
zobowiazaniaE<-subset(zobowiazaniaE,Rodzaj!="CE"& Rodzaj!="PW" & Rodzaj!="PX" & Rodzaj!="SA" & Rodzaj!="KA" & Rodzaj!="PK")

zobowiazaniaE<-zobowiazaniaE[order(zobowiazaniaE$Konto),]
zobowiazaniaE$Konto<-as.numeric(zobowiazaniaE$Konto)
sesta<-left_join(zobowiazaniaE,partnerzy,by="Konto")
write.csv2(sesta,file = "~/sesta.csv")

View(sesta)
}

