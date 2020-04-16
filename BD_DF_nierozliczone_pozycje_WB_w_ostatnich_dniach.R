nierozliczone_pozycje_WB_w_ostatnich_dniach_WB<-function(iLe=2)
  
{
  partnerzy_o<-partnerzy
  colnames(partnerzy_o)[1]<-"Konto"
  colnames(partnerzy_o)[2]<-"Nazwa"
  partnerzy_o$Konto<-as.character(partnerzy_o$Konto)
  
  nierozliczone_WB<-subset(zobowiazania,Rodzaj=="WB" & is.na(Rozlicz.))
  colnames(nierozliczone_WB)[20]<-"EnterDate"
  nierozliczone_ostatnie<-subset(nierozliczone_WB,EnterDate>=today()-days(iLe))
  nierozliczone_ostatnie2<-left_join(nierozliczone_ostatnie,partnerzy_o,by="Konto")
  Late_Lia2<-nierozliczone_ostatnie2[,c(5,22,1,3,6,9,13,14,15,16,20)]
  try(Late_Lia2<-Late_Lia2[order(Late_Lia2$Konto),])
  
  Late_Lia2$Opis<-strtrim(Late_Lia2$Opis,7)
  Late_Lia2$Nazwa<-strtrim(Late_Lia2$Nazwa,12)
  
  return(kable(Late_Lia2))
  
}


nierozliczone_pozycje_WB_w_ostatnich_dniach_WB(5)
