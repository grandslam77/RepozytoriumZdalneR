
blokady_W<-function()
  {
  
    zobowiazanka<-zobowiazania[zobowiazania$Rodzaj %in% dok_do_zapl,]
    zobowiazanka<-subset(zobowiazanka,is.na(Rozlicz.))
    zobowiazanka<-subset(zobowiazanka,BPł=="W")   
    zobowiazanka<-zobowiazanka[,c(1,2,3,5,6,9,13,14,15,16,20)]
    colnames(zobowiazanka)[8]<-"DataPlat"
    zobowiazanka<-subset(zobowiazanka,DataPlat<today()+5)
    zobowiazanka<-zobowiazanka[order(zobowiazanka$Konto, zobowiazanka$DataPlat), ]
    kable(zobowiazanka)
    
    
}

blokady_W()
