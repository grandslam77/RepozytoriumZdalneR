

nadwyzki_ALL<-function(){

forwardy_X<-forwardy
  
attach(forwardy_X)

  
forwardy_X[,ncol(forwardy_X)+1]<-WartośćWal-WartZDok
colnames(forwardy_X)[ncol(forwardy_X)]<-c("Zostalo")

forwardy_X<-subset(forwardy_X, as.Date(DataRozl)>=today())
ForEUR<-subset(forwardy_X,c(Waluta == "EUR"))
ForEUR<-subset(ForEUR,WartośćWal<1010000 & WartośćWal>0)
ForUSD<-subset(forwardy_X,c(Waluta == "USD"))
ForUSD<-subset(ForUSD,WartośćWal<1010000 & WartośćWal>0)

nadwyzkiUSD<-subset(ForUSD,Zostalo>1000)
nadwyzkiUSD<-select(nadwyzkiUSD,one_of(c("NrTrans","DataRozl","KursSpot","Zostalo")))
colnames(nadwyzkiUSD)[4]<-"Zostalo"
nadwyzkiUSD[,5]<-"USD"
colnames(nadwyzkiUSD)[5]<-"Waluta"

nadwyzkiEUR<-subset(ForEUR,Zostalo>5000)
nadwyzkiEUR<-select(nadwyzkiEUR,one_of(c("NrTrans","DataRozl","KursSpot","Zostalo")))
colnames(nadwyzkiEUR)[4]<-"Zostalo"
nadwyzkiEUR[,5]<-"EUR"
colnames(nadwyzkiEUR)[5]<-"Waluta"
Wolne<-rbind(nadwyzkiEUR,nadwyzkiUSD)


}
nadwyzki_ALL()

