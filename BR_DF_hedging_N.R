

hedging_wykres_N<-function(){
  
  poniedzialeko<-function (x,t=24)
  {
    x<-as.Date(x)
    m<-month(x)
    while (wday(x)!=2)
    {
      x<- as.Date(x)+1
    }
    if(day(x)>t) 
    {
      while(month(ymd(x))==m)
      {
        x<-ymd(x)+1  
      }
    }
    if(wday(x)==1|wday(x)==7)
    {
      while(wday(x)!=2)
      {
        x<-ymd(x)+1
      }
    }
    return(x)
  }
  vPon<-Vectorize(poniedzialeko)
  MaFunkcja<-function(x)
  {
    x<-unname(vPon(x)+ymd("1970-01-01"))
    return(x)          
  }
  poniedzialeko_zob<-function (x,t=24)
  {
    x1<-as.Date(x)
    if (wday(x1)>2)
    {
      while(wday(x1)!=2)
      {
        x1<- ymd(x1)-1   #cofnięcie na poniedziałek
      }
    }
    else if (wday(x1)==1)
    {
      x1<- ymd(x1)+1
    }
    m<-month(x1)
    if(day(x1)>t) 
    {
      while(month(ymd(x1))==m)
      {
        x1<-ymd(x1)+1  
      }
    } 
    if(wday(x1)==1|wday(x1)==7)
    {
      while(wday(x1)!=2)
      {
        x1<-ymd(x1)+1
      }
    }
    return(x1)
  }
  vPon_zob<-Vectorize(poniedzialeko_zob)
  MaFunkcja_zob<-function(x)
  {
    x<-unname(vPon_zob(x)+ymd("1970-01-01"))
    return(x)          
  }
  
naleznosci4<-naleznosci
colnames(naleznosci4)[3]<-"DokRozl"
colnames(naleznosci4)[8]<-"Kurs"
colnames(naleznosci4)[9]<-"Due_date"
colnames(naleznosci4)[14]<-"Clear_date"
colnames(naleznosci4)[11]<-"Amount"
naleznosci5<-naleznosci4[naleznosci4$Rodzaj %in% dok_do_wplyw,]
naleznosci5<-subset(naleznosci5,Wal.=="EUR")
NALrozliczone<-subset(naleznosci5,DokRozl!="")


nierozliczone<-subset(naleznosci5, is.na(Clear_date))   #ważne
nierozliczone<-mutate(nierozliczone,Destination=MaFunkcja(Due_date))
NALrozliczone<-mutate(NALrozliczone,Destination=MaFunkcja(Due_date))


nierozliczone_sum<-group_by(nierozliczone,Destination)%>% summarise(Suma=sum(Amount), Kurs_=mean(Kurs))
NALrozliczone_sum<-group_by(NALrozliczone,Destination)%>% summarise(Suma=sum(Amount), Kurs_=mean(Kurs))

naleznosciUSD<-subset(naleznosci4,Rodzaj=="UC" & Wal.=="USD")  #dodać kosztowe, jakie kody mają korekty?
nierozliczoneUSD<-subset(naleznosciUSD, is.na(Clear_date))   #ważne
nierozliczoneUSD<-transform(nierozliczoneUSD,Destination=MaFunkcja(Due_date))
nierozliczone_sUSD<-group_by(nierozliczoneUSD,Destination)%>% summarise(Suma=sum(Amount), Kurs_=mean(Kurs))

zobowiazania2<-zobowiazania

colnames(zobowiazania2)[14]<-"Due_date"
colnames(zobowiazania2)[15]<-"Amount"
colnames(zobowiazania2)[17]<-"Kurs"

zobowiazania2<-subset(zobowiazania2,Wal.=="EUR")
zobowiazania2<- zobowiazania2[zobowiazania2$Rodzaj %in% dok_do_zapl,]
zobowiazania2<-subset(zobowiazania2,is.na(Rozlicz.))

zobowiazania2<-mutate(zobowiazania2,Destination=MaFunkcja_zob(Due_date))

zobowiazania2sum<-group_by(zobowiazania2,Destination)%>% summarise(Suma=sum(abs(Amount)), Kurs_=mean(Kurs))

zobowiazania2sumF<-subset(zobowiazania2sum,Destination>today()-7 & Destination<today()+120)
nierozliczone_sumF<-subset(nierozliczone_sum,Destination>today()-7 & Destination<today()+120)
NALRozliczone_sumF<-subset(NALrozliczone_sum,Destination<today()-7 & Destination>today()-90)
NAL_sredni_wplyw<-mean(NALRozliczone_sumF$Suma,trim=0.2)

tabela<-full_join(nierozliczone_sumF,zobowiazania2sumF,by="Destination")
colnames(tabela)[1]<-"Data"
colnames(tabela)[2]<-"Receivables"
colnames(tabela)[3]<-"Rec_ex_rate"
colnames(tabela)[4]<-"Liabilities"
colnames(tabela)[5]<-"Lia_ex_rate"

attach(forwardy)
forwardy[,ncol(forwardy)+1]<-WartośćWal-WartZDok
colnames(forwardy)[ncol(forwardy)]<-c("Zostalo")

forwardy<-subset(forwardy, as.Date(DataRozl)>=today())
ForEUR<-subset(forwardy,c(Waluta == "EUR"))
ForEUR<-subset(ForEUR,WartośćWal<1010000 & WartośćWal>0)
ForUSD<-subset(forwardy,c(Waluta == "USD"))
ForUSD<-subset(ForUSD,WartośćWal<1010000 & WartośćWal>0)

nadwyzkiUSD<-subset(ForUSD,Zostalo>1000)
nadwyzkiUSD<-select(nadwyzkiUSD,one_of(c("NrTrans","DataRozl","KursSpot","Zostalo")))
colnames(nadwyzkiUSD)[4]<-"Zostało USD"


nadwyzkiEUR<-subset(ForEUR,Zostalo>1000)
nadwyzkiEUR<-select(nadwyzkiEUR,one_of(c("NrTrans","DataRozl","KursSpot","Zostalo")))
colnames(nadwyzkiEUR)[4]<-"Zostało EUR"
kable(nadwyzkiUSD)

ForEUR$PokrNależn[is.na(ForEUR$PokrNależn)] <- ""
ForEUR_zakup<-subset(ForEUR,PokrNależn=="")
ForEUR_nat<-subset(ForEUR,PokrNależn!="")

#DplyRApproach2<-group_by(ForEUR,DataRozl,PokrNależn)%>% summarise(suma=sum(WartośćWal), śRednia=mean(KursForw))
DplyRApproach3<-group_by(ForEUR_zakup,DataRozl)%>% summarise(suma=sum(WartośćWal))

DplyRApproach3<-group_by(ForEUR_zakup,DataRozl)%>% summarise(suma=sum(WartośćWal))
DplyRApproach3a<-group_by(ForEUR_zakup,DataRozl)%>% summarise(suma=sum(WartośćWal),sumaPLN=sum(WrtWalKr))

DplyRApproach4<-group_by(ForEUR_nat,DataRozl)%>% summarise(suma=sum(WartośćWal))
DplyRApproach4a<-group_by(ForEUR_nat,DataRozl)%>% summarise(suma=sum(WartośćWal),sumaPLN=sum(WrtWalKr))

#Forwardy7_nal<-transform(DplyRApproach4,Rozl77=MaFunkcja_zob(DataRozl))
#Forwardy77_nal<-group_by(Forwardy7_nal,Rozl77)%>% summarise(Amount=sum(suma))

Forwardy7_nal_A<-transform(DplyRApproach4a,Rozl77=MaFunkcja_zob(DataRozl))
Forwardy77_nal_A<-group_by(Forwardy7_nal_A,Rozl77)%>% summarise(Amount=sum(suma),AmountPLN=sum(sumaPLN))

Forwardy7<-transform(DplyRApproach3a,Rozl77=MaFunkcja_zob(DataRozl))
Forwardy77<-group_by(Forwardy7,Rozl77)%>% summarise(Amount=sum(suma),AmountPLN=sum(sumaPLN))
Forwardy77<-subset(Forwardy77,Rozl77>today()-7 & Rozl77<today()+120)
Forwardy77_nal<-subset(Forwardy77_nal_A,Rozl77>today()-7 & Rozl77<today()+120)
Forwardy77[,4]<-Forwardy77[,3]/Forwardy77[,2]
Forwardy77_nal[,4]<-Forwardy77_nal[,3]/Forwardy77_nal[,2]
colnames(Forwardy77)[4]<-"KursFORW"
colnames(Forwardy77_nal)[4]<-"KursNAL"


colnames(Forwardy77)[1]<-"Data"
colnames(Forwardy77_nal)[1]<-"Data"

tabela2<-left_join(tabela,Forwardy77,by="Data")
tabela2<-left_join(tabela2,Forwardy77_nal,by="Data")
tabela2$Amount.x[is.na(tabela2$Amount.x)] <- 0
tabela2$Amount.y[is.na(tabela2$Amount.y)] <- 0

#naleznosci i forward
to_plot <- data.frame(Data2=tabela2$Data,Forward=round(tabela2$Amount.x/1000,0),Naleznosci=round(tabela2$Receivables/1000,0))
melted<-melt(to_plot, id.vars="Data2")
#zobowiazania
to_plot_for<-data.frame(Data=tabela2$Data,ZobowiazanieEUR=round(tabela2$Liabilities/1000,0))
melted2<-melt(to_plot_for,id.vars = "Data")
#pokryto naleznosciami
to_plot_nal<-data.frame(Data3=tabela2$Data,Pokryto_NAL=round(tabela2$Amount.y/1000,0))
melted3<-melt(to_plot_nal,id.vars = "Data3")
#kurs EUR
to_plot_Kurs<-data.frame(Data4=tabela2$Data,KursFor=round(tabela2$KursFORW,3))
melted4<-melt(to_plot_Kurs,id.vars = "Data4")

wykres<-ggplot() + 
  geom_hline(data = melted2,aes(x=Data,y=value), yintercept = round(NAL_sredni_wplyw/1000),linetype="dotdash", color = "black", size=1)+
  geom_bar(data=melted2,aes(x=Data,y=value,fill=variable),stat="identity", alpha=.8,width=3)  +
  geom_text(data=melted2,aes(x=Data,y=value,label = value),size=3)+
  #geom_label(data=melted2,aes(x=Data,y=value,label=value),alpha=0.1)+
  geom_bar(data=melted,aes(x=Data2,y=value,fill=variable), stat="identity", alpha=0.5) +
  
  geom_text(data=melted,aes(x=Data2[10],y=round(NAL_sredni_wplyw/1000+200),label = "ŚREDNI TYGODNIOWY WPŁYW USD"),size=3)+
  #geom_label(data=melted,aes(x=Data2,y=value,label=value),alpha=0.1)+
  #geom_text(aes(y = mid_y))
  geom_segment(data=melted3,aes(x=Data3,y=0,xend=Data3,yend=value),stat="identity",size=3,alpha=0.5)+
  geom_label(data=melted4,aes(x=Data4,y=value,label = value),position = "identity",size=3)+
  scale_x_date(labels = date_format("%Y-%m-%d"),date_breaks ="1 week",name="Data rozliczenia" )+
  scale_y_continuous(name="Wartość w USD (tys.)",limits=(c(0,max(max(tabela2$Liabilities/1000+500),max((tabela2$Receivables+tabela2$Amount.x)/1000+500)))),
                     breaks =c(200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000,3200,3400,3600,3800,4000,4200,4400,4600,4800,5000,5200,5400,5600,5800,6000,6200,6400,6600,6800,7000,7200))+
  theme_light()+theme(legend.position="bottom")+
  annotation_custom(tableGrob(tabela2), xmin=350, xmax=500, ymin=-2.5, ymax=-1)


wykres


}


