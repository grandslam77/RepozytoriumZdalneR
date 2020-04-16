
require(anytime)
przedzialy<-function(z,past){
  s<-as.numeric(today()-past-as.Date(z))
  
  if (s>0 & s<=10){
      l<-10 
    } else if (10<s & s<=30){
      l<-30
    } else if (30<s & s<=45){
      l<-45
    } else if (45<s & s<=60){
      l<-60
    } else if (60<s & s<=90){
      l<-90
    } else if (90<s & s<=120){
      l<-120
    } else if (120<s & s<=150){
      l<-150
    } else if (150<s & s<=180){ 
      l<-180
    } else if (180<s){
      l<-999
    } else {l<-0}
  return(l)
}



przedzialV<-Vectorize(przedzialy)


zobowiazania_bez_WB<-subset(zobowiazania,Rodzaj!="WB")
colnames(zobowiazania_bez_WB)[7]<-"DokRozl"
colnames(zobowiazania_bez_WB)[15]<-"Amount"

zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Rodzaj!="WX")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Rodzaj!="BO")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Rodzaj!="PK")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Rodzaj!="IP")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Rodzaj!="ST")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Rodzaj!="SA")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Rodzaj!="SA")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Wal.=="EUR")
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,Amount<0)

colnames(zobowiazania_bez_WB)[7]<-"DokRozl"

zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,substr(DokRozl,1,1)!=3 |is.na(zobowiazania_bez_WB$DokRozl))
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,substr(DokRozl,1,1)!=9 |is.na(zobowiazania_bez_WB$DokRozl))
zobowiazania_bez_WB<-subset(zobowiazania_bez_WB,substr(DokRozl,1,1)!=7 |is.na(zobowiazania_bez_WB$DokRozl))

colnames(zobowiazania_bez_WB)[14]<-"DataPllat"

zobowiazania_bez_WB<-mutate(zobowiazania_bez_WB,Destination=difftime(zobowiazania_bez_WB$DataPllat, zobowiazania_bez_WB$`Data dok.`, units = "days"))


naleznosci_bez_WB<-subset(naleznosci,Rodzaj!="WB")
colnames(naleznosci_bez_WB)[3]<-"DokRozlN"
colnames(naleznosci_bez_WB)[11]<-"AmountN"
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="WX")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="BO")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="PK")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="IP")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="ST")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="PW")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="PX")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Rodzaj!="SA")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,Wal.=="EUR")
naleznosci_bez_WB<-subset(naleznosci_bez_WB,AmountN>0)



naleznosci_bez_WB<-subset(naleznosci_bez_WB,substr(DokRozlN,1,1)!=3 |is.na(naleznosci_bez_WB$DokRozlN))
naleznosci_bez_WB<-subset(naleznosci_bez_WB,substr(DokRozlN,1,1)!=7 |is.na(naleznosci_bez_WB$DokRozlN))
naleznosci_bez_WB<-subset(naleznosci_bez_WB,substr(DokRozlN,1,1)!=9 |is.na(naleznosci_bez_WB$DokRozlN))

colnames(naleznosci_bez_WB)[9]<-"DataPlat"

naleznosci_bez_WB<-mutate(naleznosci_bez_WB,Destination=difftime(naleznosci_bez_WB$DataPlat, naleznosci_bez_WB$`Data dok.`, units = "days"))

naleznosci_bez_WB<-mutate(naleznosci_bez_WB,Klasa_NZ="N")
zobowiazania_bez_WB<-mutate(zobowiazania_bez_WB,Klasa_NZ="Z")


table_laczna<-rbind(naleznosci_bez_WB[,15:16],zobowiazania_bez_WB[,22:23])
table_laczna$Destination<-as.numeric(table_laczna$Destination)


ggplot(table_laczna, aes(x=as.numeric(Destination), fill=Klasa_NZ,color=Klasa_NZ)) +
  #geom_histogram(binwidth=15, alpha=0.5, position="identity")+
  geom_density(alpha=0.6)+
  theme(legend.position="top")+
  scale_x_continuous(breaks = seq(0, 150, 15), lim = c(0, 150))+
  labs(title="ROZKŁAD TERMINÓW PŁATNOŚCI FAKTUR OTRZYMANYCH I WYSTAWIONYCH",x="DNI PŁATNOŚCI", y = "ILOŚĆ FAKTUR")+
  theme_classic()
  

zobr<-subset(table_laczna,Klasa_NZ=="Z")
#zobow<-hist(zobr$Destination)


naleZ<-subset(table_laczna,Klasa_NZ=="N")
#hist_nal<-hist(naleZ$Destination)




median(zobr$Destination)
mean(zobr$Destination)
median(naleZ$Destination)
mean(naleZ$Destination)


ggplot(data = table_laczna, aes(x=Klasa_NZ, y=Destination)) + 
  geom_boxplot(aes(fill=Klasa_NZ))+
  scale_y_continuous(breaks = seq(0, 150, 15), lim = c(0, 150))+
  theme_classic()

quantile(naleZ$Destination)
quantile(zobr$Destination)

naleznosci_z_WB_NIEROZL_NOW<-subset(naleznosci,is.na(Rozlicz.))
colnames(naleznosci_z_WB_NIEROZL_NOW)[3]<-"DokRozlNW"
colnames(naleznosci_z_WB_NIEROZL_NOW)[11]<-"WartoscN"



naleznosci_AK<-naleznosci

naleznosci_z_WB_NIEROZL_1MTHPAST<-naleznosci_AK

colnames(naleznosci_z_WB_NIEROZL_1MTHPAST)[3]<-"DokRozlNW"
colnames(naleznosci_z_WB_NIEROZL_1MTHPAST)[13]<-"ENTERDATE"
colnames(naleznosci_z_WB_NIEROZL_1MTHPAST)[11]<-"WartoscN"
colnames(naleznosci_z_WB_NIEROZL_1MTHPAST)[9]<-"DataPlat"



pattern<-"^2"
string<-c("1234","2541","322")
grep(pattern, string)

# d<-d[!(d$A=="B" & d$E==0),] kasowanie niechcianych wierszy według zadanych kryteriów
naleznosci_z_WB_NIEROZL_1MTHPAST<-naleznosci_z_WB_NIEROZL_1MTHPAST[!(naleznosci_z_WB_NIEROZL_1MTHPAST$Rodzaj=="WB" & naleznosci_z_WB_NIEROZL_1MTHPAST$DataPlat==naleznosci_z_WB_NIEROZL_1MTHPAST$Rozlicz.),]

#usuwanie kontrahentów grupowych
naleznosci_z_WB_NIEROZL_1MTHPAST<-naleznosci_z_WB_NIEROZL_1MTHPAST[!(grepl("^2",naleznosci_z_WB_NIEROZL_1MTHPAST$Konto)),]


naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="WX")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="BO")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="PK")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="IP")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="ST")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="PW")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="PX")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="SA")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="KA")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="KX")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="NO")
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,Rodzaj!="ON")




naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,substr(DokRozlNW,1,1)!=3 |is.na(naleznosci_z_WB_NIEROZL_1MTHPAST$DokRozlNW))
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,substr(DokRozlNW,1,1)!=7 |is.na(naleznosci_z_WB_NIEROZL_1MTHPAST$DokRozlNW))
naleznosci_z_WB_NIEROZL_1MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,substr(DokRozlNW,1,1)!=9 |is.na(naleznosci_z_WB_NIEROZL_1MTHPAST$DokRozlNW))


nested <- data.frame(matrix(ncol = 0, nrow = 10))
x <- c("0","10","30","45","60","90","120","150","180","999")

rownames(nested) <- x

#write.csv2(naleznosci_z_WB_NIEROZL_1MTHPAST, file = "naleznosci.csv")



#foreach (i in(0:360),.combine=cbind) %dopar%
for (i in seq(30,0,-10)) 
{
    #print (i)
    naleznosci_z_WB_NIEROZL_2MTHPAST<-subset(naleznosci_z_WB_NIEROZL_1MTHPAST,today()-days(i)<Rozlicz. | is.na(naleznosci_z_WB_NIEROZL_1MTHPAST$Rozlicz.))
    naleznosci_z_WB_NIEROZL_2MTHPAST<-subset(naleznosci_z_WB_NIEROZL_2MTHPAST,today()-days(i)>ENTERDATE)
    naleznosci_z_WB_NIEROZL_2MTHPAST<-mutate(naleznosci_z_WB_NIEROZL_2MTHPAST,Zale=przedzialV(naleznosci_z_WB_NIEROZL_2MTHPAST$DataPlat,i))
    
    #niezalegle<-subset(naleznosci_z_WB_NIEROZL_2MTHPAST,Zale==0) 
    #sumaNiezaleglych<-sum(niezalegle$`Kwota w Wkr`)
    #naleznosci_z_WB_NIEROZL_2MTHPAST<-subset(naleznosci_z_WB_NIEROZL_2MTHPAST,Zale!=0)  
    seth<-as.data.frame(naleznosci_z_WB_NIEROZL_2MTHPAST %>% group_by(Zale) %>% summarise(sumA=sum(`Kwota w Wkr`)))
    #print (seth)
    seth$sumA[which(seth$sumA<0)]<-0
    
    nested[ , paste0("-", i)] <- seth$sumA
    #print(nested)

    #nested[,i] <- seth$sumA
  }

nested$zel<-as.numeric(row.names(nested))


#?foreach

#datm <- melt(cbind(dat, ind = rownames(dat)), id.vars = c('ind'))
nestedMelt<-melt(nested,id.vars = "zel")


#View(nested)
options(scipen = 999)



wykresnested<-ggplot(data=nestedMelt,aes(x=,y=value, fill=as.factor(zel))) + 

  geom_bar(position="fill",stat = "identity", width=1,alpha=0.8)+
  scale_y_continuous(labels = scales::percent,name="")+#,limits=c(0,0.25),breaks=c(0.05,0.10,0.15,0.20,0.25))+
  coord_cartesian(ylim=c(0,0.25))+  #odcina oś Y do pożądanej wartości
  scale_x_discrete(name="DNI W STECZ")+
  #scale_color_manual(values = mycolors)+
  scale_fill_brewer(palette="Paired")+
  ggtitle("STRUKTURA NALEŻNOŚCI W CZASIE")+
  labs(fill = "Przeterminowanie \nw dniach")+
  theme_light()+theme(legend.position="right",legend.text = element_text(size=7))+
  theme(axis.text.x = element_text(size=7,angle = 90))+ #fomatowanie osi x
  theme(legend.title = element_text(size=7))



wykresnested



nested[11,] = colSums(nested[1:10,1:12])
nested[12,] = colSums(nested[2:10,1:12])
nested[13,] = round(sweep(nested[12,],2, nested[11,],"/"),4)  #dzielenie wierszy w ramce danych sweep()


ma33<-nested[-c(1:12),]
ma33<-ma33[,-12]
ma33[2,]<-colnames(ma33)
maNested<-nestedMelt<-melt(ma33,id.vars = "NEO")
ma33[1,]
ma34<-as.data.frame(t(ma33))
colnames(ma34)[1]<-"ONICO"
colnames(ma34)[2]<-"PRECENTY"

drugi<-wykresnested+
  geom_text(aes(y=ma34$PRECENTY,label=ma34$ONICO), vjust=0,position = position_dodge(0.9))
  

drugi  
  
mat <- matrix(1:25, ncol = 5)
vec <- seq(2, by = 2, length = 5)
sweep(mat, 1, vec, `/`)








# ------------------- do forwardów ---------------------------------------------------------
#(expenses <- data_frame(                          #fajne inicjowanie tabeli
#  date=seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by=1),
#  amount=rgamma(length(date), shape = 2, scale = 20)))





forwardy_bez_poz<-forwardy[!(abs(forwardy$WartośćWal)>1000000) & forwardy$WartośćWal>0,]  #wyrzucone pożyczki
zea<-forwardy_bez_poz %>% group_by(DatA=floor_date(Dnia, "month"),Bank=(PokrNależn)) %>% #group_by(`Bank wł.`)
  summarize(kwota=sum(WrtWalKr)) #świetne grupowanie danych według przedziałów czasowych, rewelacja.


#x <- ymd_hms("2009-08-03 12:01:59.23")
#floor_date(x, "month")
#ceiling_date(x, "month")
#ceiling_date(x, "month", change_on_boundary = TRUE)

zea$Bank[zea$Bank=="X"]<-"NATURALNY"
zea$Bank[is.na(zea$Bank)]<-"BANK"

wykres_hedge_pwe<-ggplot(data=zea,aes(x=as.Date(DatA),y=kwota, fill=Bank)) + 
  
  geom_bar(position="fill",stat = "identity",alpha=0.8,)+
  scale_y_continuous(labels = percent_format(),name="")+
  scale_x_date(labels = date_format("%b\n%Y "),date_breaks ="1 month", name="")+ #formatowanie daty na wykresie, określenie jej interwałów
  geom_hline(yintercept = 0.5,linetype="dotdash", color = "black", size=1)+
  #scale_color_manual(values = mycolors)+
  scale_fill_brewer(palette="Paired")+
  ggtitle("HEDGING NATURALNY   VS   ZAKUPY WALUTY W BANKU")+
  guides(fill=guide_legend(title=""))+
  theme_minimal()+theme(legend.position="right")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_text(size=9, hjust=0.5,margin=margin(r=-20,t=-20)))


wykres_hedge_pwe

library(data.table)
geos<-fread('https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_2018.csv',sep = ";")
View(geos)

library(anytime)

geos$V1<-anytime(as.character(geos$V1))
geos$V4<-as.numeric(geos$V4)
geos$V4<-scan(text=geos$V4, dec=",", sep=".")
dee
zea$Bank[zea$Bank=="X"]<-"NATURALNY"






























