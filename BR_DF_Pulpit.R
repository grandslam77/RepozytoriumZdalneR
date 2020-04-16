getwd()

#
source("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/BR_DF_start_startowy.r") # > ctrl+enter
warnings()
K               


#//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/
#######################   WYSZUKIWARKA  #############################                   
wyszukiwarka("4500081555")                                               # > ctrl+enter
#####################################################################

################## ZESTAWIENIA PŁATNOŚCI/ROZLICZEŃ ##################
zestawienie_platnosci(firma="TAMIN",kiedy="2018-08-25")        # > ctrl+enter
#####################################################################
# hint:pole "PD" przy imporcie robi się numeric zamiast char.

################## OSTATNIE KSIĘGOWANIA #############################
ost_ksieg(5,3)                                                      # > ctrl+enter
#####################################################################

legoland<-ost_ksieg(180,-2)
is.na(legoland$BPł)<-"Z"
legoland <- legoland %>%
  mutate(legolan = replace(legoland$BPł,is.na(legoland$BPł),0))

zestiu<-subset(legoland,legoland$legolan!="B" || legoland$legolan==0)
zestiu2|| is.na(legoland$BPł) & legoland$Wal.!="PLN")
write.csv2(zestiu,"kreda.csv")

########################## BLOKADY W ################################
blokady_W()                                                         # > ctrl+enter
#####################################################################

########################## BLOKADY W ################################
literkiEUR()                                                        # > ctrl+enter
#####################################################################

############################ WYKRES HEDGINGU ########################
hedging_wykres()                                                    # > ctrl+enter
#####################################################################
hedging_wykres_N()

hedging_wykres_Na()
####################### BLOKADY PŁATNOŚCI ###########################
blkd()     # w excelu nalezy usunąc duplikaty!                      # > ctrl+enter
#####################################################################
getwd()
########## ZESTAWIENIE PRZETERMINOWANYCH NALEZNOŚCI #################
naleznosci()                                                        # w projektowaniu
#####################################################################

########## ZESTAWIENIE PRZETERMINOWANYCH NALEZNOŚCI #################
hedging_wykres_USD()
########## ZESTAWIENIE PRZETERMINOWANYCH NALEZNOŚCI #################
nierozliczone_WB()                                                  # w projektowaniu
#####################################################################
nadwyzki_ALL()

library(tensorflow)
install_tensorflow(conda="tf=keras")

hedging_wykres_N()

library(devtools)
create("brentagiAK1")
getwd()
build("brentagiAK",path="C:/Users/plakopec/Documents")
library(XRJulia)
findJulia(test=TRUE)

remove.packages("plotly")

zs<-subset(zobowiazania,is.na(BPł))
colnames(zs)[14]<-"PLATNOSC"
zs2<-subset(zs,PLATNOSC<"2017-07-25")
zs3<-subset(zs2,is.na(Rozlicz.) & Wal.!="PLN")
zobowiazania2kl<- zs3[zs3$Rodzaj %in% dok_do_zapl,]
zobowiazania3kl<-zobowiazania2kl[,c(1,5,6,9,13,14,15,16,17,18,20,21)]
write.csv2(zobowiazania3kl, file = "zaplaty.csv",sep=";",dec=",",quote=FALSE)

library(rgl)
demo("bivar", package = "rgl", echo = FALSE)
par3d(zoom = 0.7)
dput(names( iris))
c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
iris

setClass("student", slots=list(name="character", age="numeric", GPA="numeric"))


mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()



### kontrahenci, na których znajdują się nierozliczone przelewy
dok_w<-subset(zobowiazania,Rodzaj=="WB" & is.na(Rozlicz.))
dok_w1<-distinct(dok_w[,5])
View(dok_w1)


### funkcja zwracająca listę kontrahentów na których w ciągu
### ostatnich X dni zaksięgowano nierozliczone do dzisiaj dokumenty WB
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




### nierozliczone dokumenty z blokadą W
blokady_W()  

### zaksięgowane w ciągu ostatnich X dni dokumenty z blokadami W   (funkcja)


