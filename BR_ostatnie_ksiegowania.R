#' Wyszukiwanie ostatnich księgowań 
#'
#' Funkcja ost_ksieg() wyszukuje księgowania zobowiązań z zadanych ostatnich x dni
#' 
#' @param ostatnich_dni liczba minionych od dnia bieżącego dni, z których księgowania mają zostać wyświetlone, domyślnie 1.
#' @param do_kiedy liczba dodanych do dnia bieżącego dni, określająca maksymalną datę płatności, domyślnie 2.
#' 
#' @example
#' \dontrun{
#' ost_ksieg(ostatnich_dni=1,do_kiedy=2)
#' ost_ksieg(5)
#' }
#' 
#' @export


ost_ksieg<-function (ostatnich_dni=1,do_kiedy=2) 
  {
  if (wday(today())==2 & ostatnich_dni==1){
    ostatnich_dni=3
  }
  
  zobowiazania_ost<-zobowiazania
  colnames(zobowiazania_ost)[21]<-"Rozli_data"
  
  zobowiazania_ost<-subset(zobowiazania_ost,is.na(Rozli_data))
  partnerzy_ost<-partnerzy
  colnames(partnerzy_ost)[1]<-"Konto"
  colnames(partnerzy_ost)[2]<-"Nazwa"
  colnames(zobowiazania_ost)[20]<-"EnterDate"
  colnames(zobowiazania_ost)[14]<-"DueDate"
  
  dokumenty$Do_wplywow[is.na(dokumenty$Do_wplywow)]<-""
  dokumenty$Do_zaplaty[is.na(dokumenty$Do_zaplaty)]<-""
  dok_do_zapl<-dokumenty$KOD[dokumenty$Do_zaplaty=="x"]
  #dok_do_wplyw<-dokumenty$KOD[dokumenty$Do_wplywow=="x"]
  
  partnerzy_ost$Konto<-as.character(partnerzy_ost$Konto)
  ostatnie_ZOB<-subset(zobowiazania_ost,EnterDate>=today()-days(ostatnich_dni) & DueDate<=today()+days(do_kiedy))
  ostatnie_ZOB2<- ostatnie_ZOB[ostatnie_ZOB$Rodzaj %in% dok_do_zapl,]
  
  FakturkiZal<-left_join(ostatnie_ZOB2,partnerzy_ost,by="Konto")
  Late_Lia<-FakturkiZal[,c(5,22,1,3,6,9,13,14,15,16,20)]
  
  try(Late_Lia<-Late_Lia[order(Late_Lia$Konto),])
  
  Late_Lia$Opis<-strtrim(Late_Lia$Opis,7)
  Late_Lia$Nazwa<-strtrim(Late_Lia$Nazwa,12)
  
  rm(ostatnie_ZOB)
  rm(partnerzy_ost)
  rm(zobowiazania_ost)
  rm(dok_do_zapl)
  rm(ostatnie_ZOB2)
  FakturkiZal
  
  return(kable(Late_Lia))
}



