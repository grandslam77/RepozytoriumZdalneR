#' Wyszukiwanie partnerów biznesowych
#'
#' Funkcja wyszukiwarka() wyszukuje partnerów biznesowych w SAP po numerze konta w SAP,
#' lub na podstawie części nazwy, potrafi wyszukac także dane zamówienia i faktur do zamówień
#' jeśli takie zostały zaksiegowane a także zawiera książkę telefoniczną.
#'
#' @param x Numer kontrahenta w SAP lub część nazwy podana w cudzysłowie
#' @param tylko_numer Jeśli ma wartość TRUE wyszuka tylko numer kontrahenta
#'
#' @example
#' \dontrun{
#' wyszukiwarka("10033964")
#' wyszukiwarka("Henkel")
#' wyszukiwarka("Henkel",T)     #wyszuka tylko numery kontrahentów mających w nazwie słowo "HenKeL"
#' wyszukiwarka("4500016260")   #pokaże dane do zamówienia
#' }
#'
#' @export






wyszukiwarka<-function(x,tylko_konto=F) {
  
  if (tylko_konto==TRUE)
  {
    partnerzy$Konto<-as.character(partnerzy$Konto)
    pattern <- x
    string <- partnerzy$Nazwa
    dopas<-grep(pattern, string, ignore.case=TRUE)  
    zesta<-partnerzy[dopas,]
    return(zesta$Konto)
  }
  
  
if (nchar(x)!=1 & strtrim(x,5)!="45000" & strtrim(x,1)!="!")   #szukanie albo po nazwie albo po koncie
  {
   
    partnerzy$Konto<-as.character(partnerzy$Konto)
    pattern <- x
    string <- partnerzy$Nazwa
    string2<-partnerzy$Konto
    dopas<-grep(pattern, string, ignore.case=TRUE)
    dopas2<-grep(pattern, string2, ignore.case=TRUE)
    wektor<-c(dopas,dopas2)  #dopas3    
    zesta<-partnerzy[wektor,]
    return(kable(zesta))
  }
  
else if (strtrim(x,1)=="!")                                   #szukanie z książki telefonicznej

  {
    
    patternL<-substr(x, 2,nchar(x))
    nazwiska<-users$FullName
    dop<-grep(patternL, nazwiska, ignore.case=TRUE)
    zesta<-users[dop,c(2,4,7,11)]
    return(kable(zesta))
  }
  else if(strtrim(x,5)=="45000")                              #szukanie nr zamówień
  {
   
    pattern<-x
    orders<-zamowienia
    colnames(orders)[2]="Zamowienie"
    liabilitie<-zobowiazania
    liabilitie<-liabilitie[,c(1,2,3,5,7,10,13,14,15,16,20,21)]
    orders<-orders[,c(2,3,4,5,8,11,15)]
    zamowienia_zobow<-liabilitie$Przypisanie
    zamowienia_orders<-orders$Zamowienie
    dopas3<-grep(pattern, zamowienia_zobow, ignore.case=TRUE)
    dopas4<-grep(pattern, zamowienia_orders, ignore.case=TRUE)
    zesta<-merge(orders[dopas4,],liabilitie[dopas3,],by.x="Zamowienie",by.y="Przypisanie",all.x = TRUE)
    return(kable(zesta))
  }
  else if (nchar(x)==1 & x=="X")
  {
    wzor_pattern<-x
    string_iks<-zobowiazania$PD
    dopas_iksy<-grep(wzor_pattern, string_iks, ignore.case=TRUE)
    zesta<-zobowiazania[dopas_iksy,]
    zesta<-zesta[,c(1,2,4,5,10,13,14,15,16)]
    zesta<-subset(zesta,zesta$Wal.!="PLN")
    return(kable(zesta))
  }
  else if (nchar(x)==1 & x=="D")
  {
    wzor_patternD<-x
    zobowiazan23<-subset(zobowiazania,is.na(zobowiazania$Rozlicz.))
    colnames(zobowiazan23)[3]<-"Bpl"
    string_dek<-zobowiazan23$Bpl
    dopas_deki<-grep(wzor_patternD, string_dek, ignore.case=TRUE)
    zestwa<-zobowiazan23[dopas_deki,]
    zestwa<-zestwa[,c(1,2,3,4,5,10,13,14,15,16)]
    zestwa<-subset(zestwa,zestwa$Wal.!="PLN")
    return(kable(zestwa))
  }
}  


