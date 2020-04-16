#' Sporządzanie zestwień blokad faktur do płatności wraz z listą mailingową
#'
#' Funkcja blkd() sporządza plik blokady_end.csv w katalogu roboczym zawierający listę 
#' zablokowanych do płatności faktur wraz z listą mailingową osób, które zamawiały towar 
#' dla zestawionych faktur
#' 
#' @param df minimalna liczba dni, które upłynęły od daty wystawienia faktury (domyślnie df=14)
#' @param dp maksymalna liczba dni, jaka pozostała do daty płatności (domyślnie dp=14)
#'
#' @example
#' \dontrun{
#' blkd()                    #to samo co blkd(14,14)
#' blkd(3,30)                #zestawienie zablokowanych faktur zaksięgowanych wcześniej niż 3 dni temu
#'                           #z datą płatności przypadającą za 30 dni i późniejszą
#' }
#'
#' @export

blkd<-function(df=14,dp=14){
  
blokady <- zobowiazania

blokady2<-subset(blokady,BPł=="B" & is.na(Rozlicz.))

colnames(blokady2)[13]<-"DataFaktury"
colnames(blokady2)[14]<-"DataPlatnosci"
blokady2<-subset(blokady2,as.Date(DataFaktury)>"2016-06-30")
blokady3<-subset(blokady2,as.Date(DataFaktury)-today()>-df |as.Date(DataPlatnosci)-today()<dp)
transport<-subset(blokady3,Opis=="TRANSPORT")
blokady5<-subset(blokady3,Opis!="TRANSPORT")
finalna<-blokady5[,c(1,2,3,4,5,6,9,13,14,15,16,20,21)]
ost_zam<-zamowienia[,c(2,4,5,8,11)]
colnames(ost_zam)[1]<-"Zamowienie"
users2<-users[,c(12,5)]
ost_zam$Zamowienie<-as.character(ost_zam$Zamowienie)


The_tabelki<-merge(finalna,ost_zam,by.x=2,by.y=1,all.x=TRUE)
The_tabelki<-distinct(The_tabelki)
The_tabelki3<-merge(The_tabelki,users2,by.x=17,by.y =1,all.x = TRUE)
The_tabelki3<-distinct(The_tabelki3)

#lizurek2<-distinct(subset(select(lizurek,one_of(c("Nr.fakt.","Krok"))),Nr.fakt.!=""))
#The_tabela2<-left_join(The_tabela,lizurek2,by=c("Referencja"="Nr.fakt."))

   #tbl_df(The_tabela)


ListaP<-unique(The_tabelki3$EMAIL)
listaM<-ListaP[1]

for (i in 2:length(ListaP))
  {
  
    listaM<-paste(listaM,ListaP[i],sep="&")
  
  }

The_tabelki3[17]<-NULL
The_tabelki3[length(The_tabelki3$Konto)+1,1]<-listaM

write.csv2(The_tabelki3, file = "blokady_end.csv",sep=";",dec=",",quote=FALSE)

}

