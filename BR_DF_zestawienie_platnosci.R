#' Sporządzanie zestwień płatności 
#'
#' Funkcja zestawienie_platnosci() sporządza zestawienia zawierające listę zapłaconych faktur oraz wartości przelewów 
#' w zadanym okresie.  
#' 
#' 
#' @param  firma numer kontrahenta lub część nazwy kontrahenta, dla którego chcemy uzyskać specyfikację przelewów (domyślnie z pliku)
#' @param  kiedy liczba ostatnich dni, dla których chcemy uzyskać specyfikację przelewów (domyślnie kiedy=5)
#' @param  dlugosc_listy liczba faktur w płatności, powyżej której ma się tworzyć zestawienie (domyślnie dlugosc_listy=0)
#
#'
#' @example
#' \dontrun{
#'zest_plat()       #zestawienia płatności z ostatnich 5 dni dla klientów z pliku kontakty.xlsm, nawet jeśli zapłacono tylko 1 fakturę
#'zest_plat(kiedy="2017-01-01",
#'dlugosc_listy=3)  #zestawienia płatności od 2017-01-01 do dziś dla klientów z kontakty.xlsm jeśli płatność obejmowała conajmniej 4 faktury               
#'zest_plat("BASF",kiedy="2017-01-01",
#'dlugosc_listy=3) #jak wyżej z tym, że zestawienie generowane dla firm mających w nazwie frazę "BASF"                          #
#' }
#'
#' @export

zestawienie_platnosci<-function(firma=kontakty$KONTO,iksy="W",dlugosc_listy=0,kiedy=today()-5){       #"W" wykonane w przeszłości i dziś ze zleceniami. kiedy "od kiedy płatności)

  if (any(is.na(as.numeric(firma)))==TRUE)
    {
      firma<-wyszukiwarka(firma,T)
      
    }
  
  naglowek_wydrukowano<-FALSE
  
  if(iksy=="W")
      {
          zobowiazania$PD<-as.character(zobowiazania$PD)
          ziksami<-subset(zobowiazania,PD=="X") #podzbiór na płatności z dnia bieżącego i nierozliczonych płatności hint:informować o nierozliczonych
          podzbior_hist<-subset(zobowiazania,is.na(Rozlicz.)==FALSE & Rozlicz.>=as.Date(kiedy))     #finalnie 'kiedy'

          ziksami<-ziksami[ziksami$Konto %in% firma,]
          podzbior_hist<-podzbior_hist[podzbior_hist$Konto %in% firma,]
          ziksami<-ziksami[,c(1,5,13,14,15,16,21)]
          podzbior_hist<-podzbior_hist[,c(1,5,6,7,13,14,15,16,21)]
          unikalne_daty<-unique(podzbior_hist$Rozlicz.)

          for (i in 1:length(firma))
            {
              naglowek_wydrukowano<-FALSE
  #================================================================================================================================
              zestawienie<-subset(ziksami,Konto==firma[i])
              #browser()
              if (length(zestawienie$Konto)>dlugosc_listy & length(zestawienie$Konto)>0)          
                {
                  sumy<-sum(ziksami[ziksami$Konto==firma[i],5])
                  zestawienie[length(zestawienie$Konto)+1,5]<-sumy
                  zestawienie[length(zestawienie$Konto),1]<-"Total amount:"
                  zestawienie[length(zestawienie$Konto),2]<-""
            
                  print(kable(kontakty[kontakty$KONTO==firma[i],3],col.names = kontakty[kontakty$KONTO==firma[i],2]))
                  print(kable("Below you will find specification of our last payments.",col.names = kontakty[kontakty$KONTO==firma[i],5]))
                  print(kable(zestawienie,format.args = list(decimal.mark = ",", big.mark = ".",nsmall=2)))
                  naglowek_wydrukowano<-TRUE
                }
             
              for(z in 1:length(unikalne_daty))
                {
                  
                podzbior_klient<-subset(podzbior_hist,Konto==firma[i] & Rozlicz.==unikalne_daty[z])
                #browser()   
                if (length(podzbior_klient$Konto)>dlugosc_listy & length(podzbior_klient$Konto)>0)
                    {
                      sumy<-sum(podzbior_klient[podzbior_klient$Konto==firma[i],7])
                      podzbior_klient[length(podzbior_klient$Konto)+1,7]<-sumy
                      podzbior_klient[length(podzbior_klient$Konto),1]<-"Total amount:"
                      podzbior_klient[length(zestawienie$Konto),2]<-""

                      print(kable(podzbior_klient,format.args = list(decimal.mark = ",", big.mark = ".",nsmall=2)))
                    }
                }
          }
  }

}



