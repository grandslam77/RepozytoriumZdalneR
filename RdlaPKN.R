
#****************************************************************
Z_PKN <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/PKN_zest.xlsm")
Produkty <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/PKN_produkty.xlsm")
colnames(Produkty)[1]<-"Pozycja faktury"
tabel<-left_join(Z_PKN,Produkty,by="Pozycja faktury")
colnames(tabel)[3]<-"NR_REJ"
FOR_ACCOUNTING<-tabel %>% group_by(`klasa produktu`,NR_REJ) %>% summarise(Netto=sum(`Wartość netto fak`),Vat=sum(`Wartość vat fak`),Brutto=sum(`Wartość fak`))
write.csv2(FOR_ACCOUNTING, file = "pkN.csv")



