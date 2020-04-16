
  pkg <- c("dplyr", "knitr", "devtools", "DT", "xtable","lubridate","readxl","scales","reshape2",
           "ggplot2","stringi","knitr","gridExtra","roxygen2","openxlsx","plotly","plyr","plotly","stringr")
  new.pkg <- pkg[!(pkg %in% installed.packages())]
  if (length(new.pkg)) 
    {
   install.packages(new.pkg, repos = "http://cran.rstudio.com")
  }
  
  require(lubridate)
  require(readxl)
  require(plyr)
  require(dplyr)
  require(ggplot2)
  require(reshape2)
  require(scales)
  require(gridExtra)
  require(knitr)
  require(roxygen2)
  require(openxlsx)
  require(stringr)
  require(plotly)



  wd<-"M:/PL/finanse Kedzierzyn/DF_Brenntag/pliki_podstawowe"
  #setwd(wd)
  
  setwd("C:/Users/plakopec/Documents")

  naleznosci <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/naleznosci.xlsm", na = "NA")
  faktoring <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/faktoring.xlsm", na = "NA")
  zobowiazania <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/zobowiazania.xlsm")
  forwardy <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/forwardy.xlsm", na = "NA")
  
  #naleznosci <- read_excel("C:/Users/grandslam/OneDrive/AK_PRACA/naleznosci.xlsm", na = "NA")
  #zobowiazania <- read_excel("C:/Users/grandslam/OneDrive/AK_PRACA/zobowiazania.xlsm")
  #forwardy <- read_excel("C:/Users/grandslam/OneDrive/AK_PRACA/forwardy.xlsm", na = "NA")

  
  zamowienia<- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/zamowienia.xlsm", na = "NA")
  dokumenty <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/rodzaje_dokumentow.xlsm", na = "NA")
  partnerzy <- read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/partnerzy.xlsm", na = "NA")
  users<-read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/users.xlsm", na = "NA")
  kontakty<-read_excel("//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/kontakty.xlsm", na = "NA")
  
  require(brentagiAK1)
  
  faktoring<-faktoring$KONTOFAK
  
  dokumenty$Do_wplywow[is.na(dokumenty$Do_wplywow)]<-""
  dokumenty$Do_zaplaty[is.na(dokumenty$Do_zaplaty)]<-""
  dok_do_zapl<-dokumenty$KOD[dokumenty$Do_zaplaty=="x"]
  dok_do_wplyw<-dokumenty$KOD[dokumenty$Do_wplywow=="x"]
  users<-users[,c(1,2,3,4,32,36,41,46,47,48,49,86)]
  users<-users[users$EMAIL!="",]
  
  p <- c(naleznosci_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/naleznosci.xlsm",
         zobowiazania_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/zobowiazania.xlsm",
         forwardy_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/forwardy.xlsm",
         zamowienia_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/zamowienia.xlsm",
         rodzaje_dokumentow_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/rodzaje_dokumentow.xlsm", 
         partnerzy_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/partnerzy.xlsm",
         users_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/users.xlsm",
         kontakty_xlsm="//brenntagceen.sharepoint.com@ssl/sites/shares/PL_Finanse_Kedzierzyn/DF_Brenntag/pliki_podstawowe/kontakty.xlsm")
  
  file.info(p)$mtime
  n<-names(p)
  D<-as.character(file.info(p)$mtime)
  L<-names(p)
  K<-as.data.frame(D,L)
  colnames(K)[1]<-"DATA MODYFIKACJI PLIKU"
  kable(K)
  
  
  
  library(shiny)
  library(shinydashboard)
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(),
      title = "Dashboard example"
    ),
    server = function(input, output) { }
  )
  
  
  library(shiny)
  sliderInput("obs", "Number of observations:",
              min = 0, max = 1000, value = 500
  )
  
  library(shinyWidgets)
  prettyCheckboxGroup(
    inputId = "checkgroup2",
    label = "Click me!", thick = TRUE,
    choices = c("Click me !", "Me !", "Or me !"),
    animation = "pulse", status = "info"
  )