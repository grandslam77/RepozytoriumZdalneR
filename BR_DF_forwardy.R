
hedging_zobowiazan<-function(){

forwardy_BRENNTAG<-forwardy
forwardy_BRENNTAG<-subset(forwardy_BRENNTAG,DataRozl>today())
colnames(forwardy_BRENNTAG)[6]<-"Bankwl"
colnames(forwardy_BRENNTAG)[7]<-"NALEZN"
forwardy_BRENNTAG_ING<-subset(forwardy_BRENNTAG,Bankwl=="10400" & WrtWalKr<3000000)
forwardy_BRENNTAG_mBANK<-subset(forwardy_BRENNTAG,Bankwl=="10200" & WrtWalKr<3000000)
forwardy_BRENNTAG_NAL<-subset(forwardy_BRENNTAG,NALEZN=="X")
sumaPEKAO<-as.data.frame(summarise(group_by(forwardy_BRENNTAG,Bankwl),sum(WrtWalKr)))

sumaPEKAO[1,1]<-"NALEZNOSCI"
sumaPEKAO[2,1]<-"ING"
sumaPEKAO[3,1]<-"mBANK"

colnames(sumaPEKAO)[1]<-"Bank"
colnames(sumaPEKAO)[2]<-"Kwota_w_PLN"

data <- sumaPEKAO
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

p <- plot_ly(data, labels = ~Bank, values = ~Kwota_w_PLN, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(Kwota_w_PLN, ' PLN'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Hedging zobowiazań',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


return(p)
}



hedging_zobowiazan()




