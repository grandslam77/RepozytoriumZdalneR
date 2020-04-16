require(GA)


zeestaW <- read_excel("C:/Users/grandslam/Documents/gen.xlsm", na = "NA")



colnames(zeestaW)[1]<-"Nr_dokumentu"
colnames(zeestaW)[2]<-"Kwota"


zeestaW<-zeestaW[,c(4,11)]

ZbiorWartosciFaktur<-as.matrix(zeestaW[,2])
ZbiorWartosciFaktur<-provideDimnames(ZbiorWartosciFaktur,base = list(as.matrix(zeestaW[,1]),"wartosci"))
sum(ZbiorWartosciFaktur)

WartoscPrzelewu<-56.38*-1

fitnessik <- function(string) 
  {
    inc <- which(string == 1)
    kara<-0
    if (sum(which(string==1))+1==1)
      {
        kara<-(-600000)
      }
 
    X <- sum(ZbiorWartosciFaktur[inc])
    if (X+WartoscPrzelewu>0)
      {
        kara<-(-600000)
      }
    X+WartoscPrzelewu+kara
  }

GA <- gaisl(type = "binary", 
              fitness =  fitnessik,
              nBits = length(ZbiorWartosciFaktur),
              names = rownames(ZbiorWartosciFaktur),
              popSize = 3000, 
              maxiter = 1000, run = 100,
              numIslands = 4, 
              migrationRate = 0.1,

              migrationInterval = 100)
              
summary(GA) 
plot(GA)
