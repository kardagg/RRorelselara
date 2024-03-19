# Här visas hur man kan hantera många filer samtidigt med hjälp av 
# listor och for" satser

# Ange namn på mapp där de filer man vill läsa in ligger
mappNamn="DataFiler/Sprint"

# Läs in namnet på alla filer i denna mapp
filNamnLista=list.files(mappNamn)
print(filNamnLista)

# Skapa en tom vektor
dataVector <- list()
# Läs in csv filer i en "for" sats och addera de skapade dataFrames 
# som element i dataVector
# Man måste bädda in dataFramen som listor för att dom ska förbli dataFrames
for(filNamn in filNamnLista){
  df <- read.csv2(paste(mappNamn,"/",filNamn, sep=""), 
                  header=TRUE)
  dataVector <- append(dataVector,list(df))
}

colors <- c("#FF0000FF", "#008800FF", "#0000FFFF")

# Man måste använda dubbla [ parentesser ([[nr]]) för att komma åt
# dataFramen inne i respektive lista
plot(dataVector[[1]]$TID,dataVector[[1]]$HEAT.1,col=colors[1], type="l", 
     ylim=c(0,900), ylab="Kraft (N)", xlab="TId (s)")
lines(dataVector[[1]]$TID,dataVector[[1]]$HEAT.2,col=colors[1], type="l")
lines(dataVector[[1]]$TID,dataVector[[1]]$HEAT.3,col=colors[1], type="l")
lines(dataVector[[1]]$TID,dataVector[[1]]$HEAT.4,col=colors[1], type="l")

lines(dataVector[[2]]$TID,dataVector[[2]]$HEAT.1,col=colors[2], type="l")
lines(dataVector[[2]]$TID,dataVector[[2]]$HEAT.2,col=colors[2], type="l")
lines(dataVector[[2]]$TID,dataVector[[2]]$HEAT.3,col=colors[2], type="l")
lines(dataVector[[2]]$TID,dataVector[[2]]$HEAT.4,col=colors[2], type="l")

lines(dataVector[[3]]$TID,dataVector[[3]]$HEAT.1,col=colors[3], type="l")
lines(dataVector[[3]]$TID,dataVector[[3]]$HEAT.2,col=colors[3], type="l")
lines(dataVector[[3]]$TID,dataVector[[3]]$HEAT.3,col=colors[3], type="l")
lines(dataVector[[3]]$TID,dataVector[[3]]$HEAT.4,col=colors[3], type="l")

grid()
legend("topright", c("90", "115", "135"), lty = 1, col = c(colors[2], colors[3],colors[1]))
