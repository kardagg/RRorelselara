# Här visas hur man kan hantera många filer samtidigt med hjälp av 
# listor och for" satser

# Ange namn på mapp där de filer man vill läsa in ligger
mappNamn="DataFiler/FlerFiler"

# Läs in namnet på alla filer i denna mapp
filNamnLista=list.files(mappNamn)

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

colors <- c("#FF0000FF", "#00FF00FF", "#00FFFFFF")

# Man måste använda dubbla [ parentesser ([[nr]]) för att komma åt
# dataFramen inne i respektive lista
plot(dataVector[[1]]$Tid,dataVector[[1]]$Value,col=colors[1], pch=19, ylim=c(0,4))

# for sats för att plotta resten av filerna
for(nr in 2:length(dataVector)){
  points(dataVector[[nr]]$Tid,dataVector[[nr]]$Value,col=colors[nr],pch=19)
}


