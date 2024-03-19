# Här plottas fördelningskurvor från resultaten i Stocklholm Marathon 2022
# Datan samlades in med scriptet "SkördaStockholmMarathon2022.R" i
# mappen RByggstenar/ExtraApplikationer/DataMining

# Läs in data
menMaraResult<- read.csv2("DataFiler/SthlmMara22Herrar.csv", header=TRUE, sep=";")
womenMaraResult<- read.csv2("DataFiler/SthlmMara22Damer.csv", header=TRUE, sep=";")


# Plotta Kvinnor VS Män ---------------------------------------------------

# Sannolikhetsfördelningar för sluttid i enheten timmar
densityMen <- density((menMaraResult$Tid/3600))
densityWomen <- density((womenMaraResult$Tid/3600))


# Sannolikhetsfördelning (arean=1)
p1 <- plot(densityWomen, xlab = "Tid (h)", ylab = "Sannolikhetsfördelning", 
     main = "Stockholm marathon 2022")+
  polygon(densityWomen, col = rgb(1,0,0, alpha=0.5))+
  lines(densityMen)+
  polygon(densityMen, col = rgb(0,0,1, alpha=0.5))

  legend("topright", c("Kvinnor", "Män"), lty = 1, col = c("red", "blue"), lwd=5)

# Frekvensfördelning (skalar sannolikhetsfördelnmingen med totala antalet 
# så totala arean under kurvan = totala antalet)
# Arean under respektive kurva mellan två tidpunkter blir då lika med
# antalet tävlande som erhållit sluttider i detta intervall
p2 <- plot(densityMen$x,densityMen$y*densityMen$n, type = "l", 
           col = "blue", xlab = "Tid (h)", 
     ylab = "Frekvensfördelning", main = "Stockholm marathon 2022")+
polygon(densityMen$x,densityMen$y*densityMen$n, col = rgb(0,0,1, alpha=0.5))+
lines(densityWomen$x,densityWomen$y*densityWomen$n, type = "l", col = "red",)+
polygon(densityWomen$x,densityWomen$y*densityWomen$n, 
        col = rgb(1,0,0, alpha=1))

legend("topright", c("Kvinnor", "Män"), lty = 1, col = c("red", "blue"), lwd=5)


# PLOTT ÅLDERSKATEGORIER Kvinnor(DECENNIER) -------------------------------

# Frekvensfördelningsfunktioner för de olika ålderskategorierna
female20_rows <- womenMaraResult$Ålder >= 20 &womenMaraResult$Ålder < 30
densityF20 <- density(womenMaraResult[female20_rows,]$Tid/3600)

female30_rows <- womenMaraResult$Ålder >= 30 & womenMaraResult$Ålder < 40
densityF30 <- density(womenMaraResult[female30_rows,]$Tid/3600)

female40_rows <-  womenMaraResult$Ålder >= 40 & womenMaraResult$Ålder < 50
densityF40 <- density(womenMaraResult[female40_rows,]$Tid/3600)

female50_rows <- womenMaraResult$Ålder >= 50 & womenMaraResult$Ålder < 60
densityF50 <- density(womenMaraResult[female50_rows,]$Tid/3600)

female60_rows <- womenMaraResult$Ålder >= 60 & womenMaraResult$Ålder < 70
densityF60 <- density(womenMaraResult[female60_rows,]$Tid/3600)

female70_rows <- womenMaraResult$Ålder >= 70 & womenMaraResult$Ålder < 80
densityF70 <- density(womenMaraResult[female70_rows,]$Tid/3600)

# Plott
plot(densityF40$x,densityF40$y*densityF40$n, type = "l", col = "green", lwd=5,
     xlab = "Tid (h)", 
     ylab = "Frekvensfördelning", main = "Stockholm marathon 22 Kvinnor")
lines(densityF20$x,densityF20$y*densityF20$n, type = "l", col = "red",lwd=5)
lines(densityF30$x,densityF30$y*densityF30$n, type = "l", col = "orange",lwd=5)
lines(densityF50$x,densityF50$y*densityF50$n, type = "l", col = "cyan",lwd=5)
lines(densityF60$x,densityF60$y*densityF60$n, type = "l", col = "blue",lwd=5)
lines(densityF70$x,densityF70$y*densityF70$n, type = "l", col = "purple",lwd=5)
grid()
legend("topright", c("20-29", "30-39", "40-49", "50-59" ,"60-69", "70-79"), lty = 1, lwd=5,
       col = c("red", "orange", "green", "cyan","blue", "purple"))



# PLOTT ÅLDERSKATEGORIER MÄN(DECENNIER) -----------------------------------

# Frekvensfördelningsfunktioner för de olika ålderskategorierna
male20_rows <-  menMaraResult$Ålder >= 20 & menMaraResult$Ålder < 30
densityM20 <- density(menMaraResult[male20_rows,]$Tid/3600)

male30_rows <-  menMaraResult$Ålder >= 30 & menMaraResult$Ålder < 40
densityM30 <- density(menMaraResult[male30_rows,]$Tid/3600)

male40_rows <-  menMaraResult$Ålder >= 40 & menMaraResult$Ålder < 50
densityM40 <- density(menMaraResult[male40_rows,]$Tid/3600)

male50_rows <-  menMaraResult$Ålder >= 50 & menMaraResult$Ålder < 60
densityM50 <- density(menMaraResult[male50_rows,]$Tid/3600)

male60_rows <-  menMaraResult$Ålder >= 60 & menMaraResult$Ålder < 70
densityM60 <- density(menMaraResult[male60_rows,]$Tid/3600)

male70_rows <-  menMaraResult$Ålder >= 70 & menMaraResult$Ålder < 80
densityM70 <- density(menMaraResult[male70_rows,]$Tid/3600)

# Plott
plot(densityM30$x,densityM30$y*densityM30$n, type = "l", col = "orange", lwd=5,
     xlab = "Tid (h)", 
     ylab = "Frekvensfördelning", main = "Stockholm marathon 22 Män")
lines(densityM20$x,densityM20$y*densityM20$n, type = "l", col = "red",lwd=5)
lines(densityM40$x,densityM40$y*densityM40$n, type = "l", col = "green",lwd=5)
lines(densityM50$x,densityM50$y*densityM50$n, type = "l", col = "cyan",lwd=5)
lines(densityM60$x,densityM60$y*densityM60$n, type = "l", col = "blue",lwd=5)
lines(densityM70$x,densityM70$y*densityM70$n, type = "l", col = "purple",lwd=5)
grid()
legend("topright", c("20-29", "30-39", "40-49", "50-59" ,"60-69", "70-79"), lty = 1, lwd=5,
       col = c("red", "orange", "green", "cyan","blue", "purple"))


