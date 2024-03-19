# Plottar medelvikt mot medellängd (+- standardavvikelser) för
# de 16 högst rankade UFC fighters 2021
# Samt en kurva som visar skalenlig tillväxt kallibrerad mot lägsta viktklass


# UFC
ufc <- read.csv2("DataFiler/UFC2021Top16.csv")

män <- ufc[ufc$Kön=="M",]
kvinnor <- ufc[ufc$Kön=="F",]

# Skalenlig tillväxt skalad mot männens flugvikt
vikt <- seq(from=0, to=120, by=1)
konst <- män$Medellängd[1]/män$Medelvikt[1]^(1/3)
längd <- konst*(vikt^(1/3))

plot(vikt, längd, type="l", xlab="Vikt (kg)", ylab="Längd (cm)", 
     main="UFC 2021 top 16 per viktklass")

points(män$Medelvikt, män$Medellängd, col="blue", pch=19)
arrows(män$Medelvikt-män$SAVikt , män$Medellängd, män$Medelvikt+män$SAVikt, 
       män$Medellängd, length=0.05, angle=90, code=3, col="blue")
arrows(män$Medelvikt , män$Medellängd-män$SALängd, män$Medelvikt, 
       män$Medellängd+män$SALängd, length=0.05, angle=90, code=3, col="blue")

points(kvinnor$Medelvikt, kvinnor$Medellängd, col="red", pch=19)
arrows(kvinnor$Medelvikt-kvinnor$SAVikt , kvinnor$Medellängd, 
       kvinnor$Medelvikt+kvinnor$SAVikt, 
       kvinnor$Medellängd, length=0.05, angle=90, code=3, col="red")
arrows(kvinnor$Medelvikt , kvinnor$Medellängd-kvinnor$SALängd, 
       kvinnor$Medelvikt, kvinnor$Medellängd+kvinnor$SALängd, 
       length=0.05, angle=90, code=3, col="red")

grid()
legend("topleft", c("Män", "Kvinnor", "Skalenlig"), 
       col=c("blue", "red","black"), pch=c(19,19,NA),
       lwd=c(NA,NA,1))
