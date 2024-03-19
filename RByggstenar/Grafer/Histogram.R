# Skapa en vektor med 100 slumpvärden mellan 0 och 10
# från en likformig sannolikhetsfördelning
värden1 <- runif(100, min=0, max=10)

# Gör ett histogram som visar fördelningen av värdena
hist(värden1, col = "red")

# Välja sannolikhetstäthet istället för frekvens
hist(värden1,col="red", freq = FALSE)

### Skapa en vektor med 100 slumpvärden från en nomralfördelning
# med medelvärdet 5 och standardavvikelsen 2
värden2 <- rnorm(100, mean=5, sd=2)

hist(värden2, col = "green")

### Plotta bägge histogrammen över varandra med genomskinliga färger
h1 <- hist(värden1, plot=FALSE)
h2 <- hist(värden2, plot=FALSE)
plot(h2, col=rgb(red=0, green=1, blue=0, alpha=0.5))
plot(h1, add=TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5))

