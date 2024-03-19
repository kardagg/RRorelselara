# Läs in data 
dat <- read.csv2("DataFiler/FM_Statistik_Filer/Alkoholhalt.csv", header=TRUE)

# Linjär regressionsanalys
model <- lm(BAK ~ GlasPerKg, data = dat)

# Skriv ut resultat
summary(model) 

# Plotta regressionslinjen i punktdiagrammet
plot(dat$GlasPerKg, dat$BAK, pch = 16, col = "blue") 
abline(model) #Add a regression line
