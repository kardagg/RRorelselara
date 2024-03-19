# Data randomiserad från normalfördelning
data <- rnorm(30, mean=10, sd=2) 

# Shapiro wilk test för normalfördelning 
shapiro.test(data)

hist(data)

# Läs in datafil med kastlängder med parade kast med heliumn respektive luft i bollen
data <- read.csv2("DataFiler/FM_Statistik_Filer/Heliumboll.csv", header=TRUE)

# Shapiro wilk test för normalfördelning 
shapiro.test(data$Skillnad)
