# Läs in data 
data <- read.csv2("DataFiler/FM_Statistik_Filer/Alkoholhalt.csv", header=TRUE)

# Pearsons korrelationskoefficient 
# mellan antal druckna vinglas per kg kroppsvikt och alkoholhalt i blodet (BAK)
pearsonCorrelation <- cor(data$GlasPerKg,data$BAK)

# Skriv ut resultatet
cat("Pearsons korrelationskoefficient = ", pearsonCorrelation)

# Testa om pearson correlationskefficient skiljer sig signifikant från 0
cor.test(data$GlasPerKg,data$BAK) 


# Determinationskoefficient
determinationskoefficient <- pearsonCorrelation^2
# Skriv ut resultatet
cat("Determinationskoefficient = ", determinationskoefficient)

# Spearmans korrelationskoefficient
spearmanCorrelation <- cor(data$GlasPerKg,data$BAK, method = "spearman")

# Skriv ut resultatet
cat("Spearmans korrelationskoefficient = ", spearmanCorrelation)

# Testa om spearman correlationskefficient skiljer sig signifikant från 0
cor.test(data$GlasPerKg,data$BAK,  method = "spearman") 
