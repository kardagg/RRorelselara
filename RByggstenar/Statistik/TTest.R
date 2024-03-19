# T-test ---------------------------------

# Läs in datafil med kastlängder med parade kast med heliumn respektive luft i bollen
data <- read.csv2("DataFiler/FM_Statistik_Filer/Heliumboll.csv", header=TRUE)

# Parad t-test med ensidig mothypotes: helium>Luft
t.test(data$Helium, data$Luft, alternative = "greater", paired = TRUE)

# Parad t-test med tvåsidig mothypotes
t.test(data$Helium, data$Luft, alternative = "two.sided", paired = TRUE)

# Oparad t-test med tvåsidig mothypotes
t.test(data$Helium, data$Luft, alternative = "two.sided", paired = FALSE)




# Ickeparametriska varianter ----------------------------------

# Parad - Wilcoxon tecken rangtest
wilcox.test(data$Helium, data$Luft, alternative = "two.sided", paired = TRUE)

# Oparad -  Mann-Whitney U test
wilcox.test(data$Helium, data$Luft, alternative = "two.sided", paired = FALSE)
