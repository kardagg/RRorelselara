# Läs in datafil med iris data
iris_data <- read.csv2("DataFiler/FM_Statistik_Filer/Iris.csv", header=TRUE)

# Variansanalys test
anova_result <- aov(Petal.Length ~ Species, data = iris_data)
summary(anova_result)

# Post hoc test med Tukey's HSD om variansanalysen var signifikant
posthoc_result <- TukeyHSD(anova_result)
print(posthoc_result)

# Om man vill använda Games-Howeells test (olika varians)
# måste rstatix packetet importeras

library(rstatix)

posthoc_result <- games_howell_test(iris_data, Petal.Length ~ Species, 
                                    conf.level = 0.95, detailed = FALSE)
print(posthoc_result)
