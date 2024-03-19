# Skapa slumpmässig data 
# 100 värden från en normalfördelning med medelvärde 0 och standardavvikelse 1
data <- rnorm(100)

# Skapa ett lådagram
boxplot(data, 
        main = "Lådagram", 
        ylab = "Värden", 
        col = "lightblue",
        notch = FALSE)

# Jämför fördelningen av två variabler
# Skapa två separata datamängder
data1 <- rnorm(100, mean = 10, sd = 2)
data2 <- rnorm(100, mean = 15, sd = 3)

# Skapa boxplot
boxplot(list(data1, data2), 
        names = c("Data 1", "Data 2"),
        main = "Två Boxplot Bredvid Varandra",
        ylab = "Värden",
        col = c("lightblue", "lightgreen"))

## Addera gridlines
grid () 
