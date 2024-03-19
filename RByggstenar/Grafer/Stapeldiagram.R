# Skapa fiktiva data
data <- c(25, 40, 60, 80)

# Namnge varje stapel
namn <- c("Grupp A", "Grupp B", "Grupp C", "Grupp D")

# Skapa stapeldiagram
barplot(data,
        names.arg = namn,
        main = "Stapeldiagram",
        xlab = "Grupper",
        ylab = "Värden",
        col = "lightblue",
        ylim = c(0, max(data) + 10))

## Addera felstaplar (som exempelvis skulle kunna representera standardavvikelse)


# Skapa felstaplar (standardfel, standardavvikelse, etc.)
errors <- c(5, 7, 6, 8)  # Exempelvärden för fel

# Skapa stapeldiagram
bp <- barplot(data,
              names.arg = namn,
              main = "Stapeldiagram med felstaplar",
              xlab = "Grupper",
              ylab = "Värden",
              col = "lightblue",
              ylim = c(0, max(data + errors)))

# Lägg till felstaplar
arrows(x0 = bp, y0 = data - errors, 
       x1 = bp, y1 = data + errors, 
       angle = 90, code = 3, length = 0.1)
