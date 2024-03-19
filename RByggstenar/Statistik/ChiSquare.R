
# 1. En nominal variabel med två värden.-----------------------------------------------------
# Testa om värdena skiljer sig signifikant från 50% sannolikhet för vardera värde

# Observerade frekvenser
observed <- c(30, 70) # Replace with your data

# Förväntade frekvenser (om man antar 50% sannolikhet för vardera utfall)
expected <- c(50, 50) # In this case, 100 observations are assumed

# Gör Chi-2 anpassningstest (goodness of fit test)
chisq.test(x = observed, p = rep(1/length(observed), length(observed)))



# 2. Finn ett samband mellan två nominala variabler?-----------------
# Husdjursägares gentemot icke husdjursägares överlevnad ett år efter hjärtatack
 
pet_owner <- c(50, 3)  # Counts of [Alive, Dead] for Pet Owners
not_pet_owner <- c(28, 11)  # Counts of [Alive, Dead] for Non-Pet Owners

# Skapa tabell
data <- matrix(c(pet_owner, not_pet_owner), nrow = 2, byrow = TRUE,
               dimnames = list(c("Pet Owner", "Not Pet Owner"),
                               c("Alive", "Dead")))

# Gör Chi-2 test för oberoende
# Här utan Yates' continuity correction (för att det ska vara precis som i jamovi)
test_result <- chisq.test(data, correct = FALSE)

# Skriv ut statistiska results
print(test_result)

## Skriv ut observerade värden
print(test_result$observed)

## Skriv ut förväntade värden (om variablerna är oberoende)
print(test_result$expected)
