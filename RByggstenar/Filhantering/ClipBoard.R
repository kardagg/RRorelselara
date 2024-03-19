# Här visas hur man kan använda datorns urklipp (clipboard) för att 
# komunicera med Excel eller Google Forms

# EXPORTERA UT FRÅN R
# Skapa data.frame df
df <- data.frame(Tid=c(1,2,3), Hastighet=c(20,25,15))

# Exporetera df till datorns clipboard
write.table(df, "clipboard", sep="\t", row.names=FALSE)

# Nu kan du klistra in df i ett Excel eller Google Forms dokument


# IMPORTERA IN TILL R
# Klipp ut ett område från Excell eller Google Forms

df2 <- read.table("clipboard", sep="\t")
print(df2)

# Om första raden är namnen på variablerna
df2 <- read.table("clipboard", sep="\t", header = TRUE)
print(df2)
