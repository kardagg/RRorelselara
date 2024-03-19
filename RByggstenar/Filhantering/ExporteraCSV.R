#Skapa en data.frame med Naturliga tal och Kvadrater
df <- data.frame(NaturligaTal=c(0,1,2,3,4), Kvadrater=c(0,1,4,9,16))

# Exportera data.framen df till en csv filen export.csv i mappen Filhantering
write.csv2(df, "DataFiler/Kvadrater.csv") 
# Eftersom projektet R_Rörelselära har R_Rörelselära som working directory
# är det därifrån jag måste referera adressen
# Man ska alltså inte använda en absolut sökväg (c:/...)

# Eportera utan index kolumnen
write.csv2(df, "DataFiler/Kvadrater.csv", row.names = FALSE) 

# Inspektera din exporterade fil genom att öppna den i en texteditor, 
# i Excel eller i Google Forms

# Om du vill exportera med punkt som decimaltecken,iställer för komma, 
# och komma som separator, istället för semikolon, 
# använder du write.csv istället för write.csv2

### Om du istället för en data.frame har vectorer?
naturliga <- c(0, 1,2,3, 4)
kvad <- naturliga^2

# Då måste man Skapa en data.frame 
df2 <- data.frame(NaturligaTal=naturliga, Kvadrater=kvad)

# Exportera som förut
write.csv2(df2, "DataFiler/Kvadrater.csv", row.names = FALSE) 
