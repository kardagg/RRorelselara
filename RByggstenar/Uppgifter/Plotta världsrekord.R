# Plotta världrekorden i simning från filen DataFiler/Simrekord100m.csv

# Läs in filen

sr<- read.csv2("DataFiler/Simrekord100m.csv")

# Skriv ut
print(sr)

# välj ut kvinnor och män var för sig

kvinnor <- sr[sr$Kön=="F", ]
män <- sr[sr$Kön=="M", ]

# Plotta kvinnornas rekord 

plot(kvinnor$År, kvinnor$Tid, xlab="År", ylab="Tid (s)", col="red", pch=19)

# Addera männens resulat till plotten

points(män$År, män$Tid, col="blue", pch=19)

# Ange axlarnas omfång
# xlim=c(min(sr$År),max(sr$År)), ylim=c(min(sr$Tid),max(sr$Tid)), 

# Lägg till runätslinjer

grid()

#Lägg till teckenförklaring

legend("topright", c("Kvinnor", "Män"), col=c("Red","Blue"), pch=c(19,19))


