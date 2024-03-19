# Gör en tidsserie med en sinusvåg samplad med en samplingsfrekvens på 100Hz

# Argumenten i de trigonometriska funktioerna är alltid radianer
# Ett varv = 2*pi rad = 360 grader
# Omvandling grader till radianere: pi/180
# Omvandling radianer till grader : 180/pi

sin(45*pi/180)

sin(pi/4)

# Tilldela variablen samplingsFrekvens värdet 100

samplingsFrekvens <- 100

# Skapa en vektor t med tidpunkter från 0 till 2 sekunde
# med steget 1/samplingsFrekvens (=0.01 sekunder)

t <- seq(from  =0, to = 2, by = 1/samplingsFrekvens)

# Skriv ut t för att kolla at du fått de värden du tänkt dig

print(t)

# I R kommer matematiska operationer på en vektor att verka
# på varje element i vektorn

2*t

# Skapa en sinusvåg (sv) med frekvensen 1 (en fullständig rotation per sekund)

sv <- sin(2*pi*t)

# Plotta sv (vi tittar i RByggstenar/Grafer/Punktdiagram.r för att se hur)

