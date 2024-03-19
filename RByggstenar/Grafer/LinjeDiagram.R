# Skapa en lista (vector) x med värden i steg 0.01 från 0 till 1 
x <- seq(from = 0, to = 1, by = 0.01)

# SKapa en lista f med värden på kvadratroten av x 
f <- sqrt(x)

# Gör ett linediagram (line plot) 
# med x värden på x axeln och f värden på y axeln
plot(x, f, col="red", type = "l")
## Notera att type är lilla L och inte siffran 1

## Ändra text (labels) på axlar och lägg till en överskrift
plot(x, f, col="red", xlab ="x", ylab = "y", type="l", main="Linjeplott" )

## Skapa g=1-f
g <- 1-f

## Addera g till grafen och välj en annan symbol
## I Plot.docx i Info mappen finns en bild med tillgängliga symboler
lines(x, g, col="blue" )

## Addera gridlines
grid () 

## Addera en teckenförklaring (legend)
legend("top", c("f=sqrt(x)", "g=1-f"), lty = 1, col = c("red", "blue"))