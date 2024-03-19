# Skapa en lista (vector) x med värden i steg 0.1 från -1 till 1 
x <- seq(from = -1, to = 1, by = 0.1)

# SKapa en lista med värden x*x
f <- x*x

# Gör ett punktdiagram (scatter plot) 
# med x värden på x axeln och f värden på y axeln
plot(x, f, col="red")

## Ändra text (labels) på axlar och lägg till en överskrift
plot(x, f, col="red", xlab ="x", ylab = "y", main="Punktplott" )

## Skapa g=1-f
g <- 1-f

## Addera g till grafen och välj en annan symbol
## I Plot.docx i Info mappen finns en bild med tillgängliga symboler
points(x, g, col="blue", pch=19)

## Addera gridlines
grid (NULL,NULL) 

## Addera en teckenförklaring (legend)
legend("topright", c("f=x∙x", "g=1-f"), pch = c(1, 19), col = c("red", "blue"))