# Verktyg för vektoralgebra

# Skalärprodukt
kdDot <- function(v1,v2){
  result=0
  for(i in 1:length(v1)){
    result=result+v1[i]*v2[i]
  }
  return(result)
}


# Rotationsmatris kring x-axel
kdRotMatrixX <- function(vinkel){
  matrix(c(1, 0, 0, 0, cos(vinkel), sin(vinkel), 0, -sin(vinkel), cos(vinkel)),
         nrow=3)
}

# Rotationsmatris kring y-axel
kdRotMatrixY <- function(vinkel){
  matrix(c(cos(vinkel), 0, -sin(vinkel), 0, 1, 0, sin(vinkel), 0, cos(vinkel)),
         nrow=3)
}

# Rotationsmatris kring z-axel
kdRotMatrixZ <- function(vinkel){
  matrix(c(cos(vinkel), sin(vinkel), 0, -sin(vinkel), cos(vinkel), 0, 0, 0, 1),
         nrow=3)
}

# Vrider med euler vinklar först kring y sen kring x och sist kirn z 
# i lokala koordinater (omvänd ording för globala)
kdEulerRotationZXY <- function(vektor, xVinkel,yVinkel,zVinkel){
  kdRotMatrixZ(zVinkel)%*%kdRotMatrixX(xVinkel)%*%kdRotMatrixY(yVinkel)%*%vektor
}


      