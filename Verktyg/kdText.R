# Hjälpfunktioner för att jobba med text och char vectors


# Omvandlar en sträng av typen dagar:timmar:minuter:sekunder (typ "3:2:20:35")
# till nunmeriskt antal sekunder
kdTidTextTillNumSekunder <- function(string){
  # Remove all non numeric exept :
  str <- gsub("[^0-9.-:]", "",string) #[^0-9.-:] = regular expression, not 0-9 or - or :
  # strsplit returns list of vector with parts, so unlist gets the vector
  strParts <- unlist(strsplit(str,":")) 
  # Multiplicera delarna och addera
  t <- switch(length(strParts),
              as.numeric(strParts[[1]]), #1
              sum(c(as.numeric(strParts[[1]]),
                    as.numeric(strParts[[2]]))*c(60,1)), #2
              sum(c(as.numeric(strParts[[1]]),
                    as.numeric(strParts[[2]]),
                    as.numeric(strParts[[3]]))*c(3600,60,1)), #3
              sum(c(as.numeric(strParts[[1]]),
                    as.numeric(strParts[[2]]),
                    as.numeric(strParts[[3]]),
                    as.numeric(strParts[[4]]))*c(86400,3600,60,1)) #4
  )
}