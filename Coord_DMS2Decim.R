# Inspired from http://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal
dg2dec <- function(varb, D = '°', M = '.', S = '"') {
  # use paste0("[", D, M, S, "]") to build regex [] pattern
  # therefore strsplit() will split string "varb" by what symbols you give to D, M and S (D=decimal, M=minutes and S=seconds)
  DMS <- sapply(strsplit(varb, paste0("[", D, M, S, "]")), as.integer)
  # https://en.wikipedia.org/wiki/Decimal_degrees
  return(DMS[1, ] + DMS[2, ]/60 + DMS[3, ]/3600)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
# Example
x <- read.table(text="lat     long        latN
                45°12.123' 19º05.315''  45°12.123'N
                -31°29.17' 108º15.213'' -31°29.17'N
                31°23.140' 180º45.030'' 31°23.140'N ",
                header=TRUE, stringsAsFactors=FALSE)

dg2dec(varb=x$lat, D="°", M=".", S="'")
dg2dec(varb=x$long, D="º", M=".", S="''")
dg2dec(varb=x$latN, D="°", M=".", S="'N")
