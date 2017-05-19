# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examples for https://github.com/valentinitnelav/RandomScripts/blob/master/Coord_DMS2Decim.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

x <- read.table(text="DdM     DMS_lat      DMS_lon        DMS
                45°12.123'  45°12'7.38''S  45°12'7.38''W  45°12'7.38
                31°29.17'   31°29'10.2''N  31°29'10.2''E  31°29'10.2 ",
                header=TRUE, stringsAsFactors=FALSE)
x
#          DdM       DMS_lat       DMS_lon        DMS
# 1 45°12.123' 45°12'7.38''S 45°12'7.38''W 45°12'7.38
# 2  31°29.17' 31°29'10.2''N 31°29'10.2''E 31°29'10.2

# case of "degrees decimal minutes" (DdM)
dg2dec(Varb=x$DdM, Dg="°", Min="'")
# [1] 45.20205 31.48617

# case of "degrees minutes seconds" (DMS) for latitude 
# with character for Northern (N) and Southern (S) Hemispheres
# use regular expression metacharacter | to indicate logical operation "OR"
dg2dec(Varb=x$DMS_lat, Dg="°", Min="'", Sec="''S|N")
# [1] -45.20205  31.48538

# case of "degrees minutes seconds" (DMS) for longitude 
# with character for Eastern (E) and Western (W) Hemispheres
dg2dec(Varb=x$DMS_lon, Dg="°", Min="'", Sec="''E|W")
# [1] -45.20205  31.48538

# case of "degrees minutes seconds" (DMS) without seconds symbol. 
# Note that this returns only positive coordinates!
dg2dec(Varb=x$DMS, Dg="°", Min="'")
# [1] 45.20205 31.48538

# Data table way:
library(data.table)
x.dt <- data.table(x) # transform data frame "x" to data table "x.dt"
# create new column "DMS_dec" by applying dg2dec() function on column "DMS"
x.dt[, DMS_dec := dg2dec(DMS, Dg="°", Min="'")]
x.dt # check results
#           DdM       DMS_lat       DMS_lon        DMS  DMS_dec
# 1: 45°12.123' 45°12'7.38''S 45°12'7.38''W 45°12'7.38 45.20205
# 2:  31°29.17' 31°29'10.2''N 31°29'10.2''E 31°29'10.2 31.48538