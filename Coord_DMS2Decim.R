dg2dec <- function(Varb, Dg=NA, Min=NA, Sec=NA, SW.Hemisphere="S|W")
{
    # ----------------------------------------
    # Function for converting coordinates from "DMS" or "DdM" formats to decimal format.
    # DMS = "degrees minutes seconds"; 
    # DdM = "degrees decimal minutes"
    # Author: Valentin Stefan; https://github.com/valentinitnelav
    # ----------------------------------------
    # ___ Arguments
    # Varb          = character vector of DMS coordinates,
    # Dg            = Character used for indicating degrees, 
    # Min           = Character used for indicating minutes, 
    # Sec           = Character used for indicating seconds; 
    # SW.Hemisphere = Character used for indicating the southern and western hemisphere in DMS format
    #                 (they need to be adjusted for the negative sign).
    #                 The default is S or W, but if other characters are used, then they need to be specified.
    # ___ Returns
    # Numeric vector with decimal degrees
    
    # NOTES:
    # If the format is "degrees decimal minutes" - "DdM" (e.g. 40° 26.767′ N) and not 
    # "degrees minutes seconds" - "DMS" (e.g. 40° 26′ 46″ N), then call the function only with 
    # Dg and Min arguments, like dg2dec(Varb, Dg="°", Min="′N"). 
    # Same applies when there is no seconds symbol (e.g. 45°12'7.38).
    # Note that one should not use blank spaces in Dg, Min or Sec arguments (will return NA).
    # For more details on formats see: https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#Coordinate_format_conversion
    
    # Use paste0("[", Dg, Min, Sec, "]") to build regex [] pattern
    # therefore, strsplit() will split string "Varb" by what symbols you give to Dg, Min, Sec
    DMS <- sapply(strsplit(Varb, paste0("[", Dg, Min, Sec, "]")), as.numeric)
    
    # DMS is a matrix; first row contains degrees; second - minutes; third - seconds.
    # If the format is "degrees decimal minutes" (e.g. 40° 26.767′ N) and not 
    # "degrees minutes seconds" (e.g. 40° 26′ 46″ N), then the matrix has only two valid rows: 
    # first row contains degrees; the second - minutes;
    # therefore, compute conversion for seconds only if there are more than 2 rows in DMS 
    # and Sec is different from NA (if there are seconds in the DMS format)
    decdg <- abs(DMS[1, ]) + DMS[2, ]/60 + ifelse(dim(DMS)[1] > 2  & !is.na(Sec), DMS[3, ]/3600, 0)
    
    # all coordinates from Southern or Western Hemispheres become negative in their decimal format
    SW <- grepl(pattern = SW.Hemisphere, x = Varb, ignore.case = TRUE)
    return(ifelse(SW, -1, 1) * decdg)
}

# References:
# http://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal
# https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#Coordinate_format_conversion
# https://en.wikipedia.org/wiki/Decimal_degrees
# tested conversion with http://www.pgc.umn.edu/tools/conversion
