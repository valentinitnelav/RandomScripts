extract2near_ncdf <- function(ncdf.path, 
                              vars, 
                              XYT, 
                              my.buffer, 
                              ncdf.crs = NA)
{
    # -----------------------------------------------------------------------------------------------------------------------
    # [Note: wok in progress...]
    # Function to extract the closest valid cell values from NetCDF file.
    # For each unique combination of NetCDF variable (as given in "vars") and time (as given in the 3rd column of "XYT" table), 
    # a raster is read from the NetCDF file and then a buffer extraction takes place.
    # Extractions are then used to join with the given XYT using X,Y,T columns as compound key.
    # For all points that the usual raster::extract() returns NA,
    # it searches for the closest non-NA cell value within given buffer (typically meters).
    # ___ Arguments
    # ncdf.path  - Path to NetCDF file
    # vars       - NetCDF variables; e.g. names(nc_open(ncdf.path)$var)
    # XYT        - data.table with three columns: first column=longitude; second column=latitude; third column=time (e.g. years)
    # my.buffer  - same as in raster::extract(); "The radius of a buffer around each point from which to extract cell values. [...]
    #              If the data are not projected (latitude/longitude), the unit should be meters. 
    #              Otherwise it should be in map-units (typically also meters)".
    #              buffer needs to be at least the raster's resolution (one cell)
    # ncdf.crs   - CRS of NetCDF file. This is usually red by the raster() function, 
    #              but you can give it manually if this fails to be red from the NetCDF file.
    # ___ Returns
    # data.table object with several columns. 
    # First two columns are the longitude (X.lon) & latitude (Y.lat) coordinates.
    # Columns "T.time" & "T.idx" & store the year/time and the corresponding index of the year/time inside the NetCDF file.
    # Column "cells" refers to the raster cell ID where the point falls.
    # Column "buf" indicates if a buffer extraction was used or not (1=Yes, NA=No).
    # A column for each NetCDF variable is created.
    # -----------------------------------------------------------------------------------------------------------------------
    
    # ---------------------------------------
    # Some input checking
    # ---------------------------------------
    # Are the given variables within expected NetCDF variables, or is there some typo?
    ncFile <- ncdf4::nc_open(ncdf.path)
    is.var.ok <- vars %in% names(ncFile$var)
    ncdf4::nc_close(ncFile)
    if ( ! all(is.var.ok) )
        stop("Following netcdf variable names are not correct: \n",
             paste(vars[!is.var.ok], collapse = ", "),
             "\n Expecting values from the following list: \n",
             paste(names(ncFile$var), collapse = ", "))
    # Test if XYT is a 3 column data.table
    if (!is.data.table(XYT) && dim(XYT)[2] != 3)
        stop("Expecting XYT to be a a) data.table, b) with 3 columns. Please provide a 3-columns data.table object")
    # Check if the data.table’s columns are numeric
    # The user should take care of the numeric conversion because in this way it can discover possible unwanted values.
    is.col.num <- sapply(XYT, is.numeric)
    if ( ! all(is.col.num) )
        stop("In XYT, column(s): \n",
             paste(colnames(XYT)[!is.col.num], collapse = ", "), 
             "\n not numeric! Please convert them to numeric.")
    # Check if the buffer is numeric
    # The user should take care of the numeric conversion because in this way it can discover possible unwanted values.
    if (!is.numeric(my.buffer))
        stop("Expecting numeric buffer (in meters)")
    
    # ---------------------------------------
    # Prepare the unique combinations variables-time
    # ---------------------------------------
    # this is valid when time steps are not exactly the years, but a corresponding index.
    # get time steps from netCDF object
    time.steps <- data.table(steps = ncFile$dim$time$vals) # same as 0:(ncFile$dim$time$len-1)
    # get metadata (units) for the time dimension
    time.info  <- ncFile$dim$time$units 
    # retrieve the Gregorian first year from time.info string variable
    first.year <- as.integer(
        regmatches(x = time.info, 
                   m = regexpr(pattern = "[0-9]+", 
                               text    = time.info))
    )
    # build the real years (time dimension)
    time.steps[, T.time := steps + first.year]
    setkey(time.steps, T.time)
    # finally get the index of the year.
    setnames(XYT, c("X.lon","Y.lat","T.time")) # rename columns
    # match on T.time; NA will be returned in T.idx for a no match.
    XYT[time.steps, on = .(T.time), T.idx := steps]
    # same as above
    # years.ncdf <- steps + first.year
    # XYT[, T.idx := as.integer(ifelse(T.time %in% years.ncdf, which(years.ncdf==T.time), NA)), by = 1:nrow(XYT)]
    
    # Prepare the unique combinations variables-time
    time.var <- expand.grid(T.idx = XYT[!is.na(T.idx), sort(unique(T.idx))], 
                            vars  = vars)
    
    # ---------------------------------------
    # Extract from NetCDF file
    # ---------------------------------------
    # Apply extraction warpper function on two vectors – time and variables
    # For each unique combination of NetCDF variable (as given in "vars") and time (as given in the 3rd column of "XYT" table), 
    # a raster is read from the NetCDF file and then a buffer extraction takes place using extract2near() function.
    start.time <- Sys.time()
    extr.lst <- mapply(
        function(ti,var)
        {
            # Read raster from NetCDF for given time and variable.
            # The CRS is usually red by the raster() function, 
            # but try to assign it if it fails to be red from the NetCDF file.
            rst <- raster(x       = ncdf.path,
                          band    = ti,
                          varname = var, 
                          crs = if (!is.na(ncdf.crs)) CRS(ncdf.crs) else NA)
            # buffer extraction: call extract2near with messages supressed
            # XY takes only unique pairs of coordinates as not to extract several times for the same pair of coordinates
            vls <- suppressMessages(extract2near(rst = rst, 
                                                 XY  = unique(XYT[T.idx == ti,.(X.lon, Y.lat)]),
                                                 my.buffer  = my.buffer,
                                                 simplified = FALSE,
                                                 lib.check  = FALSE))
            # In the extraction data table create two new columns that will keep track of the time and variables
            vls[, ':=' (T.idx = ti,
                        varb  = var)]
            return(vls)
        },
        # The main warpper function above is applied on these vectors:
        t = time.var$T.idx,
        v = as.character(time.var$vars),
        # Do not simplify; Force to return a list (of data.tables).
        SIMPLIFY = FALSE)
    Sys.time() - start.time
    
    # # For debuging reasons
    # options(warn=2)  # treat warnings as errors
    # for (ti in time.var$T.idx){
    #     for (var in as.character(time.var$vars)){
    #         test.rst <- raster(x       = ncdf.path,
    #                            band    = ti,
    #                            varname = var)
    #         test.extr <- suppressMessages(extract2near(rst = test.rst,
    #                                   XY  = unique(XYT[T.idx == ti, .(X.lon, Y.lat)]),
    #                                   my.buffer  = my.buffer,
    #                                   simplified = FALSE,
    #                                   lib.check  = FALSE))
    #     }
    # }
    # options(warn=1) # return to "factory mode"
    
    # ---------------------------------------
    # Manage final results
    # ---------------------------------------
    extr.dt <- data.table::rbindlist(extr.lst)
    
    # rename column 4
    data.table::setnames(extr.dt, 
                         old = 4, 
                         new = "extracted.val")
    
    # trnasform from long to wide format
    extr.dt.wide <- dcast(extr.dt, 
                          X.lon + Y.lat + cells + buf + T.idx ~ varb, 
                          value.var = "extracted.val")
    # join with XYT
    extr.res <- merge(x  = XYT, 
                      y  = extr.dt.wide, 
                      by = c("X.lon", "Y.lat", "T.idx"), 
                      all.x = TRUE, 
                      sort  = FALSE)
    return(extr.res)
}