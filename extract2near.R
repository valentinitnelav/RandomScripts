extract2near <- function(rst, 
                         XY, 
                         my.buffer, 
                         simplified = TRUE, 
                         lib.check  = TRUE){
    # ----------------------------------------
    # [Note: wok in progress...]
    # Function to extract the closest valid cell values from raster.
    # Specifically, for all points that the usual raster::extract() returns NA,
    # it searches for the closest non-NA cell value within given buffer (typically meters).
    # All other points returned by the usual raster::extract() are kept as such.
    # ___ Arguments
    # rst        - raster object
    # XY         - data.table with two columns: first column=longitude; second column=latitude
    # my.buffer  - same as in raster::extract(); "The radius of a buffer around each point from which to extract cell values. [...]
    #              If the data are not projected (latitude/longitude), the unit should be meters. 
    #              Otherwise it should be in map-units (typically also meters)".
    #              buffer needs to be at least the raster's resolution (one cell)
    # simplified - Logical argument. TRUE means that the result is a data.table with only one column - the extracted raster values.
    #              If FALSE, then 5 extra columns are added: 
    #               the coordinates (2 columns), 
    #               the ID of the raster cells where the points fall, 
    #               the raster cell values and 
    #               a column that indicates for each point if a buffer was used (NA means NO, 1 means YES)
    # ___ Returns
    # data.table object (check "simplified" argument above)
    # ----------------------------------------
    start.time <- Sys.time()
    
    # Do some library checking
    if (isTRUE(lib.check)){
        if (!requireNamespace("RANN", quietly = TRUE)) 
            stop("Please install package {RANN}")
        if (!requireNamespace("data.table", quietly = TRUE)) 
            stop("Please install package {data.table}")
        if (!requireNamespace("raster", quietly = TRUE)) 
            stop("Please install package {raster}")
    }
    
    # Inform about the CRS of the raster
    message("Please always check if the raster and the points have the same CRS \n",
            "The CRS of the given raster is: \n", rst@crs, "\n")
    
    # Extract cell value & ID at given XY point
    ext <- data.table(raster::extract(x = rst, y = XY, cellnumbers = TRUE, method = 'simple'))
    
    # If there are NA cell values, then search for nearest non-Na cell value within buffer's range
    NA.idx <- is.na(ext[[2]])
    if ( all(NA.idx) ) stop("All points returned NA! \n - most probably the given raster and points have different CRS")
    if ( any(NA.idx) ) {
        packageStartupMessage(sum(NA.idx), " points outside raster coverage encountered \n - applying buffer extraction ...")
        # get records where the extraction above returned NA-s
        ext.NA <- ext[NA.idx,]
        # get corresponding point coordinates from XY
        XY.NA <- XY[NA.idx,]
        
        # extract now a buffer of cells around these "NA" points
        # will get a list of matrices with raster cell values and cell IDs around each "NA" point
        # NOTE: buffer extraction (line below) is the most time consuming operation in all this process
        ext.bf <- raster::extract(x = rst, y = XY.NA, cellnumbers = TRUE, buffer = my.buffer, method = 'simple')
        
        # For speeding up, one idea would be to not process the entire buffer extraction if all neighbors return NA.
        # but need to take care of the return structure
        # is.bf.NA <- sapply(ext.bf, function(X) all(is.na(X[,2])))
        # if (all(is.bf.NA))
        
        # some of these neighboring cells might have as well NA values, therefore,
        # get coordinates of the centers of raster cells only for non-NA cells in each buffer (matrix)
        ext.bf.noNA.XY <- lapply( ext.bf, 
                                  function(m) {
                                      # for each buffer matrix in the list of matrices ext.bf
                                      # exclude all rows with NA cell values (in 2nd column)
                                      # The drop = FALSE option is used defensively, see more at: 
                                      # https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-do-my-matrices-lose-dimensions_003f 
                                      m  <- m[!is.na(m[,2]),, drop = FALSE]
                                      # get coordinates of the center of each raster cell
                                      xy <- raster::xyFromCell(object = rst, cell = m[,1])
                                      # bind the cleaned matrix "m" with the matrix of coordinates "xy"
                                      cbind(xy,m)} )
        
        # Knowing coordinates of the center of non-Na buffer cells and knowing coordinates of each original point,
        # then find out the nearest non-NA buffer cell
        
        # initiate a vector that will contain nearest neighbor values
        # NOTE: initiate with NA (this is safe in case there is absolutely NO nearest valid neighbor cell within given buffer)
        # also, pre-allocating memory should be more efficient/fast for bigger data
        neighbors <- rep(NA, times = length(ext.bf.noNA.XY))
        
        # for each buffer (matrix):
        for (i in 1:length(ext.bf.noNA.XY)){
            my.buffer <- ext.bf.noNA.XY[[i]]
            # avoid cases when some buffers are empty (check if buffer matrix has no rows)
            # (it happens when buffer radius is not big enough and can't find any non-NA cell value)
            if (dim(my.buffer)[1] != 0) {
                # use function nn2 {RANN} to find nearest neighbor (and corresponding distance)
                NN <- RANN::nn2(data = my.buffer[,1:2], query = matrix(XY.NA[i,], ncol=2), k = 1)
                # get value of nearest neighbor cell
                neighbors[i] <- my.buffer[NN$nn.idx,4]
            }
        }
        
        # replace NA cell values from first extraction with nearest neighbor cell values
        # If all neighbors are still NA, then no need to overwrite with NA (as NA is already in place in 2nd column of ext)
        if (!all(is.na(neighbors))) ext[NA.idx, 2 := neighbors]
        
        packageStartupMessage("...buffer extraction completed!")
    } else message("All points were inside raster coverage - extracting as usual, NO buffer extraction needed")
    # end of big IF
    
    # Return only extracted raster values or, optionally, add some extra columns
    if ( simplified ) {
        message("\n Elapsed/CPU time: ", format(round(Sys.time() - start.time, digits = 2)))
        return(ext[,2])
    } else {
        # Adds column that indicates for each point if a buffer was used. NA means NO, 1 means YES
        ext[NA.idx, buf := 1L]
        message("\n Elapsed/CPU time: ", format(round(Sys.time() - start.time, digits = 2)))
        return(cbind(XY, ext))
    }
}

# If you change the buffer size, then be aware of the followings:                                              
# The buffer needs to be at least the raster's resolution (one cell).
# If the raster's resolution is smaller than the buffer then use the buffer,
# else use as buffer the raster's resolution - sample code below:

# extr.lst <- lapply(agg.rst_lst, 
#                    function(r) extract2near(rst = r, 
#                                             XY  = XY.prj, 
#                                             my.buffer = ifelse(res(r)[1] < my.buf, 
#                                                               my.buf, 
#                                                               res(r)[1])))