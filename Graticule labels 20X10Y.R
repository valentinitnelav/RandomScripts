# __________ Create data.frame for graticule labels 
# use together with graticule of 10 dg step for latitude and 20 dg step for longitude
lbl.Y     <- data.frame(lon = rep(c(-175,175), each=17), # 175 dg used to fit latitude labels inside Earth; adjust this value to shift positions of labels
                        lat = rep(seq(from=80, to=-80, by=-10), times=2))
lbl.Y$dir <- ifelse(lbl.Y$lat == 0, "", ifelse(lbl.Y$lat > 0, "°N", "°S"))
lbl.Y$lbl <- paste0(abs(lbl.Y$lat), lbl.Y$dir)

lbl.X <- data.frame(lon = rep(seq(from=160, to=-160, by=-20), times=2), 
                    lat = rep(c(-85,85), each=17)) # 85 dg used to fit latitude labels inside Earth; adjust this value to shift positions of labels
lbl.X$dir <- ifelse(lbl.X$lon == 0, "", ifelse(lbl.X$lon > 0, "°E", "°W"))
lbl.X$lbl <- paste0(abs(lbl.X$lon), lbl.X$dir)
