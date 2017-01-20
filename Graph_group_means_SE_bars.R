# ========================================================================================
# Script to create graph for group means with SE bars.
# SE is the standard deviation (SD) of each group 
# divided by the square root of the number of observation within each group.
# ========================================================================================

library(data.table)
library(ggplot2)

# ========== Read data from .csv file ========== #
DT <- fread("Graphs/astfur1yr.csv", header=TRUE)
str(DT)

# ========== Aggregate (data.table way) ========== #
DT.means <- DT[, .(AVG=mean(r.t.), 
                   SD=sd(r.t.),
                   Freq=.N,
                   SE=sd(r.t.)/sqrt(.N)), 
               by=.(woodyComb, DeerComb)]
# Set the grouping column as factor with given order of levels
# this can be important for the order of labels in legend
# however, this can be adjusted as well with the breaks() argument in scale_*() functions within ggplot()
DT.means[, DeerComb.fct := factor(DeerComb, c("none","low","high"))]

# ========== Plot ========== #
# The errorbars will overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(data=DT.means, aes(x = woodyComb, 
                          y = AVG, 
                          group    = DeerComb.fct, 
                          linetype = DeerComb.fct, 
                          shape    = DeerComb.fct, 
                          colour   = DeerComb.fct)) +
    # add horizontal line at zero on OY
    geom_abline(intercept = 0, slope = 0, linetype="dotted", color="gray", lwd=.3) +
    
    # add the lines; "lwd" adjust line width as in plot()
    geom_line(lwd=.9, position=pd) + 
    # set type of line, see more types at : http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
    scale_linetype_manual(name='Deer browse level', 
                          values=c("none"="dashed", "low"="longdash", "high"="solid")) +
    
    # add the points (means); "size" adjusts the points size
    geom_point(size=2, position=pd) +
    # set type of point shape, see more at : http://sape.inf.usi.ch/quick-reference/ggplot2/shape
    # note that the name should be as well 'Deer browse level' so that ggplot combines points and lines in the same legend
    scale_shape_manual(name='Deer browse level', 
                       values=c("none"=15, "low"=16, "high"=17)) + 
    
    # set colors - change all colors to black if you don't need colors (see commented code line below)
    # for a wiser choice of colors check http://www.colorcombos.com/combolibrary.html
    # or http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
    scale_colour_manual(name='Deer browse level', values=c("none"="#1b9e77", "low"="#7570b3", "high"="#d95f02")) +
    # scale_colour_manual(name='Deer browse level', values=c("none"="green", "low"="blue", "high"="red")) +
    # scale_colour_manual(name='Deer browse level', values=c("none"="black", "low"="black", "high"="black")) +
    
    # plot error bars based on computed standard error of each group
    geom_errorbar(aes(ymax=AVG+SE, ymin=AVG-SE), width=.1, linetype="solid", position=pd) +
    
    # set order of discrete values on OX axes and adjust the gap between the marginal points and the OY axes
    scale_x_discrete(limits=c("none","low","high"), expand=c(0,0.2)) +
    
    # Final adjustments:
    # set axis labels
    labs(x = "Woody brush level", 
         y = "Average growth rate") +
    theme_bw() + # eliminate default backgound 
    theme(panel.grid.major = element_blank(), # eliminate major grids
          panel.grid.minor = element_blank(), # eliminate minor grids
          # set font family for all text within the plot ("serif" should work as "Times New Roman")
          # note that this can be overridden with other adjustment functions below
          text = element_text(family="serif"),
          # adjust text in X-axis label
          axis.title.x = element_text(size = 12, face = "bold"),
          # adjust text in Y-axis label
          axis.title.y = element_text(size = 12, face = "bold"),
          # adjust legend title appearance
          legend.title = element_text(size = 10, face = "bold"),
          # adjust legend label appearance
          legend.text = element_text(size = 10),
          # don't draw legend box (check element_rect() for borders and backgrounds)
          legend.background = element_blank(),
          # Put upper-right corner of legend box in upper-right corner of graph
          # Note that the numeric position in legend.position below is relative to the entire area, 
          # including titles and labels, not just the plotting area
          legend.justification = c(1,1),
          legend.position = c(1,1))

# save as pdf
ggsave("Graphs/Group means.pdf", width=5, height=4, units="in")
# save as png
ggsave("Graphs/Group means.png", width=5, height=4, units="in", dpi=600)

# another option for black & white:
# omit colour=DeerComb.fct in aes of ggplot()
# omit also scale_colour_manual() as there will be no color to deal with

# ##################
# try a simple draft
# ggplot(data=DT.means, aes(x=woodyComb, y=AVG, colour=DeerComb, group=DeerComb)) +
#     geom_line(aes(linetype=DeerComb), size=1.2) + 
#     geom_point(aes(shape=DeerComb), size=3) + 
#     geom_errorbar(aes(ymax=AVG+SE, ymin=AVG-SE), width=.1) +
#     theme_bw()
