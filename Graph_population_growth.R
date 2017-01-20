# ========================================================================================
# Script to create graph for forecasting population growth with prediction limits
# ========================================================================================

library(ggplot2)
# ========================================================================
# ================ Read data from .csv file
# ========================================================================
DB <- read.csv("Graphs/BothDeerAndBrushForGraphing.csv", header=TRUE)
# str(DB)
Dl.Bn <- read.csv("Graphs/NoBrushLowDeerForGraphing.csv", header=TRUE)
# str(Dl.Bn)
Dn.Bh <- read.csv("Graphs/NoDeerHighBrushForGraphing.csv", header=TRUE)
# str(Dn.Bh)

# ========================================================================
# ================ Prepare data for plotting
# ========================================================================
# function to compute means and preditction limits out of replicates for each time step
summary.CIs <- function(DF, scenario){
    # omit (time) first column DF[,-1], take each row, sort it and take values at positions 25 and 975
    lowerCI <- apply( DF[,-1], MARGIN=1, FUN=function(row) {sort(row)[25]} )
    upperCI <- apply( DF[,-1], MARGIN=1, FUN=function(row) {sort(row)[975]} )
    # omit first column DF[,-1] and compute means for each row
    means   <- rowMeans(DF[,-1])
    # function will return a data frame object
    return(data.frame(time=0:50, means, lowerCI, upperCI, scenario=scenario))
}

# compute summary stats using the function defined above
DB.CIs    <- summary.CIs(DB, scenario="Deer & brush")
Dl.Bn.CIs <- summary.CIs(Dl.Bn, scenario="Low deer, no brush")
Dn.Bh.CIs <- summary.CIs(Dn.Bh, scenario="No deer, high brush")

# rbind the summary stats in one long data frame
my.data <- rbind(DB.CIs, Dl.Bn.CIs, Dn.Bh.CIs)

write.csv(my.data, "Graphs/Summary_stats_all_scenarios.csv", row.names = FALSE)

# set the order of levels in the factor column "scenario"
# this can be important for the order of labels in legend
# however, this can be adjusted as well with the breaks() argument in scale_*() functions within ggplot()
my.data$scenario <- factor(my.data$scenario, 
                           c("Low deer, no brush", 
                             "No deer, high brush", 
                             "Deer & brush"))
# ========================================================================
# ================ Plot - with confidence bands (bw)
# ========================================================================
ggplot(data = my.data, aes(x=time, y=means, group=scenario)) +
    # plot CI-s as a ribbon arround each line;
    # initiate the filling (fill) with color/gray differently for each scenario and
    # also set an opacity value (alpha)
    geom_ribbon(aes(ymin = lowerCI, ymax = upperCI, fill=scenario), alpha=0.6) +
    # set manually the fill color for ribbon CI-s
    scale_fill_manual(name = 'Scenario',
                      values = c("Low deer, no brush"  = "gray70",
                                 "No deer, high brush" = "gray40",
                                 "Deer & brush"        = "gray0")) +
    # add the average lines on top of CI ribbons
    geom_line(aes(linetype=scenario), lwd=.8) +
    # set manually the type of line, see more types at : http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
    scale_linetype_manual(name = 'Scenario',
                          values = c("Low deer, no brush"  = "solid",
                                     "No deer, high brush" = "dotted",
                                     "Deer & brush"        = "dashed")) +
    
    # # if needed, put lines around the preditction ribbon
    # geom_line(aes(y=lowerCI), lwd=.5, linetype="dotted") +
    # geom_line(aes(y=upperCI), lwd=.5, linetype="dotted") +
    
    # Final adjustments:
    # set axis labels
    labs(x = "Years", 
         y = "Population growth") +
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
ggsave("Graphs/Forecast - CI ribbons, bw.pdf", width=5, height=4, units="in")
# save as png
ggsave("Graphs/Forecast - CI ribbons, bw.png", width=5, height=4, units="in", dpi=600)

# ========================================================================
# ================ Plot - with confidence bands (color)
# ========================================================================
ggplot(data = my.data, aes(x=time, y=means, group=scenario)) +
    # plot CI-s as a ribbon arround each line;
    # initiate the filling (fill) with color differently for each scenario and
    # also set an opacity value (alpha)
    geom_ribbon(aes(ymin = lowerCI, ymax = upperCI, fill=scenario), alpha=0.4) +
    # set manually the fill color for ribbon CI-s
    scale_fill_manual(name = 'Scenario',
                      values = c("Low deer, no brush"  = "#7570b3",
                                 "No deer, high brush" = "#1b9e77",
                                 "Deer & brush"        = "#d95f02")) +
    # add the average lines on top of CI ribbons
    geom_line(aes(color=scenario), lwd=.8) +
    # set manually the color
    scale_color_manual(name = 'Scenario',
                       values = c("Low deer, no brush"  = "#7570b3",
                                  "No deer, high brush" = "#1b9e77",
                                  "Deer & brush"        = "#d95f02")) +
    
    # # if needed, put lines around the preditction ribbon
    # geom_line(aes(y=lowerCI), lwd=.5, linetype="dotted") +
    # geom_line(aes(y=upperCI), lwd=.5, linetype="dotted") +
    
    # Final adjustments:
    # set axis labels
    labs(x = "Years", 
         y = "Population growth") +
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
ggsave("Graphs/Forecast - CI ribbons, color.pdf", width=5, height=4, units="in")
# save as png
ggsave("Graphs/Forecast - CI ribbons, color.png", width=5, height=4, units="in", dpi=600)

# ========================================================================
# ================ Plot - with confidence lines (bw)
# ========================================================================
ggplot(data = my.data) +
    # add the average lines
    geom_line(aes(x=time, y=means, linetype=scenario), lwd=.8) +
    # add "confidence" lines
    geom_line(aes(x=time, y=lowerCI, linetype="Confidence interval", group=scenario), lwd=.5, show.legend=FALSE) +
    geom_line(aes(x=time, y=upperCI, linetype="Confidence interval", group=scenario), lwd=.5, show.legend=FALSE) +
    
    # set manually the type of line, see more types at : http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
    scale_linetype_manual(name = 'Scenario',
                          breaks = c("Low deer, no brush", 
                                     "No deer, high brush", 
                                     "Deer & brush",
                                     "Confidence interval"),
                          values = c("Low deer, no brush"  = "solid",
                                     "No deer, high brush" = "twodash",
                                     "Deer & brush"        = "dashed",
                                     "Confidence interval" = "dotted")) +
    # to set the order as desired one needs to mention "breaks" in scale_linetype_manual()
    # using a factor would not work as expected!
    
    # Final adjustments:
    # set axis labels
    labs(x = "Years", 
         y = "Population growth") +
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
ggsave("Graphs/Forecast - CI lines, bw.pdf", width=5, height=4, units="in")
# save as png
ggsave("Graphs/Forecast - CI lines, bw.png", width=5, height=4, units="in", dpi=600)

# ========================================================================
# ================ Plot - with confidence lines (color)
# ========================================================================
# Plot the CI lines in legend if needed
ggplot(data = my.data) +
    # add the average lines
    geom_line(aes(x=time, y=means, color=scenario), lwd=.8) +
    # add "confidence" lines
    geom_line(aes(x=time, y=lowerCI, color=paste0('CI - ', scenario)), linetype = 'dotted', lwd=.5) +
    geom_line(aes(x=time, y=upperCI, color=paste0('CI - ', scenario)), linetype = 'dotted', lwd=.5) +
    
    # set color manually
    scale_color_manual(name = 'Scenario',
                       breaks = c("Low deer, no brush", 
                                  "No deer, high brush", 
                                  "Deer & brush",
                                  "CI - Low deer, no brush", 
                                  "CI - No deer, high brush", 
                                  "CI - Deer & brush"),
                       values = c("Low deer, no brush"  = "#7570b3",
                                  "No deer, high brush" = "#1b9e77",
                                  "Deer & brush"        = "#d95f02",
                                  "CI - Low deer, no brush"  = "#7570b3",
                                  "CI - No deer, high brush" = "#1b9e77",
                                  "CI - Deer & brush"        = "#d95f02")) +
    # to set the order as desired one needs to mention "breaks" in scale_linetype_manual()
    
    # override line type in leged so that CI lines are porperly represented
    guides(colour = guide_legend(override.aes = list(linetype = c(rep('solid',3), 
                                                                  rep('dotted',3))))) +
    
    # Final adjustments:
    # set axis labels
    labs(x = "Years", 
         y = "Population growth") +
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
ggsave("Graphs/Forecast - CI lines, color.pdf", width=5, height=4, units="in")
# save as png
ggsave("Graphs/Forecast - CI lines, color.png", width=5, height=4, units="in", dpi=600)


# ========================================================================
# This is an older & simpler trial - it doesn't plot the CI lines in legend
# CI lines in legend are not always needed as they can be easly mentioned in the caption of the figure
# ========================================================================
ggplot(data = my.data) +
    # add the average lines
    geom_line(aes(x=time, y=means, color=scenario), lwd=.8) +
    # add "confidence" lines
    geom_line(aes(x=time, y=lowerCI, color=scenario), linetype="dotted", lwd=.5) +
    geom_line(aes(x=time, y=upperCI, color=scenario), linetype="dotted", lwd=.5) +
    
    # set color manually
    scale_color_manual(name = 'Scenario',
                       breaks = c("Low deer, no brush", 
                                  "No deer, high brush", 
                                  "Deer & brush"),
                       values = c("Low deer, no brush"  = "#7570b3",
                                  "No deer, high brush" = "#1b9e77",
                                  "Deer & brush"        = "#d95f02")) +
    # to set the order as desired one needs to mention "breaks" in scale_linetype_manual()
    # using a factor would not work as expected!
    
    # Final adjustments:
    # set axis labels
    labs(x = "Years", 
         y = "Population growth") +
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
ggsave("Graphs/Forecast - CI lines, color 2.pdf", width=5, height=4, units="in")
# save as png
ggsave("Graphs/Forecast - CI lines, color 2.png", width=5, height=4, units="in", dpi=600)