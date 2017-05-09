Parameters <- read.csv("Data/AilanthusParameters4R.csv")

# Create an empty plot (type = "n") but with bounding box
plot(x=c(0,1), y=c(0,1), type = "n",
     ylab = "", ylim = c(1,3),      # no OY label and set OY custom limits
     xlab = "", xlim = c(0.8,5.2),  # no OX label and set OX custom limits (according to $Burn2)
     xaxt = "n", yaxt="n")          # supress OX & OY axis

# Add each layer one by one
control <- subset(Parameters, Trt == "Control") 
points(x=control$Burn2, y=control$Lambda, 
       type = "b", lty="dotted", lwd=1.5, pch=16)

Competitor <- subset(Parameters, Trt == "Competitor Removal")
points(x=Competitor$Burn2, y=Competitor$Lambda, 
       type = "b", lty="dashed", lwd=1.5, pch=1)

Herbivore <- subset(Parameters, Trt == "Herbivore Removal")
points(x=Herbivore$Burn2, y=Herbivore$Lambda, 
       type = "b", lty="solid", lwd=1.5, pch=15)

# add error bars
arrows(as.numeric(Parameters$Burn2), 
       Parameters$Lambda-Parameters$Lower.CI, 
       as.numeric(Parameters$Burn2), 
       Parameters$Lambda+Parameters$Upper.CI, 
       length=0.05, angle=90, code=3)

# add text on OX
mtext(c("Unburned","Burned"),side=1,line=1,at=c(1.05,5), cex=0.8, font=2)

# add OY axis
axis(2, at=c(1, 2, 3, 4), las=1, cex.axis=0.7, tck=-0.03)
title(ylab = expression(bold(paste('Population growth rate (', lambda,")"))), 
      font.lab = 2,
      line = 2.5)

abline(h=1, lty=4, col = "gray60")
text(x=0.92, y=3, "A",font=2, cex=1.1, las=1)

# add legend
legend(x="topright", text.font=1, cex = 0.8, bty="n",
       legend=c("Control", "Competitor Removal", "Herbivore Removal"),
       lty = c("dotted", "dashed", "solid"), lwd=1.5)
# legend=...            specify the labels to be displayed
# lty=...               the type of line
