##------------------------------------------------------------
# aDGVM2 allometry and some other possibilities
# Authors Bianca and Liam 240919

# this is plant height in aDGVM2 eq. 22 in model description
height <- function(BS, b1, b2) {exp( (log(BS)+b1)/b2 )} # (m)

BS <- seq(0.1, 1000, length=1000)
b1 <- 0.45 # note in aDGVM2 model description b1 & b2 values are swapped 
b2 <- 2.6 # note in aDGVM2 model description b1 & b2 values are swapped

b1_min <- 0.4 # note in aDGVM2 model description b1 & b2 values are swapped 
b2_min <- 2.4 # note in aDGVM2 model description b1 & b2 values are swapped

b1_max <- 0.5 # note in aDGVM2 model description b1 & b2 values are swapped 
b2_max <- 2.8 # note in aDGVM2 model description b1 & b2 values are swapped

plot(BS, height(BS, b1, b2), ylim = c(0,30))
lines(height(BS, b1_min, b2_min), col="red", lwd=3) 
lines(height(BS, b1_max, b2_max), col="green", lwd=3) 

##
# this is stem diameter in aDGVM2 eq. 23 in model description
diam <- function(BS, height, WD) {2*sqrt( BS/(pi*WD*height) )}
WD <- 600 # wood density km m^-3
WD_low <- 400
WD_high <- 1000
plot(height(BS, b1, b2), diam(BS, height(BS, b1, b2), WD), ylim=c(0,0.6))
lines(height(BS, b1, b2), diam(BS, height(BS, b1, b2), WD_low), col="red", lwd=3) 
lines(height(BS, b1, b2), diam(BS, height(BS, b1, b2), WD_high), col="green", lwd=3) 
legend("topleft", c("high WD","middle WD", "low WD"), lty=c(1,1,1), lwd=c(3,3,3), col=c("green","black","red"), bty="n", cex=1.6)

##------------------------------------------------------------------
# part of the Hcrit (28) calculation we were wondering about
WD <- seq(200, 1600, length=1000)
# numbers from aDGVM2 for Hcrit calculation. Looks to match Niklas and Spatz (2010) regression equation from Fig. 2a
# but we can't find where the exact numbers come from in that paper. 
Eu <- function(WD){11.852*WD+37.} 
plot(WD, Eu(WD), ylim=c(2000,20000))
