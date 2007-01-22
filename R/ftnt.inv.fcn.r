ftnt.inv.fcn <- function (y, a=1) {

#   Calculates the inverse distribution function for tent distribution.
#   Parameter a is height of tent wall,  must be between 0 and 1
#   Default value of 1 gives uniform distribution.
#
if(a == 1) return(y)
isn<-ac <- bc <- cc <- numeric(length(y))
h <- 2*(1-a)
        
      gp <- abs(y- .25) < .25
        if(any(gp)) {
                ac[gp] <- h
                bc[gp] <- a
                cc[gp] <- -y[gp]
                isn[gp] <- 1
                }

        gp <- abs(y-.75) <= .25
        if(any(gp)) {
                ac[gp] <- h
                bc[gp] <- -(4-3*a)
                cc[gp] <- y[gp] -a + 1
                isn[gp] <- -1
        }
        gp <- y <= 0
        if(any(gp)) {
                ac[gp] <- 1
                bc[gp] <- 0
                cc[gp] <- 0
                isn[gp] <- 1
                }
        gp <- y >= 1
        if(any(gp)) {
                ac[gp] <- 1
                bc[gp] <- -1
                cc[gp] <- 0
                isn[gp] <- 1
                }
        (-bc + isn*sqrt(bc^2 - 4*ac*cc))/(2*ac)
}
