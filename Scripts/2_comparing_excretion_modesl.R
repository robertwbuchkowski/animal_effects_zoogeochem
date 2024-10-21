# Compare two versions of an animal excretion model:

# Load in libraries:
library(pacman)
p_load(FME, tidyverse, soilfoodwebs)

# Create a basic set of nutrient models for a single organism:

baseresp <- function(t, y, pars){
  with(as.list(c(pars,y)),{

    dC = pc*ac*Fc - d*C - Rc*C - Mc
    dN = rNC*dC
    dP = rPC*dC

    Nmin = - dN + rfNC*pn*an*Fc - d*C*rNC - Mc*rNC
    Pmin = - dP + rfPC*pp*ap*Fc - d*C*rPC - Mc*rPC

    return(list(c(dC), Nmin, Pmin))
  })
}

parms <- c(pc = 0.5, ac = 0.3, Fc = 1, d = 0.1, Rc = 0.01, Mc = 0.001, rNC = 0.25, rPC = 0.01, pn = 1, an = 1, pp = 1, ap = 1, rfNC = 0.1, rfPC = 0.005)

y = c(C = 1)

ode(y = y, times = 1:10, func = baseresp, parms = parms)

