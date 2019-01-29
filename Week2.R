
#######    SEM 2019 WEEK 2
#######    Mikko Patronen 29.1.2019
#######
#######    Exercise 2.1
#

ex2.1 <- read.fortran(file = "ASC7INDM.DAT", c("40F1.0","X","6F2.0"))
colnames(ex2.1) <- c("SPPCN08", "SPPCN18", "SPPCN28", "SPPCN38", "SPPCN48", 
                     "SPPCN58", "SPPCN01", "SPPCN11" ,"SPPCN21", "SPPCN31", 
                     "SPPCN41", "SPPCN51", "SPPCN06", "SPPCN16", "SPPCN26", 
                     "SPPCN36", "SPPCN46", "SPPCN56", "SPPCN03", "SPPCN13", 
                     "SPPCN23", "SPPCN33", "SPPCN43", "SPPCN53", "SDQ2N01", 
                     "SDQ2N13", "SDQ2N25", "SDQ2N37", "SDQ2N04", "SDQ2N16", 
                     "SDQ2N28", "SDQ2N40", "SDQ2N10", "SDQ2N22", "SDQ2N34", 
                     "SDQ2N46", "SDQ2N07", "SDQ2N19", "SDQ2N31", "SDQ2N43", 
                     "MASTENG1", "MASTMAT1", "TENG1", "TMAT1", "SENG1", "SMAT1")

library(lavaan)
library(sem)

modelF4 <- "
F1 =~ SDQ2N01 + SDQ2N13 + SDQ2N25 + SDQ2N37
F2 =~ SDQ2N04 + SDQ2N16 + SDQ2N28 + SDQ2N40
F3 =~ SDQ2N10 + SDQ2N22 + SDQ2N34 + SDQ2N46
F4 =~ SDQ2N07 + SDQ2N19 + SDQ2N31 + SDQ2N43
"

fitF4 <- cfa(modelF4, data = ex2.1)
summary(fitF4, fit.measures = T)


library(sem)

semPaths(fitF4, title = FALSE, curvePivot = TRUE)

