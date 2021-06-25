## setwd("~/research/missrisks")

diagnostics <- F

load("data/growth/country-damages.RData")
growth.damages <- results

pops <- read.csv("data/ssps/total_population.csv")
pops <- subset(pops, MODEL == 'IIASA-WiC POP' & SCENARIO == 'SSP2_v9_130115') # most entries and 1 scen.

tempdf <- read.csv("data/temps/rcp85.csv")
temp1990 <- .5 # already .5 up

growth.temp2pop <- function(warming, numdraws) {
    ## Determine year to run to
    globdf <- subset(tempdf, region == '')
    baseline <- mean(globdf$mean[globdf$year <= 2000])
    lastyear <- globdf$year[which(globdf$mean > baseline + warming - temp1990)[1] - 1]

    affected <- 0
    for (reg in pops$REGION) {
        reg <- as.character(reg)
        cres <- growth.damages[[reg]]
        ctemp <- subset(tempdf, region == reg)

        if (length(cres$sdev) == 0 || is.nan(cres$baseline))
            break

        deltalog <- 0
        for (year in 1990:lastyear) {
            draws <- (cres$beta1 * (warming + cres$baseline) + cres$beta2 * (warming + cres$baseline)^2) -
                (cres$beta1 * cres$baseline + cres$beta2 * cres$baseline^2) +
                rnorm(numdraws, 0, cres$sigma) + rnorm(numdraws, 0, cres$sdev) + rnorm(numdraws, 0, cres$regional.sdev.temp)
            deltalog <- deltalog + draws
        }

        total <- pops$X2010[pops$REGION == reg] * 1e6
        affected <- affected - sample(total * (pmin(exp(deltalog), 2) - 1), numdraws) # reshuffle, flip sign
    }

    affected
}

growth.temp2pop.frac <- function(warming, numdraws) {
    growth.temp2pop(warming, numdraws) / get.population("SSP2", 2010)
}

if (diagnostics) {
    plotdf <- data.frame(warming=rep(2:4, each=4000), pop=c(growth.temp2pop(2), growth.temp2pop(3), growth.temp2pop(4)))

    library(ggplot2)

    ggplot(plotdf, aes(pop, colour=factor(temp))) +
        geom_density()
}
