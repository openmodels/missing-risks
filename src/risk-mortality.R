##setwd("~/research/missrisks")

do.bornnows <- F
generation <- F
diagnostics <- F

library(dplyr)
source("src/calc.yod.R")

## Construct average GMST
gmstdf <- data.frame()
for (rcp in c('rcp45', 'rcp85')) {
    temps <- matrix(NA, 2099 - 1981 + 1, 0)
    path <- file.path("data/gmt_anom", rcp)
    for (filename in list.files(path, ".*csv")) {
        if (length(grep("pattern", filename)) > 0)
            next
        gcmdf <- read.csv(file.path(path, filename))
        temps <- cbind(temps, gcmdf$global_tas)
    }

    for (year1 in c(1980, 2000, 2020, 2040, 2060, 2080)) {
        gmstdf <- rbind(gmstdf, data.frame(year=paste0(year1, '-', year1+19), rcp, gmst=mean(temps[(year1 - 1980):(year1 - 1980 + 19),])))
    }
}

paths <- list("young"="data/mortality/young-aggregated",
              "young.costs"="data/mortality/young-costs-aggregated",
              "older"="data/mortality/older-aggregated",
              "older.costs"="data/mortality/older-costs-aggregated",
              "oldest"="data/mortality/oldest-aggregated",
              "oldest.costs"="data/mortality/oldest-costs-aggregated",
              "tas"="data/covariates/tas-aggregated",
              "loggdppc"="data/covariates/loggdppc-aggregated")

df <- data.frame()
for (filename in list.files(paths[[names(paths)[1]]])) {
    scenarios <- strsplit(gsub(".csv", "", filename), '-')[[1]]
    scendf <- NULL
    for (variable in names(paths)) {
        subdf <- read.csv(file.path(paths[[variable]], filename))
        names(subdf) <- c('region', 'year', variable, paste0(variable, '.sd'))
        if (is.null(scendf))
            scendf <- subdf
        else
            scendf <- scendf %>% left_join(subdf)
    }

    df <- rbind(df, cbind(rcp=scenarios[1], ssp=scenarios[2], scendf))
}

df <- df %>% left_join(gmstdf)


temp2deaths <- function(warming) {
    ## Assume that all temps lie on along RCP 8.5 trajectory
    period <- gmstdf$year[which.min(abs(gmstdf$gmst[gmstdf$rcp == 'rcp85'] - warming))]
    subdf <- subset(df, rcp == 'rcp85' & year == period & ssp == 'SSP3')

    qval <- runif(1)
    results <- data.frame(region=c(), lifeprob=c())
    for (region in subdf$region) {
        probs <- get.probofdeath(region)
        if (all(is.na(probs)))
            next
        probs.byyear <- rep(probs, each=5) / 5
        row <- subdf[subdf$region == region,]

        rate.young <- qnorm(qval, row$young, row$young.sd) + qnorm(qval, row$young.costs, row$young.costs.sd)
        rate.older <- qnorm(qval, row$older, row$older.sd) + qnorm(qval, row$older.costs, row$older.costs.sd)
        rate.oldest <- qnorm(qval, row$oldest, row$oldest.sd) + qnorm(qval, row$oldest.costs, row$oldest.costs.sd)

        temprates <- c(rate.young, rep(rate.older, 12), rep(rate.oldest, length(probs) - 13))
        temprates.byyear <- rep(temprates, each=5)

        totalchance <- 0
        for (age in 0:104) {
            chancealive <- sum(probs.byyear[(age+1):length(probs.byyear)])
            chancedie <- chancealive * temprates.byyear[age+1]
            totalchance <- totalchance + chancedie
        }

        results <- rbind(results, data.frame(region, lifeprob=totalchance, pop=get.pop(2010, region)))
    }

    sum(results$lifeprob * results$pop)
}

if (generation) {
    deaths <- list(at2c=c(), at3c=c(), at4c=c())
    for (ii in 1:10000) {
        print(ii)
        at2c <- temp2deaths(2)
        deaths[['at2c']] <- c(deaths[['at2c']], at2c)
        at3c <- temp2deaths(3)
        deaths[['at3c']] <- c(deaths[['at3c']], at3c)
        at4c <- temp2deaths(4)
        deaths[['at4c']] <- c(deaths[['at4c']], at4c)
    }

    save(deaths, file="data/mortality/deathsatc.RData")
} else {
    load("data/mortality/deathsatc.RData")
}

temp2deaths.mc <- function(warming, numdraws) {
    values <- deaths[[paste0('at', warming, 'c')]]
    sample(values, numdraws, replace=T) / get.population('History', 2010)
}

if (diagnostics) {
    alldeaths <- data.frame(warming=c(rep(2, length(deaths$at2c)), rep(3, length(deaths$at3c)), rep(4, length(deaths$at4c))),
                            deaths=c(deaths$at2c, deaths$at3c, deaths$at4c))

    library(ggplot2)
    ggplot(alldeaths, aes(deaths)) +
        facet_grid(warming ~ .) +
        geom_density()
}
