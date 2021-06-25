## Calculate probability of dying in each year of life
##setwd("~/research/missrisks/mortality")

library(dplyr)

cohort.pops <- read.csv("data/ssps/cohort_population.csv")
cohort.pops <- subset(cohort.pops, MODEL == 'IIASA-WiC POP' & SCENARIO == 'SSP2_v9_130115' & VAR2 != 'Total' & VAR3 != 'Total') # most entries and 1 scen.

cohort.pops2 <- cohort.pops %>% group_by(REGION, VAR3) %>% summarize(pop2010=sum(X2010)*1e6)
cohort.pops2.global <- cohort.pops2 %>% group_by(VAR3) %>% summarize(pop2010=sum(pop2010))

ages <- c(paste0("Aged", seq(0, 95, 5), '-', seq(4, 99, 5)), 'Aged100+')

birthrate <- read.csv("data/mortality/birthrate.csv")
pophist <- read.csv("data/mortality/pophist.csv")
mortunder5 <- read.csv("data/mortality/mortunder5.csv")

get.pop <- function(year, region) {
    pophists <- t(pophist[pophist$Country.Code == region, -(1:4)])
    popyears <- 1960:2020

    pophists[!is.na(pophists)][which.min(abs(popyears[!is.na(pophists)] - year))]
}

get.births <- function(year, region) {
    birthrates <- t(birthrate[birthrate$Country.Code == region, -(1:4)])
    birthyears <- 1960:2020

    ## Closest to year
    rate <- birthrates[!is.na(birthrates)][which.min(abs(birthyears[!is.na(birthrates)] - year))]
    if (length(rate) == 0) {
        birthrates <- t(birthrate[birthrate$Country.Code == 'WLD', -(1:4)])
        rate <- birthrates[!is.na(birthrates)][which.min(abs(birthyears[!is.na(birthrates)] - 2010))]
    }

    pop <- get.pop(year, region)

    rate * pop / 1000
}

get.cohort.deaths <- function(region, youngestage2010, cohortpop, earlierdeaths) {
    birthsinto <- sum(sapply(youngestage2010:(youngestage2010 + 4), function(age2010)
        get.births(2010 - age2010, region)))

    if (youngestage2010 == 0) {
        mortrates <- t(mortunder5[mortunder5$Country.Code == region, -(1:4)])
        mortyears <- 1960:2020
        rate <- mortrates[!is.na(mortrates)][which.min(abs(mortyears[!is.na(mortrates)] - 2010))]
        if (length(rate) == 0) {
            mortrates <- t(mortunder5[mortunder5$Country.Code == 'WLD', -(1:4)])
            rate <- mortrates[!is.na(mortrates)][which.min(abs(mortyears[!is.na(mortrates)] - 2010))]
        }

        return(birthsinto * rate / 1000) # per 1000 births
    }

    birthsinto - earlierdeaths - cohortpop
}

probofdeath.cache <- list()

get.probofdeath <- function(region) {
    if (!(region %in% cohort.pops2$REGION))
        return(NA)
    if (!(region %in% pophist$Country.Code))
        return(NA)

    if (region %in% names(probofdeath.cache))
        return(probofdeath.cache[[region]])

    cohortdeaths <- c()
    earlierdeaths <- 0
    for (youngestage2010 in seq(0, 100, 5)) {
        if (youngestage2010 == 100)
            cohort <- "Aged100+"
        else
            cohort <- paste0("Aged", youngestage2010, '-', youngestage2010+4)
        cohortpop <- cohort.pops2$pop2010[cohort.pops2$REGION == region & cohort.pops2$VAR3 == cohort]

        deaths <- get.cohort.deaths(region, youngestage2010, cohortpop, earlierdeaths)
        if (deaths < 0)
            deaths <- 0

        cohortdeaths <- c(cohortdeaths, deaths)
        earlierdeaths <- earlierdeaths + deaths
    }

    probofdeath <- cohortdeaths / sum(cohortdeaths)
    probofdeath.cache[[region]] <<- probofdeath
    probofdeath
}
