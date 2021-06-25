## Population at risk from SLR
diagnostics <- F

## All lo and hi are 90% CI
## Table 1 of https://www.nature.com/articles/s41467-019-12808-z
rcpyr2pop.df <- data.frame(rcp=rep(c('rcp45', 'rcp85', 'rcp45', 'rcp85'), 2), year=rep(c(2050, 2050, 2100, 2100), 2),
                           model=rep(c('K14', 'K17'), each=4),
                           pop.lo=c(280, 280, 310, 330, 260, 270, 320, 380),
                           pop.hi=c(320, 330, 420, 460, 330, 340, 510, 630))
rcpyr2slr.df <- data.frame(rcp=rep(c('rcp45', 'rcp85', 'rcp45', 'rcp85'), 2), year=rep(c(2050, 2050, 2100, 2100), 2),
                           model=rep(c('K14', 'K17'), each=4),
                           slr.lo=c(.18, .21, .35, .51, .14, .17, .50, .93),
                           slr.hi=c(.35, .39, .95, 1.23, .43, .48, 1.58, 2.43))

library(dplyr)
slr.df <- rcpyr2pop.df %>% left_join(rcpyr2slr.df)

## Determine mean and std.dev
slr.df$pop <- (slr.df$pop.hi + slr.df$pop.lo) / 2
slr.df$slr <- (slr.df$slr.hi + slr.df$slr.lo) / 2
slr.df$pop.sd <- (slr.df$pop.hi - slr.df$pop) / qnorm(.95)
slr.df$slr.sd <- (slr.df$slr.hi - slr.df$slr) / qnorm(.95)

## Load temperature changes for each GCM, relative to 1985-2005
gmstdf <- data.frame()
for (rcp in c('rcp45', 'rcp85')) {
    path <- file.path("data/gmt_anom", rcp)
    for (filename in list.files(path, ".*csv")) {
        gcmdf <- read.csv(file.path(path, filename))
        climtemp <- stats::filter(c(rep(gcmdf$global_tas[1], 14), gcmdf$global_tas, rep(gcmdf$global_tas[nrow(gcmdf)], 15)), rep(1, 30) / 30, method="convolution")
        gcmdf$climtemp <- climtemp[!is.na(climtemp)]
        gmstdf <- rbind(gmstdf, data.frame(longgcm=substring(filename, 1, nchar(filename)-4), rcp, year=c(2050, 2100),
                                           gmst=c(gcmdf$climtemp[gcmdf$year == 2050], gcmdf$climtemp[nrow(gcmdf)])))
    }
}

gmstdf$gcm <- tolower(substring(gmstdf$longgcm, nchar("global_tas_") + 1, nchar(as.character(gmstdf$longgcm))))

## Load the weights
weights1 <- read.csv("data/gcp/rcp45-weights.csv")
weights1$rcp <- 'rcp45'
weights2 <- read.csv("data/gcp/rcp85-weights.csv")
weights2$rcp <- 'rcp85'
weights <- rbind(weights1, weights2)

gmstdf2 <- gmstdf %>% left_join(weights)
gmstdf2$weight[is.na(gmstdf2$weight)] <- 0

slr.temp2pop <- function(warming) {
    ## Choose a GCM and RCP
    gcmii <- sample(1:nrow(gmstdf2), size=1, prob=gmstdf2$weight)
    gcm <- gmstdf2$gcm[gcmii]
    rcp <- gmstdf2$rcp[gcmii]
    ## Choose a SLR model
    model <- sample(c('K14', 'K17'), size=1)
    ## Extract points for estimate
    mod.df <- slr.df[slr.df$model == model & slr.df$rcp == rcp,] %>% left_join(gmstdf2[gmstdf2$gcm == gcm & gmstdf2$rcp == rcp,], by=c('rcp', 'year'))
    ## Take random draw of uncertainty
    qval <- runif(1)

    mod.df$y <- mod.df$pop + qnorm(qval) * mod.df$pop.sd
    ##print(mod.df)

    mod <- lm(y ~ gmst, data=mod.df)
    predict(mod, data.frame(gmst=warming))
}

slr.temp2pop.mc <- function(warming, numdraws) {
    sapply(1:numdraws, function(ii) slr.temp2pop(warming)) * 1e6 / get.population('History', 2010)
}

if (diagnostics) {
    slr.temp2pop.3 <- sapply(1:4000, function(ii) slr.temp2pop(3))

    library(ggplot2)
    ggplot(data.frame(pop=slr.temp2pop.3), aes(pop)) +
        geom_density()
}

