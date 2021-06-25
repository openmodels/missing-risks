## setwd("~/research/missrisks")
## Measures of population at risk, at various temperatures.

library(ggplot2)

source("src/risk-slr.R")
source("src/risk-mortality.R")
source("src/risk-growth.R")

source("combined/prepare.R")




num.impacts <- 3 # SLR, Mortality, Growth
CC.slr.mortality <- 0
CC.slr.growth <- 0
CC.mortality.growth <- .5
CC.correlation <- matrix(c(1, CC.slr.mortality, CC.slr.growth, CC.slr.mortality, 1, CC.mortality.growth, CC.slr.growth, CC.mortality.growth, 1), 3, 3)

rr.mortality.after.slr <- 1
rr.growth.after.slrmortality <- 1 - (180+260) / sum(pops$X2010)
rr <- c(1, rr.mortality.after.slr, rr.growth.after.slrmortality)

finres <- data.frame()
for (temp in 2:4) {
    print(temp)
    ## Sort, so have EDF
    slr.risks <- sort(sapply(1:4000, function(ii) slr2pop(temp2slr(temp))) * 1e6)
    death.risks <- sort(deaths[[paste0("at", temp, "c")]])
    growth.risks <- sort(growth.temp2pop(temp))

    copula <- normalCopula(P2p(CC.correlation), dim=num.impacts, dispstr='un')
    udraws <- rCopula(10000, copula)

    slr.impacts <- slr.risks[ceiling(udraws[, 1] * length(slr.risks))]
    death.impacts <- death.risks[ceiling(udraws[, 2] * length(death.risks))]
    growth.impacts <- growth.risks[ceiling(udraws[, 3] * length(growth.risks))]

    impacts <- cbind(slr.impacts, death.impacts, growth.impacts)

    finals <- as.numeric(rr %*% t(impacts))
    finres <- rbind(finres, data.frame(temp=paste(temp, "C"), affected=finals))
}

meandf <- finres %>% group_by(temp) %>% summarize(affected=mean(affected))
meandf$metric <- 'Mean'
q99df <- finres %>% group_by(temp) %>% summarize(affected=quantile(affected, .99))
q99df$metric <- '99th p\'ile'
linedf <- rbind(meandf, q99df)
linedf$metric <- factor(linedf$metric, levels=c('Mean', '99th p\'ile'))

ggplot(finres, aes(affected / 1e6, fill=temp, colour=temp)) +
    geom_density(alpha=.5) + geom_vline(data=linedf, aes(xintercept=affected / 1e6, colour=temp, linetype=metric)) +
    theme_bw() + xlab("Population Affected (millions)") + ylab("Probability density") +
    scale_fill_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_colour_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_linetype_discrete(name="Summary metrics")

## If I also have plotdf in memory

meandf <- plotdf %>% group_by(impact, temp) %>% summarize(affected=mean(affected))
meandf$metric <- 'Mean'
q99df <- plotdf %>% group_by(impact, temp) %>% summarize(affected=quantile(affected, .99))
q99df$metric <- '99th p\'ile'
linedf1 <- rbind(meandf, q99df)
linedf1$metric <- factor(linedf1$metric, levels=c('Mean', '99th p\'ile'))

meandf <- finres %>% group_by(temp) %>% summarize(affected=mean(affected))
meandf$metric <- 'Mean'
q99df <- finres %>% group_by(temp) %>% summarize(affected=quantile(affected, .99))
q99df$metric <- '99th p\'ile'
linedf2 <- rbind(meandf, q99df)
linedf2$metric <- factor(linedf2$metric, levels=c('Mean', '99th p\'ile'))
linedf2$impact <- "Combined Risks"
linedf <- rbind(as.data.frame(linedf1), as.data.frame(linedf2))
linedf$impact <- factor(linedf$impact, c('Sea-level Rise Innundation', 'Premature Mortality', 'Economic Contraction', 'Combined Risks'))

plotdf$impact <- as.character(plotdf$impact)
finres$impact <- "Combined Risks"
plotdf2 <- rbind(plotdf, finres)
plotdf2$impact <- factor(plotdf2$impact, c('Sea-level Rise Innundation', 'Premature Mortality', 'Economic Contraction', 'Combined Risks'))

ggplot(plotdf2, aes(affected / 1e6, fill=temp, colour=temp)) +
    facet_wrap(impact ~ ., scales="free_y", ncol=1) +
    geom_density(alpha=.5) + geom_vline(data=linedf, aes(xintercept=affected / 1e6, colour=temp, linetype=metric)) +
    theme_bw() + xlab("Population Affected (millions)") + ylab("Probability density") +
    scale_fill_manual(name="Change in GMST", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_colour_manual(name="Change in GMST", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_linetype_discrete(name="Summary metrics") + coord_cartesian(xlim=c(-2000, 6000))
