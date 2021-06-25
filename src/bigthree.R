## setwd("~/research/missrisks/missing-risks/")

library(ggplot2)

source("src/risk-slr.R")
source("src/risk-growth.R")
source("src/risk-mortality.R")

slr.impacts2 <- slr.temp2pop.mc(2, 4000)
slr.impacts3 <- slr.temp2pop.mc(3, 4000)
slr.impacts4 <- slr.temp2pop.mc(4, 4000)
slr.impacts <- data.frame(impact='Sea-level Rise Innundation', temp=rep(c('2 C', '3 C', '4 C'), each=4000),
                          affected=c(slr.impacts2, slr.impacts3, slr.impacts4))

growth.impacts2 <- growth.temp2pop(2, 4000)
growth.impacts3 <- growth.temp2pop(3, 4000)
growth.impacts4 <- growth.temp2pop(4, 4000)
growth.impacts <- data.frame(impact='Economic Contraction', temp=rep(c('2 C', '3 C', '4 C'), each=4000),
                             affected=c(growth.impacts2, growth.impacts3, growth.impacts4))

death.impacts2 <- temp2deaths.mc(2, 4000)
death.impacts3 <- temp2deaths.mc(3, 4000)
death.impacts4 <- temp2deaths.mc(4, 4000)
death.impacts <- data.frame(impact='Premature Mortality', temp=rep(c('2 C', '3 C', '4 C'), each=4000),
                            affected=c(death.impacts2, death.impacts3, death.impacts4))

plotdf <- rbind(slr.impacts, growth.impacts, death.impacts)
plotdf$impact <- factor(plotdf$impact, c('Sea-level Rise Innundation', 'Premature Mortality', 'Economic Contraction'))

meandf <- plotdf %>% group_by(impact, temp) %>% summarize(affected=mean(affected))
meandf$metric <- 'Mean'
q99df <- plotdf %>% group_by(impact, temp) %>% summarize(affected=quantile(affected, .99))
q99df$metric <- '99th p\'ile'
linedf <- rbind(meandf, q99df)
linedf$metric <- factor(linedf$metric, levels=c('Mean', '99th p\'ile'))

ggplot(plotdf, aes(affected / 1e6, fill=temp, colour=temp)) +
    facet_wrap(impact ~ ., scales="free_y", ncol=1) +
    geom_density(alpha=.5) + geom_vline(data=linedf, aes(xintercept=affected / 1e6, colour=temp, linetype=metric)) +
    theme_bw() + xlab("Population Affected (millions)") + ylab("Probability density") +
    scale_fill_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_colour_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_linetype_discrete(name="Summary metrics")

source("src/prepare.R")
source("src/combine.R")

risks <- list("Sea innundation"=list(slr.temp2pop.mc),
              "Economic growth"=list(growth.temp2pop.frac),
              "Mortality"=list(temp2deaths.mc))

mat.corr <- get.mat.corr()
mat.remain <- get.mat.remain()

udraws <- get.udraws(mat.corr, 10)
finres <- combine.risks(risks, 10, udraws)

ggplot(finres, aes(affected, fill=temp, colour=temp)) +
    geom_density(alpha=.5) +
    theme_bw() + xlab("Population Affected (%)") + ylab("Probability density") +
    scale_fill_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_colour_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c'))

