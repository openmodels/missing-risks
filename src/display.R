setwd("~/research/missrisks")

library(ggplot2)
library(dplyr)

source("combined/prepare.R")

## Combine across references
df2 <- df %>% group_by(Risk, Region, Warming) %>% summarize(Central=mean(Central), postproc=ifelse(any(postproc == 'Imputed'), 'Imputed', 'Reported'))
## Combine across regions
df3 <- df2 %>% group_by(Risk, Warming) %>% summarize(Central=sum(Central), postproc=ifelse(any(postproc == 'Imputed'), 'Imputed', 'Reported'))

## Infer all of the missing values
ggplot(df3, aes(Risk, Central, fill=postproc)) +
    coord_flip() + facet_wrap(~ Warming, nrow=1) +
    geom_bar(stat='identity') + scale_y_log10()
