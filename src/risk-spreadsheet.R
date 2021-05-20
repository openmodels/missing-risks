setwd("~/research/missrisks")

source("combined/dists.R")
library(copula)
library(Matrix)
library(ggplot2)
library(dplyr)

source("combined/prepare.R")
source("combined/combine.R")

## Combine across regions with each reference
df2 <- df %>% group_by(Risk, Warming, `Full Reference`, Scenario, `Pop. Year`) %>% summarize(Central=sum(Central), Upper=sum(Upper), Quantile=mean(Quantile))

mat.corr <- as.matrix(read_excel("combined/Missing risks evidence.xlsx", sheet=2)[, -1])
mat.corr[lower.tri(mat.corr)] <- t(mat.corr)[lower.tri(mat.corr)]

mat.corr2 <- nearPD(mat.corr, corr=T)$mat
copula <- normalCopula(P2p(mat.corr2), dim=dim(mat.corr)[1], dispstr='un')
udraws <- rCopula(4000, copula)

mat.remain <- 1 - as.matrix(read_excel("combined/Missing risks evidence.xlsx", sheet=3)[, -1])
mat.remain[lower.tri(mat.remain)] <- t(mat.remain)[lower.tri(mat.remain)]

risks <- list()
for (risk in unique(df2$Risk)) {
    gens <- list()
    for (ref in unique(df2$`Full Reference`[df2$Risk == risk])) {
        for (scenario in unique(df2$Scenario[df2$Risk == risk & df2$`Full Reference` == ref])) {
            letter <- function() {
                rows <- df2[df2$Risk == risk & df2$`Full Reference` == ref & df2$Scenario == scenario,]

                function(warming, numdraws) {
                    row <- rows[rows$Warming == warming,]
                    stopifnot(nrow(row) == 1)
                    pop <- get.population(row$Scenario, as.numeric(row$`Pop. Year`))

                    if (!is.na(row$Central) & !is.na(row$Upper) & !is.na(row$Quantile)) {
                        fit <- fit.beta(row$Central / pop, row$Quantile, row$Upper / pop)
                        rbeta(numdraws, fit$alpha, fit$beta)
                    } else if (!is.na(row$Central) & !is.na(row$Upper) & is.na(row$Quantile)) {
                        ## Treat as SD
                        mydraws <- rnorm(numdraws, row$Central / pop, row$Upper / pop)
                        mydraws <- mydraws[mydraws >= 0 & mydraws <= 1]
                        sample(mydraws, numdraws, replace=T)
                    } else if (!is.na(row$Central) & is.na(row$Upper) & is.na(row$Quantile)) {
                        mydraws <- rnorm(numdraws, row$Central / pop, (row$Central / pop) / 5)
                        mydraws <- mydraws[mydraws >= 0 & mydraws <= 1]
                        sample(mydraws, numdraws, replace=T)
                    } else {
                        c()
                    }
                }
            }
            gens[[length(gens) + 1]] <- letter()
        }
    }
    risks[[risk]] <- gens
}

finres <- combine.risks(risks, 4000)



ggplot(finres, aes(affected, fill=temp, colour=temp)) +
    geom_density(alpha=.5) +
    theme_bw() + xlab("Population Affected (%)") + ylab("Probability density") +
    scale_fill_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_colour_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c'))
