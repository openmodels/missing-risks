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

finres <- data.frame()
for (warming in 2:4) {
    impacts <- matrix(NA, 4000, 0)
    ## Take draws from all available
    for (risk in unique(df2$Risk)) {
        ## Mixed distribution across references
        draws <- c()
        for (ref in unique(df2$`Full Reference`[df2$Risk == risk])) {
            for (scenario in unique(df2$Scenario[df2$Risk == risk & df2$`Full Reference` == ref])) {
                row <- df2[df2$Warming == warming & df2$Risk == risk & df2$`Full Reference` == ref & df2$Scenario == scenario,]
                stopifnot(nrow(row) == 1)
                pop <- get.population(row$Scenario, as.numeric(row$`Pop. Year`))

                if (!is.na(row$Central) & !is.na(row$Upper) & !is.na(row$Quantile)) {
                    fit <- fit.beta(row$Central / pop, row$Quantile, row$Upper / pop)
                    mydraws <- rbeta(4000, fit$alpha, fit$beta)
                    draws <- c(draws, mydraws)
                } else if (!is.na(row$Central) & !is.na(row$Upper) & is.na(row$Quantile)) {
                    ## Treat as SD
                    mydraws <- rnorm(4000, row$Central / pop, row$Upper / pop)
                    mydraws <- mydraws[mydraws >= 0 & mydraws <= 1]
                    draws <- c(draws, sample(mydraws, 4000, replace=T))
                } else if (!is.na(row$Central) & is.na(row$Upper) & is.na(row$Quantile)) {
                    mydraws <- rnorm(4000, row$Central / pop, (row$Central / pop) / 5)
                    mydraws <- mydraws[mydraws >= 0 & mydraws <= 1]
                    draws <- c(draws, sample(mydraws, 4000, replace=T))
                }
            }
        }

        if (length(draws) == 0)
            values <- rep(0, 4000)
        else
            values <- draws[ceiling(udraws[, colnames(mat.corr) == risk] * length(draws))]
        impacts <- cbind(impacts, values)
    }

    if (dim(impacts)[2] == 1)
        total <- impacts[, 1]
    else {
        total <- c()
        for (ii in 1:nrow(impacts)) {
            jjs <- order(impacts[ii,], decreasing=T)
            mytotal <- impacts[ii, jjs[1]]
            for (kk in 2:length(jjs)) {
                remains <- min(mat.remain[jjs[kk], jjs[1:(kk-1)]])
                mytotal <- tworisk.double.count(mytotal, impacts[ii, jjs[kk]] * remains)
            }
            total <- c(total, mytotal)
        }
    }

    finres <- rbind(finres, data.frame(temp=paste(warming, "C"), affected=total))
}


ggplot(finres, aes(affected, fill=temp, colour=temp)) +
    geom_density(alpha=.5) +
    theme_bw() + xlab("Population Affected (%)") + ylab("Probability density") +
    scale_fill_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c')) +
scale_colour_manual(name="Change in GMST (C)", values=c('#fecc5c', '#fd8d3c', '#e31a1c'))
