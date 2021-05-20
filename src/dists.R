setwd("~/research/missrisks")

fit.beta <- function(mu, qq, aa) {
    ## Find a beta for which alpha > 1, beta > 1, alpha < beta, mean = alpha / (alpha + beta) = mu
    ## Parameterize as alpha = mu v, beta = (1 - mu) v
    sol <- optim(2 / mu, function(v) {
        alpha <- mu * v
        beta <- (1 - mu) * v
        (aa - qbeta(qq, alpha, beta))^2
    }, method="L-BFGS-B", lower=1 / mu)

    list(alpha = mu * sol$par, beta = (1 - mu) * sol$par, sqrerr=sol$value)
}

## pops <- read.csv("SYB63_1_202009_Population, Surface Area and Density.csv")

sres <- data.frame(year=rep(c(2025, 2050, 2085), each=4), scenario=rep(c('A1', 'B1', 'A2', 'B2'), 3),
                   pop=c(7926, 7926, 8714, 8036, 8709, 8709, 11778, 9541, 7914, 7914, 14220, 10235))

get.population <- function(scenario, year) {
    if (is.na(year))
        year <- 2020
    if (scenario %in% sres$scenario)
        return(sres$pop[sres$scenario == scenario & abs(sres$year - year) == min(abs(sres$year - year))])
    else
        get.population('A2', year)
}

pop.a2.2050 <- get.population('A2', 2050)

fit <- fit.beta(1903 / pop.a2.2050, 0.8571, 2761 / pop.a2.2050)

pdf <- data.frame(xx=seq(0, 1, length.out=100))
pdf$yy <- dbeta(pdf$xx, fit$alpha, fit$beta)

library(ggplot2)

ggplot(pdf, aes(xx, yy)) +
    geom_line() +
    geom_vline(xintercept=1903 / pop.a2.2050) +
    geom_vline(xintercept=2761 / pop.a2.2050) + geom_vline(xintercept=qbeta(0.8571, fit$alpha, fit$beta), col=2, linetype='dashed') +
    xlab("Portion of population (in 2050 under A2)") + ylab("Density") +
    theme_bw() + scale_x_continuous(expand=c(0, 0))

pop.2020 <- 7.8e9
