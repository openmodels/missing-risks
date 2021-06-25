library(copula)

tworisk.double.count <- function(pp1, pp2) {
    pp1 + pp2 - pp1 * pp2
}

get.udraws <- function(mat.corr, numdraws) {
    copula <- normalCopula(P2p(mat.corr), dim=dim(mat.corr)[1], dispstr='un')
    rCopula(numdraws, copula)
}

## risks = list(risk=list(function -> 4000 draws))
combine.risks <- function(risks, numdraws, udraws) {
    finres <- data.frame()
    for (warming in 2:4) {
        impacts <- matrix(NA, numdraws, 0)
        ## Take draws from all available
        for (risk in names(risks)) {
            ## Mixed distribution across references
            draws <- c()
            for (ii in 1:length(risks[[risk]]))
                draws <- c(draws, risks[[risk]][[ii]](warming, numdraws))

            if (length(draws) == 0)
                values <- rep(0, numdraws)
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

    ## Don't use-- instead using tworisk.double.count
    ## Final transformation:
    ## Assume independence as new individuals are added, so after x people if we add one more y increases:
    ##   dy/dx = 1 - (y / P)
    ## Some math later:
    ##   y = P (1 - exp(-x / P))

    ## master.double.count <- function(pp) {
    ##     xx <- pp * pop.2020
    ##     (1 - exp(-xx / pop.2020))
    ## }

    ## pp <- seq(0, 2, length.out=200)
    ## yy <- pop.2020 * master.double.count(pp)
    ## plot(pp, yy / pop.2020)
    ## lines(pp, pp)

    finres
}

