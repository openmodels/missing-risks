library(readxl)

df <- read_excel("combined/Missing risks evidence.xlsx", sheet=1, skip=1)
df2 <- subset(df, is.na(Include) | Include == 'Yes')

df3 <- rbind(cbind(df2[, c(1:2, 5:16, 28)], Warming=2, Central=df2$`Millions at risk...17`,
                   Upper=df2$`Upper bound...18`, Quantile=as.numeric(df2$`Quantile of U.B....19`)),
             cbind(df2[, c(1:2, 5:16, 28)], Warming=3, Central=df2$`Millions at risk...20`,
                   Upper=df2$`Upper bound...21`, Quantile=as.numeric(df2$`Quantile of U.B....22`)),
             cbind(df2[, c(1:2, 5:16, 28)], Warming=4, Central=df2$`Millions at risk...23`,
                   Upper=df2$`Upper bound...24`, Quantile=as.numeric(df2$`Quantile of U.B....25`)))
df3$row <- rep(1:nrow(df2), 3)

## Infer all of the missing values
df3$logcentral <- log(df3$Central)
df3$logcentral[!is.finite(df3$logcentral)] <- NA
mod <- lm(logcentral ~ 0 + Warming + factor(row), data=df3)

df3$postproc <- "Reported"
df3$postproc[is.na(df3$Central)] <- "Imputed"
df3$Central[is.na(df3$Central)] <- exp(predict(mod, df3[is.na(df3$Central),]) + var(mod$residuals)/2)

## Fill in quantiles and upper values
for (row in unique(df3$row[df3$postproc == 'Imputed'])) {
    iis <- which(df3$row == row)
    hasupper <- !is.na(df3$Upper[iis])
    if (all(hasupper))
        next

    if (any(hasupper)) {
        for (jj in which(!hasupper)) {
            df3$Upper[iis[jj]] <- mean(df3$Upper[iis][hasupper] * exp(mod$coeff[1] * (df3$Warming[iis][jj] - df3$Warming[iis][hasupper])))
        }

        hasquantile <- !is.na(df3$Quantile[iis])
        if (any(hasquantile))
            df3$Quantile[iis[!hasquantile]] <- mean(df3$Quantile[iis[hasquantile]])
    }
}

df <- df3
