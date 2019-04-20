## read in wine data files ###
rw <- read.csv('data/winequality-red.csv', header = T, sep = ';')
ww <- read.csv('data/winequality-white.csv', header = T, sep = ';')
## combine datasets
rw$type <- 'red'
ww$type <- 'white'
aw <- data.frame(rbind(rw, ww)) ## create full dataset with all wines

## look at quality distributions
table(rw$quality)
table(ww$quality)
table(aw$quality)
## histograms
hist(rw$quality, breaks = 0:10)
hist(ww$quality, breaks = 0:10)
## test normality
shapiro.test(rw$quality)
shapiro.test(ww$quality)

## look at correlations between numeric predictors

cor(ww)
pairs(ww)
pairs(rw)
names(aw)
par(mar = c(8,4,4,4))

round(cor(aw[-c(length(names(aw)), length(names(aw))-1)]),2)

heatmap(cor(aw[-c(length(names(aw)), length(names(aw))-1)]),
        scale = 'column',
        col = cm.colors(256),
        Colv = NA, Rowv = NA)

summary(ww)
summary(rw)

## create dummy vars for quality
mm <- data.frame(model.matrix(~ factor(quality) + 0, aw))
aw2 <- cbind(aw, mm)
names(aw2)
cols <- cbind(aw2$factor.quality.3, aw2$factor.quality.4, aw2$factor.quality.5, 
              aw2$factor.quality.6, aw2$factor.quality.7, aw2$factor.quality.8, 
              aw2$factor.quality.9)
## fit cumulative logit model with proportional odds
library(VGAM)
fit.po <- vglm(cols ~ fixed.acidity + volatile.acidity + citric.acid +
                 residual.sugar + chlorides + free.sulfur.dioxide +
                 total.sulfur.dioxide + density + pH +
                 sulphates + alcohol + factor(type),
               family=cumulative(parallel=TRUE), data=aw2)


fit.po.sc <- vglm(cols ~ scale(fixed.acidity) + scale(volatile.acidity) + scale(citric.acid) +
                 scale(residual.sugar) + scale(chlorides) + scale(free.sulfur.dioxide) +
                 scale(total.sulfur.dioxide) + scale(density) + scale(pH) +
                 scale(sulphates) + scale(alcohol) + factor(type),
                family=cumulative(parallel=TRUE), data=aw2)

fit.po2 <- vglm(cols ~ fixed.acidity + volatile.acidity +
                   residual.sugar + free.sulfur.dioxide +
                   total.sulfur.dioxide + density + pH +
                   sulphates + alcohol + factor(type),
            family=cumulative(parallel=TRUE), data=aw2)
summary(fit.po)
summary(fit.po.sc)

pchisq(deviance(fit.po2)-deviance(fit.po),df=df.residual(fit.po2)-df.residual(fit.po),lower.tail=FALSE)
## diagnostic check against model without proportional odds
fit.npo <- vglm(cols ~ scale(fixed.acidity) + scale(volatile.acidity) + scale(citric.acid) +
                  scale(residual.sugar) + scale(chlorides) + scale(free.sulfur.dioxide) +
                  scale(total.sulfur.dioxide) + scale(density) + scale(pH) +
                  scale(sulphates) + scale(alcohol),
            family=cumulative(), data=aw2) ## model returns error, probably because there are too many parameters
summary(fit.npo) 
pchisq(deviance(fit.po)-deviance(fit.npo),df=df.residual(fit.po)-df.residual(fit.npo),lower.tail=FALSE)

## fit log-log link model
fit.ll <- vglm(cols ~ fixed.acidity + volatile.acidity + citric.acid +
                 residual.sugar + chlorides + free.sulfur.dioxide +
                 total.sulfur.dioxide + density + pH +
                 sulphates + alcohol,
               family=cumulative(link = cloglog,parallel=TRUE), data=aw2)
summary(fit.ll)


## more complex models are unable to fit
## change to three categories y to simplify

aw$quality.bucket <- ifelse(aw$quality <= 4, 'Low',
                            ifelse(aw$quality <= 6, 'Med', 'High'))
table(aw$quality.bucket)
mm <- data.frame(model.matrix(~ factor(quality.bucket) + 0, aw))
aw2 <- cbind(aw, mm)
names(aw2)
cols <- cbind(aw2$factor.quality.bucket.Low,
              aw2$factor.quality.bucket.Med,
              aw2$factor.quality.bucket.High)

fit.po <- vglm(cols ~ fixed.acidity + volatile.acidity + citric.acid +
                 residual.sugar + chlorides + free.sulfur.dioxide +
                 total.sulfur.dioxide + density + pH +
                 sulphates + alcohol,
               family=cumulative(parallel=TRUE), data=aw2)
summary(fit.po)
## diagnostic check against model without proportional odds
fit.npo <- vglm(cols ~ fixed.acidity + volatile.acidity + citric.acid +
                  residual.sugar + chlorides + free.sulfur.dioxide +
                  total.sulfur.dioxide + density + pH +
                  sulphates + alcohol,
                family=cumulative(link = probit), data=aw2) ## model returns error, probably because there are too many parameters
summary(fit.npo) 

## fit log-log link model
fit.ll <- vglm(cols ~ fixed.acidity + volatile.acidity + citric.acid +
                 residual.sugar + chlorides + free.sulfur.dioxide +
                 total.sulfur.dioxide + density + pH +
                 sulphates + alcohol,
               family=cumulative(link = cloglog,parallel=TRUE), data=aw2)
summary(fit.ll)
?vglm
## diagnostic check comparing link functions by deviance, both have the same df
deviance(fit.po)
deviance(fit.ll)
deviance(fit.npo)
