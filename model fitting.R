## read in wine data files ###
rw <- read.csv('data/winequality-red.csv', header = T, sep = ';')
ww <- read.csv('data/winequality-white.csv', header = T, sep = ';')
## combine datasets
rw$type <- 'red'
ww$type <- 'white'
aw <- data.frame(rbind(rw, ww)) ## create full dataset with all wines

#distribution of y variables
rw_y = hist(rw$quality+0.001, main = 'Red Vinho Verde Wine',
     xlab = "Sensory Preference Score", las = 1, breaks = 6, xaxt = 'n')
axis(side=1,at=rw_y$mids,labels=seq(3,8))
ww_y = hist(ww$quality+0.001, main = 'White Vinho Verde Wine',
     xlab = "Sensory Preference Score", ylim = c(0, 2500), las = 1, breaks = 7, xaxt = 'n')
axis(side=1,at=ww_y$mids,labels=seq(3,9))
summary(ww$quality)
summary(rw$quality)

#proportion calculations in each category
fair = c(5,6)
excellent = c(7,8,9)
poor = c(3,4)
rw_fair_prop = nrow(rw[rw$quality %in% fair,])/nrow(rw)
ww_fair_prop = nrow(ww[ww$quality %in% fair,])/nrow(ww)
rw_ex_prop = nrow(rw[rw$quality %in% excellent,])/nrow(rw)
ww_ex_prop = nrow(ww[ww$quality %in% excellent,])/nrow(ww)
rw_poor_prop = nrow(rw[rw$quality %in% poor,])/nrow(rw)
ww_poor_prop = nrow(ww[wwe$quality %in% poor,])/nrow(ww)

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

#descriptive statistic for white wine
library('dplyr')
ww_mean <- ww %>%
  group_by(quality) %>%
  summarise_all(funs(mean))
ww_mean <-t(ww_mean)

ww_std <- ww %>%
  group_by(quality) %>%
  summarise_all(funs(sd))
ww_std <-t(ww_std)

#descriptive statistics for red wine
rw_mean <- rw %>%
  group_by(quality) %>%
  summarise_all(funs(mean))
rw_mean <-t(rw_mean)

rw_std <- rw %>%
  group_by(quality) %>%
  summarise_all(funs(sd))
rw_std <-t(rw_std)

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

#relationships between input variables
#draftsman plot
my_cols <- c("#FC4E07", "#E7B800")
names(aw) <- c('Fixed Acidity', 'Volatile Acidity', 'Citric Acid', 'Residual Sugar',
                      'Chloride', 'Free Sulfure Dioxide', 'Total Sulfur Dioxide','Density',
                      'pH','Sulphates','Alcohol', 'Quality','Type')
aw$Type <- as.factor(aw$Type)

#upper panel function
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(aw[1:11], pch = 19, col = my_cols[aw$Type], upper.panel = panel.cor, main ="Draftman Plot of Vinho Verde")
par(xpd = TRUE)

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
