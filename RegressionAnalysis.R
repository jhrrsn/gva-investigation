setwd("~/Development/GVA")
library(ggplot2)
library(directlabels)
library(reshape2)

# Read in GVA Table 1
gva.raw <- read.csv("GVA-All.csv")
colnames(gva.raw)[22] <- "X2015"
gva <- melt(gva.raw[,c(2, 4:22)], id.vars = "NUTS.code")
gva$variable <- as.numeric(substr(gva$variable, 2, 5))
colnames(gva) <- c("NUTS", "year", "gva")

# Subset GVA indices to UKK12. Using indices to compare with other index values.
gva.bath <- subset(gva, NUTS == 'UKK12')


# We could see from our historical analysis that the GVA in Bath 
# is roughly approximate to a straight line over time.


# Plot GVA in Bath. 
ggplot(gva.bath, aes(year, gva)) + geom_line() + geom_smooth(method="lm") + 
  xlab("Year") + ylab("GVA") + ggtitle("GVA, with line of best fit")

# Fit a linear model to determine how GVA varies over time.
# Adjusted R-squared: 0.9814, p-value: 2.317e-16
time.lm <- lm(gva ~ year, data=gva.bath)
summary(time.lm)

# We could use this model now to predict future values of GVA, if so inclined.
predict(time.lm, data.frame(year=2016))


# However, time is a bad predictor in this instance because it's a proxy for something else!
# The primary component of the growth over time is probably inflation, so let's include the CPI.


# Consumer Price Index.
# National statistic, published monthly, annual average.

# Read in CPI data.
cpi <- read.csv("CPI.csv")
colnames(cpi) <- c("year", "cpi")

# Fit a linear model.
# Adjusted R-squared: 0.8864, p-value: 1.153e-09
cpi.lm <- lm(gva.bath$gva ~ cpi$cpi)
summary (cpi.lm)

# Plot the residuals.
qplot(gva.bath$gva, resid(cpi.lm)) + 
  geom_hline(aes(yintercept=0))


# That's a strong correlation, explaining a good deal of the variance in GVA. 
# However, there is a distinct pattern in the residuals that suggests a missing variable.
# UKK12 has a significant public sector component to its GVA, so let's include public expenditure.


# Public Expenditure - Total Managed Expenditure.
# National statistic, published annually, with 3-year planned values also published.

# Read in public spending data.
public.spend <- read.csv("Public-Spending.csv")

# Fit linear model & summarise.
# Adjusted R-squared:  0.9445, p-value: 2.566e-12
public.lm <- lm(gva.bath$gva ~ public.spend$total.spend)
summary(public.lm)

# What's the effect of adding this factor to our model?
# F-stat: 54.299, p-value: 1.584e-06
anova(cpi.lm, lm(gva.bath$gva ~ cpi$cpi + public.spend$total.spend))

# Great, let's create a multivariate LM.
# Adjusted R-squared: 0.9725, p-value: 1.264e-13
combination.lm <- lm(gva.bath$gva ~ cpi$cpi + public.spend$total.spend)
summary(combination.lm)

# Plot resulting model against observed values.
ggplot(gva.bath) + geom_line(aes(year, gva, color="observed")) + 
  geom_line(aes(x=gva.bath$year, y=predict.lm(combination.lm), color="predicted"),
            linetype="dotted") +
  xlab("Year") + ylab("GVA") + ggtitle("GVA, observed vs. predicted")


# It still looks like we're missing something that signals the post-2008 recovery.
# House prices are usually quite sensitive to that sort of thing, and they're local.


# Land Registry House Price Index.
# Local statistic (UKK12), published monthly.

# Read in LR data & aggregate annually.
lr.raw <- read.csv("LR-HPI-Sales.csv")
lr.hpi <- aggregate(lr.raw$HPI, by=list(substr(lr.raw$Period, 1, 4)), FUN=mean)
colnames(lr.hpi) <- c("year", "hpi")

# Fit linear model & summarise.
# Adjusted R-squared:  0.9290, p-value: 2.084e-11
hpi.lm <- lm(gva.bath$gva ~ lr.hpi$hpi)
summary(hpi.lm)

# What's the effect of adding this factor to our model?
# F-stat: 93.37, p-value: 7.831e-08
anova(combination.lm, lm(gva.bath$gva ~ cpi$cpi + public.spend$total.spend + lr.hpi$hpi))

# Cool. Let's add it to our multivariate LM. We'll first create a dataframe of our factors.
combined.factors <- data.frame(cpi, public.spend$total.spend, lr.hpi$hpi)
colnames(combined.factors) <- c("year", "cpi", "total.spend", "hpi")

# Adjusted R-squared: 0.9959, p-value: < 2.2e-16
combination.lm <- lm(gva.bath$gva ~ cpi + total.spend + hpi, data=combined.factors)
summary(combination.lm)

# Plot resulting model against observed values.
ggplot(gva.bath) + geom_line(aes(year, gva, color="observed")) + 
  geom_line(aes(x=gva.bath$year, y=predict.lm(combination.lm), color="predicted"),
            linetype="dotted") +
  xlab("Year") + ylab("GVA") + ggtitle("GVA, observed vs. predicted")


# Looking good! Let's try predicting 2016's GVA in UKK12 using our model.
# 2016 values - CPI: 100.7, Public Spending: 771.9, HPI: 112.97

gva.2016 <- predict(combination.lm, data.frame(cpi=100.7, total.spend=771.9, hpi=112.97))

# The predicted GVA for 2016 is *18618.06*.
predicted.values <- rbind(data.frame(gva=predict.lm(combination.lm)), data.frame(gva=gva.2016))
gva.bath.extra <- rbind(gva.bath, data.frame(NUTS="UKK12", year=2016, gva=NA))

ggplot(gva.bath.extra) + geom_line(aes(year, gva, color="observed")) + 
  geom_line(aes(x=1997:2016, y=predicted.values$gva, color="predicted"),
            linetype="dotted") +
  xlab("Year") + ylab("GVA") + ggtitle("GVA, observed vs. predicted, extended to 2016")

