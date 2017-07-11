setwd("~/Development/GVA")
library(ggplot2)
library(directlabels)
library(reshape2)

# Read in GVA Table 2 (per head)
gva.perhead.raw <- read.csv("GVA-PerHead.csv")
colnames(gva.perhead.raw)[22] <- "X2015"

# Read in GVA Table 3 (indices).
gva.indices.raw <- read.csv("GVA-Indices.csv")
colnames(gva.indices.raw)[22] <- "X2015"

# Read in GVA Table 6 (industry).
gva.industry.raw <- read.csv("GVA-Industry.csv")
colnames(gva.industry.raw)[24] <- "X2015"

# Create long-format versions of tables.
gva.perhead <- melt(gva.perhead.raw[,c(2, 4:22)], id.vars = "NUTS.code")
gva.perhead$variable <- as.numeric(substr(gva.perhead$variable, 2, 5))
colnames(gva.perhead) <- c("NUTS", "year", "gva")

gva.indices <- melt(gva.indices.raw[,c(2, 4:22)], id.vars = "NUTS.code")
gva.indices$variable <- as.numeric(substr(gva.indices$variable, 2, 5))
colnames(gva.indices) <- c("NUTS", "year", "gva")

gva.industry <- melt(gva.industry.raw[,c(2, 4, 6:24)], id.vars = c("NUTS.code", "SIC07.code"))
gva.industry$variable <- as.numeric(substr(gva.industry$variable, 2, 5))
colnames(gva.industry) <- c("NUTS", "SIC", "year", "gva")


# Plot GVA (per head) at multiple levels of aggregation. Using 'per head' to allow comparison.
gva.perhead.subset <- subset(gva.perhead, NUTS == 'UK' |
                               NUTS == "UKK" | NUTS == "UKK12")
ggplot(gva.perhead.subset, aes(year, gva, group = NUTS, color = NUTS)) +
  geom_line() + ylab("GVA per head of population") + ggtitle("GVA Trends, 1997-2015")


# Plot GVA (indices) at multiple levels of aggregation.
gva.indices.subset <- subset(gva.indices, NUTS == 'UK' |
                               NUTS == "UKK" | NUTS == "UKK12")
ggplot(gva.indices.subset, aes(year, gva, group = NUTS, color = NUTS)) +
  geom_line() + ylab("GVA index") + ggtitle("GVA Trends, 1997-2015")


# Plot GVA (indices) at multiple levels of aggregation.
gva.industry.subset <- subset(gva.industry, NUTS == 'UKK12' & SIC != "All")

# Line plot
ggplot(gva.industry.subset, aes(year, gva, group = SIC, color = SIC)) +
  geom_line() +
  geom_dl(aes(label = SIC), method = list(dl.trans(x=x+0.1),vertical.qp("last.points"), cex = 0.6)) +
  theme(legend.position="none") +
  xlim(1997, 2016) +
  ylab("GVA") + ggtitle("UKK12 GVA Trends by Industry, 1997-2015")

# Stacked area plot
ggplot(gva.industry.subset, aes(year, gva, group = SIC, color = SIC, fill = SIC)) +
  geom_area() +
  xlim(1997, 2016) +
  ylab("GVA") + ggtitle("UKK12 GVA Trends by Industry, 1997-2015")
