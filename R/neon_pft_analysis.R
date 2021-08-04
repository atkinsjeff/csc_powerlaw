# This script coallates NEON veg structure data
# Author:  jeff atkins (@atkinsjeff)

# dependenancies
require(tidyverse)
require(readr)
require(data.table)
require(ggplot2)
require(RColorBrewer)

# NEON data files director
data.dir <- "./data/neon/"

# list files
ind.files <- list.files(path = data.dir, pattern = "*vst_apparent*", full.names = TRUE)
map.files <- list.files(path = data.dir, pattern = "*vst_mapping*", full.names = TRUE)

# read in files
ind.files %>%
  map_df(~read_csv(., col_types = cols(.default = "c"))) -> ind

map.files %>%
  map_df(~read_csv(., col_types = cols(.default = "c"))) -> mapp


# ind is the data frame of all indinviduals. We need to check the values and test what we want

unique(ind$siteID)
unique(ind$shape)
# plantStatus
# growthForm
# 
# stemDiameter
# height
# 
# baseCrownDiameter
# maxCrownDiameter <chr>,
# ninetyCrownDiameter <chr>,
# canopyPosition <chr>,
# shape <chr>,
# maxBaseCrownDiameter <chr>, ninetyBaseCrownDiameter <chr>,

# sort ind down
pft <- ind[, c("domainID", "siteID", "plotID", "individualID", "plantStatus", "growthForm", "measurementHeight", "stemDiameter",
               "height", "maxCrownDiameter", "ninetyCrownDiameter", "shape", "maxBaseCrownDiameter", "ninetyBaseCrownDiameter")]
# sort out bad bois
pft %>%
  filter(plantStatus == "Live") -> pft

pft %>%
  filter(growthForm == "single bole tree" | growthForm ==   "multi-bole tree"  | growthForm ==  "small tree") -> pft

x11()
plot(pft$height, pft$maxCrownDiameter)

# bring in map data
map.files %>%
  map_df(~read_csv(., col_types = cols(.default = "c"))) -> map
# filter down
map %>%
  select(individualID, taxonID ) %>%
  unique() -> map

# merge them together
big.boi <- merge(pft, map, by = c("individualID"))

# NEON taxons
tax.jenk <- read.csv("./data/neon_taxon_jenkins.csv")

# how many are there?
taxons <- unique(big.boi$taxonID)

taxons <- data.frame(taxons)

names(taxons)[1]<-paste("taxonID")

df2 <- merge(tax.jenk, taxons, by = "taxonID", all = TRUE)

# big one
df <- merge(big.boi, df2)

# cleaning house
df$height <- as.numeric(df$height)
df$maxCrownDiameter <- as.numeric(df$maxCrownDiameter)
df$ninetyCrownDiameter <- as.numeric(df$ninetyCrownDiameter)
df$maxBaseCrownDiameter <- as.numeric(df$maxBaseCrownDiameter)
df$ninetyBaseCrownDiameter <- as.numeric(df$ninetyBaseCrownDiameter)
df$pft <- as.factor(df$pft)
df$stemDiameter <- as.numeric(df$stemDiameter)

#remove outlier
df %>%
  filter(maxCrownDiameter < 50) %>%
  data.frame() -> df

# plot
x11()
ggplot(df, aes(x = height, y = maxCrownDiameter, color = as.factor(pft), fill = as.factor(pft)))+
  geom_point(shape = 21, size = 3, color = "black", alpha = 0.3)+
  xlim(0, 75)+
  ylim(0, 30)+
  geom_smooth(method = "lm" )

#
df$crownArea <- pi * (df$maxCrownDiameter / 2) * (df$ninetyCrownDiameter / 2)


df %>%
  filter(pft == "ENF" | pft == "DBF") ->df3

x11(width = 10, height = 4)
ggplot(df3, aes(x = height, y = maxCrownDiameter))+
  geom_bin2d(bins = 25, color = "white")+
  scale_fill_gradient(low = "#00AFBB", high = "#FC4E07", name = "No. of Trees", limits = c(0, 100), breaks = c(0, 20, 40, 60, 80))+
  xlab(expression("Height [m]"))+
  ylab(expression("Max Crown Diameter [m]"))+
  theme_bw()+
  stat_smooth(method = "lm", color = "#444444", size = 1.5, se = FALSE)+
  theme(legend.justification = c(0.01, 0.98), legend.position = c(0.01, 0.98), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"))+
  facet_grid(.~pft)


x11()
ggplot(df, aes(x = height, y = maxCrownDiameter, fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.3, 0.3, 0.3), guide = FALSE)+
  xlab(expression("Height [m]"))+
  ylab(expression("Crown Diameter [m]"))+
  theme_light()+
  geom_smooth(data = subset(df, pft == "DBF"),
              method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(df, pft == "ENF"),
              method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(df, pft == "EBF"),
              method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.justification = c(0.98, 0), 
        legend.position = c(0.98, 0.01),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))



# make area
df3$crownArea <- pi * (df3$maxCrownDiameter / 2) * (df3$ninetyCrownDiameter / 2)



#  make regression stats
summary(lm(log10(crownArea) ~ log10(height), data = subset(df3, pft == "ENF")))
summary(lm(log10(crownArea) ~ log10(height), data = subset(df3, pft == "DBF")))

# ANCOVA for difference in slopes
require(car)

# make the base ANOVA
fit.pft.neon <- aov(log10(crownArea) ~ log10(height) + pft, data = df3)
Anova(fit.pft.neon, type="III")

x11(width = 4, height = 4)
p.log <- ggplot(df3, aes(x = log10(height), y = log10(crownArea), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("log10(Height) [m]"))+
  ylab(expression(" log10(Crown Area) [m]"))+
  theme_light()+
  geom_smooth(data = subset(df3, pft == "DBF"),
              method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(df3, pft == "ENF"),
              method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "MF"),
  #             method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.justification = c(0.98, 0), 
        legend.position = c(0.98, 0.01),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))

# make area
df3$baseCrownArea <- pi * (df3$maxBaseCrownDiameter / 2) * (df3$ninetyBaseCrownDiameter / 2)




x11(width = 4, height = 4)
ggplot(df3, aes(x = log(height), y = log(baseCrownArea), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("ln Height [m]"))+
  ylab(expression(" ln Crown Diameter [m]"))+
  theme_light()+
  geom_smooth(data = subset(df3, pft == "DBF"),
              method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(df3, pft == "ENF"),
              method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "MF"),
  #             method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.justification = c(0.98, 0), 
        legend.position = c(0.98, 0.01),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))



# make ratio
df3$CDH.ratio <- df3$crownArea / df3$height
# make dbh _class
df3$dbh_class <- round(df3$stemDiameter/5) * 5

# dbh.classes <- data.frame(table(inv$dbh_class, inv$plot_id))
# a <- spread(dbh.classes, Var1, Freq)

df3 %>%
  group_by(pft, dbh_class) 

df3 %>%
  filter(CDH.ratio < 25) %>%
  data.frame() -> df4

x11(width = 12, height = 4)
ggplot(df4, aes(x = as.factor(dbh_class), y = CDH.ratio, fill = pft))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  #scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("DBH Class [cm]"))+
  ylab(expression(" Crown Area to Height Ratio"))+
  theme_light()+
  theme(legend.justification = c(0.98, 0.98), 
        legend.position = c(0.98, 0.98),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))

x11(width = 10, height = 4)
p.dbh.box <- ggplot(df4, aes(x = as.factor(dbh_class), y = CDH.ratio, fill = pft))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  #scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("DBH Class [cm]"))+
  ylab(expression(" Crown Area to Height Ratio"))+
  theme_light()+
  theme(legend.justification = c(0.98, 0.98), 
        legend.position = c(0.98, 0.98),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))

#######

# bring in latitude
# list files
lat.files <- list.files(path = data.dir, pattern = "*vst_perplot*", full.names = TRUE)

# read in files
lat.files %>%
  map_df(~read_csv(., col_types = cols(.default = "c"))) -> lat

# shorten
lat <- lat[, c("plotID", "decimalLatitude", "elevation")]

# try to make pft by plot
df5 <- merge(df4, lat, by = c("plotID"))
df5$decimalLatitude <- as.numeric(df5$decimalLatitude)
df5$elevation <- as.numeric(df5$elevation)

# make lat class
# make dbh _class
df5$lat_class <- round(df5$decimalLatitude/5) * 5


x11(width = 4, height = 4)
p.lat <- ggplot(df5, aes(x = as.factor(lat_class), y = CDH.ratio, fill = pft))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("Latitude"))+
  ylab(expression("Crown Area to Height Ratio"))+
  theme_light()+
  # geom_smooth(data = subset(df3, pft == "DBF"),
  #             method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(df3, pft == "ENF"),
  #             method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "MF"),
  #             method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.justification = c(0.98, 0.98), 
        legend.position = c(0.98, 0.98),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))


x11(width = 4, height = 4)
p.elev <- ggplot(df5, aes(x = elevation, y =CDH.ratio, fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("Elevation [m]"))+
  ylab(expression("Crown Area to Height Ratio"))+
  theme_light()+
  geom_smooth(data = subset(df5, pft == "DBF"),
              method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(df5, pft == "ENF"),
              method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "MF"),
  #             method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.justification = c(0.98, 0.98), 
        legend.position = c(0.98, 0.98),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))


require(cowplot)
x11(width = 10, height = 4)
plot_grid(p1, p2, p3, labels =  c("A", "B", "C"), nrow = 1, label_size = 12)

top_row <- plot_grid(p.log, p.elev, p.lat, labels = c('A', 'B', 'C'), nrow = 1, label_size = 12)

x11(width = 10, height = 6)
plot_grid(top_row, p.dbh.box, labels = c('', 'D'), label_size = 12, ncol = 1)
