#########################
# Jeff Atkins via GitHub @atkinsjeff
#

# dependencies
require(ggplot2)
require(tidyverse)
require(cowplot)
require(gridExtra)
require(maps)
require(ggrepel)
require(viridis)
require(maptools)
require(mapproj)
require(rgeos)
require(rgdal)
require(usmap)
require(scatterpie)
library(dplyr)
library(forcats)
library(ggridges)

########################################
# This code produces the map in Figure 1. 
# data
csc <- read.csv("./data/pcl_output_20201109.csv") 
#pft <- read.csv("neon_pft_plots.csv")
csc$pft[csc$pft == "BDF"] <- "DBF"

# 
unique(csc$siteID)

# filters out plots that don't match criteria
csc %>%
  filter(cover.fraction > 25) %>%
  filter(can.max.ht < 50) %>%
  filter(management == "unmanaged") %>%
  filter(disturbance == "") %>%
  filter(plotID != "?" & plotID != "") %>%
  select(siteID, plotID, transect.length, pft, moch, can.max.ht, rugosity, 
         enl, fhd) %>%
  group_by(plotID, siteID, pft) %>%
  summarise_at(.vars = vars(moch, can.max.ht, rugosity, enl, fhd),
               .funs = "mean") %>%
  data.frame() -> cst

# cleans up naming conventions
names(cst) <- gsub(x = names(cst), pattern = "\\.", replacement = "_")  


# builds list of sites
cst %>%
  group_by(siteID) %>%
  summarize(plots = n_distinct(plotID)) %>%
  data.frame()  -> site.table

# list of pfts
pft.table <- data.frame(cst$siteID, cst$pft)

# bring in site metadata
site.meta <- read.csv("./data/pcl_site_metadata_powerlaw.csv")

# merge for map making
site.table <- merge(site.table, site.meta)

# map script
map_data("world") %>%
  filter(region == "USA") -> US
US <- map_data("state")

# shorten
site.table %>%
  select(longitude, latitude, siteID, plots) %>%
  data.frame() -> map.df

# change names
names(map.df)[1] <- "lon"
names(map.df)[2] <- "lat"
names(map.df)[4] <- "no_plots"

map.df <- na.omit(map.df)
map.df <- usmap_transform(map.df)

### MAP!!!!!
plot_usmap(exclude = c( .west_region, .west_south_central, .west_north_central),
           fill = "#eeeded", color = "black")+
  geom_point(data =  map.df, aes(x = lon.1, y = lat.1), size = 3, color = "black")+
  theme_void()+
  geom_label_repel(data = map.df, aes(x = lon.1, y = lat.1, label = siteID),
                   size = 3, alpha = 1, segment.colour = "black", segment.size = 1,
                   seed = 666)+
  labs(title = "Field Sites Surveyed")






###### ridge plots

p1 <- ggplot(cst, aes(y = siteID))+
  geom_density_ridges(aes(x = rugosity, fill = pft),
                      alpha = 0.8, color = "white")+
  labs(
    x = "Rugosity [m]",
    y = "Site",
    title = "Complexity by Site")+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))+
  scale_fill_manual(
    breaks = c("DBF", "ENF", "MF"),
    labels = c("DBF", "ENF", "MF"),
    values = c("DBF" = "#1B9E77",
               "ENF" = "#D95F02", 
               "MF" = "#7570B3"))+
  theme_ridges(grid = FALSE)

p2 <- ggplot(cst, aes(y = siteID))+
  geom_density_ridges(aes(x = can_max_ht, fill = pft),
                      alpha = 0.8, color = "white")+
  labs(
    x = "Max Canopy Height [m]",
    y = "Site",
    title = "Max Canopy Height by Site")+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))+
  scale_fill_manual(
    breaks = c("DBF", "ENF", "MF"),
    labels = c("DBF", "ENF", "MF"),
    values = c("DBF" = "#1B9E77",
               "ENF" = "#D95F02", 
               "MF" = "#7570B3"))+
  theme_ridges(grid = FALSE)


require(cowplot)
x11(width = 10, height = 6)
plot_grid(p1, p2, labels =  c("B", "C"), label_size = 12)








############################
#### FIGURE 2
#   complexity and height relationships

forte.pal <- fortedata::forte_colors()

p1 <- ggplot(cst, aes(x = log10(can_max_ht), y = log10(rugosity), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab(expression("R"[c]*{}*" [m]"))+
  theme_light()+
  geom_smooth(data = subset(cst, pft == "DBF"),
              method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "ENF"),
              method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "MF"),
              method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.justification = c(0.98, 0),
        legend.position = c(0.98, 0.01),
        legend.title = element_blank(),
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))

p2 <- ggplot(cst, aes(x = log10(can_max_ht), y = log10(enl), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab("ENL")+
  theme_light()+
  geom_smooth(data = subset(cst, pft == "DBF"),
              method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "ENF"),
              method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "MF"),
              method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.position = "none")

p3 <- ggplot(cst, aes(x = log10(can_max_ht), y = log10(fhd), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.1, 0.5, 0.5), guide = FALSE)+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab("FHD")+
  theme_light()+
  # geom_smooth(data = subset(cst, pft == "DBF"),
  #             method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "ENF"),
              method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "MF"),
              method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.position = "none")




require(cowplot)
x11(width = 10, height = 4)
plot_grid(p1, p2, p3, labels =  c("A", "B", "C"), nrow = 1, label_size = 12)



p4 <- ggplot(cst, aes(x = log10(moch), y = log10(rugosity), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.1, 0.1, 0.5), guide = FALSE)+
  xlab("MOCH [m]")+
  ylab(expression("R"[c]*{}*" [m]"))+
  theme_light()+  
  # geom_smooth(data = subset(cst, pft == "DBF"),
  #             method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "MF"),
              method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "ENF"),
  #             method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  theme(legend.position = "none")

p5 <- ggplot(cst, aes(x = log10(moch), y = log10(enl), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.1, 0.1), guide = FALSE)+
  xlab("MOCH [m]")+
  ylab("ENL")+
  geom_smooth(data = subset(cst, pft == "DBF"),
              method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "MF"),
  #             method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  # # geom_smooth(data = subset(cst, pft == "ENF"),
  #             method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  theme_light()+
  theme(legend.position = "none")


p6 <- ggplot(cst, aes(x = log10(moch), y = log10(fhd), fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.1, 0.1, 0.5), guide = FALSE)+
  xlab("MOCH [m]")+
  ylab("FHD")+
  theme_light()+
  # geom_smooth(data = subset(cst, pft == "ENF"),
  #             method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  geom_smooth(data = subset(cst, pft == "MF"),
              method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  theme(legend.position = "none")



require(cowplot)
x11(width = 10, height = 7)
plot_grid(p1, p2, p3, p4, p5, p6, labels =  c("A", "B", "C", "D", "E", "F"), nrow = 2, label_size = 12)










### FIGURE 3 (TOP)


###### NLS PLOT
x11(width = 4, height = 4)
ggplot(cst, aes(x = can_max_ht, y = rugosity, fill = pft, alpha = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab(expression("R"[c]*{}*" [m]"))+
  theme_light()+
  theme(legend.justification = c(0.98, 0), 
        legend.position = c(0.98, 0.01),
        legend.title = element_blank(), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
        legend.text = element_text(size = 12))+
  geom_smooth(method = "nls", method.args = list(
    formula = (y ~ a * x^b), start = list(a = 1, b = 1)),
    data = subset(cst, pft == "DBF"),
    se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE )+
  geom_smooth(method = "nls", method.args = list(
    formula = (y ~ a * x^b), start = list(a = 1, b = 1)),
    data = subset(cst, pft == "ENF"),
    se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE )+
  geom_smooth(method = "nls", method.args = list(
    formula = (y ~ a * x^b), start = list(a = 1, b = 0.1)),
    data = subset(cst, pft == "MF"),
    se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE )


x11(width = 4, height = 4)
ggplot(cst, aes(x = can_max_ht , y = fhd, alpha = pft, fill = pft))+
  geom_point(size = 2, shape = 21)+
  scale_fill_brewer(palette = "Dark2")+
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = FALSE)+
  xlab(expression("MOCH [m]"))+
  ylab(expression("FHD"))+
  theme_light()+
  # geom_smooth(data = subset(cst, pft == "DBF"),
  #             method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "ENF"),
  #             method = lm, se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE)+
  # geom_smooth(data = subset(cst, pft == "MF"),
  #             method = lm, se = FALSE, color = "#7570B3", size = 2, show.legend = FALSE)+
  # theme(legend.justification = c(0.98, 0), 
  #       legend.position = c(0.98, 0.01),
  #       legend.title = element_blank(), 
  #       legend.background = element_rect(linetype = 1, size = 0.5, color = "darkgrey"),
  #       legend.text = element_text(size = 12))+
theme(legend.position = "none")+
  geom_smooth(method = "nls", method.args = list(
    formula = (y ~ a * x^b), start = list(a = 1, b = 1)),
    data = cst, se = FALSE, color = "#2F4F4F", size = 2, 
    show.legend = FALSE, aes(fill = NULL, alpha = NULL) )

geom_smooth(method = "nls", method.args = list(
  formula = (y ~ a * x^b), start = list(a = 1, b = 1)),
  data = subset(cst, pft == "ENF"),
  se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE )


############################  bOTTOM TILE PLOTS

geom_smooth(data = subset(cst, pft == "DBF"),
            method = lm, se = FALSE)+
  x11(width = 10, height = 4)
ggplot(cst, aes(x = can_max_ht, y = rugosity))+
  geom_bin2d(bins = 25, color = "white")+
  scale_fill_gradient(low = "#00AFBB", high = "#FC4E07", name = "No. of Plots" )+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab(expression("R"[c]*{}*" [m]"))+
  theme_bw()+
  theme(legend.justification = c(0.01, 0.98), legend.position = c(0.01, 0.98), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"))+
  facet_grid(.~pft)

x11(width = 10, height = 4)
ggplot(cst, aes(x = can_max_ht, y = fhd))+
  geom_bin2d(bins = 25, color = "white")+
  scale_fill_gradient(low = "#00AFBB", high = "#FC4E07", name = "No. of Plots" )+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab(expression("FHD"))+
  theme_bw()+
  theme(legend.justification = c(0.99, 0.01), legend.position = c(0.99, 0.01), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"))+
  facet_grid(.~pft)

x11(width = 10, height = 4)
ggplot(cst, aes(x = can_max_ht, y = enl))+
  geom_bin2d(bins = 25, color = "white")+
  scale_fill_gradient(low = "#00AFBB", high = "#FC4E07", name = "No. of Plots", limits = c(0, 8), breaks = c(0, 2, 4, 6, 8))+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab(expression("ENL"))+
  theme_bw()+
  stat_smooth(method = "lm", color = "#444444", size = 1.5, se = FALSE)+
  theme(legend.justification = c(0.01, 0.98), legend.position = c(0.01, 0.98), 
        legend.background = element_rect(linetype = 1, size = 0.5, color = "black"))+
  facet_grid(.~pft)













###### PREVIOUS SUPPLEMENTAL
cst %>%
  select(siteID, moch, can_max_ht) %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarize(moch = mean(moch), 
                   max_ht = mean(can_max_ht), max.sd = sd(can_max_ht)) %>%
  data.frame() -> bob

cst %>%
  select(siteID, moch, can_max_ht) %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarize(moch.sd = sd(moch)) %>%
  data.frame() -> bill

bob <- merge(bob, bill)
require(ggrepel)

h2 <- ggplot(bob, aes(x = max_ht, y = moch, label = siteID))+
  geom_errorbar(aes(ymin = moch - moch.sd, ymax = moch + moch.sd), width = 0.1, color = "grey")+
  geom_errorbarh(aes(xmin = max_ht - max.sd, xmax = max_ht + max.sd), width = 0.1, color = "grey")+
  geom_point(size = 8, shape = 21, fill = "grey")+
  #scale_fill_brewer(palette = "Dark2")+
  xlab(expression("H"[Max]*{}*" [m]"))+
  ylab("MOCH [m]")+
  theme_light()+
  # xlim(5, 45)+
  # ylim(5, 30)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(size = 12))+
  geom_abline(slope = 1, intercept = 0, size = 1, color = "black")+
  geom_label_repel(fill = "white", box.padding = 0.6)

x11(width = 12, height = 6)
plot_grid(h1, h2, labels =  c("A", "B"), label_size = 12)


#####
dbf.model  <- 
  x11()
ggplot(cst, aes(x = can_max_ht, y = rugosity, fill = pft, alpha = pft))+
  #geom_point(size = 2, shape = 21)+
  #scale_fill_brewer(palette = "Dark2")+  
  #scale_alpha_manual(values = c(0.5, 0.5, 0.1), guide = FALSE)+
  geom_smooth(data = subset(cst, pft == "DBF"), 
              method = "nls", formula = (y ~ a * x ^ b), 
              method.args = list(start = c(a = 1, b = 1)),
              se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE )+
  stat_smooth(data = subset(cst, pft == "ENF"), 
              method = "nls", formula = (y ~ a * x^b),
              method.args = list(start = c(a = 1, b = 1)),
              se = FALSE, color = "#D95F02", size = 2, show.legend = FALSE )

geom_smooth(data = subset(cst, pft == "DBF"),
            method = lm, se = FALSE, color = "#1B9E77", size = 2, show.legend = FALSE)



