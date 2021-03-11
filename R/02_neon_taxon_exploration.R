# just how many species are in the NEON data set?

# LEt's try to bring in all these files

require(tidyverse)
require(data.table)
require(ggplot2)
# options(stringsAsFactors=FALSE)

# this brings in all of the apparentindividual files with plot info and dbh
# list.files(path = "./data/NEON_struct-woody-plant_all",
#            pattern = "*apparent*", 
#            full.names = T) %>% 
#     map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
#     data.frame() -> df.ind

# this brings in the species and mapping information 
list.files(path = "./data/NEON_struct-woody-plant_all",
           pattern = "*mapping*", 
           full.names = T) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
    data.frame() -> df.map.all
    
# NEON taxons
tax.jenk <- read.csv("./data/neon_taxon_jenkins.csv")

# how many are there? 
taxons <- unique(df.map.all$taxonID)

taxons <- data.frame(taxons)

names(taxons)[1]<-paste("taxonID")

df2 <- merge(tax.jenk, taxons, by = "taxonID", all = TRUE)





#write.csv(df2, "./data/taxons_from_dataset.csv")
