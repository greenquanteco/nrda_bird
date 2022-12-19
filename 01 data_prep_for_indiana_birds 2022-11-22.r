# script to set up data for level of effort analysis

#Base directory for data####
base_dir <- "Data/"
base_dir <- "C:/Users/ngreen62/OneDrive - Kennesaw State University/research"
base_dir <- paste(base_dir, "nrda/bird/indiana_bird_for_nick", sep="/")


#Read in  metadata####
#has info on which species are regional conservation concern
#and which are interior forest specialists
in.name <- "bird_taxa_info_2022-11-02.txt"
in.path <- paste(base_dir, in.name, sep="/")
met_raw <- read.table(in.path, header=TRUE, stringsAsFactors = FALSE)

#Read in resampled bird data. ####
#All species are non-passage migrant landbirds;
#great horned owl is also already excluded (nocturnal)
#Currently in long format
in.name <- "bird_resamp_data_2022-11-09.txt"
in.path <- paste(base_dir, in.name, sep="/")
dat_long <- read.table(in.path, header=TRUE, stringsAsFactors = FALSE)

#Get species codes for each of the three species groups of interest####

#Non-passage migrant, diurnal landbirds
landbird_spp <- unique(dat_long$sp_alpha)

# looks like there are some species in met_raw that are 
# NOT in the resampled data:
sort(met_raw$sp_alpha[which(!met_raw$sp_alpha %in% dat_long$sp_alpha)])

#Partners in Flight (PIF) regional conservation concern species in 
#Bird Conservation Region 23, designated in 2021
pif_rc_spp <- (subset(
  met_raw[,c("sp_alpha","pif_rc_2021","landbird")],
  pif_rc_2021 == 1 & sp_alpha %in% dat_long$sp_alpha)
  )$sp_alpha
pif_rc_spp

#Interior forest specialists, defined by Archer et al. 2019 as having 
#medium or high confidence in specialization
forest_spp <-
  (
    subset(
      met_raw[,c("sp_alpha","int_for_spec")],
      int_for_spec == "high"|int_for_spec == "med")
  )$sp_alpha
forest_spp

#Prep data into a "wide" matrix format####

#Convert site to numeric
#dat_long$site <- 
#  as.numeric(
#    factor(dat_long$site, levels = c(
#      "Bell", #site 1
#      "Bluffton", # site 2
#      "Deetz", # site 3
#      "Holden" #site 4
#      ))
#  )#as.numeric

# easier way, and you don't lose the site names:
dat_long$site.num <- match(dat_long$site, sort(unique(dat_long$site)))

unique(dat_long[,c("site", "site.num")])
#     site site.num
#     Bell        1
# Bluffton        2
#    Deetz        3
#   Holden        4

dat_wide <- reshape(dat_long,
                    #(notes are for how reshape works so I remember later)
                    idvar = "resamp_id", #"Anchor" column (unique value per row)
                    timevar = "sp_alpha", #New column names
                    v.names = "obs_pres", #New column values
                    direction = "wide" #Make data wide format
                    )

#Get rid of prefix on bird column names
# suggest do this first, will make calculations easier later
spp.cols <- grep("obs_pres[.]", names(dat_wide))
# nice!
names(dat_wide) <- gsub("obs_pres[.]","",names(dat_wide))


#Convert to list of matrices
# easier way, see below
# dat_mat_ls <-
# lapply(
#   list(landbird = landbird_spp,
#        pif_rc = pif_rc_spp,
#        forest = forest_spp),
#   function(x){
#     subs_dat <- cbind(
#       dat_wide[,2:7],
#       dat_wide[,c(x)]
#     )
#     
#     return(
#       as.matrix(subs_dat)
#     )
#   }
#   )

# data frame to use with JAGS
dat.cols <- c(8, 3:7)
dat <- dat_wide[,dat.cols]
names(dat)[1] <- "site"

# should be TRUE (no NAs):
all(apply(dat_wide[,spp.cols], 2, function(x){length(which(is.na(x)))}) == 0)

# should be TRUE (no negatives):
all(apply(dat_wide[,spp.cols], 2, function(x){length(which(x < 0))}) == 0)

# function to count nonzeros
count.pres <- function(x){length(which(x > 0))}

# total species richness
dat$rich.tot <- apply(dat_wide[,spp.cols], 1, count.pres)

# PIF concern species richness
# should be TRUE:
all(pif_rc_spp %in% names(dat_wide))
dat$rich.pif <- apply(dat_wide[,pif_rc_spp], 1, count.pres)

# interior forest species richness
# should be TRUE:
all(forest_spp %in% names(dat_wide))
dat$rich.for <- apply(dat_wide[,forest_spp], 1, count.pres)

# columns in dat:
#1) site: numeric id for site where data were collected
#2) nsamps: number of samples in simulated survey, i.e., point count plots
#3) nsubs: number subsamples in simulated survey, i.e., visits to each point
#4) repl: replicate simulation. Used for cutting dataset down for model testing
#5) hab_woody: at least one sample in simulated survey had primarily woody 
#              vegetation cover
#6) hab_grass: at least one sample in simulated survey had primarily grassy/
#              graminoid veg cover
#7) rich.tot: total bird species richness
#8) rich.pif: richness of species of Partners in Flight concern
#9) rich.for: richness of forest interior specialists

#Goal for each of the three species diversity metrics (landbird species 
#richness, PIF region conservation concern species, interior forest species)
#is to find the top Michaelis-Menten model for two separate independent variables:
#nsamps and nsubs (six top models total, 3 metrics * 2 independent variables)

# write out final organized dataset for JAGS
out.name <- "bird_data_org_2022-12-09.csv"
out.path <- paste(base_dir, out.name, sep="/")
write.csv(dat, out.path, row.names=FALSE)

# end script!