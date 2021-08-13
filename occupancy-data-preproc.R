### Data pre-processing steps
### Reads in messy csv files, extracts variables needed for occupancy modelling, makes sure variables are consistently named across datasets,
### and saves clean csv files and a Rdata file with all objects needed for occupancy.

library(dplyr)
library(tidyr)

# Contains covariates for predicting to surveyed area
Mongolia1<-read.csv(file="data/csv/Occupancy_final_1200.csv", header=TRUE, stringsAsFactors = TRUE)
names(Mongolia1)[38]<-"Rgd_Availz" # Rename covariate value to match the names used in the PAO
Mongolia_studyarea <- Mongolia1 %>% dplyr::select(Site = SL_Mongo_2, Rgd_Availz, NDVI_z, Alt_z, Forest_z)

# Contains covariates for predicting to entire SL range in Mongolia
Mongolia.country <- read.csv(file="data/csv/Country_Occ.csv")
Mongolia.4000 <- read.csv("data/csv/Occupancy_final.csv")
Mongolia.country.covs <- inner_join(Mongolia.4000, Mongolia.country, by = "SL_Mongo_2") #merge covariates
names(Mongolia.country.covs)[38]<-"Rgd_Availz" #Rename a covariate to match the names used in PAO file
Mongolia_fullrange <- Mongolia.country.covs %>% dplyr::select(Site = SL_Mongo_2, Rgd_Availz, NDVI_z, Alt_z, Forest_z)

# Contains site ("unit") specific covariates
Site_Cov<-read.csv("data/csv/Site_Cov_2019.csv")
Site_Cov <- Site_Cov %>% dplyr::select(Site, Rgd_Availz, NDVI_z, Alt_z, Forest_z, Rgd_Avail, NDVI, Alt = Altitude, Forest = Forest_Cov)

# Contains survey specific covariates
Walk_cov<-read.csv("data/csv/Seg_Walk_R.csv")
Rgd_cov<-read.csv("data/csv/Seg_Rgd_fixed.csv")
Walk_cov <- Walk_cov %>% rename(id = Walk)
Rgd_cov <- Rgd_cov %>% rename(id = Row.Labels)
Walk_cov_l <- Walk_cov %>% pivot_longer(cols = -id, names_to = "Time", names_prefix = "X", values_to = "Walk") %>% mutate(Time = as.numeric(Time))
Rgd_cov_l <- Rgd_cov %>% pivot_longer(cols = -id, names_to = "Time", names_prefix = "X", values_to = "Ruggedness_z") %>% mutate(Time = as.numeric(Time))
SurvCov <- Walk_cov_l %>% left_join(Rgd_cov_l, by = c("id", "Time")) %>% arrange(Time)

# Contains presence/absence results from survey
Pres<-read.csv("data/csv/Occu_SL_2019_R.csv")

# Write clean csvs
write.csv(Mongolia_studyarea, file="data/csv/cleaned/Mongolia_studyarea.csv", row.names = FALSE)
write.csv(Mongolia_fullrange, file="data/csv/cleaned/Mongolia_fullrange.csv", row.names = FALSE)
write.csv(Site_Cov, file = "data/csv/cleaned/Site_Cov_2019.csv", row.names = FALSE)
write.csv(SurvCov, "data/csv/cleaned/SurvCovs.csv", row.names = FALSE)
write.csv(Pres, "data/csv/cleaned/Occu_SL_2019_R.csv", row.names = FALSE)

# Write everything to one file
save(SurvCov, Site_Cov, Pres, Mongolia_studyarea, Mongolia_fullrange, file = "data/Mongolia_occupancy_inputs.Rdata")


