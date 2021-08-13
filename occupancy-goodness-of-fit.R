library(dplyr)
library(ggplot2)
library(ggpubr)
library(RPresence)

### load inputs

load("Mongolia_occupancy_inputs.Rdata")
load("output/fitted_occ_models.Rdata") # previous fitted models on full dataset

### Create a trimmed dataset to estimate goodness of fit 

Pres.t1<-Pres[,c(1:4)] # survey presence/absence data
Site_Cov <- Site_Cov # doesn't change
SurvCov.t1 <- SurvCov %>% dplyr::filter(Time <= 3)

### Fitting occupancy models

# Create PAO file for occupancy analysis
Mongol_Pres.t1<-createPao(data= Pres.t1[,c(2:4)],unitcov=Site_Cov, survcov = SurvCov.t1) #Trimmed dataset

# Run model on whole and trimmed dataset for c-hat estimation of goodness of fit
Mongol.5.gof<-occMod(model=list(psi~Rgd_Availz+Alt_z+I(Alt_z^2)+Forest_z*NDVI_z, p~ Walk*Ruggedness_z), 
                     data = Mongol_Pres.t1, type = "so",modfitboot = 1000)
Mongol.5.gof$gof #Check the value of c-hat. If less than 2, model fit is good enough
Mongol.5.gof.full<-occMod(model=list(psi~Rgd_Availz+Alt_z+I(Alt_z^2)+Forest_z*NDVI_z, p~ Walk*Ruggedness_z), 
                          data = Mongol_Pres, type = "so",modfitboot = 1000)
Mongol.5.gof.full$gof #Check the value of c-hat. If less than 2, model fit is good enough

# Create a combined dataset for coefficients from full and trimmed datasets
data.frame(coefficients(Mongol.5, "psi"), coefficients(Mongol.5.gof, "psi"))
data.frame(coefficients(Mongol.5, "p"), coefficients(Mongol.5.gof, "p"))

# Upload saved file with coefficients from full and trimmed datasets
gof.compare<-read.csv("./Final Occupancy Files/gof_compare.csv", 
                      header=TRUE, stringsAsFactors = TRUE)

# Plot the fited estimates of psi match in case of full and trimmed datasets
# Near straight line indicates results from full and trimmed datasets are similar
plot(fitted(Mongol.5.gof.full, "psi")$est, fitted(Mongol.5.gof, "psi")$est)

# Plot coefficients of the full and trimmed datasets for comparison
ggplot(gof.compare, aes (x=est.FULL, y = Parameters, color = Data), size = 4)+
  geom_point(position = position_dodge(0.9))+
  geom_errorbar(aes(y = Parameters, xmin=est.FULL-se.FULL, xmax=est.FULL+se.FULL), 
                position = position_dodge(width = 0.9))+labs(x = "Coefficient")+
  geom_hline(yintercept=4.5)+
  theme_classic()