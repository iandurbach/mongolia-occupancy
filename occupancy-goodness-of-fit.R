library(dplyr)
library(ggplot2)
library(ggpubr)
library(RPresence)

### load inputs

load("data/Mongolia_occupancy_inputs.Rdata")
load("output/fitted_occ_models.Rdata") # previous fitted models on full dataset

### Create PAO file for occupancy analysis
Mongol_Pres <- createPao(data = Pres[,c(2:41)], unitcov = Site_Cov, survcov = SurvCov) 

### Create a trimmed dataset to estimate goodness of fit 

Pres.t1<-Pres[,c(1:4)] # survey presence/absence data
Site_Cov <- Site_Cov # doesn't change
SurvCov.t1 <- SurvCov %>% dplyr::filter(Time <= 3)

### Fitting occupancy models

# Create PAO file for occupancy analysis
Mongol_Pres.t1<-createPao(data= Pres.t1[,c(2:4)],unitcov=Site_Cov, survcov = SurvCov.t1) #Trimmed dataset

# Run model on whole and trimmed dataset for c-hat estimation of goodness of fit
Mongol.5.gof <- occMod(model=list(psi~Rgd_Availz+Alt_z+I(Alt_z^2)+Forest_z*NDVI_z, p~ Walk*Ruggedness_z), 
                     data = Mongol_Pres.t1, type = "so",modfitboot = 1000)
Mongol.5.gof$gof #Check the value of c-hat. If less than 2, model fit is good enough
Mongol.5.gof$gof$chat

Mongol.5.gof.full <- occMod(model=list(psi~Rgd_Availz+Alt_z+I(Alt_z^2)+Forest_z*NDVI_z, p~ Walk*Ruggedness_z), 
                          data = Mongol_Pres, type = "so",modfitboot = 1000)
Mongol.5.gof.full$gof$chat #Check the value of c-hat. If less than 2, model fit is good enough

# uncomment to save
# save(Mongol.5.gof, Mongol.5.gof.full, file = "output/fitted_gof_objects.Rdata")

# Create a combined dataset for coefficients from full and trimmed datasets
data.frame(coefficients(Mongol.5, "psi"), coefficients(Mongol.5.gof, "psi"))
data.frame(coefficients(Mongol.5, "p"), coefficients(Mongol.5.gof, "p"))

coef_full <- rbind(coefficients(Mongol.5, "psi"), coefficients(Mongol.5, "p"))
coef_trim <- rbind(coefficients(Mongol.5.gof, "psi"), coefficients(Mongol.5.gof, "p"))
coef_all <- rbind(coef_full, coef_trim)
coef_all <- data.frame(Parameter = c(row.names(coef_full), row.names(coef_trim)), est = coef_all$est, se = coef_all$se, Data = c(rep("Full dataset", nrow(coef_full)), rep("Trimmed dataset", nrow(coef_trim))))

# Plot the fited estimates of psi match in case of full and trimmed datasets
# Near straight line indicates results from full and trimmed datasets are similar
plot(fitted(Mongol.5.gof.full, "psi")$est, fitted(Mongol.5.gof, "psi")$est)

# Plot coefficients of the full and trimmed datasets for comparison
ggplot(coef_all, aes (x=est, y = Parameter, color = Data), size = 4)+
  geom_point(position = position_dodge(0.5))+
  geom_errorbar(aes(y = Parameter, xmin=est-1.96*se, xmax=est+1.96*se), 
                position = position_dodge(width = 0.5))+labs(x = "Coefficient")+
  geom_hline(yintercept=4.5)+
  theme_classic() + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.margin=margin(t=-0.2, r=-0.2, b=-0.2, l=-0.2, unit="cm"))
