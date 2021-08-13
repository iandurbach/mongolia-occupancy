library(dplyr)
library(ggplot2)
library(ggpubr)
library(RPresence)

### load all input data

load("data/Mongolia_occupancy_inputs.Rdata")

# Notes: 
# 1) Pres: presence/absence occupancy survey results, used for model fitting
# 2) Site_Cov: unit-specific covariates, used for model fitting
# 3) SurvCov: survey-specific covariates, used for model fitting
# 4) Mongolia_studyarea: covariates for whole survey area, used for prediction
# 5) Mongolia_fullrange: covariates across whole expected snow leopard range, used for prediction

### Fitting occupancy models

# Create PAO file for occupancy analysis
Mongol_Pres <- createPao(data = Pres[,c(2:41)], unitcov = Site_Cov, survcov = SurvCov) 

# Run occupancy models
Mongol.1 <- occMod(model = list(psi~1, p~1), 
                   data = Mongol_Pres, type = "so")
Mongol.2 <- occMod(model = list(psi~1, p~ Walk*Ruggedness_z), 
                   data = Mongol_Pres, type = "so")
Mongol.3 <- occMod(model = list(psi~Rgd_Availz+NDVI_z+Alt_z+I(Alt_z^2)+Forest_z, p~ Walk*Ruggedness_z), 
                 data = Mongol_Pres, type = "so")
Mongol.4 <- occMod(model = list(psi~1, p~ Ruggedness_z), 
                   data = Mongol_Pres, type = "so")
Mongol.5 <- occMod(model = list(psi~Rgd_Availz+Alt_z+I(Alt_z^2)+Forest_z*NDVI_z, p~ Walk*Ruggedness_z), 
                 data = Mongol_Pres, type = "so")
Mongol.6 <- occMod(model = list(psi~Rgd_Availz+NDVI_z+Alt_z+I(Alt_z^2), p~ Walk*Ruggedness_z), 
                 data = Mongol_Pres, type = "so")
Mongol.7 <- occMod(model = list(psi~Rgd_Availz+NDVI_z+Alt_z, p~ Walk*Ruggedness_z), 
                 data = Mongol_Pres, type = "so")
Mongol.8 <- occMod(model = list(psi~Rgd_Availz+NDVI_z, p~ Walk*Ruggedness_z), 
                 data = Mongol_Pres, type = "so")

# uncomment to save models
#save(Mongol.1, Mongol.2, Mongol.3, Mongol.4, Mongol.5, Mongol.6, Mongol.7, Mongol.8, file = "output/fitted_occ_models.Rdata")

# Compile and compare all models run so far
Mongol.models<-list(Mongol.1, Mongol.2, Mongol.3, Mongol.4, Mongol.5, Mongol.6, Mongol.7, Mongol.8) # Bring together all models run so far
Mongol.results<-createAicTable(Mongol.models) # Put all models and their AIC values together
summary(Mongol.results) # Create comparable AIC table from the above list

Top.Mongol <- Mongol.results$models[[1]] # Save the top model based on minimum AIC

coef(Top.Mongol,"psi") #regression coefficients for occupancy
coef(Top.Mongol,"p") #regression coefficients for occupancy

### Predictions from best fitted model

# Predict to estimate values at unsampled locations
preds_Mongolia_studyarea <- predict(Top.Mongol, newdata = Mongolia_studyarea, param="psi") #Predict occupancy to the entire study area
Mongolia_studyarea <- cbind(Mongolia_studyarea, preds_Mongolia_studyarea) # Join predicted file with covariate file

# Predict to the extent of entire West Mongolia that is considered to be potential snow leopard range
preds_Mongolia_fullrange <- predict(Top.Mongol, newdata = Mongolia_fullrange, param="psi") #Predict occupancy to the entire study area
Mongolia_fullrange <- cbind(Mongolia_fullrange, preds_Mongolia_fullrange) # Join predicted file with covariate file

### Covariate plots

chosen.Model <- Top.Mongol

# Create new data for plots
x_Rgd_Availz <- seq(from = min(Mongol_Pres$unitcov$Rgd_Availz), to = max(Mongol_Pres$unitcov$Rgd_Availz), length.out = 500)
x_Alt_z <- seq(from = min(Mongol_Pres$unitcov$Alt_z), to = max(Mongol_Pres$unitcov$Alt_z), length.out = 500)
x_NDVI_z <- seq(from = min(Mongol_Pres$unitcov$NDVI_z), to = max(Mongol_Pres$unitcov$NDVI_z), length.out = 500)
x_Forest_z <- seq(from = min(Mongol_Pres$unitcov$Forest_z, na.rm=T), to = max(Mongol_Pres$unitcov$Forest_z, na.rm=T), length.out = 500)
x_Ruggedness_z <- seq(from = min(Mongol_Pres$survcov$Ruggedness_z, na.rm=T), to = max(Mongol_Pres$survcov$Ruggedness_z, na.rm=T), length.out = 500)

# Create data frame with zero values for all other covariates
newd_Rgd <- data.frame(Rgd_Availz = x_Rgd_Availz, Alt_z = 0, NDVI_z = 0, Forest_z=0, Walk = 0, Ruggedness_z = 0)
newd_Alt <- data.frame(Rgd_Availz = 0, Alt_z = x_Alt_z, NDVI_z = 0, Forest_z=0, Walk = 0, Ruggedness_z = 0)
newd_NDVI <- data.frame(Rgd_Availz = 0, Alt_z = 0, NDVI_z = x_NDVI_z, Forest_z=0, Walk = 0, Ruggedness_z = 0)
newd_Forest <- data.frame(Rgd_Availz = 0, Alt_z = 0, NDVI_z = x_NDVI_z, Forest_z=x_Forest_z, Walk = 0, Ruggedness_z = 0)

newd_Walk0_Ruggedness <- data.frame(Rgd_Availz = 0, Alt_z = 0, NDVI_z = x_NDVI_z,  Walk = 0, Ruggedness_z = x_Ruggedness_z)
newd_Walk1_Ruggedness <- data.frame(Rgd_Availz = 0, Alt_z = 0, NDVI_z = x_NDVI_z,  Walk = 1, Ruggedness_z = x_Ruggedness_z)

# predictions from occupancy part 
preds_Rgd <- predict(chosen.Model, newdata = newd_Rgd, param = "psi")
preds_Alt <- predict(chosen.Model, newdata = newd_Alt, param = "psi")
preds_NDVI <- predict(chosen.Model, newdata = newd_NDVI, param = "psi")
preds_Forest <- predict(chosen.Model, newdata = newd_Forest, param = "psi")

# predictions from detection part
preds_Walk0_Ruggedness <- predict(chosen.Model, newdata = newd_Walk0_Ruggedness, param = "p")
preds_Walk1_Ruggedness <- predict(chosen.Model, newdata = newd_Walk1_Ruggedness, param = "p")

# add in the unstandardized covariate values, since these are what we want to plot
# backtransformations use the values in Table 2
preds_Rgd <- preds_Rgd %>% 
  mutate(covariate = "Ruggedness", Rgd_Availz = newd_Rgd$Rgd_Availz) %>%
  mutate(Rgd_Avail = (Rgd_Availz * 12.63) + 50.58)

preds_Alt <- preds_Alt %>% 
  mutate(covariate = "Ruggedness", Alt_z = newd_Alt$Alt_z) %>%
  mutate(Alt = (Alt_z * 478) + 2056)

preds_NDVI <- preds_NDVI %>% 
  mutate(covariate = "Ruggedness", NDVI_z = newd_NDVI$NDVI_z) %>%
  mutate(NDVI = (NDVI_z * 0.06) + 0.13)

preds_Forest <- preds_Forest %>% 
  mutate(covariate = "Forest", Forest_z = newd_Forest$Forest_z) %>%
  mutate(Forest = (Forest_z * 11809) + 5417)

preds_Walk0_Ruggedness <- preds_Walk0_Ruggedness %>% 
  mutate(covariate = "Ruggedness (Walk = 0)", Ruggedness_z = newd_Walk0_Ruggedness$Ruggedness_z) %>%
  mutate(Ruggedness = (Ruggedness_z * 19.885) + 22.315)

preds_Walk1_Ruggedness <- preds_Walk1_Ruggedness %>% 
  mutate(covariate = "Ruggedness (Walk = 1)", Ruggedness_z = newd_Walk0_Ruggedness$Ruggedness_z) %>%
  mutate(Ruggedness = (Ruggedness_z * 19.885) + 22.315)

# Create plots for covariates on occupancy

p1 <- ggplot(preds_Rgd, aes(x = Rgd_Avail, y = est)) + geom_line() +
  geom_ribbon(aes(ymin = lower_0.95, ymax = upper_0.95), fill = "gray80", alpha = 0.5) + 
  #geom_rug(data = Mongolia1, aes(x = Rgd_Avail)) # try this later
  theme_bw(base_size = 14) + labs(x = "Ruggedness", y = "Occupancy") + ylim(c(0,1)) +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.title.y = element_blank()) 

p1 <- p1 + geom_segment(data = Site_Cov, aes(x = Rgd_Avail, y = 0, xend = Rgd_Avail, yend = .05))

p1

p2 <- ggplot(preds_Alt, aes(x = Alt, y = est)) + geom_line() +
  geom_ribbon(aes(ymin = lower_0.95, ymax = upper_0.95), fill = "gray80", alpha = 0.5) + 
  theme_bw(base_size = 14) + labs(x = "Altitude") + ylim(c(0,1)) +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.text.y=element_blank(), axis.title.y = element_blank()) 

p2 <- p2 + geom_segment(data = Site_Cov, aes(x = Alt, y = 0, xend = Alt, yend = .05))

p2


p3 <- ggplot(preds_NDVI, aes(x = NDVI, y = est)) + geom_line() +
  geom_ribbon(aes(ymin = lower_0.95, ymax = upper_0.95), fill = "gray80", alpha = 0.5) + 
  theme_bw(base_size = 14) + labs(x = "NDVI", y = "Occupancy") + ylim(c(0,1)) +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.title.y = element_blank()) 

p3 <- p3 + geom_segment(data=Site_Cov, aes (x= NDVI, y = 0, xend = NDVI, yend = 0.05))

p3

p4 <- ggplot(preds_Forest, aes(x = Forest, y = est)) + geom_line() +
  geom_ribbon(aes(ymin = lower_0.95, ymax = upper_0.95), fill = "gray80", alpha = 0.5) + 
  theme_bw(base_size = 14) + labs(x = "Forest") + ylim(c(0,1)) +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.text.y=element_blank(), axis.title.y = element_blank()) 

p4 <- p4 + geom_segment(data=Site_Cov, aes (x= Forest, y = 0, xend = Forest, yend = 0.05))

p4

# Create plots for covariates on detection probability

p5 <- ggplot(preds_Walk0_Ruggedness, aes(x = Ruggedness, y = est)) + geom_line() +
  geom_ribbon(aes(ymin = lower_0.95, ymax = upper_0.95), fill = "gray80", alpha = 0.5) + 
  theme_bw(base_size = 14) + labs(x = "Ruggedness (Vehicle)") + ylim(c(0,1)) +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 

p6 <- ggplot(preds_Walk1_Ruggedness, aes(x = Ruggedness, y = est)) + geom_line() +
  geom_ribbon(aes(ymin = lower_0.95, ymax = upper_0.95), fill = "gray80", alpha = 0.5) + 
  theme_bw(base_size = 14) + labs(x = "Ruggedness (Walk)") + ylim(c(0,1)) +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.text.y=element_blank(), axis.title.y = element_blank())

# Combine plots

p_occ <- ggarrange(p1,p2,p3,p4, ncol=2, nrow=2, widths = c(5.7,5), common.legend = TRUE, legend="bottom")
p_occ <- annotate_figure(p_occ, left = text_grob("Occupancy", rot = 90, size = 15))
p_occ

p_det <- ggarrange(p5,p6, ncol=2, nrow=1, widths = c(5.7,5), common.legend = TRUE, legend="bottom")
p_det<-annotate_figure(p_det, left = text_grob("Detection", rot = 90, size = 15))
p_det

p_both <- ggarrange(p_occ,p_det, ncol=1, heights = c(2,1))
p_both

# uncomment to save
#ggsave("output/responsecurves.png", p_both, width=6, height=6, dpi = 300)  




