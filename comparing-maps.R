library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(mgcv)
library(viridis)
library(patchwork)

invlogit <-  function(x) exp(x)/(1+exp(x))

### load data
load("data/Map_comparisons.Rdata")

# Notes: 
# 1) x$Occupanc_1: predicted occupancy probabilities
# 2) x$Panthera_unciaSign2019: MaxEnt habitat suitabilities (cumulative probabilities, 0 - 100)
# 3) x$SLProbabil: expert map
# 4) o2df: outline of study area, for plotting 

# round coordinates a tiny bit for plotting convenience
xdf <- x %>% st_drop_geometry() %>%
  cbind(x %>% st_centroid() %>% st_coordinates() %>% as.data.frame() %>% rename(x = X, y = Y)) %>%
  mutate(x = round(x, 0), y = round(y, 0))

### fit model to relate MaxEnt habitat suitability output to predicted occupancy from an occ. model
### binomial or beta, gam or glm

m0glm <- gam(Occupanc_1 ~ Panthera_unciaSign2019, data = xdf, family = betar(link="logit"))
m0 <- gam(Occupanc_1 ~ s(Panthera_unciaSign2019,k=6), data = xdf, family = betar(link="logit"))
AIC(m0,m0glm) # choose gam

# model checking
gam.check(m0)
plot(m0)
summary(m0)
cor(xdf$Occupanc_1, predict(m0, type="response"))

# get predictions
m0pred <- predict(m0, type = "response", newdata = data.frame(Panthera_unciaSign2019 = seq(min(xdf$Panthera_unciaSign2019, na.rm = TRUE),max(xdf$Panthera_unciaSign2019, na.rm = TRUE),length.out=1000)), se.fit = TRUE)
m0pred_link <- predict(m0, type = "link", newdata = data.frame(Panthera_unciaSign2019 = seq(min(xdf$Panthera_unciaSign2019, na.rm = TRUE),max(xdf$Panthera_unciaSign2019, na.rm = TRUE),length.out=1000)), se.fit = TRUE)
m0df <- data.frame(Panthera_unciaSign2019 = seq(min(xdf$Panthera_unciaSign2019, na.rm = TRUE),max(xdf$Panthera_unciaSign2019, na.rm = TRUE),length.out=1000), 
                   Occupanc_1 = m0pred$fit, lcl = invlogit(m0pred_link$fit-1.96*m0pred_link$se.fit),
                   ucl = invlogit(m0pred_link$fit+1.96*m0pred_link$se.fit))

# add into existing data
xdf <- xdf %>% mutate(Panthera_unciaSign2019 = replace_na(Panthera_unciaSign2019, 0.001)) %>%
  mutate(MaxEntOcc = as.numeric(predict(m0, newdata = data.frame(Panthera_unciaSign2019 = xdf$Panthera_unciaSign2019), type = "response"))) %>% 
  mutate(q_maxent = cut_number(Panthera_unciaSign2019, n = 100, labels = FALSE),
                      q_occ = cut_number(Occupanc_1, n = 100, labels = FALSE))

# prepare for plotting
xt1 <- rbind(xdf %>% pivot_longer(cols = c("Occupanc_1")) %>% dplyr::select(x, y, name, value) %>% mutate(mapname = "Occupancy (Raw)"),
             xdf %>% pivot_longer(cols = c("Panthera_unciaSign2019")) %>% dplyr::select(x, y, name, value) %>% mutate(value = value/100, mapname = "MaxEnt (Raw)"),
             xdf %>% pivot_longer(cols = c("SLProbabil_B")) %>% dplyr::select(x, y, name, value) %>% mutate(value = 0.6 * value, mapname = "Expert (Raw)"))

xt2 <- xdf %>% pivot_longer(cols = c("q_occ", "q_maxent")) %>% dplyr::select(x, y, name, value) %>% mutate(value = value/100, mapname = rep(c("Occupancy (Percentile)", "MaxEnt (Percentile)"), nrow(xdf)))

xt3 <- xdf %>% pivot_longer(cols = c("MaxEntOcc")) %>% dplyr::select(x, y, name, value) %>% mutate(mapname = "MaxEnt (GAM)")

xt <- rbind(xt1, xt2, xt3) %>% mutate(mapname = factor(mapname, levels = c("Occupancy (Raw)", "MaxEnt (Raw)", "Expert (Raw)",
                                                                           "Occupancy (Percentile)", "MaxEnt (Percentile)", "MaxEnt (GAM)")))

txt_df <- data.frame(x = min(xt$x)+40000, y = max(xt$y)-30000, 
                     mapname = c("Occupancy (Raw)", "MaxEnt (Raw)", "Expert (Raw)", "Occupancy (Percentile)", "MaxEnt (Percentile)", "MaxEnt (GAM)"),
                     txt = c("(a)","(b)","(c)","(d)","(e)","(f)"),
                     value = 1)

# plots
p5 <- xt %>% 
  ggplot(aes(x = x, y = y, fill = value)) + 
  facet_wrap(~mapname) + 
  geom_tile() +
  geom_path(data = o2df, inherit.aes = FALSE, aes(x = x, y = y), colour = "grey80") +
  geom_text(data = txt_df, aes(label = txt), size = 4) +
  scale_fill_viridis_c(option = "B", direction = -1, breaks = c(0,0.25,0.5,0.75,1)) + 
  coord_equal(xlim = c(min(xt$x), max(xt$x)), ylim = c(min(xt$y), max(xt$y))) + 
  theme_bw(base_size = 14) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y=element_blank(),legend.position="bottom", legend.key.width = unit(2, "cm"), 
        legend.title = element_blank(),
        panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
p5

gamplot <- ggplot(m0df, aes(x = Panthera_unciaSign2019/100, y = Occupanc_1)) + 
  geom_line() +
  geom_ribbon(aes(x = Panthera_unciaSign2019/100, ymin = lcl, ymax = ucl), colour = NA, fill = "red", alpha = 0.3) +
  coord_equal(xlim = c(0,0.82), ylim=c(0,1)) +
  theme_bw(base_size = 14) +
  scale_x_continuous(name = "MAXENT", limits = c(0,0.82), breaks = c(0,0.4,0.8)) +
  scale_y_continuous(name = expression(widehat(Occ)), limits = c(0,1), breaks = c(0,0.5,1)) +
  theme(legend.position="none", legend.title = element_blank(),
        panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
gamplot

#ggsave("output/maps_occ_max_exp.png", p5, width=7, height=5.5, dpi = 300)  
#ggsave("output/gamplot.png", gamplot, width=3, height=2, dpi = 300)  
