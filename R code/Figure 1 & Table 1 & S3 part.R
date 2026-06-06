################################################################################
################################### Figure 1 ###################################
################################################################################

# loading package
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(patchwork)
library(effectsize)
library(ggpmisc)
library(scales)
library(nlme)
library(MuMIn)
library(emmeans)
library(AICcmodavg)
library(piecewiseSEM)

# loading field survey dataset
figure_1_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
colnames(figure_1_data)

# check which sites collected only A. philoxeroides (AP), only A. sessilis (AS), or both Species.
site_sum = figure_1_data[,c("Site", "Species", "Latitude", "Longitude")] %>%
  group_by(Site, Latitude, Longitude) %>%
  dplyr::summarise(num = n(), .groups = "drop")
site_sum$group <- ifelse(site_sum$num == 2, "Both", "Only")

figure_1_data_add <- figure_1_data %>% left_join(site_sum)

# Add group
# Both site
Both_site <- unique(subset(figure_1_data_add, num == 2)$Site); length(Both_site)
# AP site
AP_site <- subset(figure_1_data_add, num == 1 & Species == "Alternanthera_philoxeroides")$Site; length(AP_site)
# AS site
AS_site <- subset(figure_1_data_add, num == 1 & Species == "Alternanthera_sessilis")$Site; length(AS_site)

figure_1_data <- figure_1_data %>%
  mutate(Site_group = case_when(
    Site %in% Both_site ~ "Both",
    Site %in% AP_site ~ "Invasive",
    Site %in% AS_site ~ "Native"))

################################################################################
################################## Figure 1A ###################################
################################################################################

# Coordinates of province
site_coordinates <- tribble(
  ~Longitude, ~Latitude, ~Province,
  115.5304, 38.23771, "Hebei",
  118.0207, 36.67020, "Shandong",
  112.7531, 34.0000, "Henan",
  117.2830, 31.8612, "Anhui",
  111.5080, 32.0100, "Hubei",
  116.8925, 28.6767, "Jiangxi",
  111.9830, 28.1160, "Hunan",
  114.2669, 24.0333, "Guangdong",
  109.3275, 23.5155, "Guangxi")

#### load map
china_map <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") 

figure_1_data$Site_group <- factor(figure_1_data$Site_group, levels = c("Native","Invasive","Both"))

library(scales)
ggplot(china_map)+
  geom_sf(data = china_map,fill= "#E9E9E9",size = 1, color = "black") + 
  xlim(108,120) + ylim(20,40) + 
  geom_sf(data = china_map[c(3,12,14,15:20),], size=1, fill="#E9E9E9", color = "black") + 
  geom_point(data = figure_1_data, mapping = aes(x = Longitude, y = Latitude, color = Site_group, fill = Site_group),
             size = 3.5, pch = 21, stroke =0.7) + 
  geom_text(data=site_coordinates,aes(x=Longitude, y=Latitude ,label=Province),
            size=4.2,colour="black") +
  theme_bw() +
  annotate("segment", y = 30.5, yend = 30.5, x = 113, xend = 113.5, colour = "black", 
           arrow = arrow(length = unit(0.2, "cm"), type = "open", angle = 30), size = 0.7) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  annotate("text", x = 109 , y = 34, label = "CHINA",colour="black", size = 5) +  
  annotate("point", x =114, y = 30.5, color = "black", shape = 16 , size = 3.5) +
  annotate("text", x =  111.8 , y =  30.54622, label = "Wuhan",colour="black", size = 4) +
  annotation_scale(location = "br", style = "ticks",line_width = 1.5,pad_y = unit(0.5, "cm"),text_cex = 1) + 
  annotation_north_arrow(location = "tl", which_north = T, 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(text = element_text(size = 11),
        legend.position = c(0.85,0.13),
        panel.grid = element_blank(), 
        axis.text=element_text(color="black", size=11),
        panel.background = element_rect(fill = NA),
        legend.text= element_text(size=11),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold"))+
  labs(x=NULL, y=NULL, tag = "A") -> Figure_1A; Figure_1A


################################################################################
################################# Figure 1B-1F #################################
################################################################################
jitter_max_deg <- 3.2 * 1e-6  # Approximately 0.35 meters

set.seed(123456)

# for the same site dataset
figure_1_data_same <- subset(figure_1_data, figure_1_data$Group == "Both")
figure_1_data_same$lat_jitter <- figure_1_data_same$Latitude + runif(nrow(figure_1_data_same), -jitter_max_deg, jitter_max_deg)
figure_1_data_same$lon_jitter <- figure_1_data_same$Longitude + runif(nrow(figure_1_data_same), -jitter_max_deg, jitter_max_deg)

# for the unique site dataset
figure_1_data_unique <- subset(figure_1_data, figure_1_data$Group != "Both")
figure_1_data_unique$lat_jitter <- figure_1_data_unique$Latitude
figure_1_data_unique$lon_jitter <- figure_1_data_unique$Longitude

figure_1_data_reshape = rbind(figure_1_data_same, figure_1_data_unique)
colnames(figure_1_data_reshape)

figure_1_data_reshape = figure_1_data

# for both
#figure_1_data_reshape = subset(figure_1_data_reshape, Group == "Both")

# Figure 1B
# Soil_wc
Soil_wc_data <- unique(figure_1_data_reshape[,c("Site", "Soil_wc_all", "Latitude", "Longitude", "Group")])
Soil_wc_data <- Soil_wc_data[complete.cases(Soil_wc_data[, "Soil_wc_all"]), ]
# raw data
mod1 <- gls(Soil_wc_all ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = Soil_wc_data, method = "REML", na.action = na.omit)
mod2 <- gls(Soil_wc_all ~ Latitude, correlation = corGaus(form = ~ Latitude + Longitude), data = Soil_wc_data, method = "REML", na.action = na.omit)
mod3 <- gls(Soil_wc_all ~ Latitude, correlation = corLin(form = ~ Latitude + Longitude), data = Soil_wc_data, method = "REML", na.action = na.omit)
mod4 <- gls(Soil_wc_all ~ Latitude, correlation = corRatio(form = ~ Latitude + Longitude), data = Soil_wc_data, method = "REML", na.action = na.omit)
mod5 <- gls(Soil_wc_all ~ Latitude, correlation = corSpher(form = ~ Latitude + Longitude), data = Soil_wc_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# selected model
mod1 <- gls(Soil_wc_all ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = Soil_wc_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod1)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

Soil_wc_data$F0 <- predictSE(mod1, Soil_wc_data, level = 0)$fit
Soil_wc_data$SE <- predictSE(mod1, Soil_wc_data, level = 0)$se.fit

ggplot(data = Soil_wc_data, aes(x = Latitude, y = Soil_wc_all)) + 
  geom_point(size = 2.5, aes(color = Group, fill = Group), pch = 21, stroke = 0.7) + 
  geom_line(aes(y=F0), size=1) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
  scale_shape_manual(values = c(1,2)) + 
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, y = "Soil water content (%)", tag = "B") -> Figure_1B; Figure_1B


# Figure 1C
# Soil_C
Soil_C_data = figure_1_data_reshape[complete.cases(figure_1_data_reshape[, "Soil_C_all"]), ]
# raw data
mod1 <- gls(Soil_C_all ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
mod2 <- gls(Soil_C_all ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
mod3 <- gls(Soil_C_all ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
mod4 <- gls(Soil_C_all ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
mod5 <- gls(Soil_C_all ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(Soil_C_all ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))

# log10 translation
Soil_C_data$LOGSoil_C = log10(Soil_C_data$Soil_C_all)
mod1 <- gls(LOGSoil_C ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(LOGSoil_C ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_C_data, method = "REML")
m_lat <- gls(LOGSoil_C ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_C_data, method = "REML")
m_lat_spp <- gls(LOGSoil_C ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_C_data, method = "REML")
m_full <- gls(LOGSoil_C ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_C_data, method = "REML")

# Calculate the R-squared.
r2_0 <- piecewiseSEM::rsquared(m0)$R.squared
r2_lat <- piecewiseSEM::rsquared(m_lat)$R.squared
r2_lat_spp <- piecewiseSEM::rsquared(m_lat_spp)$R.squared
r2_full <- piecewiseSEM::rsquared(m_full)$R.squared

# Calculate the contribution.
r2_contribution <- c(
  Latitude = r2_lat - r2_0,
  Species = r2_lat_spp - r2_lat,
  Interaction = r2_full - r2_lat_spp
)

# Calculate Cohen's f.
denom <- 1 - r2_full
f_lat <- sqrt((r2_lat - r2_0) / denom)
f_spp <- sqrt((r2_lat_spp - r2_lat) / denom)
f_int <- sqrt((r2_full - r2_lat_spp) / denom)

# print result
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

Soil_C_data$F0 <- predictSE(mod1, Soil_C_data, level = 0)$fit
Soil_C_data$SE <- predictSE(mod1, Soil_C_data, level = 0)$se.fit

# Global model
mod0 <- gls(LOGSoil_C ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
Soil_C_data$F00 <- predictSE(mod0, Soil_C_data, level = 0)$fit
Soil_C_data$SE0 <- predictSE(mod0, Soil_C_data, level = 0)$se.fit

ggplot(data = Soil_C_data, aes(x = Latitude, y = log10(Soil_C_all))) + 
  geom_point(size = 2.5, aes(color = Group, fill = Group), pch = 21, stroke = 0.7) + 
  geom_line(aes(y=F00), size=1, linetype = 2, color = "black") + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(breaks = seq(-0.8, 1.2, by = 0.4), limits = c(-0.8, 1.2), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, 
       y = expression("Soil carbon content (g/kg, "~log[10]~")"), 
       tag = "C") -> Figure_1C; Figure_1C


# Figure 1D
# Soil_N
Soil_N_data = figure_1_data_reshape[complete.cases(figure_1_data_reshape[, "Soil_N_all"]), ]

# raw data
mod1 <- gls(Soil_N_all ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
mod2 <- gls(Soil_N_all ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
mod3 <- gls(Soil_N_all ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
mod4 <- gls(Soil_N_all ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
mod5 <- gls(Soil_N_all ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# selected model
mod1 <- gls(Soil_N_all ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))

# log10 translation
Soil_N_data$LOGSoil_N <- log10(Soil_N_data$Soil_N_all)
mod1 <- gls(LOGSoil_N ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(LOGSoil_N ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_N_data, method = "REML")
m_lat <- gls(LOGSoil_N ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_N_data, method = "REML")
m_lat_spp <- gls(LOGSoil_N ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_N_data, method = "REML")
m_full <- gls(LOGSoil_N ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_N_data, method = "REML")

# Calculate the R-squared.
r2_0 <- piecewiseSEM::rsquared(m0)$R.squared
r2_lat <- piecewiseSEM::rsquared(m_lat)$R.squared
r2_lat_spp <- piecewiseSEM::rsquared(m_lat_spp)$R.squared
r2_full <- piecewiseSEM::rsquared(m_full)$R.squared

# Calculate the contribution.
r2_contribution <- c(
  Latitude = r2_lat - r2_0,
  Species = r2_lat_spp - r2_lat,
  Interaction = r2_full - r2_lat_spp
)

# Calculate Cohen's f.
denom <- 1 - r2_full
f_lat <- sqrt((r2_lat - r2_0) / denom)
f_spp <- sqrt((r2_lat_spp - r2_lat) / denom)
f_int <- sqrt((r2_full - r2_lat_spp) / denom)

# print result
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

Soil_N_data$F0 = predictSE(mod1, Soil_N_data, level = 0)$fit
Soil_N_data$SE <- predictSE(mod1, Soil_N_data, level = 0)$se.fit

# Global model
mod0 <- gls(LOGSoil_N ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_N_data, method = "REML", na.action = na.omit)
car::Anova(mod0, type = "III", test.statistic = "Chisq")
Soil_N_data$F00 = predictSE(mod0, Soil_N_data, level = 0)$fit
Soil_N_data$SE0 <- predictSE(mod0, Soil_N_data, level = 0)$se.fit

ggplot(data = Soil_N_data, aes(x = Latitude, y = LOGSoil_N)) + 
  geom_point(size = 2.5, aes(color = Group, fill = Group), pch = 21, stroke = 0.7) + 
  geom_line(aes(y=F00), size=1, linetype = 1, color = "black") + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(breaks = seq(-1.2, -0.0, by = 0.2), limits = c(-1.2, -0.0), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, 
       y = expression("Soil nitrogen content (g/kg, "~log[10]~")"), 
       tag = "D") -> Figure_1D; Figure_1D


# Figure 1E
# Soil_ph
Soil_ph_data = figure_1_data_reshape[complete.cases(figure_1_data_reshape[, "Soil_ph_all"]), ]

# raw data
mod1 <- gls(Soil_ph_all ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_ph_data, method = "REML", na.action = na.omit)
mod2 <- gls(Soil_ph_all ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Soil_ph_data, method = "REML", na.action = na.omit)
mod3 <- gls(Soil_ph_all ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Soil_ph_data, method = "REML", na.action = na.omit)
mod4 <- gls(Soil_ph_all ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Soil_ph_data, method = "REML", na.action = na.omit)
mod5 <- gls(Soil_ph_all ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Soil_ph_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# selected model
mod1 <- gls(Soil_ph_all ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_ph_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(Soil_ph_all ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_ph_data, method = "REML")
m_lat <- gls(Soil_ph_all ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_ph_data, method = "REML")
m_lat_spp <- gls(Soil_ph_all ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_ph_data, method = "REML")
m_full <- gls(Soil_ph_all ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Soil_ph_data, method = "REML")

# Calculate the R-squared.
r2_0 <- piecewiseSEM::rsquared(m0)$R.squared
r2_lat <- piecewiseSEM::rsquared(m_lat)$R.squared
r2_lat_spp <- piecewiseSEM::rsquared(m_lat_spp)$R.squared
r2_full <- piecewiseSEM::rsquared(m_full)$R.squared

# Calculate the contribution.
r2_contribution <- c(
  Latitude = r2_lat - r2_0,
  Species = r2_lat_spp - r2_lat,
  Interaction = r2_full - r2_lat_spp
)

# Calculate Cohen's f.
denom <- 1 - r2_full
f_lat <- sqrt((r2_lat - r2_0) / denom)
f_spp <- sqrt((r2_lat_spp - r2_lat) / denom)
f_int <- sqrt((r2_full - r2_lat_spp) / denom)

# print result
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

Soil_ph_data$F0 <- predictSE(mod1, Soil_ph_data, level = 0)$fit
Soil_ph_data$SE <- predictSE(mod1, Soil_ph_data, level = 0)$se.fit

# Global model
mod0 <- gls(Soil_ph_all ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Soil_C_data, method = "REML", na.action = na.omit)
Soil_ph_data$F00 = predictSE(mod0, Soil_ph_data, level = 0)$fit
Soil_ph_data$SE0 <- predictSE(mod0, Soil_ph_data, level = 0)$se.fit

ggplot(data = Soil_ph_data, aes(x = Latitude, y = Soil_ph_all)) + 
  geom_point(size = 2.5, aes(color = Group, fill = Group), pch = 21, stroke = 0.7) + 
  geom_line(aes(y=F00), size=1, linetype = 1, color = "black") + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(breaks = seq(4.5, 8.5, by = 1), limits = c(4.5, 8.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, y = "Soil pH", tag = "E") -> Figure_1E; Figure_1E


# Figure 1F
# Bio1
bio1_data <- unique(figure_1_data_reshape[,c("Site", "Bio1", "Latitude", "Longitude", "Group")])

mod1 <- gls(Bio1 ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = bio1_data, method = "REML", na.action = na.omit)
mod2 <- gls(Bio1 ~ Latitude, correlation = corGaus(form = ~ Latitude + Longitude), data = bio1_data, method = "REML", na.action = na.omit)
mod3 <- gls(Bio1 ~ Latitude, correlation = corLin(form = ~ Latitude + Longitude), data = bio1_data, method = "REML", na.action = na.omit)
mod4 <- gls(Bio1 ~ Latitude, correlation = corRatio(form = ~ Latitude + Longitude), data = bio1_data, method = "REML", na.action = na.omit)
mod5 <- gls(Bio1 ~ Latitude, correlation = corSpher(form = ~ Latitude + Longitude), data = bio1_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(Bio1 ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = bio1_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod1)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# predictor 
bio1_data$F0 = predictSE(mod1, bio1_data, level = 0)$fit
bio1_data$SE <- predictSE(mod1, bio1_data, level = 0)$se.fit

ggplot(bio1_data, aes(x=Latitude, y=Bio1, fill = Group)) +
  geom_point(size = 2.5, aes(color = Group, fill = Group), pch = 21, stroke = 0.7) + 
  geom_line(aes(y=F0), size=1) + 
  scale_y_continuous(breaks = seq(10, 26, by = 4), limits = c(10, 26), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.25,0.25),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = expression("Annual mean temperature (°C)"), 
       tag = "F") -> Figure_1F; Figure_1F


# Figure 1G
# Bio15
bio15_data <- unique(figure_1_data_reshape[,c("Site", "Bio15", "Latitude", "Longitude", "Group")])
bio15_data$Latitude2 <- bio15_data$Latitude^2
mod1 <- gls(Bio15 ~ poly(Latitude, 2), correlation = corExp(form = ~ Latitude + Longitude), data = bio15_data, method = "REML", na.action = na.omit)
mod2 <- gls(Bio15 ~ poly(Latitude, 2), correlation = corGaus(form = ~ Latitude + Longitude), data = bio15_data, method = "REML", na.action = na.omit)
mod3 <- gls(Bio15 ~ poly(Latitude, 2), correlation = corLin(form = ~ Latitude + Longitude), data = bio15_data, method = "REML", na.action = na.omit)
mod4 <- gls(Bio15 ~ poly(Latitude, 2), correlation = corRatio(form = ~ Latitude + Longitude), data = bio15_data, method = "REML", na.action = na.omit)
mod5 <- gls(Bio15 ~ poly(Latitude, 2), correlation = corSpher(form = ~ Latitude + Longitude), data = bio15_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

#
mod1 <- gls(Bio15 ~ poly(Latitude, 2), correlation = corExp(form = ~ Latitude + Longitude), data = bio15_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod1)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# predictor 
mod1 <- gls(Bio15 ~ Latitude + Latitude2, correlation = corExp(form = ~ Latitude + Longitude), data = bio15_data, method = "REML", na.action = na.omit)
bio15_data$F0 = predictSE(mod1, bio15_data, level = 0)$fit
bio15_data$SE <- predictSE(mod1, bio15_data, level = 0)$se.fit

ggplot(bio15_data, aes(x=Latitude, y=Bio15)) +
  geom_point(size = 2.5, aes(color = Group, fill = Group), pch = 21, stroke = 0.7) + 
  geom_line(aes(y=F0), size=1) + 
  scale_y_continuous(breaks = seq(40, 140, by = 20), limits = c(40, 140), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = expression("Precipitation seasonality"), 
       tag = "G") -> Figure_1G; Figure_1G


(Figure_1B/Figure_1D/Figure_1F)|(Figure_1C/Figure_1E/Figure_1G) -> Figure_1B_G

#ggsave("Figure_1B_G-0606.pdf", plot = Figure_1B_G, width = 8.5, height = 10.0, units = "in", dpi = 300)
#ggsave("Figure_1A.pdf", plot = Figure_1A, width = 8.9, height = 9.8, units = "in", dpi = 300)


