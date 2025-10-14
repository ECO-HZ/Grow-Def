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

# loading field survey dataset
figure_1_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
colnames(figure_1_data)

# check which sites collected only A. philoxeroides (AP), only A. sessilis (AS), or both Species.
site_sum = figure_1_data[,c("Site", "Species", "Latitude", "Longitude")] %>%
  group_by(Site, Latitude, Longitude) %>%
  summarise(num = n(), .groups = "drop")
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

# Figure 1B
# Soil_wc
mod <- lm(Soil_wc ~ Latitude*Species, data = figure_1_data)
shapiro.test(residuals(mod))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_1_data, aes(x = Latitude, y = Soil_wc)) + 
  geom_point(size = 2.5, aes(color = Site_group, fill = Site_group), pch = 21, stroke = 0.7) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
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
# raw data
mod <- lm(Soil_C ~ Latitude*Species, data = figure_1_data)
shapiro.test(residuals(mod)) # W = 0.75433, p-value = 1.22e-11
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# log10 translation
mod <- lm(log10(Soil_C) ~ Latitude*Species, data = figure_1_data)
shapiro.test(residuals(mod)) # W = 0.96646, p-value = 0.01193
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_1_data, aes(x = Latitude, y = log10(Soil_C))) + 
  geom_point(size = 2.5, aes(color = Site_group, fill = Site_group), pch = 21, stroke = 0.7) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black", linetype = 2) +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  #scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-0.8, 1.2, by = 0.4), limits = c(-0.8, 1.2), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
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
# raw data
mod <- lm(Soil_N ~ Latitude*Species, data = figure_1_data)
shapiro.test(residuals(mod)) # W = 0.93001, p-value = 4.982e-05
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# log10 translation
mod <- lm(log10(Soil_N) ~ Latitude*Species, data = figure_1_data)
shapiro.test(residuals(mod)) # W = 0.99013, p-value = 0.6752
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_1_data, aes(x = Latitude, y = log10(Soil_N))) + 
  geom_point(size = 2.5, aes(color = Site_group, fill = Site_group), pch = 21, stroke = 0.7) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black", linetype = 1) +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  #scale_y_continuous(breaks = seq(0, 0.6, by = 0.1), limits = c(0, 0.6), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-1.2, -0.2, by = 0.2), limits = c(-1.2, -0.2), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
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
mod <- lm(Soil_ph ~ Latitude*Species, data = figure_1_data)
shapiro.test(residuals(mod)) 
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_1_data, aes(x = Latitude, y = Soil_ph)) + 
  geom_point(size = 2.5, aes(color = Site_group, fill = Site_group), pch = 21, stroke = 0.7) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black", linetype = 1) +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(breaks = seq(4.5, 8.5, by = 0.5), limits = c(4.5, 8.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
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
bio1_data <- unique(figure_1_data[,c("Site", "Bio1", "Latitude")])
mod <- lm(Bio1 ~ Latitude, data = bio1_data)
shapiro.test(residuals(mod)) # W = 0.9831, p-value = 0.4079
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_1_data, aes(x = Latitude, y = Bio1)) + 
  geom_point(size = 2.5, aes(color = Site_group, fill = Site_group), pch = 21, stroke = 0.7) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black", linetype = 1) +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(breaks = seq(10, 26, by = 2), limits = c(10, 26), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.25,0.25),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", y = "Annual mean temperature", tag = "F") -> Figure_1F; Figure_1F


# Figure 1G
# Bio15
bio15_data <- unique(figure_1_data[,c("Site", "Bio15", "Latitude")])
mod <- lm(Bio15 ~ poly(Latitude, 2), data = bio15_data)
shapiro.test(residuals(mod)) # W = 0.95023, p-value = 0.004769 
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_1_data, aes(x = Latitude, y = Bio15)) + 
  geom_point(size = 2.5, aes(color = Site_group, fill = Site_group), pch = 21, stroke = 0.7) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, color = "black", linetype = 1) +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ poly(x, 2), parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5), "Both" = alpha("#424768", 0.3))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225", "Both" = "#424768")) + 
  scale_y_continuous(breaks = seq(40, 140, by = 20), limits = c(40, 140), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", y = "Precipitation seasonality", tag = "G") -> Figure_1G; Figure_1G


(Figure_1B/Figure_1D/Figure_1F)|(Figure_1C/Figure_1E/Figure_1G) -> Figure_1_right #; Figure_1_right

# 9.09 x 8.11
# ggsave("Figure_1_left.pdf", plot = Figure_1A, width = 9.09, height = 8.11, units = "in", dpi = 300)
# ggsave("Figure_1_right_30s.pdf", plot = Figure_1_right, width = 7.4, height = 8.5, units = "in", dpi = 300)
