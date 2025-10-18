################################################################################
################################### Figure 2 ###################################
################################################################################

# loading package
library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)
library(effectsize)
library(emmeans)
library(ggpmisc)

# loading field survey dataset
figure_2_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_2_data$Origin <- ifelse(figure_2_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_2_data$Origin <- factor(figure_2_data$Origin, levels = c("Native", "Invasive"))
figure_2_data$Species <- as.factor(figure_2_data$Species)

# Figure 2A
# Plant species richness (site level)
ALLplSR_data <- unique(figure_2_data[,c("Site", "ALLplSR", "Latitude")])
mod <- lm(ALLplSR ~ Latitude, data = ALLplSR_data)
shapiro.test(residuals(mod)) # W = 0.98045, p-value = 0.2993
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = ALLplSR_data, aes(x = Latitude, y = ALLplSR)) + 
  geom_point(size = 3, pch = 21, color = "black", stroke = 0.7, fill = alpha("black", 0.3)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.85,0.25),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, y = "Plant species richness", tag = "A") -> Figure_2A; Figure_2A


# Figure 2B
# Insect herbivore family richness
HerbFR_data <- unique(figure_2_data[,c("Site", "HerbFR", "Latitude")])
mod <- lm(HerbFR ~ Latitude, data = HerbFR_data)
shapiro.test(residuals(mod)) # W = 0.96702, p-value = 0.05865
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = HerbFR_data, aes(x = Latitude, y = HerbFR)) + 
  geom_point(size = 3, pch = 21, color = "black", stroke = 0.7, fill = alpha("black", 0.3)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(0, 16, by = 4), limits = c(0, 16), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.85,0.25),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, y = "Insect herbivore family richness", tag = "B") -> Figure_2B; Figure_2B

# Figure 2C
# Insect herbivore abundance
herbAB_data <- unique(figure_2_data[,c("Site", "HerbAB", "Latitude")])

# raw data
mod <- lm(HerbAB ~ Latitude, data = herbAB_data)
shapiro.test(residuals(mod)) # W = 0.89801, p-value = 2.873e-05
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# sqrt-root translation
mod <- lm(sqrt(HerbAB) ~ Latitude, data = herbAB_data) 
shapiro.test(residuals(mod)) # W = 0.98325, p-value = 0.4635
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = herbAB_data, aes(x = Latitude, y = sqrt(HerbAB))) + 
  geom_point(size = 3, pch = 21, color = "black", stroke = 0.7, fill = alpha("black", 0.3)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  #scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.85,0.25),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, y = "Insect herbivore abundance (sqrt)", tag = "C") -> Figure_2C; Figure_2C

# Figure 2D
# Foliar defoliation 
# raw data
mod <- lm(Defol ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.80617, p-value = 1.321e-11

# log10 transformed was best, similar to your analysis
mod <- lm(log10(Defol) ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.9827, p-value = 0.108
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_2_data, aes(x = Latitude, y = log10(Defol))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(-1.5, 2.5, by = 1), limits = c(-1.5, 2.5), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.85,0.15),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, 
       y = expression("Foliar defoliation (%," ~ log[10] ~ ")"), 
       tag = "D") -> Figure_2D; Figure_2D


# Figure 2E
# Foliar pathogen infection
# raw data
mod <- lm(Disease ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.98345, p-value = 0.1275

# sqrt-root translation
mod <- lm(sqrt(Disease) ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.98443, p-value = 0.1583
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_2_data, aes(x = Latitude, y = sqrt(Disease))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = NULL, y = "Foliar pathogen infection (%, sqrt)", tag = "E") -> Figure_2E; Figure_2E


# Figure 2F
# Soil entire fungal richness
mod <- lm(FUNGSR ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.99085, p-value = 0.5769
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# Figure 3F
ggplot(data = figure_2_data, aes(x = Latitude, y = (FUNGSR))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  #scale_y_continuous(breaks = seq(2.6, 3.1, by = 0.1), limits = c(2.6, 3.1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(400, 1200, by = 200), limits = c(400, 1200), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
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
       #y = expression("Soil entire fungal richness ("~log[10]~")"), 
       y = expression("Soil entire fungal richness"), 
       tag = "F") -> Figure_2F; Figure_2F


# Figure 2G
# Soil pathogenic fungi richness
# raw data
mod <- lm(PATHSR ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.97944, p-value = 0.05199

# log10 translation
mod <- lm(log10(PATHSR) ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.98661, p-value = 0.2537
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_2_data, aes(x = Latitude, y = log10(PATHSR))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(1.0, 1.8, by = 0.2), limits = c(1.0, 1.8), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
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
       y = expression("Soil pathogenic fungi richness ("~log[10]~")"), 
       tag = "G") -> Figure_2G; Figure_2G


# Figure 2H
# Soil AMF richness
# raw data
mod <- lm(AMFSR ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.86381, p-value = 2.146e-09

mod <- lm(sqrt(AMFSR) ~ Latitude*Species, data = figure_2_data)
shapiro.test(residuals(mod)) # W = 0.97464, p-value = 0.01799
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)
mod_emtrends <- emmeans::emtrends(mod, pairwise ~ Species, var = "Latitude")
test(mod_emtrends, adjust = "none")


mod <- lm(sqrt(AMFSR) ~ Latitude, data = subset(figure_2_data, Species == "Alternanthera_sessilis"))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

mod <- lm(sqrt(AMFSR) ~ Latitude, data = subset(figure_2_data, Species == "Alternanthera_philoxeroides"))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# Figure 3H
ggplot(data = figure_2_data, aes(x = Latitude, y = sqrt(AMFSR))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, aes(color = Origin, linetype = Origin)) +
  #ggpmisc::stat_poly_eq(aes(color = Origin, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(2, 1)) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color = "black", size = 11),
        legend.text= element_text(size = 11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", y = "Soil AMF richness (sqrt)", tag = "H") -> Figure_2H; Figure_2H


(Figure_2A/Figure_2C/Figure_2E/Figure_2G)|(Figure_2B/Figure_2D/Figure_2F/Figure_2H) -> Figure_2

