################################################################################
################################## Figure S2 ###################################
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
figure_S2_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_S2_data$Origin <- ifelse(figure_S2_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_S2_data$Origin <- factor(figure_S2_data$Origin, levels = c("Native", "Invasive"))
figure_S2_data$Species <- as.factor(figure_S2_data$Species)

# plant cover
# raw data
mod <- lm(Rel_cover*100 ~ Latitude*Species, data = figure_S2_data)
shapiro.test(residuals(mod)) # W = 0.89844, p-value = 9.275e-08
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

mod <- lm(log10(Rel_cover*100) ~ Latitude*Species, data = figure_S2_data)
shapiro.test(residuals(mod)) # W = 0.97768, p-value = 0.03514
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

ggplot(data = figure_S2_data, aes(x = Latitude, y = log10(Rel_cover*100))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  #scale_y_continuous(breaks = seq(1.0, 1.8, by = 0.1), limits = c(1.0, 1.8), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  theme_classic() +
  scale_y_continuous( expand = c(0, 0.3)) +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.12, 0.92),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = "Focal species' relative abundance\n(%, Log10-transformed)")
