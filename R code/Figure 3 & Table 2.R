################################################################################
################################### Figure 3 ###################################
################################################################################

# loading package
library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)
library(effectsize)
library(emmeans)
library(ggpmisc)

# loading greenhouse experiment dataset
figure_3_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_3_data$Origin <- ifelse(figure_3_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_3_data$Origin <- factor(figure_3_data$Origin, levels = c("Native", "Invasive"))
figure_3_data$Species <- as.factor(figure_3_data$Species)

# Figure 3A
# Aboveground mass
mod <- lm(Con_mass ~ Latitude*Species, data = figure_3_data)
shapiro.test(residuals(mod))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# native dataset
mod <- lm(Con_mass ~ Latitude, data = subset(figure_3_data, Species == "Alternanthera_sessilis"))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# invasive dataset
mod <- lm(Con_mass ~ Latitude, data = subset(figure_3_data, Species == "Alternanthera_philoxeroides"))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = 0, y = 0.08 * diff(range(figure_3_data$Con_mass, na.rm = TRUE)))

ggplot(data = figure_3_data, aes(x = Latitude, y = Con_mass)) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, aes(color = Origin, linetype = Origin)) +
  #ggpmisc::stat_poly_eq(aes(color = Origin, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(0, 16, by = 4), limits = c(0, 16), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(1, 2)) + 
  geom_segment(data = arrow_df, aes(x = x, xend = xend, y = y, yend = yend), colour = "black", 
               arrow = arrow(length = unit(0.25, "cm"), type = "open", angle = 30), size = 0.5) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color = "black", size = 11),
        legend.text= element_text(size = 11),
        legend.position = c(0.85,0.85),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = expression(frac("            Aboveground mass (g)           ", 
                           "           Increasing growth rate           ")), 
       tag = "A") -> Figure_3A; Figure_3A


# Figure 3B
# Loading leaf beetle survival dataset 
Bsurv_data <- read.xlsx("Greenhouse_exp_dataset.xlsx", sheet = "Greenhouse_exp2", colNames = T)
Bsurv_data$Origin <- ifelse(Bsurv_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
Bsurv_data$Origin <- factor(Bsurv_data$Origin, levels = c("Native", "Invasive"))
Bsurv_data$Species <- factor(Bsurv_data$Species)

# Bsurv
mod = glm(cbind(surv, start - surv) ~ Latitude * Species, family=binomial(link="logit"), data = Bsurv_data)
anova(mod, test = "F")

# partial R2 of Latitude
5.4302/ (5.4302 + 523.95)

# partial R2 of Species 
25.6351/ (25.6351 + 498.31)

# partial R2 of Latitude:Species
18.4971/ (18.4971 + 479.81)

# native dataset
mod = glm(cbind(surv, start - surv) ~ Latitude, family=binomial(link="logit"), data = subset(Bsurv_data, Species == "Alternanthera_sessilis"))
anova(mod, test = "F") 
# partial R²
21.2 / (21.2 + 264.0)

# invasive dataset
mod = glm(cbind(surv, start - surv) ~ Latitude, family=binomial(link="logit"), data = subset(Bsurv_data, Species == "Alternanthera_philoxeroides"))
anova(mod, test = "F") 
# partial R²
3.4704 / (3.4704 + 215.81)

mod = glm(cbind(surv, start - surv) ~ Latitude * Species, family=binomial(link="logit"), data = Bsurv_data)
anova(mod, test = "F")

# plot 
mod = glm(cbind(surv, start - surv) ~ Latitude * Species, family=binomial(link="logit"), data = Bsurv_data)

# create a prediction data frame
newdata <- expand.grid(
  Latitude = seq(min(Bsurv_data$Latitude), max(Bsurv_data$Latitude), length.out = 100),
  Species = levels(Bsurv_data$Species)
)

# predict linear predictors (logit scale) and calculate standard errors
pred <- predict(mod, newdata, type = "link", se.fit = TRUE)

# calculate confidence intervals
newdata$fit <- plogis(pred$fit)  # convert back to probabilities
newdata$lower <- plogis(pred$fit - 1.96*pred$se.fit)
newdata$upper <- plogis(pred$fit + 1.96*pred$se.fit)
newdata$Origin <- ifelse(newdata$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
newdata$Origin <- factor(newdata$Origin, levels = c("Native", "Invasive"))

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = 0, y = 0.08 * diff(range((Bsurv_data$Percent_surv)/100, na.rm = TRUE)))

ggplot(data = Bsurv_data, aes(x = Latitude, y = Percent_surv/100)) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) +
  geom_line(data = newdata, mapping = aes(x = Latitude, y = fit, color = Origin, linetype = Origin), size = 1) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(1, 2)) + 
  geom_segment(data = arrow_df, aes(x = x, xend = xend, y = y, yend = yend), colour = "black", 
               arrow = arrow(length = unit(0.30, "cm"), type = "open", angle = 25), size = 0.5) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color = "black", size = 11),
        legend.text= element_text(size = 11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = expression(frac("             Beetle survival odds              ", 
                           "   Decreasing herbivore resistance   ")), 
       tag = "B") -> Figure_3B; Figure_3B


# Figure 3C
# Leaf fungal pathogen infection
# raw data
mod <- lm(Lesion ~ Latitude*Species, data = figure_3_data)
shapiro.test(residuals(mod)) # W = 0.97767, p-value = 0.09363

# sqrt-root translation
mod <- lm(sqrt(Lesion) ~ Latitude*Species, data = figure_3_data)
shapiro.test(residuals(mod)) # W = 0.98076, p-value = 0.1615
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = 1, y = 1+0.08 * diff(range(sqrt(figure_3_data$Lesion), na.rm = TRUE)))

ggplot(data = figure_3_data, aes(x = Latitude, y = sqrt(Lesion))) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, aes(color = Origin, linetype = Origin)) +
  #ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), color = "black", 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(1, 5, by = 1), limits = c(1, 5), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(2, 2)) + 
  geom_segment(data = arrow_df, aes(x = x, xend = xend, y = y, yend = yend), colour = "black", 
               arrow = arrow(length = unit(0.25, "cm"), type = "open", angle = 30), size = 0.5) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color = "black", size = 11),
        legend.text= element_text(size = 11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = expression(frac("Leaf fungal pathogen\ninfection intensity (sqrt)", 
                           "   Decreasing pathogen resistance   ")), 
       tag = "C") -> Figure_3C; Figure_3C


# Figure 3D
# Root nematode infection
# raw data
mod <- lm(Knots ~ Latitude*Species, data = figure_3_data)
shapiro.test(residuals(mod)) # W = 0.91247, p-value = 5.733e-06

# sqrt-root translation
mod <- lm(sqrt(Knots) ~ Latitude*Species, data = figure_3_data)
shapiro.test(residuals(mod)) # W = 0.98593, p-value = 0.3697
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# native dataset
mod <- lm(sqrt(Knots) ~ Latitude, data = subset(figure_3_data, Species == "Alternanthera_sessilis"))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

# invasive dataset
mod <- lm(sqrt(Knots) ~ Latitude, data = subset(figure_3_data, Species == "Alternanthera_philoxeroides"))
anova(mod)
effectsize::eta_squared(mod, partial = TRUE)

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = 0, y = 0.08 * diff(range(sqrt(figure_3_data$Knots), na.rm = TRUE)))

ggplot(data = figure_3_data, aes(x = Latitude, y = sqrt(Knots))) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, aes(color = Origin, linetype = Origin)) +
  #ggpmisc::stat_poly_eq(aes(color = Origin, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
  #                      formula = y ~ x, parse = TRUE, size = 4, label.y.npc = "top", rr.digits = 3) + 
  scale_y_continuous(breaks = seq(0, 16, by = 4), limits = c(0, 16), expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(20, 39, by = 2), limits = c(20, 39), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(1, 2)) + 
  geom_segment(data = arrow_df, aes(x = x, xend = xend, y = y, yend = yend), colour = "black", 
               arrow = arrow(length = unit(0.25, "cm"), type = "open", angle = 30), size = 0.5) + 
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color = "black", size = 11),
        legend.text= element_text(size = 11),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = expression(frac("Root nematode infection\n(# of root knots, sqrt)", 
                           "   Decreasing nematode resistance   ")), 
       tag = "D") -> Figure_3D; Figure_3D

# save plot
(Figure_3A/Figure_3C)|(Figure_3B/Figure_3D) -> Figure_3
