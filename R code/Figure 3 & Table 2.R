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
library(nlme)
library(scales)
library(car)
library(AICcmodavg)

# loading greenhouse experiment dataset
figure_3_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_3_data$Origin <- ifelse(figure_3_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_3_data$Origin <- factor(figure_3_data$Origin, levels = c("Native", "Invasive"))
figure_3_data$Species <- as.factor(figure_3_data$Species)

#
jitter_max_deg <- 3.2 * 1e-6  # 约0.35米

set.seed(123456)

# for the same site dataset
figure_3_data_same <- subset(figure_3_data, figure_3_data$Group == "Both")
figure_3_data_same$lat_jitter <- figure_3_data_same$Latitude + runif(nrow(figure_3_data_same), -jitter_max_deg, jitter_max_deg)
figure_3_data_same$lon_jitter <- figure_3_data_same$Longitude + runif(nrow(figure_3_data_same), -jitter_max_deg, jitter_max_deg)

# for the unique site dataset
figure_3_data_unique <- subset(figure_3_data, figure_3_data$Group != "Both")
figure_3_data_unique$lat_jitter <- figure_3_data_unique$Latitude
figure_3_data_unique$lon_jitter <- figure_3_data_unique$Longitude

figure_3_data_reshape = rbind(figure_3_data_same, figure_3_data_unique)
colnames(figure_3_data_reshape)

# for both
#figure_3_data_reshape = subset(figure_3_data_reshape, Group == "Both")
#dim(figure_3_data_reshape)

# Figure 3A
# Aboveground mass
Con_mass_data = figure_3_data_reshape[complete.cases(figure_3_data_reshape[, "Con_mass"]), ]

# raw data
mod1 <- gls(Con_mass ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Con_mass_data, method = "REML", na.action = na.omit)
mod2 <- gls(Con_mass ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Con_mass_data, method = "REML", na.action = na.omit)
mod3 <- gls(Con_mass ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Con_mass_data, method = "REML", na.action = na.omit)
mod4 <- gls(Con_mass ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Con_mass_data, method = "REML", na.action = na.omit)
mod5 <- gls(Con_mass ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Con_mass_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod4,mod5)

# raw data
mod1 <- gls(Con_mass ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Con_mass_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(Con_mass ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Con_mass_data, method = "REML")
m_lat <- gls(Con_mass ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Con_mass_data, method = "REML")
m_lat_spp <- gls(Con_mass ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Con_mass_data, method = "REML")
m_full <- gls(Con_mass ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Con_mass_data, method = "REML")

# 计算 R²
r2_0 <- piecewiseSEM::rsquared(m0)$R.squared
r2_lat <- piecewiseSEM::rsquared(m_lat)$R.squared
r2_lat_spp <- piecewiseSEM::rsquared(m_lat_spp)$R.squared
r2_full <- piecewiseSEM::rsquared(m_full)$R.squared

# 计算贡献
r2_contribution <- c(
  Latitude = r2_lat - r2_0,
  Species = r2_lat_spp - r2_lat,
  Interaction = r2_full - r2_lat_spp
)

# 计算 Cohen's f
denom <- 1 - r2_full
f_lat <- sqrt((r2_lat - r2_0) / denom)
f_spp <- sqrt((r2_lat_spp - r2_lat) / denom)
f_int <- sqrt((r2_full - r2_lat_spp) / denom)

# 结果
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

# post-hoc test
# as
mod_as <- gls(Con_mass ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Con_mass_data, Species == "Alternanthera_sessilis"), method = "REML")
car::Anova(mod_as, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_as)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# ap
mod_ap <- gls(Con_mass ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Con_mass_data, Species == "Alternanthera_philoxeroides"), method = "REML")
car::Anova(mod_ap, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_ap)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# model predict
Con_mass_data$F0 <- predictSE(mod1, Con_mass_data, level = 0)$fit
Con_mass_data$SE <- predictSE(mod1, Con_mass_data, level = 0)$se.fit

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = 0, y = 0.08 * diff(range(Con_mass_data$Con_mass, na.rm = TRUE)))

ggplot(data = Con_mass_data, aes(x = Latitude, y = Con_mass)) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F0, color = Origin, linetype = Origin), size = 1) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(1, 2)) + 
  scale_y_continuous(breaks = seq(0, 16, by = 4), limits = c(0, 16), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
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
       y = expression(frac("            Aboveground mass (g)           ", 
                           "           Increasing growth rate           ")), 
       tag = "A") -> Figure_3A; Figure_3A


# Figure 3B
# Loading leaf beetle survival dataset 
Bsurv_data <- figure_3_data_reshape[complete.cases(figure_3_data_reshape[, "Bsurv"]), ]
Bsurv_data$Bsurv_logit <- logit(Bsurv_data$Bsurv/100)

# raw data
mod1 <- gls(Bsurv_logit ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Bsurv_data, method = "REML", na.action = na.omit)
mod2 <- gls(Bsurv_logit ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Bsurv_data, method = "REML", na.action = na.omit)
mod3 <- gls(Bsurv_logit ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Bsurv_data, method = "REML", na.action = na.omit)
mod4 <- gls(Bsurv_logit ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Bsurv_data, method = "REML", na.action = na.omit)
mod5 <- gls(Bsurv_logit ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Bsurv_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(Bsurv_logit ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Bsurv_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(Bsurv_logit ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Bsurv_data, method = "REML")
m_lat <- gls(Bsurv_logit ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Bsurv_data, method = "REML")
m_lat_spp <- gls(Bsurv_logit ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Bsurv_data, method = "REML")
m_full <- gls(Bsurv_logit ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Bsurv_data, method = "REML")

# 计算 R²
r2_0 <- piecewiseSEM::rsquared(m0)$R.squared
r2_lat <- piecewiseSEM::rsquared(m_lat)$R.squared
r2_lat_spp <- piecewiseSEM::rsquared(m_lat_spp)$R.squared
r2_full <- piecewiseSEM::rsquared(m_full)$R.squared

# 计算贡献
r2_contribution <- c(
  Latitude = r2_lat - r2_0,
  Species = r2_lat_spp - r2_lat,
  Interaction = r2_full - r2_lat_spp
)

# 计算 Cohen's f
denom <- 1 - r2_full
f_lat <- sqrt((r2_lat - r2_0) / denom)
f_spp <- sqrt((r2_lat_spp - r2_lat) / denom)
f_int <- sqrt((r2_full - r2_lat_spp) / denom)

# 结果
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

Bsurv_data$F0 <- predictSE(mod1, Bsurv_data, level = 0)$fit
Bsurv_data$SE <- predictSE(mod1, Bsurv_data, level = 0)$se.fit

# post-hoc test
# as
mod_as <- gls(Bsurv_logit ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Bsurv_data, Species == "Alternanthera_sessilis"), method = "REML")
car::Anova(mod_as, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_as)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# ap
mod_ap <- gls(Bsurv_logit ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Bsurv_data, Species == "Alternanthera_philoxeroides"), method = "REML")
car::Anova(mod_ap, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_ap)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))


# Global model
mod0 <- gls(Bsurv_logit ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Bsurv_data, method = "REML", na.action = na.omit)
car::Anova(mod0, type = "III", test.statistic = "Chisq")
Bsurv_data$F00 <- predictSE(mod0, Bsurv_data, level = 0)$fit
Bsurv_data$SE0 <- predictSE(mod0, Bsurv_data, level = 0)$se.fit

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = min(Bsurv_data$Bsurv_logit), y = min(Bsurv_data$Bsurv_logit) + 0.08 * diff(range((Bsurv_data$Bsurv_logit), na.rm = TRUE)))

ggplot(data = Bsurv_data, aes(x = Latitude, y = Bsurv_logit)) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F0, color = Origin, linetype = Origin), size = 1) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(2, 2)) + 
  scale_y_continuous(breaks = seq(-3, 2, by = 1), limits = c(-3, 2), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
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
       y = expression(frac("             Beetle survival (logit)              ", 
                           "   Decreasing herbivore resistance   ")), 
       tag = "B") -> Figure_3B; Figure_3B


# Figure 3C
# Leaf fungal pathogen infection
Lesion_data = figure_3_data_reshape[complete.cases(figure_3_data_reshape[, "Lesion"]), ]

# raw data
mod1 <- gls(Lesion ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
mod2 <- gls(Lesion ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
mod3 <- gls(Lesion ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
mod4 <- gls(Lesion ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
mod5 <- gls(Lesion ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(Lesion ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# sqrt-root tranlation
Lesion_data$SGQTLesion <- sqrt(Lesion_data$Lesion)
mod1 <- gls(SGQTLesion ~ Latitude * Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(SGQTLesion ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Lesion_data, method = "REML")
m_lat <- gls(SGQTLesion ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Lesion_data, method = "REML")
m_lat_spp <- gls(SGQTLesion ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Lesion_data, method = "REML")
m_full <- gls(SGQTLesion ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Lesion_data, method = "REML")

# 计算 R²
r2_0 <- piecewiseSEM::rsquared(m0)$R.squared
r2_lat <- piecewiseSEM::rsquared(m_lat)$R.squared
r2_lat_spp <- piecewiseSEM::rsquared(m_lat_spp)$R.squared
r2_full <- piecewiseSEM::rsquared(m_full)$R.squared

# 计算贡献
r2_contribution <- c(
  Latitude = r2_lat - r2_0,
  Species = r2_lat_spp - r2_lat,
  Interaction = r2_full - r2_lat_spp
)

# 计算 Cohen's f
denom <- 1 - r2_full
f_lat <- sqrt((r2_lat - r2_0) / denom)
f_spp <- sqrt((r2_lat_spp - r2_lat) / denom)
f_int <- sqrt((r2_full - r2_lat_spp) / denom)

# 结果
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

# post-hoc test
# as
mod_as <- gls(SGQTLesion ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Lesion_data, Species == "Alternanthera_sessilis"), method = "REML")
car::Anova(mod_as, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_as)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# ap
mod_ap <- gls(SGQTLesion ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Lesion_data, Species == "Alternanthera_philoxeroides"), method = "REML")
car::Anova(mod_ap, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_ap)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

Lesion_data$F0 <- predictSE(mod1, Lesion_data, level = 0)$fit
Lesion_data$SE <- predictSE(mod1, Lesion_data, level = 0)$se.fit

# Global model
mod0 <- gls(SGQTLesion ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Lesion_data, method = "REML", na.action = na.omit)
Lesion_data$F00 <- predictSE(mod0, Lesion_data, level = 0)$fit
Lesion_data$SE0 <- predictSE(mod0, Lesion_data, level = 0)$se.fit

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = 1, y = 1+0.08 * diff(range(sqrt(figure_3_data$Lesion), na.rm = TRUE)))

ggplot(data = Lesion_data, aes(x = Latitude, y = SGQTLesion)) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F0, color = Origin, linetype = Origin), size = 1) + 
  scale_y_continuous(breaks = seq(1, 5, by = 1), limits = c(1, 5), expand = c(0, 0)) +
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
Knots_data = figure_3_data_reshape[complete.cases(figure_3_data_reshape[, "Knots"]), ]

# raw data
mod1 <- gls(Knots ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
mod2 <- gls(Knots ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
mod3 <- gls(Knots ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
mod4 <- gls(Knots ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
mod5 <- gls(Knots ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(Knots ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# sqrt-root translation
Knots_data$SQRTKnots <- sqrt(Knots_data$Knots)
mod1 <- gls(SQRTKnots ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(SQRTKnots ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Knots_data, method = "REML")
m_lat <- gls(SQRTKnots ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Knots_data, method = "REML")
m_lat_spp <- gls(SQRTKnots ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Knots_data, method = "REML")
m_full <- gls(SQRTKnots ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Knots_data, method = "REML")

# 计算 R²
r2_0 <- piecewiseSEM::rsquared(m0)$R.squared
r2_lat <- piecewiseSEM::rsquared(m_lat)$R.squared
r2_lat_spp <- piecewiseSEM::rsquared(m_lat_spp)$R.squared
r2_full <- piecewiseSEM::rsquared(m_full)$R.squared

# 计算贡献
r2_contribution <- c(
  Latitude = r2_lat - r2_0,
  Species = r2_lat_spp - r2_lat,
  Interaction = r2_full - r2_lat_spp
)

# 计算 Cohen's f
denom <- 1 - r2_full
f_lat <- sqrt((r2_lat - r2_0) / denom)
f_spp <- sqrt((r2_lat_spp - r2_lat) / denom)
f_int <- sqrt((r2_full - r2_lat_spp) / denom)

# 结果
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

Knots_data$F0 <- predictSE(mod1, Knots_data, level = 0)$fit
Knots_data$SE <- predictSE(mod1, Knots_data, level = 0)$se.fit

# post-hoc test
# as
mod_as <- gls(SQRTKnots ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Knots_data, Species == "Alternanthera_sessilis"), method = "REML")
car::Anova(mod_as, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_as)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# ap
mod_ap <- gls(SQRTKnots ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Knots_data, Species == "Alternanthera_philoxeroides"), method = "REML")
car::Anova(mod_ap, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_ap)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))


# Global model
mod0 <- gls(SQRTKnots ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Knots_data, method = "REML", na.action = na.omit)
car::Anova(mod0, type = "III", test.statistic = "Chisq")
Knots_data$F00 <- predictSE(mod0, Knots_data, level = 0)$fit
Knots_data$SE0 <- predictSE(mod0, Knots_data, level = 0)$se.fit

arrow_df <- data.frame(x = 30.5, xend = 30.5, yend = 0, y = 0.08 * diff(range(sqrt(Knots_data$Knots), na.rm = TRUE)))

ggplot(data = Knots_data, aes(x = Latitude, y = SQRTKnots)) + 
  geom_point(size = 2.5, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F0, color = Origin, linetype = Origin), size = 1) + 
  scale_y_continuous(breaks = seq(0, 16, by = 4), limits = c(0, 16), expand = c(0, 0)) +
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

# ggsave("Figure 3-0415.pdf", plot = Figure_3, width = 10.35, height = 7.7, units = "in", dpi = 300)


