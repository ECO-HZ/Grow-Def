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
library(scales)
library(nlme)
library(MuMIn)
library(car)
library(AICcmodavg)

# loading field survey dataset
figure_2_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_2_data$Origin <- ifelse(figure_2_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_2_data$Origin <- factor(figure_2_data$Origin, levels = c("Native", "Invasive"))
figure_2_data$Species <- as.factor(figure_2_data$Species)

#
jitter_max_deg <- 3.2 * 1e-6  # Approximately 0.35 meters.

#
set.seed(123456)
# for the same site dataset
figure_2_data_same <- subset(figure_2_data, figure_2_data$Group == "Both")
figure_2_data_same$lat_jitter <- figure_2_data_same$Latitude + runif(nrow(figure_2_data_same), -jitter_max_deg, jitter_max_deg)
figure_2_data_same$lon_jitter <- figure_2_data_same$Longitude + runif(nrow(figure_2_data_same), -jitter_max_deg, jitter_max_deg)

# for the unique site dataset
figure_2_data_unique <- subset(figure_2_data, figure_2_data$Group != "Both")
figure_2_data_unique$lat_jitter <- figure_2_data_unique$Latitude
figure_2_data_unique$lon_jitter <- figure_2_data_unique$Longitude

figure_2_data_reshape = rbind(figure_2_data_same, figure_2_data_unique)
colnames(figure_2_data_reshape)

# for both
#figure_2_data_reshape = subset(figure_2_data_reshape, Group == "Both")
#dim(figure_2_data_reshape)


# Figure 2A
# Plant species richness (site level)
ALLplSR_data <- unique(figure_2_data_reshape[,c("Site", "ALLplSR", "Latitude", "Longitude")])
ALLplSR_data <- ALLplSR_data[complete.cases(ALLplSR_data[, "ALLplSR"]), ]

# raw data
mod1 <- gls(ALLplSR ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = ALLplSR_data, method = "REML", na.action = na.omit)
mod2 <- gls(ALLplSR ~ Latitude, correlation = corGaus(form = ~ Latitude + Longitude), data = ALLplSR_data, method = "REML", na.action = na.omit)
mod3 <- gls(ALLplSR ~ Latitude, correlation = corLin(form = ~ Latitude + Longitude), data = ALLplSR_data, method = "REML", na.action = na.omit)
mod4 <- gls(ALLplSR ~ Latitude, correlation = corRatio(form = ~ Latitude + Longitude), data = ALLplSR_data, method = "REML", na.action = na.omit)
mod5 <- gls(ALLplSR ~ Latitude, correlation = corSpher(form = ~ Latitude + Longitude), data = ALLplSR_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# selected model
mod1 <- gls(ALLplSR ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = ALLplSR_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod1)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# predictor 
ALLplSR_data$F0 <- predictSE(mod1, ALLplSR_data, level = 0)$fit
ALLplSR_data$SE <- predictSE(mod1, ALLplSR_data, level = 0)$se.fit

ggplot(ALLplSR_data, aes(x=Latitude, y=ALLplSR)) +
  geom_point(size = 3, pch = 21, color = "black", stroke = 0.7, fill = alpha("black", 0.3)) + 
  #geom_point(size = 3, color = "black", fill = "grey", pch = 21) + 
  geom_line(aes(y=F0), size=1) + 
  scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0)) +
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
HerbFR_data <- unique(figure_2_data_reshape[,c("Site", "HerbFR", "Latitude", "Longitude")])
HerbFR_data <- HerbFR_data[complete.cases(HerbFR_data[, "HerbFR"]), ]

# raw data
mod1 <- gls(HerbFR ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = HerbFR_data, method = "REML", na.action = na.omit)
mod2 <- gls(HerbFR ~ Latitude, correlation = corGaus(form = ~ Latitude + Longitude), data = HerbFR_data, method = "REML", na.action = na.omit)
mod3 <- gls(HerbFR ~ Latitude, correlation = corLin(form = ~ Latitude + Longitude), data = HerbFR_data, method = "REML", na.action = na.omit)
mod4 <- gls(HerbFR ~ Latitude, correlation = corRatio(form = ~ Latitude + Longitude), data = HerbFR_data, method = "REML", na.action = na.omit)
mod5 <- gls(HerbFR ~ Latitude, correlation = corSpher(form = ~ Latitude + Longitude), data = HerbFR_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

mod1 <- gls(HerbFR ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = HerbFR_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod1)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# predictor 
HerbFR_data$F0 <- predictSE(mod1, HerbFR_data, level = 0)$fit
HerbFR_data$SE <- predictSE(mod1, HerbFR_data, level = 0)$se.fit

# Extract the y-axis range from the already created Figure A.
x_limits <- layer_scales(Figure_2A)$x$range$range
x_breaks <- layer_scales(Figure_2A)$x$breaks

ggplot(HerbFR_data, aes(x=Latitude, y=HerbFR)) +
  #geom_point(size = 3, color = "black", fill = "grey", pch = 21) + 
  geom_point(size = 3, pch = 21, color = "black", stroke = 0.7, fill = alpha("black", 0.3)) + 
  geom_line(aes(y=F0), size=1) + 
  scale_y_continuous(breaks = seq(0, 16, by = 4), limits = c(0, 16), expand = c(0, 0)) +
  #scale_x_continuous(breaks = breaks_width(4)) +
  scale_x_continuous(breaks = x_breaks, limits = x_limits) + 
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
herbAB_data <- unique(figure_2_data_reshape[,c("Site", "HerbAB", "Latitude", "Longitude")])
herbAB_data <- herbAB_data[complete.cases(herbAB_data[, "HerbAB"]), ]

# raw data
mod1 <- gls(HerbAB ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = herbAB_data, method = "REML", na.action = na.omit)
mod2 <- gls(HerbAB ~ Latitude, correlation = corGaus(form = ~ Latitude + Longitude), data = herbAB_data, method = "REML", na.action = na.omit)
mod3 <- gls(HerbAB ~ Latitude, correlation = corLin(form = ~ Latitude + Longitude), data = herbAB_data, method = "REML", na.action = na.omit)
mod4 <- gls(HerbAB ~ Latitude, correlation = corRatio(form = ~ Latitude + Longitude), data = herbAB_data, method = "REML", na.action = na.omit)
mod5 <- gls(HerbAB ~ Latitude, correlation = corSpher(form = ~ Latitude + Longitude), data = herbAB_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(HerbAB ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = herbAB_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# sqrt-root translation
mod1 <- gls(sqrt(HerbAB) ~ Latitude, correlation = corExp(form = ~ Latitude + Longitude), data = herbAB_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod1)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# predictor 
herbAB_data$F0 <- predictSE(mod1, herbAB_data, level = 0)$fit
herbAB_data$SE <- predictSE(mod1, herbAB_data, level = 0)$se.fit

ggplot(herbAB_data, aes(x=Latitude, y=sqrt(HerbAB))) +
  geom_point(size = 3, pch = 21, color = "black", stroke = 0.7, fill = alpha("black", 0.3)) + 
  #geom_point(size = 3, color = "black", fill = "grey", pch = 21) + 
  geom_line(aes(y=F0), size=1) + 
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), expand = c(0, 0)) +
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
Defol_data = figure_2_data_reshape[complete.cases(figure_2_data_reshape[, "Defol"]), ]

# raw data
mod1 <- gls(Defol ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
mod2 <- gls(Defol ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
mod3 <- gls(Defol ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
mod4 <- gls(Defol ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
mod5 <- gls(Defol ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(Defol ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# log10 transformed was best
Defol_data$LOGDefol <- log10(Defol_data$Defol)
mod1 <- gls(LOGDefol ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(LOGDefol ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Defol_data, method = "REML")
m_lat <- gls(LOGDefol ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Defol_data, method = "REML")
m_lat_spp <- gls(LOGDefol ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Defol_data, method = "REML")
m_full <- gls(LOGDefol ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Defol_data, method = "REML")

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

Defol_data$F0 <- predictSE(mod1, Defol_data, level = 0)$fit
Defol_data$SE <- predictSE(mod1, Defol_data, level = 0)$se.fit

# Global model
mod0 <- gls(LOGDefol ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Defol_data, method = "REML", na.action = na.omit)
Defol_data$F00 <- predictSE(mod0, Defol_data, level = 0)$fit
Defol_data$SE0 <- predictSE(mod0, Defol_data, level = 0)$se.fit

ggplot(data = Defol_data, aes(x = Latitude, y = log10(Defol))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F00), size = 1) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_y_continuous(breaks = seq(-1.5, 2.5, by = 1), limits = c(-1.5, 2.5), expand = c(0, 0)) +
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
       y = expression("Leaf defoliation (%," ~ log[10] ~ ")"), 
       tag = "D") -> Figure_2D; Figure_2D



# Figure 2E
# Foliar pathogen infection
Disease_data = figure_2_data_reshape[complete.cases(figure_2_data_reshape[, "Disease"]), ]

# raw data
mod1 <- gls(Disease ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
mod2 <- gls(Disease ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
mod3 <- gls(Disease ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
mod4 <- gls(Disease ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
mod5 <- gls(Disease ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod4,mod5)

# raw data
mod1 <- gls(Disease ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# sqrt-root traslantion
Disease_data$SQRTDisease <- sqrt(Disease_data$Disease)
mod1 <- gls(SQRTDisease ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(SQRTDisease ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Disease_data, method = "REML")
m_lat <- gls(SQRTDisease ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Disease_data, method = "REML")
m_lat_spp <- gls(SQRTDisease ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Disease_data, method = "REML")
m_full <- gls(SQRTDisease ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = Disease_data, method = "REML")

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

# Print result
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

Disease_data$F0 <- predictSE(mod1, Disease_data, level = 0)$fit
Disease_data$SE <- predictSE(mod1, Disease_data, level = 0)$se.fit

# Global model
mod0 <- gls(SQRTDisease ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Disease_data, method = "REML", na.action = na.omit)
Disease_data$F00 <- predictSE(mod0, Disease_data, level = 0)$fit
Disease_data$SE0 <- predictSE(mod0, Disease_data, level = 0)$se.fit

ggplot(data = Disease_data, aes(x = Latitude, y = SQRTDisease)) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F00), size = 1) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12), expand = c(0, 0)) +
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
  labs(x = NULL, y = "Leaf pathogen infection (%, sqrt)", tag = "E") -> Figure_2E; Figure_2E


# Figure 2F
# Soil entire fungal richness
FUNGSR_data = figure_2_data_reshape[complete.cases(figure_2_data_reshape[, "FUNGSR"]), ]

# raw data
mod1 <- gls(FUNGSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = FUNGSR_data, method = "REML", na.action = na.omit)
mod2 <- gls(FUNGSR ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = FUNGSR_data, method = "REML", na.action = na.omit)
mod3 <- gls(FUNGSR ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = FUNGSR_data, method = "REML", na.action = na.omit)
mod4 <- gls(FUNGSR ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = FUNGSR_data, method = "REML", na.action = na.omit)
mod5 <- gls(FUNGSR ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = FUNGSR_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(FUNGSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = FUNGSR_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(FUNGSR ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = FUNGSR_data, method = "REML")
m_lat <- gls(FUNGSR ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = FUNGSR_data, method = "REML")
m_lat_spp <- gls(FUNGSR ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = FUNGSR_data, method = "REML")
m_full <- gls(FUNGSR ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = FUNGSR_data, method = "REML")

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

# Print result
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

FUNGSR_data$F0 <- predictSE(mod1, FUNGSR_data, level = 0)$fit
FUNGSR_data$SE <- predictSE(mod1, FUNGSR_data, level = 0)$se.fit

# Global model
mod0 <- gls(FUNGSR ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = FUNGSR_data, method = "REML", na.action = na.omit)
FUNGSR_data$F00 <- predictSE(mod0, FUNGSR_data, level = 0)$fit
FUNGSR_data$SE0 <- predictSE(mod0, FUNGSR_data, level = 0)$se.fit

ggplot(data = FUNGSR_data, aes(x = Latitude, y = FUNGSR)) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F00), size=1, linetype = 1, color = "black") + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_y_continuous(breaks = seq(400, 1200, by = 200), limits = c(400, 1200), expand = c(0, 0)) +
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
       y = expression("Soil overall fungal richness"), 
       tag = "F") -> Figure_2F; Figure_2F


# Figure 2G
# Soil pathogenic fungi richness
PATHSR_data = figure_2_data_reshape[complete.cases(figure_2_data_reshape[, "PATHSR"]), ]

# raw data
mod1 <- gls(PATHSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
mod2 <- gls(PATHSR ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
mod3 <- gls(PATHSR ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
mod4 <- gls(PATHSR ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
mod5 <- gls(PATHSR ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(PATHSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# log10 translation
PATHSR_data$LOGPATHSR <- log10(PATHSR_data$PATHSR)
mod1 <- gls(LOGPATHSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(LOGPATHSR ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = PATHSR_data, method = "REML")
m_lat <- gls(LOGPATHSR ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = PATHSR_data, method = "REML")
m_lat_spp <- gls(LOGPATHSR ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = PATHSR_data, method = "REML")
m_full <- gls(LOGPATHSR ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = PATHSR_data, method = "REML")

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

# Print result
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

PATHSR_data$F0 <- predictSE(mod1, PATHSR_data, level = 0)$fit
PATHSR_data$SE <- predictSE(mod1, PATHSR_data, level = 0)$se.fit

# Global model
mod0 <- gls(LOGPATHSR ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = PATHSR_data, method = "REML", na.action = na.omit)
PATHSR_data$F00 <- predictSE(mod0, PATHSR_data, level = 0)$fit
PATHSR_data$SE0 <- predictSE(mod0, PATHSR_data, level = 0)$se.fit

ggplot(data = PATHSR_data, aes(x = Latitude, y = LOGPATHSR)) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F00), size=1, linetype = 1, color = "black") + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_y_continuous(breaks = seq(1.0, 1.8, by = 0.2), limits = c(1.0, 1.8), expand = c(0, 0)) +
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
  labs(x = "Latitude (North degress)", 
       y = expression("Soil pathogenic fungi richness ("~log[10]~")"), 
       tag = "G") -> Figure_2G; Figure_2G


# Figure 2H
# Soil AMF richness
AMFSR_data = figure_2_data_reshape[complete.cases(figure_2_data_reshape[, "AMFSR"]), ]

# raw data
mod1 <- gls(AMFSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
mod2 <- gls(AMFSR ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
mod3 <- gls(AMFSR ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
mod4 <- gls(AMFSR ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
mod5 <- gls(AMFSR ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) # remove mod3 (false convergence)
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(AMFSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# sqrt-root translation
AMFSR_data$SQRTAMFSR <- sqrt(AMFSR_data$AMFSR)
mod1 <- gls(SQRTAMFSR ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")
piecewiseSEM::rsquared(mod1)

#
m0 <- gls(SQRTAMFSR ~ 1,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = AMFSR_data, method = "REML")
m_lat <- gls(SQRTAMFSR ~ Latitude,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = AMFSR_data, method = "REML")
m_lat_spp <- gls(SQRTAMFSR ~ Latitude + Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = AMFSR_data, method = "REML")
m_full <- gls(SQRTAMFSR ~ Latitude * Species,correlation = corExp(form = ~ lat_jitter + lon_jitter),data = AMFSR_data, method = "REML")

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

# Print result
result <- data.frame(
  Predictor = c("Latitude", "Species", "Interaction"),
  R2_Contribution = r2_contribution,
  Percent_Variance = r2_contribution * 100,
  Cohen_f = c(f_lat, f_spp, f_int),
  Effect_Size = ifelse(c(f_lat, f_spp, f_int) < 0.10, "Small",
                       ifelse(c(f_lat, f_spp, f_int) < 0.25, "Medium", "Large")))
print(result)

# as
mod_as <- gls(SQRTAMFSR ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(AMFSR_data, Species == "Alternanthera_sessilis"), method = "REML")
car::Anova(mod_as, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_as)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

# ap
mod_ap <- gls(SQRTAMFSR ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(AMFSR_data, Species == "Alternanthera_philoxeroides"), method = "REML")
car::Anova(mod_ap, type = "III", test.statistic = "Chisq")
r2_full <- rsquared(mod_ap)$R.squared
print(f <- sqrt(r2_full / (1 - r2_full)))

AMFSR_data$F0 <- predictSE(mod1, AMFSR_data, level = 0)$fit
AMFSR_data$SE <- predictSE(mod1, AMFSR_data, level = 0)$se.fit

# Global model
mod0 <- gls(SQRTAMFSR ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = AMFSR_data, method = "REML", na.action = na.omit)
AMFSR_data$F00 <- predictSE(mod0, AMFSR_data, level = 0)$fit
AMFSR_data$SE0 <- predictSE(mod0, AMFSR_data, level = 0)$se.fit

ggplot(data = AMFSR_data, aes(x = Latitude, y = SQRTAMFSR)) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  geom_line(aes(y=F0, color = Origin, linetype = Origin), size = 1) + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4), expand = c(0, 0)) +
  scale_x_continuous(breaks = breaks_width(4)) +
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
#ggsave("Figure 2-0415.pdf", plot = Figure_2, width = 10.9, height = 13.90, units = "in", dpi = 300)
