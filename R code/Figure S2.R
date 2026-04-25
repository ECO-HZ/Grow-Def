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
library(scales)
library(AICcmodavg)

# loading field survey dataset
figure_2_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_2_data$Origin <- ifelse(figure_2_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_2_data$Origin <- factor(figure_2_data$Origin, levels = c("Native", "Invasive"))
figure_2_data$Species <- as.factor(figure_2_data$Species)

#
jitter_max_deg <- 3.2 * 1e-6  # 约0.35米

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

library(nlme)
library(MuMIn)
Rel_cover_data = figure_2_data_reshape[complete.cases(figure_2_data_reshape[, "Rel_cover"]), ]
Rel_cover_data$Rel_cover = Rel_cover_data$Rel_cover*100
# raw data
mod1 <- gls(Rel_cover ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
mod2 <- gls(Rel_cover ~ Latitude*Species, correlation = corGaus(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
mod3 <- gls(Rel_cover ~ Latitude*Species, correlation = corLin(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
mod4 <- gls(Rel_cover ~ Latitude*Species, correlation = corRatio(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
mod5 <- gls(Rel_cover ~ Latitude*Species, correlation = corSpher(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
anova(mod1,mod2,mod3,mod4,mod5) 
MuMIn::AICc(mod1,mod2,mod3,mod4,mod5)

# raw data
mod1 <- gls(Rel_cover ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)

# log10 transformed was best
mod1 <- gls(log10(Rel_cover) ~ Latitude*Species, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
shapiro.test(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1), resid(mod1, type = "normalized"))
abline(h = 0, lty = 2)
car::Anova(mod1, type = "III", test.statistic = "Chisq")

Rel_cover_data$F0 <- predictSE(mod1, Rel_cover_data, level = 0)$fit
Rel_cover_data$SE <- predictSE(mod1, Rel_cover_data, level = 0)$se.fit

mod_as <- gls(log10(Rel_cover) ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Rel_cover_data, Species == "Alternanthera_sessilis"), method = "REML")
car::Anova(mod_as, type = "III", test.statistic = "Chisq")
summary(mod_as)

mod_ap <- gls(log10(Rel_cover) ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), 
              data = subset(Rel_cover_data, Species == "Alternanthera_philoxeroides"), method = "REML")
car::Anova(mod_ap, type = "III", test.statistic = "Chisq")
summary(mod_ap)


# Global model
mod0 <- gls(log10(Rel_cover) ~ Latitude, correlation = corExp(form = ~ lat_jitter + lon_jitter), data = Rel_cover_data, method = "REML", na.action = na.omit)
Rel_cover_data$F00 <- predictSE(mod0, Rel_cover_data, level = 0)$fit
Rel_cover_data$SE0 <- predictSE(mod0, Rel_cover_data, level = 0)$se.fit

ggplot(data = Rel_cover_data, aes(x = Latitude, y = log10(Rel_cover))) + 
  geom_point(size = 3, pch = 21, stroke = 0.7, aes(color = Origin, fill = Origin)) + 
  #geom_line(aes(y=F0, color = Origin, linetype = Origin), size = 1) + 
  geom_line(aes(y=F00), size=1, linetype = 1, color = "black") + 
  scale_fill_manual(values = c("Native" = alpha("#00688B", 0.5), "Invasive" = alpha("#FFC225", 0.5))) + 
  scale_color_manual(values = c("Native" = "#00688B", "Invasive" = "#FFC225")) + 
  scale_x_continuous(breaks = breaks_width(4)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0.3)) +
  theme(axis.title = element_text(size = 13),
        axis.text=element_text(color="black", size=11),
        legend.text= element_text(size=11),
        legend.position = c(0.12,0.95),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(), legend.background = element_blank(), 
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Latitude (North degress)", 
       y = "Focal species' relative abundance\n(%, Log10-transformed)")

