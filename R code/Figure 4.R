################################################################################
################################### Figure 4 ###################################
################################################################################

# loading package
library(openxlsx)
library(dplyr)
library(ggplot2)
library(psych)
library(ggtext)

# loading field survey dataset
figure_4_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_4_data$Origin <- ifelse(figure_4_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_4_data$Origin <- factor(figure_4_data$Origin, levels = c("Native", "Invasive"))
figure_4_data$Species <- as.factor(figure_4_data$Species)

# data translation
figure_4_data$LOGSoil_C <- log10(figure_4_data$Soil_C_all)
figure_4_data$LOGSoil_N <- log10(figure_4_data$Soil_N_all)
figure_4_data$SQRTHerbAB <- sqrt(figure_4_data$HerbAB)

figure_4_data$LOGdefol <- log10(figure_4_data$Defol)
figure_4_data$SQRTDisease <- sqrt(figure_4_data$Disease)
figure_4_data$LOGPATH <- log10(figure_4_data$PATHSR)
figure_4_data$SQRTAMF <- sqrt(figure_4_data$AMFSR)

# 
figure_4_data$logitBsurv <- logit(figure_4_data$Bsurv/100)
figure_4_data$SQRTlesion <- sqrt(figure_4_data$Lesion)
figure_4_data$SQRTknots <- sqrt(figure_4_data$Knots)

#
jitter_max_deg <- 3.2 * 1e-6  #Approximately 0.35 meters.

set.seed(123456)

# for the same site dataset
figure_4_data_same <- subset(figure_4_data, figure_4_data$Group == "Both")
figure_4_data_same$lat_jitter <- figure_4_data_same$Latitude + runif(nrow(figure_4_data_same), -jitter_max_deg, jitter_max_deg)
figure_4_data_same$lon_jitter <- figure_4_data_same$Longitude + runif(nrow(figure_4_data_same), -jitter_max_deg, jitter_max_deg)

# for the unique site dataset
figure_4_data_unique <- subset(figure_4_data, figure_4_data$Group != "Both")
figure_4_data_unique$lat_jitter <- figure_4_data_unique$Latitude
figure_4_data_unique$lon_jitter <- figure_4_data_unique$Longitude

figure_4_data_reshape = rbind(figure_4_data_same, figure_4_data_unique)
colnames(figure_4_data_reshape)
rownames(figure_4_data_reshape) = figure_4_data_reshape$Popu_code

library(FactoMineR)
library(factoextra)

# PCA for climate, soil, abover and below groups
# CLIMATE PC
pca_climate <- PCA(figure_4_data_reshape[,c("Bio1", "Bio15")], scale.unit = TRUE, graph = F)
pca_climate_loadings <- as.data.frame(pca_climate$var$coord)
pca_climate_loadings$Variable <- rownames(pca_climate_loadings)
variance_explained <- round(as.data.frame(pca_climate$eig)$`percentage of variance`, 1)
pca_scores_climate <- as.data.frame(pca_climate$ind$coord)
colnames(pca_scores_climate) <- c("climate1_r", "climate2_r")
pca_scores_climate$Popu_code <- rownames(pca_scores_climate)
#fviz_pca_biplot(pca_climate, col.var = "black", col.ind = "black", repel = TRUE, label = "var")             
#summary(pca_scores_climate$climate1_r)

ggplot(pca_scores_climate, aes(x = climate1_r, y = climate2_r)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  geom_point(size = 3, alpha = 1, shape = 21, fill = "grey", color = "black") +  
  geom_segment(data = pca_climate_loadings, aes(x = 0, y = 0, xend = Dim.1 * 1.5, yend = Dim.2 * 1.5),               
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +  
  geom_text(data = pca_climate_loadings,             
            aes(x = Dim.1 * 1.6, y = Dim.2 * 1.6, label = Variable), color = "black") +  
  labs(x = paste0("PC1 (", variance_explained[1], "%)"),       
       y = paste0("PC2 (", variance_explained[2], "%)"), tag = "A") + 
  theme_classic() +
  theme(axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 11, color = "black"),
        plot.tag = element_text(size = 14, face = "bold"))-> P1; P1

# soil PC
#soil_pc <- figure_4_data_reshape[,c("Soil_wc_all", "LOGSoil_C", "LOGSoil_N", "Soil_ph_all")]
#soil_pc <- na.omit(soil_pc)
#pca_soil <- PCA(pca_input, scale.unit = TRUE, graph = T)
#pca_soil$var$coord   
#pca_soil$eig 
#pca_scores_soil <- as.data.frame(pca_soil$ind$coord)[,c(1:2)]
#colnames(pca_scores_soil) <- c("soil1_r", "soil2_r")
#pca_scores_soil$Popu_code <- rownames(pca_scores_soil)
#fviz_pca_biplot(pca_soil, col.var = "black", col.ind = "black", repel = TRUE, label = "var")             

soil_pc <- figure_4_data_reshape[,c("Soil_wc_all", "LOGSoil_C", "LOGSoil_N", "Soil_ph_all")]
soil_pc <- na.omit(soil_pc)
pca_soil <- prcomp((soil_pc), center = TRUE, scale. = TRUE)
pca_scores_soil <- as.data.frame(pca_soil$x)[,1:2]
colnames(pca_scores_soil) = c("soil1_r", "soil2_r")
pca_scores_soil$Popu_code <- rownames(pca_scores_soil)
variance_explained <- round(pca_soil$sdev^2 / sum(pca_soil$sdev^2) * 100, 1)
pca_soil_loadings <- as.data.frame(pca_soil$rotation[, 1:2])
pca_soil_loadings$Variable <- rownames(pca_soil_loadings)

ggplot(pca_scores_soil, aes(x = soil1_r, y = soil2_r)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  geom_point(size = 3, alpha = 1, shape = 21, fill = "grey", color = "black") +  
  geom_segment(data = pca_soil_loadings, aes(x = 0, y = 0, xend = PC1 * 3.5, yend = PC2 * 3.5),               
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +  
  geom_text(data = pca_soil_loadings,             
            aes(x = PC1 * 3.6, y = PC2 * 3.6, label = Variable), color = "black") +  
  labs(x = paste0("PC1 (", variance_explained[1], "%)"),       
       y = paste0("PC2 (", variance_explained[2], "%)"), tag = "B") + 
  theme_classic() +
  theme(axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 11, color = "black"),
        plot.tag = element_text(size = 14, face = "bold"))-> P2; P2


# above biotic variables PC
above_pc <- figure_4_data_reshape[,c("ALLplSR", "SQRTHerbAB", "SQRTDisease", "LOGdefol")]
above_pc <- na.omit(above_pc)
pca_above <- PCA(above_pc, scale.unit = TRUE, graph = F)
pca_above_loadings <- as.data.frame(pca_above$var$coord)
pca_above_loadings$Variable <- rownames(pca_above_loadings)
variance_explained <- round(as.data.frame(pca_above$eig)$`percentage of variance`, 1)
pca_scores_above <- as.data.frame(pca_above$ind$coord)[,c(1:2)]
colnames(pca_scores_above) <- c("above1_r", "above2_r")
pca_scores_above$Popu_code <- rownames(pca_scores_above)
#fviz_pca_biplot(pca_above, col.var = "black", col.ind = "black", repel = TRUE, label = "var")             

ggplot(pca_scores_above, aes(x = above1_r, y = above2_r)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  geom_point(size = 3, alpha = 1, shape = 21, fill = "grey", color = "black") +  
  geom_segment(data = pca_above_loadings, aes(x = 0, y = 0, xend = Dim.1 * 3.8, yend = Dim.2 * 3.8),               
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +  
  geom_text(data = pca_above_loadings,             
            aes(x = Dim.1 * 3.9, y = Dim.2 * 3.9, label = Variable), color = "black") +  
  labs(x = paste0("PC1 (", variance_explained[1], "%)"),       
       y = paste0("PC2 (", variance_explained[2], "%)"), tag = "C") + 
  theme_classic() +
  theme(axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 11, color = "black"),
        plot.tag = element_text(size = 14, face = "bold"))-> P3; P3


# soil biotic variables PC
below_pc <- figure_4_data_reshape[,c("FUNGSR", "LOGPATH", "SQRTAMF")]
below_pc <- na.omit(below_pc)
pca_below <- PCA(below_pc, scale.unit = TRUE, graph = F)
pca_below_loadings <- as.data.frame(pca_below$var$coord)
pca_below_loadings$Variable <- rownames(pca_below_loadings)
variance_explained <- round(as.data.frame(pca_below$eig)$`percentage of variance`, 1)
pca_scores_below <- as.data.frame(pca_below$ind$coord)[,c(1:2)]
colnames(pca_scores_below) <- c("below1_r", "below2_r")
pca_scores_below$Popu_code <- rownames(pca_scores_below)
#fviz_pca_biplot(pca_below, col.var = "black", col.ind = "black", repel = TRUE, label = "var")             

ggplot(pca_scores_below, aes(x = below1_r, y = below2_r)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  geom_point(size = 3, alpha = 1, shape = 21, fill = "grey", color = "black") +  
  geom_segment(data = pca_below_loadings, aes(x = 0, y = 0, xend = Dim.1 * 2.5, yend = Dim.2 * 2.5),               
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +  
  geom_text(data = pca_below_loadings,             
            aes(x = Dim.1 * 2.6, y = Dim.2 * 2.6, label = Variable), color = "black") +  
  labs(x = paste0("PC1 (", variance_explained[1], "%)"),       
       y = paste0("PC2 (", variance_explained[2], "%)"), tag = "D") + 
  theme_classic() +
  theme(axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 11, color = "black"),
        plot.tag = element_text(size = 14, face = "bold"))-> P4; P4

#(P1|P2)/(P3|P4)

################################################################################
figure_4_data_reshape = figure_4_data_reshape %>% 
  left_join(pca_scores_climate[,c("Popu_code", "climate1_r")]) %>% 
  left_join(pca_scores_soil[,c("Popu_code", "soil1_r")]) %>% 
  left_join(pca_scores_below[,c("Popu_code", "below1_r")]) %>% 
  left_join(pca_scores_above[,c("Popu_code", "above1_r")])

################################################################################
colnames(figure_4_data_reshape)

library(dplyr)
figure_4_data_reshape <- figure_4_data_reshape %>%
  rename(climate1 = climate1_r,
         SoilPhys1 = soil1_r,
         AGbio1 = above1_r,
         SoilBio1 = below1_r)

################################################################################
# only for both site
figure_4_data_all <- subset(figure_4_data_reshape, Group == "Both")
figure_4_data_all <- na.omit(figure_4_data_all)
dim(figure_4_data_all)
################################################################################
library(piecewiseSEM)
library(nlme)

SEM_updated0 = psem(
  gls(SoilPhys1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(AGbio1 ~ climate1 + SoilPhys1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(SoilBio1 ~ climate1 + SoilPhys1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  #
  AGbio1 %~~% SoilBio1
)

SEM_summary = summary(SEM_updated0, .progressBar = TRUE, standardize = "scale")
max_p_row <- which.max(SEM_summary$coefficients$P.Value)
SEM_summary$coefficients[max_p_row, ]

# remove SoilPhys1 -> SoilBio1
SEM_updated1 = psem(
  gls(SoilPhys1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(AGbio1 ~ climate1 + SoilPhys1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(SoilBio1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  #
  AGbio1 %~~% SoilBio1
)

SEM_summary1 <- summary(SEM_updated1, .progressBar = TRUE, standardize = "scale")
max_p_row <- which.max(SEM_summary1$coefficients$P.Value)
SEM_summary1$coefficients[max_p_row, ]


# SoilPhys1 -> AGbio1
SEM_updated2 = psem(
  gls(SoilPhys1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(AGbio1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(SoilBio1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  #
  AGbio1 %~~% SoilBio1
)

SEM_summary2 = summary(SEM_updated2, .progressBar = TRUE, standardize = "scale")
max_p_row <- which.max(SEM_summary2$coefficients$P.Value)
SEM_summary2$coefficients[max_p_row, ]


# AGbio1 %~~% SoilBio1
SEM_updated3 = psem(
  gls(SoilPhys1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(AGbio1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all), 
  
  gls(SoilBio1 ~ climate1, 
      correlation = corExp(form = ~ lat_jitter + lon_jitter), data = figure_4_data_all) 
  
  #
  #AGbio1 %~~% SoilBio1
)

SEM_summary3 = summary(SEM_updated3, .progressBar = TRUE, standardize = "scale")
max_p_row <- which.max(SEM_summary3$coefficients$P.Value)
SEM_summary3$coefficients[max_p_row, ]

################################################################################
all_models <- list(
  SEM_updated0 = SEM_updated0,
  SEM_updated1 = SEM_updated1, 
  SEM_updated2 = SEM_updated2,
  SEM_updated3 = SEM_updated3
)

comparison_table <- data.frame()

for(i in 1:length(all_models)) {
  model <- all_models[[i]]
  
  mod_sum <- summary(model, .progressBar = TRUE)
  
  comparison_table <- rbind(comparison_table, 
                            data.frame(Model = names(all_models)[i], 
                                       AIC = mod_sum$IC$AIC,
                                       df = mod_sum$Cstat$df,
                                       Fisher_C = mod_sum$Cstat$Fisher.C,
                                       Fisher_P = mod_sum$Cstat$P.Value))
}

# rank by AIC 
comparison_table <- comparison_table[order(comparison_table$AIC), ]
print(comparison_table)

################################ Native dataset ################################
figure_4_data_nat <- subset(figure_4_data_all, Origin == "Native")

t.modlist = psem(
  gls(Con_mass ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1, 
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_nat, method = "ML"),  
  gls(logitBsurv ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1, 
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_nat, method = "ML"),
  gls(SQRTlesion ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1,   
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_nat, method = "ML"),  
  gls(SQRTknots ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1, 
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_nat, method = "ML"))

library(MuMIn)
### Model optimization ###
# Perform model selection (dredge) for each lme component and refit using REML
optimized_models <- lapply(t.modlist, function(model) {
  if (inherits(model, "gls")) {
    best_model <- dredge(model, trace = 2, rank = "AIC") %>%
      get.models(1) %>%.[[1]] 
    model <- update(best_model, method = "REML")
    return(model) 
  }
})

library(car)
Anova(optimized_models[[1]], type = "III", test.statistic = "Chisq")
#summary(optimized_models[[1]])
effectsize::effectsize(optimized_models[[1]])
AIC(optimized_models[[1]])


Anova(optimized_models[[2]], type = "III", test.statistic = "Chisq")
#summary(optimized_models[[2]])
effectsize::effectsize(optimized_models[[2]])
AIC(optimized_models[[2]])

Anova(optimized_models[[3]], type = "III", test.statistic = "Chisq")
#summary(optimized_models[[3]])
effectsize::effectsize(optimized_models[[3]])
AIC(optimized_models[[3]])

Anova(optimized_models[[4]], type = "III", test.statistic = "Chisq")
effectsize::effectsize(optimized_models[[4]])
summary(optimized_models[[4]])
AIC(optimized_models[[4]])

############################## invasive dataset ################################
figure_4_data_inv <- subset(figure_4_data_all, Origin == "Invasive")

t.modlist = psem(
  gls(Con_mass ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1, 
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_inv, method = "ML"),  
  gls(logitBsurv ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1, 
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_inv, method = "ML"),
  gls(SQRTlesion ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1,   
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_inv, method = "ML"),  
  gls(SQRTknots ~ climate1 + SoilPhys1 + AGbio1 + SoilBio1, 
      correlation = corExp(form = ~ Latitude + Longitude), data = figure_4_data_inv, method = "ML"))


### Model optimization ###
# Perform model selection (dredge) for each lme component and refit using REML
optimized_models <- lapply(t.modlist, function(model) {
  if (inherits(model, "gls")) {
    best_model <- dredge(model, trace = 2, rank = "AIC") %>%
      get.models(1) %>%.[[1]] 
    model <- update(best_model, method = "REML")
    return(model) 
  }
})

library(car)
Anova(optimized_models[[1]], type = "III", test.statistic = "Chisq")
#summary(optimized_models[[1]])
effectsize::effectsize(optimized_models[[1]])
AIC(optimized_models[[1]])


Anova(optimized_models[[2]], type = "III", test.statistic = "Chisq")
#summary(optimized_models[[2]])
effectsize::effectsize(optimized_models[[2]])
AIC(optimized_models[[2]])

Anova(optimized_models[[3]], type = "III", test.statistic = "Chisq")
#summary(optimized_models[[3]])
effectsize::effectsize(optimized_models[[3]])
AIC(optimized_models[[3]])

Anova(optimized_models[[4]], type = "III", test.statistic = "Chisq")
#summary(optimized_models[[4]])
effectsize::effectsize(optimized_models[[4]])
AIC(optimized_models[[4]])

