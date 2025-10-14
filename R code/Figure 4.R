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

colnames(figure_4_data)
# data translation
figure_4_data$SQRTlesion <- sqrt(figure_4_data$Lesion)
figure_4_data$SQRTknots <- sqrt(figure_4_data$Knots)
figure_4_data$LOGdefol <- log10(figure_4_data$Defol)
figure_4_data$SQRTdisease <- sqrt(figure_4_data$Disease)
#figure_4_data$LOGFUNG <- log10(figure_4_data$FUNGSR) # no translation
figure_4_data$LOGPATH <- log10(figure_4_data$PATHSR)
figure_4_data$SQRTAMF <- sqrt(figure_4_data$AMFSR)

# data translation
figure_4_data$LOGSoil_C <- log10(figure_4_data$Soil_C)
figure_4_data$LOGSoil_N <- log10(figure_4_data$Soil_N)
figure_4_data$SQRTHerbAB <- sqrt(figure_4_data$HerbAB)

################################ Native dataset ################################
figure_4_data_nat <- subset(figure_4_data, Origin == "Native")
select_var <- c("Soil_wc", "LOGSoil_C", "LOGSoil_N", "Soil_ph", "Bio1", "Bio15", "ALLplSR", "HerbFR", "SQRTHerbAB", "LOGdefol", 
                "SQRTdisease", "FUNGSR", "LOGPATH", "SQRTAMF")

cor_results <- corr.test(figure_4_data_nat[,c("Con_mass", "Bsurv", "SQRTlesion", "SQRTknots")], 
                         figure_4_data_nat[, select_var], method = "spearman", adjust = "none")

cor_matrix <- cor_results$r 
p_matrix <- cor_results$p   
n_matrix <- cor_results$n  

# Convert cor_matrix and p_matrix to data frames and merge them
cor_df <- as.data.frame(cor_matrix) %>%
  tibble::rownames_to_column(var = "Respond_var") %>%
  tidyr::pivot_longer(cols = -Respond_var, names_to = "Predict_var", values_to = "Correlation")

p_df <- as.data.frame(p_matrix) %>%
  tibble::rownames_to_column(var = "Respond_var") %>%
  tidyr::pivot_longer(cols = -Respond_var, names_to = "Predict_var", values_to = "p_value")

# Merge the two data frames
plot_data_nat <- left_join(cor_df, p_df, by = c("Respond_var", "Predict_var"))
plot_data_nat$Origin = "Native plant - A. sessilis"


############################### Invasive dataset ###############################
figure_4_data_inv <- subset(figure_4_data, Origin == "Invasive")

cor_results <- corr.test(figure_4_data_inv[,c("Con_mass", "Bsurv", "SQRTlesion", "SQRTknots")], 
                         figure_4_data_inv[, select_var], method = "spearman", adjust = "none")

cor_matrix <- cor_results$r 
p_matrix <- cor_results$p   
n_matrix <- cor_results$n  

# Convert cor_matrix and p_matrix to data frames and merge them
cor_df <- as.data.frame(cor_matrix) %>%
  tibble::rownames_to_column(var = "Respond_var") %>%
  tidyr::pivot_longer(cols = -Respond_var, names_to = "Predict_var", values_to = "Correlation")

p_df <- as.data.frame(p_matrix) %>%
  tibble::rownames_to_column(var = "Respond_var") %>%
  tidyr::pivot_longer(cols = -Respond_var, names_to = "Predict_var", values_to = "p_value")

# Merge the two data frames
plot_data_inv <- left_join(cor_df, p_df, by = c("Respond_var", "Predict_var"))
plot_data_inv$Origin = "Invasive plant - A. philoxeroides"

# Merge the native and exotic datasets
plot_data = rbind(plot_data_nat, plot_data_inv)

# Visualize figure
# Add significance markers (* for p < 0.05)
plot_data <- plot_data %>%
  mutate(
    significance = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01  ~ "**",
      p_value <= 0.05  ~ "*",
      p_value <= 0.1   ~ "†",
      TRUE ~ ""
    )
  )


# Rename variables
plot_data$Respond_var[plot_data$Respond_var == "Con_mass"] <- "Plant growth"
plot_data$Respond_var[plot_data$Respond_var == "Bsurv"] <- "Beetle survival"
plot_data$Respond_var[plot_data$Respond_var == "SQRTlesion"] <- "Disease lesions"
plot_data$Respond_var[plot_data$Respond_var == "SQRTknots"] <- "Root knots"

plot_data$Predict_var[plot_data$Predict_var == "LOGSoil_C"] <- "Soil carbon content"
plot_data$Predict_var[plot_data$Predict_var == "LOGSoil_N"] <- "Soil nitrogen content"
plot_data$Predict_var[plot_data$Predict_var == "Soil_wc"] <- "Soil water content"
plot_data$Predict_var[plot_data$Predict_var == "Soil_ph"] <- "Soil pH"

plot_data$Predict_var[plot_data$Predict_var == "Bio1"] <- "Annual mean temperature"
plot_data$Predict_var[plot_data$Predict_var == "Bio15"] <- "Precipitation seasonality"

plot_data$Predict_var[plot_data$Predict_var == "ALLplSR"] <- "Plant species richness"
plot_data$Predict_var[plot_data$Predict_var == "HerbFR"] <- "Herbivore richess"
plot_data$Predict_var[plot_data$Predict_var == "SQRTHerbAB"] <- "Herbivore abundance"
plot_data$Predict_var[plot_data$Predict_var == "LOGdefol"] <- "Foliar defoliation"
plot_data$Predict_var[plot_data$Predict_var == "SQRTdisease"] <- "Foliar pathogen infection"

plot_data$Predict_var[plot_data$Predict_var == "FUNGSR"] <- "Soil entire fungal richness"
plot_data$Predict_var[plot_data$Predict_var == "LOGPATH"] <- "Soil pathogenic fungal richness"
plot_data$Predict_var[plot_data$Predict_var == "SQRTAMF"] <- "Soil AMF richness"

# Adjust factor order
plot_data$Respond_var <- factor(plot_data$Respond_var, levels = c("Plant growth", "Beetle survival", 
                                                                  "Disease lesions", "Root knots"))

plot_data$Predict_var <- factor(plot_data$Predict_var, levels = rev(c("Soil water content", "Soil carbon content", "Soil nitrogen content", "Soil pH",
                                                                     "Annual mean temperature", "Precipitation seasonality",
                                                                     "Plant species richness", "Herbivore richess", "Herbivore abundance", "Foliar defoliation", "Foliar pathogen infection",
                                                                     "Soil entire fungal richness", "Soil pathogenic fungal richness", "Soil AMF richness")))

plot_data$Origin <- factor(plot_data$Origin, levels = c("Native plant - A. sessilis",
                                                       "Invasive plant - A. philoxeroides"))


plot_data$Predict_group <- dplyr::case_when(
  plot_data$Predict_var %in% c("Soil water content", "Soil carbon content", "Soil nitrogen content", "Soil pH") ~ "Soil",
  plot_data$Predict_var %in% c("Annual mean temperature", "Precipitation seasonality") ~ "Climate",
  plot_data$Predict_var %in% c("Plant species richness", "Herbivore richess", "Herbivore abundance", "Foliar defoliation", "Foliar pathogen infection") ~ "Plant",
  plot_data$Predict_var %in% c("Soil entire fungal richness", "Soil pathogenic fungal richness", "Soil AMF richness") ~ "Fungi")

ggplot(plot_data, aes(x = Respond_var, y = Predict_var, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) + 
  geom_text(aes(label = sprintf("%.2f\n%s", Correlation, significance)), 
            color = "black", size = 3.5) + 
  scale_fill_gradient2(
    low = "#E5BF38", mid = "white", high = "#4578AB",
    midpoint = 0, limits = c(-0.65, 0.65)) +
  theme_bw() + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #geom_hline(yintercept = c(14), color = "black", linetype = 1) + 
  #geom_vline(xintercept = c(1), color = "black", linetype = 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_blank(),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.border = element_blank(),
        strip.background = element_rect(color="white", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "black"),
        legend.position = "right") + # bottom
  labs(x = NULL, y = NULL,  fill = "Spearman\ncorrelation") +
  facet_wrap(~ Origin, strip.position = "top") + # bottom
  guides(fill = guide_colorbar( title.position = "top", title.hjust = 0.5,barwidth = 1, barheight = 22)) +
  coord_cartesian(clip = 'off') -> Figure_4; Figure_4

# ggsave("Figure_4_new.pdf", plot = Figure_4, width = 8.35, height = 7.36, units = "in", dpi = 300)

