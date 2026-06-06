###################### Selection of bioclimatic variables ######################

# loading package
library(geodata)
library(openxlsx)
library(terra)
library(psych)
library(corrplot)
library(dplyr)
library(vegan)

# download WorldClim 2.1 bioclimatic variables at 30 arc-second resolution
#wc <- worldclim_global(var="bio", res=2.5, path="bioclimate")

files <- list.files("F:/2024-2025分析相关文件/植物抗性纬度变化/J eco 9-21/bioclimate_30s/climate/wc2.1_30s", 
                    pattern = "tif$", full.names = TRUE)
wc <- rast(files)
#plot(wc$wc2.1_30s_bio_1)

# loading coordinates dataset
field_survey_dataset <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T, rowNames = F)
coords_data <- unique(field_survey_dataset[, c("Site", "Longitude", "Latitude")])
coords <- coords_data[, c("Longitude", "Latitude")]

# obtained 19 bioclimatic variables for each sampling site
clim_values <- terra::extract(wc, coords_data[,-1])
#colnames(clim_values)[-1] = paste0("Bio", 1:19)
colnames(clim_values)[-1] <- gsub("^wc2\\.1_30s_bio_", "Bio", colnames(clim_values)[-1])
climate_data <- cbind(coords_data, clim_values[-1])
#write.csv(climate_data,"climate_data.csv")
#colnames(field_survey_dataset)
colnames(climate_data)
#climate_data = unique(field_survey_dataset[,c(5,6,25:43)])
# Spearman correlation matrix
colnames(figure_2_data_reshape)

#corr_matrix <- corr.test(climate_data[,c(4,15:22,5:14)], method = "spearman", adjust = "none")
corr_matrix <- corr.test(figure_2_data_reshape[,c(33:51)], method = "spearman", adjust = "none")

# only show the signification correlation
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank', method = 'number',type = 'lower',
         diag = FALSE, col=col(200), tl.cex = 0.8,tl.col = "black", number.cex = 0.8, order = "original",tl.srt = 45)

corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank', method = 'square',
         add = TRUE, type = 'lower', diag = FALSE, col=col(200), tl.pos = 'n', cl.pos = 'n',outline = F, order = "original",
         addCoef.col = "black", number.cex = 0.8) 


# retained bio1 (annual mean temperature) and bio15 (precipitation seasonality) 
# given their low collinearity with other variables (Spearman |R| < 0.7) and 
# ecological relevance to plant performance.
cor.test(field_survey_dataset$Bio1, field_survey_dataset$Bio15)

################## Classification of fungal functional guilds ##################

# loading package
library(R.filesets)
library(FUNGuildR)

## fungal taxa information
tax_default = read.xlsx("Field_survey_OTU_tables.xlsx", sheet = "OTU_table", colNames = T, rowNames = T)
tax_default = data.frame(OTU_ID = rownames(tax_default),
                         taxonomy = tax_default$taxonomy)

# loading FUNGuild database
# fung <- get_funguild_db()
# saveRDS(fung, 'funguild.rds')
fung <- loadRDS('funguild.rds')
fungdataframe <- data.frame(fung)
# write.xlsx(fungdataframe, file = "FUNGuild_dataset.xlsx")

fung_guilds <- funguild_assign(tax_default, db = fungdataframe,tax_col = "taxonomy")
head(fung_guilds[,1:6])

OTU_tax <- tax_default %>% tidyr::separate(col = taxonomy, 
                                           into = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                           sep = ";\\s*", fill = "right") %>% as.data.frame()


############################### Plant Pathogen #################################
# Plant Pathogen
Pathogen_id1 <- subset(fung_guilds, guild == "|Plant Pathogen|" | guild == "Plant Pathogen")
unique(Pathogen_id1$confidenceRanking)
Pathogen_id1 <- subset(Pathogen_id1, confidenceRanking != "Possible")$OTU_ID; length(Pathogen_id1)

# genus - Fusarium
Pathogen_id2 <- subset(OTU_tax, Genus == "g__Fusarium")$OTU_ID

### |Plant Pathogen| & Fusarium
Pathogens_ID <- unique(c(Pathogen_id1, Pathogen_id2)); length(Pathogens_ID)

############################ Arbuscular Mycorrhizal ############################
AMF_ID <- subset(OTU_tax, Phylum == "p__Glomeromycota")$OTU_ID; length(AMF_ID)

# calculated the richness of fungi
Field_fungal_otu <- read.xlsx("Field_survey_OTU_tables.xlsx", sheet = "OTU_table", colNames = T, rowNames = T)
Field_fungal_otu$taxonomy <- NULL

# Total fungal richness
FUNGSR <- as.data.frame(specnumber(t(Field_fungal_otu)))
colnames(FUNGSR) <- "FUNGSR"
FUNGSR$Popu_code <- rownames(FUNGSR)

# Fungal pathogen richness
PATHSR <- as.data.frame(specnumber(t(Field_fungal_otu[Pathogens_ID,])))
colnames(PATHSR) <- "PATHSR"
PATHSR$Popu_code <- rownames(PATHSR)

# AMF richness
AMFSR <- as.data.frame(specnumber(t(Field_fungal_otu[AMF_ID,])))
colnames(AMFSR) <- "AMFSR"
AMFSR$Popu_code <- rownames(AMFSR)

# Merge data
Field_fungal_SR <- FUNGSR %>% left_join(PATHSR) %>% left_join(AMFSR)
# write.xlsx(Field_fungal_SR, file = "Field_fungal_SR.xlsx")


################## Spatial autocorrelation test for variables ##################
# loading package
library(openxlsx)
library(dplyr)
library(spdep)
library(sp)

# loading field survey dataset
figure_4_data <- read.xlsx("Field_survey_dataset.xlsx", sheet = "Field_survey", colNames = T)
figure_4_data$Origin <- ifelse(figure_4_data$Species == "Alternanthera_philoxeroides", "Invasive", "Native")
figure_4_data$Origin <- factor(figure_4_data$Origin, levels = c("Native", "Invasive"))
figure_4_data$Species <- as.factor(figure_4_data$Species)
colnames(figure_4_data)

set.seed(123456)
# raw data
test_var = c("Bio1", "Bio15", 
             "Soil_wc_all", "Soil_C_all", "Soil_N_all", "Soil_ph_all", 
             "ALLplSR", "HerbFR", "HerbAB", "Defol_med", "Disease_med", 
             "FUNGSR", "PATHSR", "AMFSR",
             "Con_mass", "Bsurv", "Lesion", "Knots", "Rel_cover")

colnames(figure_4_data)

# Create an empty data frame
moran_results <- data.frame(
  Variable = character(),
  Moran_I = numeric(),
  Expected_I = numeric(),
  Variance = numeric(),
  Std_Dev = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

# Alternanthera_philoxeroides  Alternanthera_sessilis
figure_4_data_sel = subset(figure_4_data, Species == "Alternanthera_philoxeroides")

for (var_name in test_var) {
  
  figure_4_data_filter <- figure_4_data_sel[complete.cases(figure_4_data_sel[, var_name]), ]
  
  coords_mat <- as.matrix(figure_4_data_filter[, c("Longitude", "Latitude")])
  variable_values <- figure_4_data_filter[[var_name]]
  
  nb <- knn2nb(knearneigh(coords_mat, k = 5))
  listw <- nb2listw(nb, style = "W")
  
  # Moran's I
  moran_test <- moran.test(variable_values, listw, randomisation = TRUE)
  
  # created a dataframe
  moran_results <- rbind(moran_results, data.frame(
    Variable = var_name,
    Moran_I = round(moran_test$estimate[1], 2),
    Expected_I = moran_test$estimate[2],
    Variance = moran_test$estimate[3],
    Std_Dev = as.numeric(moran_test$statistic),
    P_value = round(moran_test$p.value, 3),
    stringsAsFactors = FALSE
  ))
}

print(moran_results)

