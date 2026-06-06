The following files allow one to reproduce analyses in the manuscript entitled "Latitudinal patterns in growth and defense against multiple enemies of an invasive species and its native congener".

DATA & FILE OVERVIEW

***In Datasets folder***

The experimental data are also stored in Figshare [![DOI](https://zenodo.org/badge/DOI/10.6084/m9.figshare.30286726.svg)](https://doi.org/10.6084/m9.figshare.30286726.v1).
Before the manuscript is officially published, experimental and analytical data must remain confidential. 
If needed, please contact the first or corresponding author in advance to obtain the relevant experimental data. 
All data will be made available upon acceptance of the manuscript.

*List of experimental data files*

    * 1. Field_survey_dataset.xlsx  
    * 2. Field_survey_OTU_tables.xlsx  
    * 3. FUNGuild_dataset.xlsx  
    
***In R code folder***

The names of R-scripts correspond to the statistical analysis and visualization of the corresponding figures in this manuscript.

*List of R-scripts*

    * 1. 00-Selection of bioclimatic variables and classification of fungal functional guilds & Table S2.R
    * 2. Figure 1 & Table 1 & S3 part.R
    * 3. Figure 2 & Table 1 & S3 part.R
    * 4. Figure 3 & Table 2 & S3 part.R
    * 5. Figure 4 & Table S4.R
    * 6. Figure S2 & Table S3 part.R 
    
**Data-specific onformation for:** ***Field_survey_dataset.xlsx***

    Variable list:

    * Popu_code: Code of population of studying species in greenhouse experiment
    * Site: Sampling site (population) ID
    * Species: Latin name of study species
    * Group: Group of sampling site (population)
    * Latitude: Latitude of sampling sites
    * Longitude: Longitude of sampling sites
    * lat_jitter: Jittered latitude
    * lon_jitter: Jittered longitude
    * FUNGSR: Soil entire fungal richness
    * PATHSR: Soil pathogenic fungal richness
    * AMFSR: Soil AMF richness
    * Soil_wc_all: Soil water content
    * Soil_C_all: Soil carbon content
    * Soil_N_all: Soil nitrogen content
    * Soil_ph_all: Soil pH
    * HerbAB: Abundance of herbivorous insects (site level)
    * HerbFR: Richness (family) of herbivorous insects (site level)
    * ALLplSR: Plant species richness
    * Rel_cover: Relative cover of focal species
    * Defol_med: Foliar defoliation (%)
    * Disease_med: Foliar pathogen infection (%)
    * Con_mass: Aboveground biomass of studying sepcies (g)
    * Lesion: Leaf fungal pathogen infection (# of lesions)
    * Knots: Root nematode infection (# of root knots)
    * Bsurv: Survival rate of beetle larvae
    * Bio1-Bio19: 19 bioclimatic variables for each sampling site from the WorldClim database    
      (http://www.worldclim.org/) at 30 arc-second resolution

**Data-specific onformation for:** ***Field_survey_OTU_tables.xlsx (This file contains 2 sheets)***

    Sheets: OTU_table - OTU table in field survey
    
    Variable list:
    
    * OTU_ID: Code of soil fungal taxa
    * Columns 2 to 127: Fungal taxa composition information of arthropod communites per site 
    * taxonomy: Fungal taxonomic information
    
    Sheets: Taxonomy_information - Taxonomy information of fungi in field survey
    
    Variable list:
    
    * OTU_ID: Code of soil fungal taxa
    * FUNGuild: FUNGuild information
    * Taxonomy: Fungal taxonomic information

**Data-specific onformation for:** ***FUNGuild_dataset.xlsx***

    * The FUNGuild dataset (Nguyen, N. H., Song, Z., Bates, S. T., Branco, S., Tedersoo, L., Menke, J., . . . Kennedy, P. G. (2016) FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. Fungal Ecology,   
      20, 241-248.)
