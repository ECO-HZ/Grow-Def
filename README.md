The following files allow one to reproduce analyses in the manuscript entitled "Latitudinal patterns in growth and defense against multiple enemies of an invasive species and its native congener".

DATA & FILE OVERVIEW

***In Data folder***

The experimental data are also stored in Figshare [![DOI](https://zenodo.org/badge/DOI/10.6084/m9.figshare.28733072.svg)](https://doi.org/10.6084/m9.figshare.28733072.v1).
Before the manuscript is officially published, experimental and analytical data must remain confidential. 
If needed, please contact the first or corresponding author in advance to obtain the relevant experimental data. 
All data will be made available upon acceptance of the manuscript.

*List of experimental data files*

    * 1. Field_survey_dataset.xlsx  * 2. Field_survey_OTU_tables.xlsx  * 3. Plant_community_dataset.xlsx
    * 4. Arthropod_community_dataset.xlsx  * 5. Greenhouse_exp_dataset.xlsx  *6. FUNGuild_dataset.xlsx  
***In Code folder***

The names of R-scripts correspond to the statistical analysis and visualization of the corresponding figures in this manuscript.

*List of R-scripts*

    * 1. 00-Selection of bioclimatic variables and classification of fungal functional guilds.R  
    * 2. Figure 1 & Table 1 part.R  
    * 3. Figure 2 & Table 1 part.R  
    * 4. Figure 3 & Table 2.R  
    * 5. Figure 4.R   
    
**Data-specific onformation for:** ***Field_survey_dataset.xlsx***

Variable list:

    * Popu_code: Code of population of studying species in greenhouse experiment
    * Site: Sampling site (population) ID
    * Species: Latin name of study species
    * Group: Group of sampling site (population)
    * Latitude: Latitude of sampling sites
    * Longitude: Longitude of sampling sites
    * FUNGSR: Soil entire fungal richness
    * PATHSR: Soil pathogenic fungal richness
    * AMFSR: Soil AMF richness
    * Soil_wc: Soil water content
    * Soil_C: Soil carbon content
    * Soil_N: Soil nitrogen content
    * Soil_ph: Soil pH
    * HerbAB: Abundance of herbivorous insects (site level)
    * HerbFR: Richness (family) of herbivorous insects (site level)
    * ALLplSR: Plant species richness
    * Cover: Relative abundance of studying species
    * Defol: Foliar defoliation (%)
    * Disease: Foliar pathogen infection (%)
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

**Data-specific onformation for:** ***Plant_community_dataset.xlsx***

    Sheets: Plant_community - Species composition information of plant communites per site
    Variable list:
    * Site: Sampling site (population) ID
    * Survey_year: Year of survey
    * Latitude: Latitude of sampling sites
    * Longitude: Longitude of sampling sites
    * ALLplSR: Plant species richness per site
    * Columns 6 to 132: Species composition information of plant communites per site 

**Data-specific onformation for:** ***Arthropod_community_dataset.xlsx (This file contains 2 sheets)***

    Sheets: Arthropod_com - Species composition information of arthropod communites per site 
    Variable list:
    * Site: Sampling site (population) ID
    * Survey year: Year of survey
    * Latitude: Latitude of sampling sites
    * Longitude: Longitude of sampling sites
    * Herb_num: Abundance of herbivorous insects (site level)
    * HerbFR: Richness (family) of herbivorous insects (site level)
    * Columns 7 to 63: Species composition information of arthropod communites per site 

    Sheets: Arthropod_group - Classification of Arthropod functional group in field survey
    Variable list:
    * Order: Order of Arthropod functional group 
    * Group: Group of Arthropod functional group 
    * Trophic group: Classification of Arthropod functional group 

**Data-specific onformation for:** ***Greenhouse_exp_dataset.xlsx (This file contains 2 sheets)***

    Sheets: Greenhouse_exp1&3&4 - Plant growth and resistance to Alternaria foliar fungal pathogens, and root knot nematodes estimated in greenhouse experiment
    Variable list:
    * Genotype_code: Code of genotype of studying species in greenhouse experiment
    * Site: Sampling site (population) ID
    * Genotype: Genotype of study species per site
    * Species: Latin name of study species
    * Con_mass: Aboveground biomass of studying sepcies (g)
    * Lesion: Leaf fungal pathogen infection (# of lesions)
    * Knots: Root nematode infection (# of root knots)

    Sheets: Greenhouse_exp2 - Plant resistance to A. hygrophila beetles estimated in greenhouse experiment
    Variable list:
    * Popu_code: Code of population of studying species in greenhouse experiment
    * Site: Sampling site (population) ID
    * Species: Latin name of study species
    * Latitude: Latitude of sampling sites
    * Longitude: Longitude of sampling sites
    * S1: Number of survivors in the first year
    * S2: Number of survivors in the second year
    * start: Total number of beetle larvae
    * surv: Total survival of beetle larvae
    * Percent_surv: Survival rate of beetle larvae

**Data-specific onformation for:** ***FUNGuild_dataset.xlsx***
    * The FUNGuild dataset (Nguyen, N. H., Song, Z., Bates, S. T., Branco, S., Tedersoo, L., Menke, J., . . . Kennedy, P. G. (2016) FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. Fungal Ecology, 20, 241-248.)
