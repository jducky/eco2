#####=========================================================
##### installing and Loading packages ========================
# Setting packages and library
packages <- c("shiny", "shinyWidgets", "shinyFiles", "shinyalert", "shinyjs", "shinythemes", "tiff", "sf", "SDMTools", "proj4", "gdata", "colorspace", "plyr", "leaflet", "RColorBrewer", "scales", "lattice", "dplyr", "maps", "maptools", "sp", "biomod2", "raster", "rgdal", "ggplot2", "hrbrthemes", "plotly", "grid", "reshape", "rgeos", "stringr", "rgdal", "bnspatial", "MigClim", "mgcv", "gsubfn", "DT", "fmsb", "data.table", "foreign", "scales", "leaflet.minicharts", "manipulateWidget", "shinydashboard", "shinyBS")
libraries <- c("shiny", "shinyWidgets", "shinyFiles", "shinyalert", "shinyjs", "shinythemes", "tiff", "sf", "SDMTools", "proj4", "gdata", "colorspace", "plyr", "leaflet", "RColorBrewer", "scales", "lattice", "dplyr", "maps", "maptools", "sp", "biomod2", "raster", "rgdal", "ggplot2", "hrbrthemes", "plotly", "grid", "reshape", "rgeos", "stringr", "rgdal", "bnspatial", "MigClim", "mgcv", "gsubfn", "DT", "fmsb", "data.table", "foreign", "scales", "leaflet.minicharts", "manipulateWidget", "shinydashboard", "shinyBS")

# installing packages 
CHK_packages <- setdiff(packages, rownames(installed.packages()))
if (length(CHK_packages) > 0) {
	for (i in 1:length(CHK_packages)) {
	  
		if (!require(CHK_packages[i],character.only = TRUE)) {
#			install.packages(CHK_packages[i], repos = "http://cran.us.r-project.org/")
		  install.packages(CHK_packages[i], repos = "https://cloud.r-project.org/")
			if (!require(CHK_packages[i],character.only = TRUE)) {
				stop("Package not found")
			}
		}
	}
}

# loading packages
CHK_libraries <- setdiff(libraries, rownames(library()))
if (length(CHK_libraries) > 0) {
	for (i in 1:length(CHK_libraries)) {
		if (!require(CHK_libraries[i],character.only = TRUE)) {
			library(CHK_libraries[i])
			if (!require(CHK_libraries[i],character.only = TRUE)) {
				stop("Library not found")
			}
		}
	}
}
##### End installing and loading packages ====================
#####=========================================================

rm(list = ls())
MOTIVE_DIR <- "C:/MOTIVE_Ecosystem/"
 
##### Path
G <- reactiveValues()
G$SE_Dir_Project <- "set a working project"
G$SE_Dir_Climate <- paste(MOTIVE_DIR, "DATA/Climate", sep='')
G$SE_Dir_Link <- paste(MOTIVE_DIR, "DATA/Link", sep='')
G$SE_Dir_Species <- paste(MOTIVE_DIR, "DATA/Species", sep='')
G$SE_Dir_GIS <- paste(MOTIVE_DIR, "DATA/GIS", sep='')
G$SE_speciesindex <- "speciesname_final.csv"
G$SE_specieslocation <- "shin_specieslocation.csv"
G_FILE_speciesindex <- read.csv(file.path(isolate(G$SE_Dir_Species), isolate(G$SE_speciesindex)), header = T, sep = ",")
G_FILE_specieslocation <- read.csv(file.path(isolate(G$SE_Dir_Species), isolate(G$SE_specieslocation)), header = T, sep = ",")
G_FILE_speciesfreq <- count(G_FILE_specieslocation, ID)
G_FILE_speciesinfo <- inner_join(G_FILE_speciesfreq, G_FILE_speciesindex, by = "ID")

G$SDM_MO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Species_Distribution", sep = "")
G$SDM_AO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Species_Distribution", sep = "")
G$IS_MO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")
G$IS_VA_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")
G$IS_MI_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")
G$IS_AO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")

Input_img <- "tif"  #asc",
Output_img <- "tif"

##### Language
SE <- reactiveValues(Language = "English")

CD_Scenarios_list <- c("RCP 4.5" = "RCP4.5",
                       "RCP 8.5" = "RCP8.5")
CD_Year_list <- c("2000" = "2000",
                  "2010" = "2010",
                  "2020" = "2020",
                  "2030" = "2030",
                  "2040" = "2040",
                  "2050" = "2050",
                  "2060" = "2060",
                  "2070" = "2070",
                  "2080" = "2080")
SDM_models_list <- c("GLM (Generalized Linear Model)" = "GLM",
                     "GAM (Generalized Additive Model)" = "GAM",
                     "GBM (Generalized Boosting Model)" = "GBM",
                     "CTA (Classification Tree Analysis)" = "CTA",
                     "ANN (Artificial Neural Network)" = "ANN",
                     "SRE (Surface Range Envelop)" = "SRE",
                     "FDA (Flexible Discriminant Analysis)" = "FDA",
                     "MARS (Multiple Adaptive Regression Splines)" = "MARS",
                     "RF (Random Forest)" = "RF",
                     "MAXENT.Phillips" = "MAXENT.Phillips")
#                    "MAXENT (low-memory multinomial logistic regression)" = "MAXENT")
SDM_models_out_list <- c("GLM (Generalized Linear Model)" = "GLM",
                         "GAM (Generalized Additive Model)" = "GAM",
                         "GBM (Generalized Boosting Model)" = "GBM",
                         "CTA (Classification Tree Analysis)" = "CTA",
                         "ANN (Artificial Neural Network)" = "ANN",
                         "SRE (Surface Range Envelop)" = "SRE",
                         "FDA (Flexible Discriminant Analysis)" = "FDA",
                         "MARS (Multiple Adaptive Regression Splines)" = "MARS",
                         "RF (Random Forest)" = "RF",
                         "MAXENT.Phillips" = "MAXENT.Phillips",
                         #                  	     "MAXENT (low-memory multinomial logistic regression)" = "MAXENT",
                         "Ensemble" = "EM")
#DM_Models_list <- c("No Dispersal" = "ND",
#                    "SDD (Short Dispersal Distance)" = "SDD",
#                    "MDD (Middle Dispersal Distance)" = "MDD",
#                    "LDD (Long Dispersal Distance)" = "LDD",
#                    "Unlimited Dispersal" = "UD")
DM_Models_list <- c("Species Distribution Model (SDM)" = "BIOMOD2",
                    "Dispersal Model (DM)" = "MIGCLIM")

observe({
  if (SE$Language == "English") {
    SE$Name_System <- "MOTIVE ECOSYSTEM (Climate Change Impact and Vulnerability Assessment Model for Ecosystem)"
  } else {
    SE$Name_System <- "MOTIVE ECOSYSTEM (생태계 기후변화 영향 및 취약성 평가모형)"
  }
})
    
    
# SE_Language <- "English"
SE_Language <- "Korean" 
    
  if (SE_Language == "English") {
    LD_Variables_list  <- c("Landuse_ssp1" = "landuse_ssp1",
                            "Landuse_ssp2" = "landuse_ssp2",
                            "Landuse_ssp3" = "landuse_ssp3",
                            "Forest fire" = "forestfire",
                            "Landslide" = "landslide")  
    CD_Variables_list <- c("BIOCLIM 01 (Annual Mean Temperature)" = "bio01",
                           "BIOCLIM 02 (Mean Diurnal Range (Mean of monthly (max temp - min temp)))" = "bio02",
                           "BIOCLIM 03 (Isothermality (BIO2/BIO7) (* 100))" = "bio03",
                           "BIOCLIM 04 (Temperature Seasonality (standard deviation *100))" = "bio04",
                           "BIOCLIM 05 (Max Temperature of Warmest Month)" = "bio05",
                           "BIOCLIM 06 (Min Temperature of Coldest Month)" = "bio06",
                           "BIOCLIM 07 (Temperature Annual Range (BIO5-BIO6))" = "bio07",
                           "BIOCLIM 08 (Mean Temperature of Wettest Quarter)" = "bio08",
                           "BIOCLIM 09 (Mean Temperature of Driest Quarter)" = "bio09",
                           "BIOCLIM 10 (Mean Temperature of Warmest Quarter)" = "bio10",
                           "BIOCLIM 11 (Mean Temperature of Coldest Quarter)" = "bio11",
                           "BIOCLIM 12 (Annual Precipitation)" = "bio12",
                           "BIOCLIM 13 (Precipitation of Wettest Month)" = "bio13",
                           "BIOCLIM 14 (Precipitation of Driest Month)" = "bio14",
                           "BIOCLIM 15 (Precipitation Seasonality (Coefficient of Variation))" = "bio15",
                           "BIOCLIM 16 (Precipitation of Wettest Quarter)" = "bio16",
                           "BIOCLIM 17 (Precipitation of Driest Quarter)" = "bio17",
                           "BIOCLIM 18 (Precipitation of Warmest Quarter)" = "bio18",
                           "BIOCLIM 19 (Precipitation of Coldest Quarter)" = "bio19") 
    SDM_Variables_list <- c("BIOCLIM 01 (Annual Mean Temperature)" = paste("bio01.", Input_img, sep = ""),
                            "BIOCLIM 02 (Mean Diurnal Range (Mean of monthly (max temp - min temp)))" = paste("bio02.", Input_img, sep = ""),
                            "BIOCLIM 03 (Isothermality (BIO2/BIO7) (* 100))" = paste("bio03.", Input_img, sep = ""),
                            "BIOCLIM 04 (Temperature Seasonality (standard deviation *100))" = paste("bio04.", Input_img, sep = ""),
                            "BIOCLIM 05 (Max Temperature of Warmest Month)" = paste("bio05.", Input_img, sep = ""),
                            "BIOCLIM 06 (Min Temperature of Coldest Month)" = paste("bio06.", Input_img, sep = ""),
                            "BIOCLIM 07 (Temperature Annual Range (BIO5-BIO6))" = paste("bio07.", Input_img, sep = ""),
                            "BIOCLIM 08 (Mean Temperature of Wettest Quarter)" = paste("bio08.", Input_img, sep = ""),
                            "BIOCLIM 09 (Mean Temperature of Driest Quarter)" = paste("bio09.", Input_img, sep = ""),
                            "BIOCLIM 10 (Mean Temperature of Warmest Quarter)" = paste("bio10.", Input_img, sep = ""),
                            "BIOCLIM 11 (Mean Temperature of Coldest Quarter)" = paste("bio11.", Input_img, sep = ""),
                            "BIOCLIM 12 (Annual Precipitation)" = paste("bio12.", Input_img, sep = ""),
                            "BIOCLIM 13 (Precipitation of Wettest Month)" = paste("bio13.", Input_img, sep = ""),
                            "BIOCLIM 14 (Precipitation of Driest Month)" = paste("bio14.", Input_img, sep = ""),
                            "BIOCLIM 15 (Precipitation Seasonality (Coefficient of Variation))" = paste("bio15.", Input_img, sep = ""),
                            "BIOCLIM 16 (Precipitation of Wettest Quarter)" = paste("bio16.", Input_img, sep = ""),
                            "BIOCLIM 17 (Precipitation of Driest Quarter)" = paste("bio17.", Input_img, sep = ""),
                            "BIOCLIM 18 (Precipitation of Warmest Quarter)" = paste("bio18.", Input_img, sep = ""),
                            "BIOCLIM 19 (Precipitation of Coldest Quarter)" = paste("bio19.", Input_img, sep = ""))
    CD_Models_list <- c("KMA (Korea Meteorological Administration)" = "KMA",
                        "KEI (Korea Environment Institute)" = "KEI")  
    SE_Name_System <- "MOTIVE ECOSYSTEM (Climate Change Impact and Vulnerability Assessment Model)" 
    SE_Name <- "Setting"
    SE_Name_Language <- "Language"
    SE_Name_WE <- "Working Environment"
    SE_Name_WE_Project <- "Working Project"
    SE_Name_DE <- "Data Environment"
    SE_Name_DE_Climate <- "Climate Data Path"
    SE_Name_DE_Link <- "Link Data Path"
    SE_Name_DE_Species <- "Species Data Path"
    SE_Name_DE_Species_Index <- "Select species index data (CSV file)"
    SE_Name_DE_Species_Location <- "Select species location data (CSV file)"
    SP_Name <- "Species Data"
    SP_Name_Info <- "Species Information"
    SP_Name_Location <- "Species Location"
    LD_Name <- "Link Data"
    LD_Name_Map <- "Map"
    LD_Name_Summary <- "Summary"
    LD_Name_Variables <- "Link Data"
    LD_Name_Variables_list <- LD_Variables_list
    LD_Name_Variables_selected <- "forestfire"
    LD_Name_Models <- "Climate Models"
    LD_Name_Models_list <- CD_Models_list
    LD_Name_Models_selected <- "KMA"    
    LD_Name_Scenarios <- "Climate Scenarios"
    LD_Name_Scenarios_list <- CD_Scenarios_list
    LD_Name_Scenarios_selected <- "RCP4.5"    
    LD_Name_Year <- "Projecting Years"
    LD_Name_Year_list <- CD_Year_list
    LD_Name_Year_selected <- "2000"       
    CD_Name <- "Climate Data"
    CD_Name_Map <- "Map"
    CD_Name_Summary <- "Summary"
    CD_Name_Variables <- "Climate Variables"
    CD_Name_Variables_list <- CD_Variables_list
    CD_Name_Variables_selected <- "bio01"
    CD_Name_Models <- "Climate Models"
    CD_Name_Models_list <- CD_Models_list
    CD_Name_Models_selected <- "KMA"    
    CD_Name_Scenarios <- "Climate Scenarios"
    CD_Name_Scenarios_list <- CD_Scenarios_list
    CD_Name_Scenarios_selected <- "RCP4.5"    
    CD_Name_Year <- "Projecting Years"
    CD_Name_Year_list <- CD_Year_list
    CD_Name_Year_selected <- "2000"     
    SDM_Name <- "Species Distribution Model"
    SDM_Name_Model <- "Modeling"
    SDM_Name_Model_Species <- "Species selection"
    SDM_Name_Model_Projection <- "Projection selection"
    SDM_Name_Model_Variable <- "Variable selection"
    SDM_Name_Model_SDM <- "SDM selection"
    SDM_Name_Model_Out <- "Modeling Outputs"
    SDM_Name_Model_Out_Validation <- "Validation"
    SDM_Name_Model_Out_Contribution <- "Contribution"
    SDM_Name_Model_Out_Probability <- "Probability Map"
    SDM_Name_Model_Out_Prediction <- "Predicted Map"
    SDM_Name_CD_Variables <- "Climate Variables"
    SDM_Name_CD_Variables_list <- SDM_Variables_list
    SDM_Name_CD_Variables_selected <- c(paste("bio01.", Input_img, sep = ""), paste("bio02.", Input_img, sep = ""), paste("bio03.", Input_img, sep = ""), paste("bio12.", Input_img, sep = ""), paste("bio13.", Input_img, sep = ""), paste("bio14.", Input_img, sep = ""))
    SDM_Name_CD_Models <- "Climate Models"
    SDM_Name_CD_Models_list <- CD_Models_list
    SDM_Name_CD_Models_selected <- "KMA"    
    SDM_Name_CD_Scenarios <- "Climate Scenarios"
    SDM_Name_CD_Scenarios_list <- CD_Scenarios_list
    SDM_Name_CD_Scenarios_selected <- "RCP4.5"    
    SDM_Name_CD_Year <- "Projecting Years"
    SDM_Name_CD_Year_list <- CD_Year_list
    SDM_Name_CD_Year_selected <- "2000"
    SDM_Name_models <- "Model types"
    SDM_Name_models_list <- SDM_models_list
    SDM_Name_models_selected <- "GLM"
    SDM_Name_EMmodels <- "Ensemble"
    SDM_Name_models_run <- "Run"
    SDM_Name_models_out <- "Model types"
    SDM_Name_models_out_list <- SDM_models_out_list
    SDM_Name_models_out_selected <- "GLM"
    SDM_Name_CD_Models_out <- "Climate Models"
    SDM_Name_CD_Models_out_list <- CD_Models_list
    SDM_Name_CD_Models_out_selected <- "KMA"    
    SDM_Name_CD_Scenarios_out <- "Climate Scenarios"
    SDM_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    SDM_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    SDM_Name_CD_Year_out <- "Projecting Years"
    SDM_Name_CD_Year_out_list <- CD_Year_list
    SDM_Name_CD_Year_out_selected <- CD_Year_list
    SDM_Name_Dir <- "SDM Output Folder"
    DM_Name <- "Dispesal Model"
    DM_Name_Model <- "Modeling"
    DM_Name_Model_SDM <- "SDM Option"
    DM_Name_Model_DM <- "Dispersal Model Option"
    DM_Name_Model_Out <- "Model Outputs"
    DM_Name_Out_Plot <- "Species Distribution Change Plot"
    DM_Name_DM_MO_Barriers <- "Barriers"
    DM_Name_DM_MO_Barriers_list <- LD_Variables_list
    DM_Name_DM_MO_Barriers_selected <- "landuse" 
    DM_Name_DM_MO_Slider <- "Select a dispersal distance"
    DM_Name_DM_MO_Action <- "Run"
    DM_Name_DM_Models <- "DIspersal Models"
    DM_Name_DM_Models_list <- DM_Models_list
    DM_Name_DM_Models_selected <- "MIGCLIM"
    DM_Name_CD_Models <- "Climate Models"
    DM_Name_CD_Models_list <- CD_Models_list
    DM_Name_CD_Models_selected <- "KMA"    
    DM_Name_CD_Scenarios <- "Climate Scenarios"
    DM_Name_CD_Scenarios_list <- CD_Scenarios_list
    DM_Name_CD_Scenarios_selected <- "RCP4.5"    
    DM_Name_CD_Year <- "Projecting Years"
    DM_Name_CD_Year_list <- CD_Year_list
    DM_Name_CD_Year_selected <- "2000"
    DM_Name_models <- "Model types"
    DM_Name_models_list <- SDM_models_list
    DM_Name_models_selected <- "GLM"
    DM_Name_EMmodels <- "Ensemble"
    DM_Name_models_run <- "Run"
    DM_Name_models_out <- "Model types"
    DM_Name_models_out_list <- SDM_models_out_list
    DM_Name_models_out_selected <- "GLM"
    DM_Name_CD_Models_out <- "Climate Models"
    DM_Name_CD_Models_out_list <- CD_Models_list
    DM_Name_CD_Models_out_selected <- "KMA"    
    DM_Name_CD_Scenarios_out <- "Climate Scenarios"
    DM_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    DM_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    DM_Name_CD_Year_out <- "Projecting Years"
    DM_Name_CD_Year_out_list <- CD_Year_list
    DM_Name_CD_Year_out_selected <- "2000"
    DM_Name_Display_types <- "Model Outputs for Display"
    DM_Name_Display_types_list <- c("SDM" = "SDM",
                               "Dispersal Model" = "DM")
    DM_Name_Display_types_selected <- "SDM"
    DM_Name_Dir <- "SDM Output Folder"
    SS_Name <- "Climate Sensitive Species"
    SS_Name_Analysis <- "Change Analysis"
    SS_Name_Out <- "Analysis Outputs"
    SS_Name_Out_ChangePlot <- "Species Distribution Change Plot"
    SS_Name_Out_Pattern <- "Vulnerability Pattern"
    SS_Name_Out_Vulnerabiity <- "Vulnerable Priority"
    SS_Name_DM_Models <- "DIspersal Types"
    SS_Name_DM_Models_list <- DM_Models_list
    SS_Name_DM_Models_selected <- "BIOMOD2"    
    SS_Name_CD_Models <- "Climate Models"
    SS_Name_CD_Models_list <- CD_Models_list
    SS_Name_CD_Models_selected <- "KMA"    
    SS_Name_CD_Scenarios <- "Climate Scenarios"
    SS_Name_CD_Scenarios_list <- CD_Scenarios_list
    SS_Name_CD_Scenarios_selected <- "RCP4.5"    
    SS_Name_CD_Year <- "Projecting Years"
    SS_Name_CD_Year_list <- CD_Year_list
    SS_Name_CD_Year_selected <- "2000"
    SS_Name_models <- "Model types"
    SS_Name_models_list <- SDM_models_list
    SS_Name_models_selected <- "GLM"
    SS_Name_EMmodels <- "Ensemble"
    SS_Name_models_run <- "Run"
    SS_Name_models_out <- "Model types"
    SS_Name_models_out_list <- SDM_models_out_list
    SS_Name_models_out_selected <- "GLM"
    SS_Name_CD_Models_out <- "Climate Models"
    SS_Name_CD_Models_out_list <- CD_Models_list
    SS_Name_CD_Models_out_selected <- "KMA"    
    SS_Name_CD_Scenarios_out <- "Climate Scenarios"
    SS_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    SS_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    SS_Name_CD_Year_out <- "Projecting Years"
    SS_Name_CD_Year_out_list <- CD_Year_list
    SS_Name_CD_Year_out_selected <- "2000"
    SS_Name_Group1_list <- c("by Species" = "Species",
                             "by Climate Model" = "Climate_Model",
                             "by Climate Scenario" = "Climate_Scenario",
                             "by Model" = "Model",
                             "by Year" = "Year")
    SS_Name_Group1_selected <- "Year"
    SS_Name_Group2_list <- c("by Species" = "Species",
                             "by Climate Model" = "Climate_Model",
                             "by Climate Scenario" = "Climate_Scenario",
                             "by Model" = "Model")
    SS_Name_Group2_selected <- "Model"
    SS_Name_Group3_list <- c("by Species" = "Species",
                             "by Climate Model" = "Climate_Model",
                             "by Climate Scenario" = "Climate_Scenario",
                             "by Model" = "Model")
    SS_Name_Group3_selected <- "Model"
    SS_Name_MO_Dir <- "Sensitive Speices Assessment Input Folder"
    SS_Name_AO_Dir <- "Sensitive Speices Assessment Output Folder"
    IS_Name <- "Invasive Species"
    IS_Name_Anlayis <- "Change Analysis"
    IS_Name_Out <- "Model Outputs"
    IS_Name_Out_Species <- "Invasive Species Distribution"
    IS_Name_Out_SR <- "Invasive species Richness"
    IS_Name_Out_SI <- "Invasive species Introduction"
    IS_Name_Out_Map <- "Map"
    IS_Name_Out_SIDO <- "SIDO"
    IS_Name_Out_SGG <- "SIGUNGU"
    IS_Name_Out_Stat <- "Statistics"
    IS_Name_DM_Models <- "DIspersal Types"
    IS_Name_DM_Models_list <- DM_Models_list
    IS_Name_DM_Models_selected <- "BIOMOD2"    
    IS_Name_CD_Models <- "Climate Models"
    IS_Name_CD_Models_list <- CD_Models_list
    IS_Name_CD_Models_selected <- "KMA"    
    IS_Name_CD_Scenarios <- "Climate Scenarios"
    IS_Name_CD_Scenarios_list <- CD_Scenarios_list
    IS_Name_CD_Scenarios_selected <- "RCP4.5"    
    IS_Name_CD_Year <- "Projecting Years"
    IS_Name_CD_Year_list <- CD_Year_list
    IS_Name_CD_Year_selected <- "2000"
    IS_Name_models <- "Model types"
    IS_Name_models_list <- SDM_models_list
    IS_Name_models_selected <- "GLM"
    IS_Name_EMmodels <- "Ensemble"
    IS_Name_models_run <- "Run"
    IS_Name_models_out <- "Model types"
    IS_Name_models_out_list <- SDM_models_out_list
    IS_Name_models_out_selected <- "GLM"
    IS_Name_Dir <- "Invasive Assessment Output Folder"
    IS_Name_Action <- "Assessing Impact and Vulnerabilty"
    IS_Name_Action_Admin <- "Grouping by Administration"
    IS_Name_Admin <- "Select a Administration Type"
    IS_Name_Admin_list <- c("SIDO" = "SD",
                            "SIGUNGU" = "SGG")
    IS_Name_Admin_selected <- "SD" 
    IS_Name_CD_Models_out <- "Climate Models"
    IS_Name_CD_Models_out_list <- CD_Models_list
    IS_Name_CD_Models_out_selected <- "KMA"    
    IS_Name_CD_Scenarios_out <- "Climate Scenarios"
    IS_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    IS_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    IS_Name_CD_Year_out <- "Projecting Years"
    IS_Name_CD_Year_out_list <- CD_Year_list
    IS_Name_CD_Year_out_selected <- "2000"
    IS_Name_Group1_list <- c("by Species" = "Species",
                             "by Climate Model" = "Climate_Model",
                             "by Climate Scenario" = "Climate_Scenario",
                             "by Model" = "Model",
                             "by Year" = "Year")
    IS_Name_Group1_selected <- "Year"
    IS_Name_Group2_list <- c("by Species" = "Species",
                             "by Climate Model" = "Climate_Model",
                             "by Climate Scenario" = "Climate_Scenario",
                             "by Model" = "Model")
    IS_Name_Group2_selected <- "Model"
    IS_Name_OU_Option1 <- "Assessment Type"
    IS_Name_OU_Option1_list <- c("Species Richness" = "IS_SR",
                                 "Species Loss" = "IS_LOSS",
                                 "Species Stay" = "IS_STAY",
                                 "Species Gain" = "IS_GAIN")
    IS_Name_OU_Option1_selected <- "IS_SR"
    IS_Name_OU_Option2 <- "Vulnerability Type"
    IS_Name_OU_Option2_list <- c("Vulnerability1 (Species Loss)" = "IS_VI1",
                                 "Vulnerability2 (Species Loss Ratio)" = "IS_VI2",
                                 "Vulnerability3 (Species Inside Loss Outside Gain)" = "IS_VI3")
    IS_Name_OU_Option2_selected <- "IS_VI1"
    IS_Name_MO_Dir <- "Invasive Species Asessment Input Folder"
    IS_Name_AO_Dir <- "Invasive Species Asessment Output Folder"
    VH_Name <- "Climate Vulnerable Habitat"
    VH_Name_Analysis <- "Change Analysis"
    VH_Name_Out <- "Model Outputs"
    VH_Name_Out_Dir <- "Habitat Assessment Output Folder"
    VH_Name_Out_SR <- "Species Richness"
    VH_Name_Out_SL <- "Species Loss"
    VH_Name_Out_SS <- "Species Stay"
    VH_Name_Out_SI <- "Species Introduction"
    VH_Name_Out_VI1 <- "Vulnerability 1"
    VH_Name_Out_VI2 <- "Vulnerability 2"
    VH_Name_Out_VI3 <- "Vulnerability 3"
    VH_Name_Out_Map <- "Map"
    VH_Name_Out_SIDO <- "SIDO"
    VH_Name_Out_SGG <- "SIGUNGU"
    VH_Name_Out_NP <- "National Park"
    VH_Name_Out_BR <- "Baekdu Range"
    VH_Name_Out_DMZ <- "DMZ"
    VH_Name_Out_Stat <- "Statistics"
    VH_Name_DM_Models <- "DIspersal Types"
    VH_Name_DM_Models_list <- DM_Models_list
    VH_Name_DM_Models_selected <- "BIOMOD2"    
    VH_Name_CD_Models <- "Climate Models"
    VH_Name_CD_Models_list <- CD_Models_list
    VH_Name_CD_Models_selected <- "KMA"    
    VH_Name_CD_Scenarios <- "Climate Scenarios"
    VH_Name_CD_Scenarios_list <- CD_Scenarios_list
    VH_Name_CD_Scenarios_selected <- "RCP4.5"    
    VH_Name_CD_Year <- "Projecting Years"
    VH_Name_CD_Year_list <- CD_Year_list
    VH_Name_CD_Year_selected <- "2000"
    VH_Name_models <- "Model types"
    VH_Name_models_list <- SDM_models_list
    VH_Name_models_selected <- "GLM"
    VH_Name_EMmodels <- "Ensemble"
    VH_Name_models_run <- "Run"
    VH_Name_Dir <- "Habitat Assessment Output Folder"
    VH_Name_Action <- "Assessing Impact and Vulnerabilty"
    VH_Name_Action_Habitat <- "Grouping by Habitat"
    VH_Name_Habitat <- "Select a Habitat Type"
    VH_Name_Habitat_list <- c("SIDO" = "SD",
                            "SIGUNGU" = "SGG",
                            "National Park" = "NP",
                            "Baekdu Range" = "BR",
                            "DMZ" = "DMZ")
    VH_Name_Habitat_selected <- "SD" 
    VH_Name_models_out <- "Model types"
    VH_Name_models_out_list <- SDM_models_out_list
    VH_Name_models_out_selected <- "GLM"
    VH_Name_CD_Models_out <- "Climate Models"
    VH_Name_CD_Models_out_list <- CD_Models_list
    VH_Name_CD_Models_out_selected <- "KMA"    
    VH_Name_CD_Scenarios_out <- "Climate Scenarios"
    VH_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    VH_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    VH_Name_CD_Year_out <- "Projecting Years"
    VH_Name_CD_Year_out_list <- CD_Year_list
    VH_Name_CD_Year_out_selected <- "2000"
    VH_Name_Group1_list <- c("by Species" = "Species",
                             "by Climate Model" = "Climate_Model",
                             "by Climate Scenario" = "Climate_Scenario",
                             "by Model" = "Model",
                             "by Year" = "Year")
    VH_Name_Group1_selected <- "Year"
    VH_Name_Group2_list <- c("by Species" = "Species",
                             "by Climate Model" = "Climate_Model",
                             "by Climate Scenario" = "Climate_Scenario",
                             "by Model" = "Model")
    VH_Name_Group2_selected <- "Model"
    VH_Name_MO_Dir <- "Vulnerable Habitat Asessment Input Folder"
    VH_Name_AO_Dir <- "Vulnerable Habitat Asessment Output Folder"
    HELP_Name <- "Help"

    
    
  } else {
    LD_Variables_list  <- c("토지이용_ssp1" = "landuse_ssp1",
                            "토지이용_ssp2" = "landuse_ssp2",
                            "토지이용_ssp3" = "landuse_ssp3",
                            "산불" = "forestfire",
                            "산사태" = "landslide")
    CD_Variables_list <- c("BIOCLIM 01 (연평균 기온)" = "bio01",
                           "BIOCLIM 02 (일평균 기온변화" = "bio02",
                           "BIOCLIM 03 (등온성 (BIO2/BIO7 * 100))" = "bio03",
                           "BIOCLIM 04 (기온 계절변동성(표준편차 *100))" = "bio04",
                           "BIOCLIM 05 (더운달의 최고기온)" = "bio05",
                           "BIOCLIM 06 (추운달의 최저기온)" = "bio06",
                           "BIOCLIM 07 (기온 연변화(BIO5-BIO6))" = "bio07",
                           "BIOCLIM 08 (습한 분기의 평균기온)" = "bio08",
                           "BIOCLIM 09 (건조한 분기의 평균기온)" = "bio09",
                           "BIOCLIM 10 (더운 분기의 평균기온)" = "bio10",
                           "BIOCLIM 11 (추운 분기의 평균기온)" = "bio11",
                           "BIOCLIM 12 (연강수량)" = "bio12",
                           "BIOCLIM 13 (습한 달의 강수량)" = "bio13",
                           "BIOCLIM 14 (건조한 달의 강수량)" = "bio14",
                           "BIOCLIM 15 (강수량의 계절변동성(변동계수)" = "bio15",
                           "BIOCLIM 16 (습한 분기의 강수량)" = "bio16",
                           "BIOCLIM 17 (건조한 분기의 강수량)" = "bio17",
                           "BIOCLIM 18 (더운 분기의 강수량)" = "bio18",
                           "BIOCLIM 19 (추운 분기의 강수량)" = "bio19") 
    SDM_Variables_list <- c("BIOCLIM 01 (연평균 기온)" = paste("bio01.", Input_img, sep = ""),
                            "BIOCLIM 02 (일평균 기온변화" = paste("bio02.", Input_img, sep = ""),
                            "BIOCLIM 03 (등온성 (BIO2/BIO7 * 100))" = paste("bio03.", Input_img, sep = ""),
                            "BIOCLIM 04 (기온 계절변동성(표준편차 *100))" = paste("bio04.", Input_img, sep = ""),
                            "BIOCLIM 05 (더운달의 최고기온)" = paste("bio05.", Input_img, sep = ""),
                            "BIOCLIM 06 (추운달의 최저기온)" = paste("bio06.", Input_img, sep = ""),
                            "BIOCLIM 07 (기온 연변화(BIO5-BIO6))" = paste("bio07.", Input_img, sep = ""),
                            "BIOCLIM 08 (습한 분기의 평균기온)" = paste("bio08.", Input_img, sep = ""),
                            "BIOCLIM 09 (건조한 분기의 평균기온)" = paste("bio09.", Input_img, sep = ""),
                            "BIOCLIM 10 (더운 분기의 평균기온)" = paste("bio10.", Input_img, sep = ""),
                            "BIOCLIM 11 (추운 분기의 평균기온)" = paste("bio11.", Input_img, sep = ""),
                            "BIOCLIM 12 (연강수량)" = paste("bio12.", Input_img, sep = ""),
                            "BIOCLIM 13 (습한 달의 강수량)" = paste("bio13.", Input_img, sep = ""),
                            "BIOCLIM 14 (건조한 달의 강수량)" = paste("bio14.", Input_img, sep = ""),
                            "BIOCLIM 15 (강수량의 계절변동성(변동계수)" = paste("bio15.", Input_img, sep = ""),
                            "BIOCLIM 16 (습한 분기의 강수량)" = paste("bio16.", Input_img, sep = ""),
                            "BIOCLIM 17 (건조한 분기의 강수량)" = paste("bio17.", Input_img, sep = ""),
                            "BIOCLIM 18 (더운 분기의 강수량)" = paste("bio18.", Input_img, sep = ""),
                            "BIOCLIM 19 (추운 분기의 강수량)" = paste("bio19.", Input_img, sep = ""))
    CD_Models_list <- c("KMA (기상청)" = "KMA",
                        "KEI (한국환경정책평가연구원)" = "KEI")
    

    
    
    SE_Name_System <- "MOTIVE ECOSYSTEM (생태계 기후변화 영향 및 취약성 평가모형)"
    SE_Name <- "환경설정"
    SE_Name_Language <- "언어"
    SE_Name_WE <- "작업환경"
    SE_Name_WE_Project <- "작업디렉토리"
    SE_Name_DE <- "데이터환경"
    SE_Name_DE_Climate <- "기후자료"
    SE_Name_DE_Link <- "연계자료"
    SE_Name_DE_Species <- "생물종자료"
    SE_Name_DE_Species_Index <- "생물종정보 인덱스 자료 (CSV file)"
    SE_Name_DE_Species_Location <- "생물종위치자료 (CSV file)"
    SP_Name <- "생물종자료"
    SP_Name_Info <- "생물종정보"
    SP_Name_Location <- "생물종위치"
    LD_Name <- "연계자료"
    LD_Name_Map <- "지도"
    LD_Name_Summary <- "통계"
    LD_Name_Variables <- "연계자료"
    LD_Name_Variables_list <- LD_Variables_list
    LD_Name_Variables_selected <- "forestfire"
    LD_Name_Models <- "기후모델"
    LD_Name_Models_list <- CD_Models_list
    LD_Name_Models_selected <- "KMA"    
    LD_Name_Scenarios <- "기후시나리오"
    LD_Name_Scenarios_list <- CD_Scenarios_list
    LD_Name_Scenarios_selected <- "RCP4.5"    
    LD_Name_Year <- "예측년도"
    LD_Name_Year_list <- CD_Year_list
    LD_Name_Year_selected <- "2000"       
    CD_Name <- "기후자료"
    CD_Name_Map <- "지도"
    CD_Name_Summary <- "통계"
    CD_Name_Variables <- "기후변수"
    CD_Name_Variables_list <- CD_Variables_list
    CD_Name_Variables_selected <- "bio01"
    CD_Name_Models <- "기후모델"
    CD_Name_Models_list <- CD_Models_list
    CD_Name_Models_selected <- "KMA"    
    CD_Name_Scenarios <- "기후시나리오"
    CD_Name_Scenarios_list <- CD_Scenarios_list
    CD_Name_Scenarios_selected <- "RCP4.5"    
    CD_Name_Year <- "예측년도"
    CD_Name_Year_list <- CD_Year_list
    CD_Name_Year_selected <- "2000"     
    SDM_Name <- "종분포모형"
    SDM_Name_Model <- "모형구동"
    SDM_Name_Model_Species <- "종선택"
    SDM_Name_Model_Projection <- "예측옵션"
    SDM_Name_Model_Variable <- "변수선택"
    SDM_Name_Model_SDM <- "SDM 모형"
    SDM_Name_Model_Out <- "모형결과"
    SDM_Name_Model_Out_Validation <- "모형검증"
    SDM_Name_Model_Out_Contribution <- "변수기여도"
    SDM_Name_Model_Out_Probability <- "확률지도"
    SDM_Name_Model_Out_Prediction <- "예측지도"
    SDM_Name_CD_Variables <- "기후변수"
    SDM_Name_CD_Variables_list <- SDM_Variables_list
    SDM_Name_CD_Variables_selected <- c(paste("bio01.", Input_img, sep = ""), paste("bio02.", Input_img, sep = ""), paste("bio03.", Input_img, sep = ""), paste("bio12.", Input_img, sep = ""), paste("bio13.", Input_img, sep = ""), paste("bio14.", Input_img, sep = ""))
    SDM_Name_CD_Models <- "기후모델"
    SDM_Name_CD_Models_list <- CD_Models_list
    SDM_Name_CD_Models_selected <- "KMA"    
    SDM_Name_CD_Scenarios <- "기후시나리오"
    SDM_Name_CD_Scenarios_list <- CD_Scenarios_list
    SDM_Name_CD_Scenarios_selected <- "RCP4.5"    
    SDM_Name_CD_Year <- "예측년도"
    SDM_Name_CD_Year_list <- CD_Year_list
    SDM_Name_CD_Year_selected <- "2000"
    SDM_Name_models <- "모델유형"
    SDM_Name_models_list <- SDM_models_list
    SDM_Name_models_selected <- "GLM"
    SDM_Name_EMmodels <- "앙상블"
    SDM_Name_models_run <- "Run"
    SDM_Name_models_out <- "모델유형"
    SDM_Name_models_out_list <- SDM_models_out_list
    SDM_Name_models_out_selected <- "GLM"
    SDM_Name_CD_Models_out <- "기후모델"
    SDM_Name_CD_Models_out_list <- CD_Models_list
    SDM_Name_CD_Models_out_selected <- "KMA"    
    SDM_Name_CD_Scenarios_out <- "기후시나리오"
    SDM_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    SDM_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    SDM_Name_CD_Year_out <- "예측년도"
    SDM_Name_CD_Year_out_list <- CD_Year_list
    SDM_Name_CD_Year_out_selected <- "2000"
    SDM_Name_Dir <- "SDM 평가결과 폴더"
    DM_Name <- "종확산모형"
    DM_Name_Model <- "모형구동"
    DM_Name_Model_SDM <- "SDM모형 옵션"
    DM_Name_Model_DM <- "확산모형 옵션"
    DM_Name_Model_Out <- "모형결과"
    DM_Name_Out_Plot <- "생물종 분포변화"
    DM_Name_DM_MO_Barriers <- "장애물"
    DM_Name_DM_MO_Barriers_list <- LD_Variables_list
    DM_Name_DM_MO_Barriers_selected <- "landuse" 
    DM_Name_DM_MO_Slider <- "확산거리를 입력하세요"
    DM_Name_DM_MO_Action <- "실행"
    DM_Name_DM_Models <- "확산모델"
    DM_Name_DM_Models_list <- DM_Models_list
    DM_Name_DM_Models_selected <- "MIGCLIM" 
    DM_Name_CD_Models <- "기후모델"
    DM_Name_CD_Models_list <- CD_Models_list
    DM_Name_CD_Models_selected <- "KMA"    
    DM_Name_CD_Scenarios <- "기후시나리오"
    DM_Name_CD_Scenarios_list <- CD_Scenarios_list
    DM_Name_CD_Scenarios_selected <- "RCP4.5"    
    DM_Name_CD_Year <- "예측년도"
    DM_Name_CD_Year_list <- CD_Year_list
    DM_Name_CD_Year_selected <- CD_Year_list
    DM_Name_models <- "모델유형"
    DM_Name_models_list <- SDM_models_list
    DM_Name_models_selected <- "GLM"
    DM_Name_EMmodels <- "앙상블"
    DM_Name_models_run <- "실행"
    DM_Name_models_out <- "모델유형"
    DM_Name_models_out_list <- SDM_models_out_list
    DM_Name_models_out_selected <- "GLM"
    DM_Name_CD_Models_out <- "기후모델"
    DM_Name_CD_Models_out_list <- CD_Models_list
    DM_Name_CD_Models_out_selected <- "KMA"    
    DM_Name_CD_Scenarios_out <- "기후시나리오"
    DM_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    DM_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    DM_Name_CD_Year_out <- "예측년도"
    DM_Name_CD_Year_out_list <- CD_Year_list
    DM_Name_CD_Year_out_selected <- "2000"
    DM_Name_Display_types <- "출력결과 모델"
    DM_Name_Display_types_list <- c("종분포모형" = "SDM",
                               "종확산모형" = "DM")
    DM_Name_Display_types_selected <- "SDM"
    DM_Name_Dir <- "SDM 평가결과 폴더"
    SS_Name <- "기후변화민감종"
    SS_Name_Analysis <- "영향 및 취약성평가"
    SS_Name_Out <- "평가결과"
    SS_Name_Out_ChangePlot <- "분포변화 결과"
    SS_Name_Out_Pattern <- "취약성 패턴"
    SS_Name_Out_Vulnerabiity <- "취약성 순위"
    SS_Name_DM_Models <- "확산유형"
    SS_Name_DM_Models_list <- DM_Models_list
    SS_Name_DM_Models_selected <- "BIOMOD2"    
    SS_Name_CD_Models <- "기후모델"
    SS_Name_CD_Models_list <- CD_Models_list
    SS_Name_CD_Models_selected <- "KMA"    
    SS_Name_CD_Scenarios <- "기후시나리오"
    SS_Name_CD_Scenarios_list <- CD_Scenarios_list
    SS_Name_CD_Scenarios_selected <- "RCP4.5"    
    SS_Name_CD_Year <- "예측년도"
    SS_Name_CD_Year_list <- CD_Year_list
    SS_Name_CD_Year_selected <- "2000"
    SS_Name_models <- "모델유형"
    SS_Name_models_list <- SDM_models_list
    SS_Name_models_selected <- "GLM"
    SS_Name_EMmodels <- "앙상블"
    SS_Name_models_run <- "실행"
    SS_Name_models_out <- "모델유형"
    SS_Name_models_out_list <- SDM_models_out_list
    SS_Name_models_out_selected <- "GLM"
    SS_Name_CD_Models_out <- "기후모델"
    SS_Name_CD_Models_out_list <- CD_Models_list
    SS_Name_CD_Models_out_selected <- "KMA"    
    SS_Name_CD_Scenarios_out <- "기후시나리오"
    SS_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    SS_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    SS_Name_CD_Year_out <- "예측년도"
    SS_Name_CD_Year_out_list <- CD_Year_list
    SS_Name_CD_Year_out_selected <- "2000"
    SS_Name_Group1_list <- c("생물종별" = "Species",
                             "기후모델별" = "Climate_Model",
                             "기후시나리오별" = "Climate_Scenario",
                             "모델유형별" = "Model",
                             "예측년도별" = "Year")
    SS_Name_Group1_selected <- "Year"
    SS_Name_Group2_list <- c("생물종별" = "Species",
                             "기후모델별" = "Climate_Model",
                             "기후시나리오별" = "Climate_Scenario",
                             "모델유형별" = "Model")
    SS_Name_Group2_selected <- "Model"
    SS_Name_Group3_list <- c("생물종별" = "Species",
                             "기후모델별" = "Climate_Model",
                             "기후시나리오별" = "Climate_Scenario",
                             "모델유형별" = "Model")
    SS_Name_Group3_selected <- "Species"
    SS_Name_MO_Dir <- "민감종평가 입력폴더"
    SS_Name_AO_Dir <- "민감종평가 결과폴더"
    IS_Name <- "외래종"
    IS_Name_Anlayis <- "영향 평가"
    IS_Name_Out <- "평가결과"
    IS_Name_Out_Species <- "외래종분포"
    IS_Name_Out_SR <- "외래종 풍부도"
    IS_Name_Out_SI <- "외래종 유입"
    IS_Name_Out_Map <- "지도"
    IS_Name_Out_SIDO <- "시도"
    IS_Name_Out_SGG <- "시군구"
    IS_Name_Out_Stat <- "통계"
    IS_Name_DM_Models <- "확산유형"
    IS_Name_DM_Models_list <- DM_Models_list
    IS_Name_DM_Models_selected <- "BIOMOD2"    
    IS_Name_CD_Models <- "기후모델"
    IS_Name_CD_Models_list <- CD_Models_list
    IS_Name_CD_Models_selected <- "KMA"    
    IS_Name_CD_Scenarios <- "기후시나리오"
    IS_Name_CD_Scenarios_list <- CD_Scenarios_list
    IS_Name_CD_Scenarios_selected <- "RCP4.5"    
    IS_Name_CD_Year <- "예측년도"
    IS_Name_CD_Year_list <- CD_Year_list
    IS_Name_CD_Year_selected <- "2000"
    IS_Name_models <- "모델유형"
    IS_Name_models_list <- SDM_models_list
    IS_Name_models_selected <- "GLM"
    IS_Name_EMmodels <- "앙상블"
    IS_Name_models_run <- "실행"
    IS_Name_models_out <- "모델유형"
    IS_Name_models_out_list <- SDM_models_out_list
    IS_Name_models_out_selected <- "GLM"
    IS_Name_Dir <- "외래종 평가결과 폴더"
    IS_Name_Action <- "영향 및 취약성 평가 실행"
    IS_Name_Action_Admin <- "행정구역별로 결과분석"
    IS_Name_Admin <- "행정구역유형을 선택하세요"
    IS_Name_Admin_list <- c("시도" = "SD",
                            "시군구" = "SGG")
    IS_Name_Admin_selected <- "SD" 
    IS_Name_CD_Models_out <- "기후모델"
    IS_Name_CD_Models_out_list <- CD_Models_list
    IS_Name_CD_Models_out_selected <- "KMA"    
    IS_Name_CD_Scenarios_out <- "기후시나리오"
    IS_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    IS_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    IS_Name_CD_Year_out <- "예측년도"
    IS_Name_CD_Year_out_list <- CD_Year_list
    IS_Name_CD_Year_out_selected <- "2000"
    IS_Name_MO_Dir <- "외래종평가 입력폴더"
    IS_Name_AO_Dir <- "외래종평가 결과폴더"
    VH_Name <- "취약서식지"
    VH_Name_Analysis <- "영향 및 취약성평가"
    VH_Name_Out <- "평가결과"
    VH_Name_Out_Dir <- "평가결과 디렉토리"
    VH_Name_Out_SR <- "생물종 풍부도"
    VH_Name_Out_SL <- "생물종 소실"
    VH_Name_Out_SS <- "생물종 유지"
    VH_Name_Out_SI <- "생물종 유입"
    VH_Name_Out_VI1 <- "취약성평가 1"
    VH_Name_Out_VI2 <- "취약성평가 2"
    VH_Name_Out_VI3 <- "취약성평가 3"
    VH_Name_Out_Map <- "지도"
    VH_Name_Out_SIDO <- "시도"
    VH_Name_Out_SGG <- "시군구"
    VH_Name_Out_NP <- "국립공원"
    VH_Name_Out_BR <- "백두대간"
    VH_Name_Out_DMZ <- "DMZ"
    VH_Name_Out_Stat <- "통계"
    VH_Name_DM_Models <- "확산유형"
    VH_Name_DM_Models_list <- DM_Models_list
    VH_Name_DM_Models_selected <- "BIOMOD2"    
    VH_Name_CD_Models <- "기후모델"
    VH_Name_CD_Models_list <- CD_Models_list
    VH_Name_CD_Models_selected <- "KMA"    
    VH_Name_CD_Scenarios <- "기후시나리오"
    VH_Name_CD_Scenarios_list <- CD_Scenarios_list
    VH_Name_CD_Scenarios_selected <- "RCP4.5"    
    VH_Name_CD_Year <- "예측년도"
    VH_Name_CD_Year_list <- CD_Year_list
    VH_Name_CD_Year_selected <- "2000"
    VH_Name_models <- "모델유형"
    VH_Name_models_list <- SDM_models_list
    VH_Name_models_selected <- "GLM"
    VH_Name_EMmodels <- "앙상블"
    VH_Name_models_run <- "실행"
    VH_Name_Dir <- "서식지 평가결과 폴더"
    VH_Name_Action <- "영향 및 취약성 평가 실행"
    VH_Name_Action_Habitat <- "서식지별로 결과분석"
    VH_Name_Habitat <- "서식지유형을 선택하세요"
    VH_Name_Habitat_list <- c("시도" = "SD",
                            "시군구" = "SGG",
                            "국립공원" = "NP",
                            "백두대간" = "BR",
                            "DMZ" = "DMZ")
    VH_Name_Habitat_selected <- "SD" 
    VH_Name_models_out <- "모델유형"
    VH_Name_models_out_list <- SDM_models_out_list
    VH_Name_models_out_selected <- "GLM"
    VH_Name_CD_Models_out <- "기후모델"
    VH_Name_CD_Models_out_list <- CD_Models_list
    VH_Name_CD_Models_out_selected <- "KMA"    
    VH_Name_CD_Scenarios_out <- "기후시나리오"
    VH_Name_CD_Scenarios_out_list <- CD_Scenarios_list
    VH_Name_CD_Scenarios_out_selected <- "RCP4.5"    
    VH_Name_CD_Year_out <- "예측년도"
    VH_Name_CD_Year_out_list <- CD_Year_list
    VH_Name_CD_Year_out_selected <- "2000"
    VH_Name_MO_Dir <- "취약서식지평가 입력폴더"
    VH_Name_AO_Dir <- "취약서식지평가 결과폴더"
    HELP_Name <- "도움말"
    
  }



