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
MOTIVE_DIR <- "D:/MOTIVE_Ecosystem/"
 
##### Path
G <- reactiveValues()
G$SE_Dir_Project <- "Set a working project"
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

observe({
    if (SE$Language == "English") {
        SE$Name_System <- "MOTIVE ECOSYSTEM (Climate Change Impact and Vulnerability Assessment Model for Ecosystem)"
    } else {
        SE$Name_System <- "MOTIVE ECOSYSTEM (생태계 기후변화 영향 및 취약성 평가모형)"
    }
})


SE_Language <- "English"
#SE_Language <- "Korean" 

if (SE_Language == "English") {
    Option_lists <- read.csv("D:/MOTIVE_Ecosystem/R/Programs/ecosystem/option_lists_ENG.csv", header = T, sep = ",")
} else {
    Option_lists <- read.csv("D:/MOTIVE_Ecosystem/R/Programs/ecosystem/option_lists_KOR.csv", header = T, sep = ",")    
}
Option_lists[is.na(Option_lists)] = ""


CD_Scenarios_list = as.character(Option_lists[,"CD_Scenarios_value"][Option_lists[,"CD_Scenarios_value"] != ""])
names(CD_Scenarios_list) = as.character(Option_lists[,"CD_Scenarios_name"][Option_lists[,"CD_Scenarios_name"] != ""])

CD_Models_list = as.character(Option_lists[,"CD_Models_value"][Option_lists[,"CD_Models_value"] != ""])
names(CD_Models_list) = as.character(Option_lists[,"CD_Models_name"][Option_lists[,"CD_Models_name"] != ""])

CD_Year_list = as.character(Option_lists[,"CD_Year_value"][Option_lists[,"CD_Year_value"] != ""])
names(CD_Year_list) = as.character(Option_lists[,"CD_Year_name"][Option_lists[,"CD_Year_name"] != ""])

SDM_models_list = as.character(Option_lists[,"SDM_models_value"][Option_lists[,"SDM_models_value"] != ""])
names(SDM_models_list) = as.character(Option_lists[,"SDM_models_name"][Option_lists[,"SDM_models_name"] != ""])

SDM_models_out_list = as.character(Option_lists[,"SDM_models_out_value"][Option_lists[,"SDM_models_out_value"] != ""])
names(SDM_models_out_list) = as.character(Option_lists[,"SDM_models_out_name"][Option_lists[,"SDM_models_out_name"] != ""])

DM_Models_list = as.character(Option_lists[,"DM_Models_value"][Option_lists[,"DM_Models_value"] != ""])
names(DM_Models_list) = as.character(Option_lists[,"DM_Models_name"][Option_lists[,"DM_Models_name"] != ""])

LD_Variables_list = as.character(Option_lists[,"LD_Variables_value"][Option_lists[,"LD_Variables_value"] != ""])
names(LD_Variables_list) = as.character(Option_lists[,"LD_Variables_name"][Option_lists[,"LD_Variables_name"] != ""])

CD_Variables_list = as.character(Option_lists[,"CD_Variables_value"][Option_lists[,"CD_Variables_value"] != ""])
names(CD_Variables_list) = as.character(Option_lists[,"CD_Variables_name"][Option_lists[,"CD_Variables_name"] != ""])

SDM_Variables_list = as.character(Option_lists[,"SDM_Variables_value"][Option_lists[,"SDM_Variables_value"] != ""])
names(SDM_Variables_list) = as.character(Option_lists[,"SDM_Variables_name"][Option_lists[,"SDM_Variables_name"] != ""])

SS_Group1_list = as.character(Option_lists[,"SS_Group1_value"][Option_lists[,"SS_Group1_value"] != ""])
names(SS_Group1_list) = as.character(Option_lists[,"SS_Group1_name"][Option_lists[,"SS_Group1_name"] != ""])

SS_Group2_list = as.character(Option_lists[,"SS_Group2_value"][Option_lists[,"SS_Group2_value"] != ""])
names(SS_Group2_list) = as.character(Option_lists[,"SS_Group2_name"][Option_lists[,"SS_Group2_name"] != ""])

SS_Group3_list = as.character(Option_lists[,"SS_Group3_value"][Option_lists[,"SS_Group3_value"] != ""])
names(SS_Group3_list) = as.character(Option_lists[,"SS_Group3_name"][Option_lists[,"SS_Group3_name"] != ""])

IS_Group_list = as.character(Option_lists[,"IS_Group_value"][Option_lists[,"IS_Group_value"] != ""])
names(IS_Group_list) = as.character(Option_lists[,"IS_Group_name"][Option_lists[,"IS_Group_name"] != ""])

VH_Group_list = as.character(Option_lists[,"VH_Group_value"][Option_lists[,"VH_Group_value"] != ""])
names(VH_Group_list) = as.character(Option_lists[,"VH_Group_name"][Option_lists[,"VH_Group_name"] != ""])


Variable_lists <- read.csv("D:/MOTIVE_Ecosystem/R/Programs/ecosystem/variable_lists.csv", header = T, sep = ",")
Variable_lists[is.na(Variable_lists)] = ""

if (SE_Language == "English") {
    lang <- 3
} else {
    lang <- 4
}

for (v in 1: nrow(Variable_lists)) {
    if(Variable_lists[v,2] == 0) {
        Variable_name <- as.character(Variable_lists[v,1][Variable_lists[v,1] != ""])
        assign(Variable_name, as.character(Variable_lists[v,lang][Variable_lists[v,lang] != ""]))
    } else {
        Variable_name <- as.character(Variable_lists[v,1][Variable_lists[v,1] != ""])
        assign(Variable_name, get(as.character(Variable_lists[v,lang])))
    }
}



