#####=========================================================
##### installing and Loading packages ========================
# Setting packages and library
# Install R program
#Shiny
#install.packages("remotes")
#remotes::install_github("marlonecobos/rangemap")

# Install zip file in the MOTIVE Ecosystem program folder
#SDMTools
#MigClim1.6.2

packages <- c("shiny", "shinyWidgets", "shinyFiles", "shinyalert", "shinyjs", "shinythemes", "shinyjqui", "tiff", "sf", "backports", "biomod2", "rangemap", "dismo", "deldir", "gstat", "proj4", "gdata", "colorspace", "plyr", "leaflet", "RColorBrewer", "scales", "lattice", "dplyr", "maps", "maptools", "sp", "raster", "spatial", "rgdal", "ggplot2", "hrbrthemes", "plotly", "grid", "reshape", "rgeos", "stringr", "rgdal", "bnspatial", "R.utils", "SDMTools", "MigClim", "mgcv", "gsubfn", "DT", "fmsb", "data.table", "foreign", "scales", "leaflet.minicharts", "manipulateWidget", "shinydashboard", "shinyBS", "tcltk")
libraries <- c("shiny", "shinyWidgets", "shinyFiles", "shinyalert", "shinyjs", "shinythemes", "shinyjqui", "tiff", "sf", "backports", "biomod2", "rangemap", "dismo", "deldir", "gstat", "proj4", "gdata", "colorspace", "plyr", "leaflet", "RColorBrewer", "scales", "lattice", "dplyr", "maps", "maptools", "sp", "raster", "spatial", "rgdal", "ggplot2", "hrbrthemes", "plotly", "grid", "reshape", "rgeos", "stringr", "rgdal", "bnspatial", "R.utils", "SDMTools", "MigClim", "mgcv", "gsubfn", "DT", "fmsb", "data.table", "foreign", "scales", "leaflet.minicharts", "manipulateWidget", "shinydashboard", "shinyBS", "tcltk")

# packages <- c("shiny", "shinyWidgets", "shinyFiles", "shinyalert", "shinyjs", "shinythemes", "tiff", "sf", "biomod2", "proj4", "gdata", "colorspace", "plyr", "leaflet", "RColorBrewer", "scales", "lattice", "dplyr", "maps", "maptools", "sp", "raster", "rgdal", "ggplot2", "hrbrthemes", "plotly", "grid", "reshape", "rgeos", "stringr", "rgdal", "bnspatial", "mgcv", "gsubfn", "DT", "fmsb", "data.table", "foreign", "scales", "leaflet.minicharts", "manipulateWidget", "shinydashboard", "shinyBS")
# libraries <- c("shiny", "shinyWidgets", "shinyFiles", "shinyalert", "shinyjs", "shinythemes", "tiff", "sf", "biomod2", "proj4", "gdata", "colorspace", "plyr", "leaflet", "RColorBrewer", "scales", "lattice", "dplyr", "maps", "maptools", "sp", "raster", "rgdal", "ggplot2", "hrbrthemes", "plotly", "grid", "reshape", "rgeos", "stringr", "rgdal", "bnspatial", "mgcv", "gsubfn", "DT", "fmsb", "data.table", "foreign", "scales", "leaflet.minicharts", "manipulateWidget", "shinydashboard", "shinyBS")


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
MOTIVE_DIR <- getwd()
G <- reactiveValues()

destfile <- file.path(MOTIVE_DIR, "System_Environment.txt")
if (length(destfile) == 0 | !file.exists(destfile)) {
  button <- tkmessageBox(title='Message',message='System Environment information is not available. Please create System_Environment.txt and rerun the system again!',type='ok')
  if(tclvalue(button) == 'ok') {stop('Exit the program!')}
} else {
  system_env <- read.csv(destfile, header = T, sep = "=")
  SE_Language <- as.character(system_env[1,2])
  G$SE_Dir_Project <- as.character(system_env[3,2])
  G$SE_Dir_Climate <- as.character(system_env[4,2])
# G$SDM_MO_Variables_Folder <- file.path(isolate(G$SE_Dir_Climate), "2000")
  G$SE_Dir_Link <- as.character(system_env[5,2])
  G$SE_Dir_GIS <- as.character(system_env[6,2])
  G$SE_Dir_Species <- as.character(system_env[7,2])
  G$SE_speciesindex <- as.character(system_env[8,2])
  G$SE_specieslocation <- as.character(system_env[9,2])
  destfile1 <- file.path(isolate(G$SE_Dir_Species), isolate(G$SE_speciesindex))
  destfile2 <- file.path(isolate(G$SE_Dir_Species), isolate(G$SE_specieslocation))
  if (!file.exists(destfile1) | !file.exists(destfile2)) {
    button <- tkmessageBox(title='Message',message='Species data is not available. Please set species data and rerun the system again!',type='ok')
    if(tclvalue(button) == 'ok') {stop('Exit the program!')}
  }
  G$SE_Species_ID <- as.character(system_env[10,2])
  G$SE_Species_Name <- as.character(system_env[11,2])
  G$SE_Species_Location_Longitude <- as.character(system_env[12,2])
  G$SE_Species_Location_Latitude <- as.character(system_env[13,2])
  G$IMG_File <- as.character(system_env[14,2])
  G$IMG_Info <- as.character(system_env[15,2])
}


destfile <- file.path(MOTIVE_DIR, "Project_Information.csv")
if (length(destfile) == 0 | !file.exists(destfile)) {
  G_Project_info_CHK <- FALSE
  button <- tkmessageBox(title='Message',message='Information of Existing Projects is not available. If possible, Please create Project_Information.csv!',type='ok')
  if(tclvalue(button) == 'ok') {print('good!')}
} else {
  G_Project_info_CHK <- TRUE
  Project_info <- read.csv(destfile, header = T, sep = ",")
}  
  
  
destfile <- file.path(isolate(G$SE_Dir_Project), "Project_Information.csv")
if (length(destfile) == 0 | !file.exists(destfile)) {
  button <- tkmessageBox(title='Message',message='Project_Information is not in your working project',type='ok')
  if(tclvalue(button) == 'ok') {print('good!')}
} else {
  Project_working <- read.csv(destfile, header = T, sep = ",")
  Project_New_Path <- as.character(Project_working[1,5])
  Project_New_Name <- as.character(Project_working[1,1])
  Project_New_Manager <- as.character(Project_working[1,2])  
  Project_New_Institute <- as.character(Project_working[1,3])
  Project_New_Date <- as.character(Project_working[1,4])
  Project_New_Description <- as.character(Project_working[1,6])
}

G$SDM_MO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Species_Distribution", sep = "")
G$SDM_AO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Species_Distribution", sep = "")
G$HA_MI_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")
G$HA_MO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")
G$HA_AO_MI_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")
G$HA_AO_MO_Dir_Folder <- paste(isolate(G$SE_Dir_Project), "/Invasive_Species", sep = "")


G$DIR_NAME_Species <- "Species_Assessment"
G$DIR_NAME_Habitat <- "Habitat_Assessment"
G$DIR_NAME_SDM <- "BIOMOD2"
G$DIR_NAME_SRM <- "SRM_"
G$DIR_NAME_DM <- "MIGCLIM_"


Variable_lists <- read.csv(file.path(MOTIVE_DIR, as.character(system_env[16,2])), header = T, sep = ",")
Variable_lists[is.na(Variable_lists)] = ""
if (SE_Language == "English") {
    language_position <- 3
} else if (SE_Language == "Korean"){
    language_position <- 4
} else {
    language_position <- 3
}

if (SE_Language == "English") {
    Option_lists <- read.csv(file.path(MOTIVE_DIR, as.character(system_env[17,2])), header = T, sep = ",")
} else if (SE_Language == "Korean"){
    Option_lists <- read.csv(file.path(MOTIVE_DIR, as.character(system_env[18,2])), header = T, sep = ",")   
} else {
    Option_lists <- read.csv(file.path(MOTIVE_DIR, as.character(system_env[17,2])), header = T, sep = ",")  
}
Option_lists[is.na(Option_lists)] = ""

CD_Models_list = as.character(Option_lists[,"CD_Models_value"][Option_lists[,"CD_Models_value"] != ""])
names(CD_Models_list) = as.character(Option_lists[,"CD_Models_name"][Option_lists[,"CD_Models_name"] != ""])

CD_Scenarios_list = as.character(Option_lists[,"CD_Scenarios_value"][Option_lists[,"CD_Scenarios_value"] != ""])
names(CD_Scenarios_list) = as.character(Option_lists[,"CD_Scenarios_name"][Option_lists[,"CD_Scenarios_name"] != ""])

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

DM_Variables_list = as.character(Option_lists[,"DM_Variables_value"][Option_lists[,"DM_Variables_value"] != ""])
names(DM_Variables_list) = as.character(Option_lists[,"DM_Variables_name"][Option_lists[,"DM_Variables_name"] != ""])

DM_Variables_Landuse_list = as.character(Option_lists[,"DM_Variables_Landuse_value"][Option_lists[,"DM_Variables_Landuse_value"] != ""])
names(DM_Variables_Landuse_list) = as.character(Option_lists[,"DM_Variables_Landuse_name"][Option_lists[,"DM_Variables_Landuse_name"] != ""])

DM_Variables_LanduseType_list = as.character(Option_lists[,"DM_Variables_LanduseType_value"][Option_lists[,"DM_Variables_LanduseType_value"] != ""])
names(DM_Variables_LanduseType_list) = as.character(Option_lists[,"DM_Variables_LanduseType_name"][Option_lists[,"DM_Variables_LanduseType_name"] != ""])

CD_Variables_list = as.character(Option_lists[,"CD_Variables_value"][Option_lists[,"CD_Variables_value"] != ""])
names(CD_Variables_list) = as.character(Option_lists[,"CD_Variables_name"][Option_lists[,"CD_Variables_name"] != ""])

SDM_Variables_list = as.character(Option_lists[,"SDM_Variables_value"][Option_lists[,"SDM_Variables_value"] != ""])
names(SDM_Variables_list) = as.character(Option_lists[,"SDM_Variables_name"][Option_lists[,"SDM_Variables_name"] != ""])

SDM_AO_Variables_list = as.character(Option_lists[,"SDM_AO_Variables_value"][Option_lists[,"SDM_AO_Variables_value"] != ""])
names(SDM_AO_Variables_list) = as.character(Option_lists[,"SDM_AO_Variables_name"][Option_lists[,"SDM_AO_Variables_name"] != ""])

SDM_Variables_selected = as.character(Option_lists[,"SDM_Variables_selected_value"][Option_lists[,"SDM_Variables_selected_value"] != ""])
SDM_AO_Variables_selected = as.character(Option_lists[,"SDM_AO_Variables_selected_value"][Option_lists[,"SDM_AO_Variables_selected_value"] != ""])

Range_Models_list = as.character(Option_lists[,"Range_Models_value"][Option_lists[,"Range_Models_value"] != ""])
names(Range_Models_list) = as.character(Option_lists[,"Range_Models_name"][Option_lists[,"Range_Models_name"] != ""])

Hull_Types_list = as.character(Option_lists[,"Hull_Types_value"][Option_lists[,"Hull_Types_value"] != ""])
names(Hull_Types_list) = as.character(Option_lists[,"Hull_Types_name"][Option_lists[,"Hull_Types_name"] != ""])

Hull_Cluster_Method_list = as.character(Option_lists[,"Hull_Cluster_Method_value"][Option_lists[,"Hull_Cluster_Method_value"] != ""])
names(Hull_Cluster_Method_list) = as.character(Option_lists[,"Hull_Cluster_Method_name"][Option_lists[,"Hull_Cluster_Method_name"] != ""])

Hull_VoronoiHull_sampling_Type_list = as.character(Option_lists[,"Hull_VoronoiHull_sampling_Type_value"][Option_lists[,"Hull_VoronoiHull_sampling_Type_value"] != ""])
names(Hull_VoronoiHull_sampling_Type_list) = as.character(Option_lists[,"Hull_VoronoiHull_sampling_Type_name"][Option_lists[,"Hull_VoronoiHull_sampling_Type_name"] != ""])

SA_Group1_list = as.character(Option_lists[,"SA_Group1_value"][Option_lists[,"SA_Group1_value"] != ""])
names(SA_Group1_list) = as.character(Option_lists[,"SA_Group1_name"][Option_lists[,"SA_Group1_name"] != ""])

SA_Group2_list = as.character(Option_lists[,"SA_Group2_value"][Option_lists[,"SA_Group2_value"] != ""])
names(SA_Group2_list) = as.character(Option_lists[,"SA_Group2_name"][Option_lists[,"SA_Group2_name"] != ""])

SA_Group3_list = as.character(Option_lists[,"SA_Group3_value"][Option_lists[,"SA_Group3_value"] != ""])
names(SA_Group3_list) = as.character(Option_lists[,"SA_Group3_name"][Option_lists[,"SA_Group3_name"] != ""])

HA_Group_list = as.character(Option_lists[,"HA_Group_value"][Option_lists[,"HA_Group_value"] != ""])
names(HA_Group_list) = as.character(Option_lists[,"HA_Group_name"][Option_lists[,"HA_Group_name"] != ""])

HA_Group1_list = as.character(Option_lists[,"HA_Group1_value"][Option_lists[,"HA_Group1_value"] != ""])
names(HA_Group1_list) = as.character(Option_lists[,"HA_Group1_name"][Option_lists[,"HA_Group1_name"] != ""])

HA_Group2_list = as.character(Option_lists[,"HA_Group2_value"][Option_lists[,"HA_Group2_value"] != ""])
names(HA_Group2_list) = as.character(Option_lists[,"HA_Group2_name"][Option_lists[,"HA_Group2_name"] != ""])

VH_Group_list = as.character(Option_lists[,"VH_Group_value"][Option_lists[,"VH_Group_value"] != ""])
names(VH_Group_list) = as.character(Option_lists[,"VH_Group_name"][Option_lists[,"VH_Group_name"] != ""])


for (v in 1: nrow(Variable_lists)) {
    if(Variable_lists[v,2] == 0) {
        Variable_name <- as.character(Variable_lists[v,1][Variable_lists[v,1] != ""])
        assign(Variable_name, as.character(Variable_lists[v,language_position][Variable_lists[v,language_position] != ""]))
    } else {
        Variable_name <- as.character(Variable_lists[v,1][Variable_lists[v,1] != ""])
        assign(Variable_name, get(as.character(Variable_lists[v,language_position])))
    }
}

destfile <- file.path(isolate(G$SE_Dir_GIS), "SD.dbf")
SD_lists <- read.dbf(destfile)
G$SIDO_List <-  as.character(SD_lists[,"SD_KOR"])

destfile <- file.path(isolate(G$SE_Dir_GIS), "SGG.dbf")
SGG_lists <- read.dbf(destfile)
G$SGG_List <-  as.character(SGG_lists[,"SGG_KOR"])

#
# MOTIVE Ecosystem Functions
#

MotiveEco_SDM_plot <- function(r)
{
  crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  pal <- colorNumeric(c("#0C2C84", "#FFFFCC", "#41B6C4"), values(r),
                      na.color = "transparent")
  
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%        
    
    addRasterImage(r, colors = pal, opacity = 0.8,) %>%
    addLegend(pal = pal, values = values(r), title = "Legend")  %>%
    setView(lng = 127.00, lat = 36.00, zoom = 7)
}


MotiveEco_BND_stat <- function(df, x, y, title, xname, yname)
{
  ggplot(data=df, aes(x=df[[x]], y=df[[y]])) + 
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=df[[y]]), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5) +
    scale_fill_brewer(palette="Paired") +
    theme_minimal() +
    labs(title = title) + labs(x = xname) + labs(y = yname)
}


MotiveEco_bnd_plot <- function(p, x, y, b, unit)
{
  pal <- colorBin("YlOrRd", domain = p[[y]], bins = b)
  labels <- sprintf(
    paste("<strong>%s</strong><br/>%g ", unit),  #"<strong>%s</strong><br/>%g Km2", 
    p[[x]], p[[y]]
  ) %>% lapply(htmltools::HTML)
  
  leaflet(p) %>%
    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%   
    addPolygons(
      fillColor = ~pal(p[[y]]),
      weight = 2,
      opacity = 1,
      color = "grey",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(pal = pal, values = ~p[[y]], opacity = 0.7, title = NULL,
              position = "bottomright")
}


MotiveEco_gis_plot <- function(dir, ol, dl, cl, ml, yl)
{
  lo <- length(ol)
  ld <- length(dl)
  lc <- length(cl)
  lm <- length(ml)
  ly <- length(yl)
  
  for (o in ol) {
    for (d in dl) {
      for (c in cl) {
        for (m in ml) {
          for (y in yl) {
            if (ly > 0) {
              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
              r <- raster(file.path(dir, Map1))
            }
          }
        }
      }
    }
  }
  
  crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")	  
  pal <- colorNumeric(c("#0C2C84", "#FFFFCC", "#41B6C4"), values(r),
                      na.color = "transparent")
  
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%        
    
    addRasterImage(r, colors = pal, opacity = 0.8) %>%
    addLegend(pal = pal, values = values(r), title = "Legend")  %>%
    
    setView(lng = 128.00, lat = 36.00, zoom = 7)
}

MotiveEco_img_plot <- function(dir, ol, dl, cl, ml, yl)
{
  lo <- length(ol)
  ld <- length(dl)
  lc <- length(cl)
  lm <- length(ml)
  ly <- length(yl)
  
  tl <- lo * ld * lc * lm * ly
  nc <- 2
  if (tl <  2) {
    nr <- round(tl / nc) + 1
  } else {
    nr <- round((tl + 0.1) / nc)
  }
  
  par(mfrow = c(nr,nc), cex.main = 1.2)
  
  for (o in ol) {
    for (d in dl) {
      for (c in cl) {
        for (m in ml) {
          for (y in yl) {
            if (ly > 0) {
              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
              R_Map1 <- raster(file.path(dir, Map1))
              plot(R_Map1, main = Map1)
            }
          }
        }
      }
    }
  }
}