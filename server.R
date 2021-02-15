
shinyServer(function(input, output) {

	output$SE_Dir_Project <- renderText({G$SE_Dir_Project})
	output$SE_Dir_Climate <- renderText({G$SE_Dir_Climate})
	output$SE_Dir_Link <- renderText({G$SE_Dir_Link})
	output$SE_Dir_GIS <- renderText({G$SE_Dir_GIS})
	output$SE_Dir_Species <- renderText({G$SE_Dir_Species})
#	output$SE_speciesindex <- renderText({G$SE_speciesindex})
#	output$SE_specieslocation <- renderText({G$SE_specieslocation})
	global <- reactiveValues(response = FALSE)

  
	output$SE_Project <- renderUI({
	  if (G_Project_info_CHK) {
	    Project_list <- as.character(Project_info[,1])
	    Project_selected <- as.character(Project_list[1])
	    selectInput("Project_Name", SE_NAME_Project_List,
	                choices = Project_list,
	                selected = Project_selected
	    )
	  } else {
	    verbatimTextOutput("Project_Name")
	  }
	})
	
	output$Project_Name <- renderPrint({
	  cat("Information of existing Projects is not available")
	})
	
	output$SE_Project_Info_Manager <- renderPrint({
	  if (G_Project_info_CHK) {
	    Project_Info_Manager <- as.character(Project_info[Project_info$Name == input$Project_Name,][1,2])
	    cat(as.character(Project_Info_Manager))
	  } else {
	    cat("")
	  }
	})
	
	output$SE_Project_Info_Institute <- renderPrint({
	  if (G_Project_info_CHK) {
	    Project_Info_Institute <- as.character(Project_info[Project_info$Name == input$Project_Name,][1,3])
	    cat(as.character(Project_Info_Institute))
	  } else {
	    cat("")
	  }
	})

	output$SE_Project_Info_Date <- renderPrint({
	  if (G_Project_info_CHK) {
	    Project_Info_Date <- as.character(Project_info[Project_info$Name == input$Project_Name,][1,4])
	    cat(as.character(Project_Info_Date))
	  } else {
	    cat("")
	  }
	})

	output$SE_Project_Info_Path <- renderPrint({
	  if (G_Project_info_CHK) {
	    Project_Info_Path <- as.character(Project_info[Project_info$Name == input$Project_Name,][1,5])
	    G$SE_Dir_Project <- Project_Info_Path
	    cat(as.character(Project_Info_Path))
#	    if (!dir.exists(Project_Info_Path)) {
#	      showModal(modalDialog(
#	        title = "Error Message",
#	        paste("Working Project folder does not exist.")
#	      ))
#	    }
	  } else {
	    cat("")
	  }
	})
	
	output$SE_Project_Info_Description <- renderPrint({
	  if (G_Project_info_CHK) {
	    Project_Info_Description <- as.character(Project_info[Project_info$Name == input$Project_Name,][1,6])
	    cat(as.character(Project_Info_Description))
	  } else {
	    cat("")
	  }
	})

	observeEvent(input$SE_Dir_Project, {
	  volumes <- c(main = G$SE_Dir_Work)  #getVolumes()
	  shinyDirChoose(input, 'SE_Dir_Project', roots = volumes)
	  G$SE_Dir_Project <<- parseDirPath(volumes, input$SE_Dir_Project)
	})
	
#	output$SE_Project_New_Path <- renderText({G$SE_Dir_Project})
	
	output$SE_Project_New_Path <- renderUI({
	    destfile <- file.path(G$SE_Dir_Project, "Project_Information.csv")
	    if (length(destfile) == 0 | !file.exists(destfile)) {
	      showModal(modalDialog(
	        title = "Error Message",
	        paste(destfile, "does not exist.")
	      ))
	    } 
	      verbatimTextOutput("SE_Project_New_Path_Name")
	})
	
	output$SE_Project_New_Path_Name <- renderText({G$SE_Dir_Project})
	
	output$SE_Project_New_Name <- renderUI({
	    destfile <- file.path(G$SE_Dir_Project, "Project_Information.csv")
	    if (length(destfile) == 0 | !file.exists(destfile)) {
	        Project_New_Name <- "Set a new project name"
	    } else {
	        Project_working <- read.csv(file.path(G$SE_Dir_Project, "Project_Information.csv"), header = T, sep = ",")
	        Project_New_Name <- as.character(Project_working[1,"Name"])
	    }
	    textInput("Project_New_Name", SE_NAME_Project_Working,
	              value = Project_New_Name)
	})
	
	output$SE_Project_New_Manager <- renderUI({
	    destfile <- file.path(G$SE_Dir_Project, "Project_Information.csv")
	    if (length(destfile) == 0 | !file.exists(destfile)) {
	        Project_New_Manager <- "Set a new project manager" 
	    } else {
	        Project_working <- read.csv(file.path(G$SE_Dir_Project, "Project_Information.csv"), header = T, sep = ",")
	        Project_New_Manager <- as.character(Project_working[1,"Manager"])
	    }
	    textInput("Project_New_Manager", SE_NAME_Project_Manager,
	              value = Project_New_Manager)
	})
	
	output$SE_Project_New_Institute <- renderUI({
	    destfile <- file.path(G$SE_Dir_Project, "Project_Information.csv")
	    if (length(destfile) == 0 | !file.exists(destfile)) {
	        Project_New_Institute <- "Set a new project institute" 
	    } else {
	        Project_working <- read.csv(file.path(G$SE_Dir_Project, "Project_Information.csv"), header = T, sep = ",")
	        Project_New_Institute <- as.character(Project_working[1,"Institute"])
	    }
	    textInput("Project_New_Institute", SE_NAME_Project_Institute,
	              value = Project_New_Institute)
	})
	
	output$SE_Project_New_Date <- renderUI({
	    destfile <- file.path(G$SE_Dir_Project, "Project_Information.csv")
	    if (length(destfile) == 0 | !file.exists(destfile)) {
	        Project_New_Date <- "Set a new project date" 
	    } else {
	        Project_working <- read.csv(file.path(G$SE_Dir_Project, "Project_Information.csv"), header = T, sep = ",")
	        Project_New_Date <- as.character(Project_working[1,"Date"])
	    }
	    dateInput("Project_New_Date", SE_NAME_Project_Date,
	              value = Project_New_Date)
	})

	output$SE_Project_New_Description <- renderUI({
	    destfile <- file.path(G$SE_Dir_Project, "Project_Information.csv")
	    if (length(destfile) == 0 | !file.exists(destfile)) {
	        Project_New_Description <- "Set a new project description" 
	    } else {
	        Project_working <- read.csv(file.path(G$SE_Dir_Project, "Project_Information.csv"), header = T, sep = ",")
	        Project_New_Description <- as.character(Project_working[1,"Description"])
	    }
	    textInput("Project_New_Description", SE_NAME_Project_Description,
	              value = Project_New_Description)
	})
	
	observeEvent(input$SE_Dir_Project_Action, {
	    destfile <- file.path(G$SE_Dir_Project, "Project_Information.csv")
	    if (!file.exists(destfile)) {
	        Project_working <- setNames(data.frame(matrix(ncol = 6, nrow = 1)), c("Name", "Manager", "Institute", "Date", "Path", "Description"))
	    }
	    Project_working["Name"] <- input$Project_New_Name
	    Project_working["Manager"] <- input$Project_New_Manager
	    Project_working["Institute"] <- input$Project_New_Institute
	    Project_working["Date"] <- input$Project_New_Date
	    Project_working["Path"] <- G$SE_Dir_Project
	    Project_working["Description"] <- input$Project_New_Description
	    write.csv(Project_working, file = file.path(G$SE_Dir_Project, "Project_Information.csv"))
	    showModal(modalDialog(
	        title = SE_Name_WE_Project_Action_Message,
	        SE_Name_WE_Project_Action_Save
	    ))
	})

	output$SE_Dir_Project_SDM <- renderUI({
	  Dir_Project_SDM_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  Dir_Project_SDM_selected <- Dir_Project_SDM_list[1]
	  selectInput("Dir_Project_SDM", SE_Name_Dir_Project_SDM_Folder,
	              choices = c(Dir_Project_SDM_list),
	              selected = Dir_Project_SDM_selected
	  )
	})
	
	output$SE_Dir_Project_SDM_Species <- renderUI({
	  Dir_Project_SDM_Species_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$Dir_Project_SDM), full.names = FALSE, recursive = FALSE)
	  Dir_Project_SDM_Species_selected <- Dir_Project_SDM_Species_list[1]
	  selectInput("Dir_Project_SDM_Species", SE_Name_Dir_Project_SDM_Species,
	              choices = c(Dir_Project_SDM_Species_list),
	              selected = Dir_Project_SDM_Species_selected
	  )
	})
	
	output$SE_Dir_Project_SDM_Species_Model <- renderUI({
	  Dir_Project_SDM_Species_Model_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$Dir_Project_SDM, input$Dir_Project_SDM_Species), full.names = FALSE, recursive = FALSE)
	  Dir_Project_SDM_Species_Model_selected <- Dir_Project_SDM_Species_Model_list[1]
	  selectInput("Dir_Project_SDM_Species_Model", SE_Name_Dir_Project_SDM_Model,
	              choices = c(Dir_Project_SDM_Species_Model_list),
	              selected = Dir_Project_SDM_Species_Model_selected
	  )
	})
	
	output$SE_Dir_Project_SDM_Species_Model_Options <- renderTable({
	    
	    destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$Dir_Project_SDM, input$Dir_Project_SDM_Species, input$Dir_Project_SDM_Species_Model, paste(input$Dir_Project_SDM_Species, "_", input$Dir_Project_SDM_Species_Model, "_variables.csv", sep = ""))
	    if (file.exists(destfile)) { 
	        SDM_variables_lists <- read.csv(destfile, header = T, sep = ",")
	        SDM_variables_lists[is.na(SDM_variables_lists)] = ""
	        
	        SDM_variables_lists_T <- data.frame(t(SDM_variables_lists))
	        rownames(SDM_variables_lists_T) <- colnames(SDM_variables_lists)
	        SDM_variables_lists_T[-1,]
	    } 
	    }, rownames = TRUE, colnames = FALSE)
	
	output$SE_Dir_Project_SDM_Species_Model_Output <- renderPrint({
      Dir_Project_SDM_Species_Model_list_csv <- list.files(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$Dir_Project_SDM, input$Dir_Project_SDM_Species, input$Dir_Project_SDM_Species_Model), pattern="\\.csv$", all.files=FALSE, full.names=FALSE)
      Dir_Project_SDM_Species_Model_list_grd <- list.files(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$Dir_Project_SDM, input$Dir_Project_SDM_Species, input$Dir_Project_SDM_Species_Model), pattern="\\.grd$", all.files=FALSE, full.names=FALSE)
      cat(as.character(Dir_Project_SDM_Species_Model_list_csv))
      cat(as.character(Dir_Project_SDM_Species_Model_list_grd))
	})
	
	output$SE_Dir_Project_HA <- renderUI({
	  Dir_Project_HA_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat), full.names = FALSE, recursive = FALSE)
	  Dir_Project_HA_selected <<- Dir_Project_HA_list[1]
	  selectInput("Dir_Project_HA", SE_Name_Dir_Project_HA_Folder,
	               choices = c(Dir_Project_HA_list),
	               selected = Dir_Project_HA_selected
	  )
	})
	
	output$SE_Dir_Project_HA_Species_Model_Options <- renderTable({
	    
	    destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$Dir_Project_HA, "HabitatAssessment_Options.csv")
	    
	    HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	    HA_Options_lists[is.na(HA_Options_lists)] = ""
	    
	    HA_Options_lists_T <- data.frame(t(HA_Options_lists))
	    rownames(HA_Options_lists_T) <- colnames(HA_Options_lists)
	    HA_Options_lists_T[-1,]
	    
	}, rownames = TRUE, colnames = FALSE)
	
	output$SE_Dir_Project_HA_Species_Model_Output <- renderPrint({
	  Dir_Project_HA_list_csv <- list.files(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$Dir_Project_HA), pattern="\\.csv$", all.files=FALSE, full.names=FALSE)
	  Dir_Project_HA_list_dbf <- list.files(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$Dir_Project_HA), pattern="\\.dbf$", all.files=FALSE, full.names=FALSE)
	  Dir_Project_HA_list_grd <- list.files(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$Dir_Project_HA), pattern="\\.grd$", all.files=FALSE, full.names=FALSE)
	  cat(as.character(Dir_Project_HA_list_csv))
	  cat('\n\n')
	  cat(as.character(Dir_Project_HA_list_dbf))
	  cat('\n\n')
	  cat(as.character(Dir_Project_HA_list_grd))
	})

	observeEvent(input$SE_Dir_Climate, {
	  volumes <- c(main = G$SE_Dir_Data)  #getVolumes()
		shinyDirChoose(input, 'SE_Dir_Climate', roots = volumes)
		G$SE_Dir_Climate <<- parseDirPath(volumes, input$SE_Dir_Climate)
		output$SE_Dir_Climate <- renderText({G$SE_Dir_Climate})
	})
  
	observeEvent(input$SE_Dir_Link, {
	  volumes <- c(main = G$SE_Dir_Data)  #getVolumes()
		shinyDirChoose(input, 'SE_Dir_Link', roots = volumes)
		G$SE_Dir_Link <<- parseDirPath(volumes, input$SE_Dir_Link)
		output$SE_Dir_Link <- renderText({G$SE_Dir_Link})
	})
	
	observeEvent(input$SE_Dir_GIS, {
	  volumes <- c(main = G$SE_Dir_Data)  #getVolumes()
	  shinyDirChoose(input, 'SE_Dir_GIS', roots = volumes)
	  G$SE_Dir_GIS <<- parseDirPath(volumes, input$SE_Dir_GIS)
	  output$SE_Dir_GIS <- renderText({G$SE_Dir_GIS})
	})
  
	observeEvent(input$SE_Dir_Species, {
	  volumes <- c(main = G$SE_Dir_Data)  #getVolumes()
		shinyDirChoose(input, 'SE_Dir_Species', roots = volumes)
		G$SE_Dir_Species <<- parseDirPath(volumes, input$SE_Dir_Species)
		output$SE_Dir_Species <- renderText({G$SE_Dir_Species})
	})

	output$SE_speciesindex <- renderUI({
	    selectInput("SE_speciesindex", SE_Name_WE_Project_Data_Index, selected = G$SE_speciesindex, choice = list.files(path = G$SE_Dir_Species, pattern="\\.csv$", all.files=FALSE, full.names=FALSE))
	})
	
	output$SE_specieslocation <- renderUI({
	    selectInput("SE_specieslocation", SE_Name_WE_Project_Data_Location, selected = G$SE_specieslocation, choice = list.files(path = G$SE_Dir_Species, pattern="\\.csv$", all.files=FALSE, full.names=FALSE))
	})

	output$SP_Info <- DT::renderDataTable({
	  if (!length(input$SE_speciesindex) == 0 | !length(input$SE_specieslocation) == 0) {
	    G_FILE_speciesindex <- read.csv(file.path(G$SE_Dir_Species, input$SE_speciesindex), header = T, sep = ",")
	    G_FILE_specieslocation <<- read.csv(file.path(G$SE_Dir_Species, input$SE_specieslocation), header = T, sep = ",")
	    G_FILE_speciesfreq <- count(G_FILE_specieslocation, !!sym(G$SE_Species_ID))
	    G_FILE_speciesinfo <<- inner_join(G_FILE_speciesfreq, G_FILE_speciesindex, by = G$SE_Species_ID)
	  } else {
	  G_FILE_speciesindex <- read.csv(file.path(G$SE_Dir_Species, G$SE_speciesindex), header = T, sep = ",")
	  G_FILE_specieslocation <<- read.csv(file.path(G$SE_Dir_Species, G$SE_specieslocation), header = T, sep = ",")
	  G_FILE_speciesfreq <- count(G_FILE_specieslocation, !!sym(G$SE_Species_ID))
	  G_FILE_speciesinfo <<- inner_join(G_FILE_speciesfreq, G_FILE_speciesindex, by = G$SE_Species_ID)
	  }
	  DT::datatable(G_FILE_speciesinfo)
	})
  
	output$SP_Map <- renderLeaflet({
	          
		rs <- input$SP_Info_rows_selected 
		if (length(rs)) {
			species_data <- inner_join(G_FILE_specieslocation, G_FILE_speciesinfo[rs, , drop = FALSE], by = G$SE_Species_ID)
			
			
#			anglerIcon <- makeIcon(
#			  iconUrl = "leaf-red.png",
#			  iconWidth = 64, iconHeight = 64,
#			  iconAnchorX = 22, iconAnchorY = 94,
#			  shadowUrl = "leaf-shadow.png",
#			  shadowWidth = 50, shadowHeight = 64,
#			  shadowAnchorX = 4, shadowAnchorY = 62
#			)

			leaflet(data = species_data) %>%

#			addTiles(
#					urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#					attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#			) %>%
			  
			addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%

#			addMarkers(popup = species_data[, G$SE_Species_ID], label = species_data[, G$SE_Species_Name], clusterOptions = markerClusterOptions(), icon = anglerIcon) %>%
			addMarkers(popup = species_data[, G$SE_Species_ID], label = species_data[, G$SE_Species_Name], clusterOptions = markerClusterOptions()) %>%
			setView(lng = 127.00, lat = 38.00, zoom = 6)
		}
	})

	output$SP_LOC_Info <- DT::renderDataTable({
	  G_FILE_speciesxy <<- inner_join(G_FILE_specieslocation, G_FILE_speciesinfo[input$SP_Info_rows_selected, , drop = FALSE], by = G$SE_Species_ID)
	  DT::datatable(G_FILE_speciesxy)
    }, server = TRUE)
	
	output$SP_LOC_Map <- renderLeaflet({
		rs <- input$SP_LOC_Info_rows_selected
		
		if (length(rs)) {
#			species_data <- G_FILE_specieslocation[rs, , drop = FALSE]
			species_data <- G_FILE_speciesxy[rs, , drop = FALSE]
#			species_data <- inner_join(G_FILE_specieslocation, G_FILE_speciesxy[rs, , drop = FALSE], by = G$SE_Species_ID)
			
#			anglerIcon <- makeIcon(
#			  iconUrl = "leaf-green.png",
#			  iconWidth = 64, iconHeight = 64,
#			  iconAnchorX = 22, iconAnchorY = 94,
#			  shadowUrl = "leaf-shadow.png",
#			  shadowWidth = 50, shadowHeight = 64,
#			  shadowAnchorX = 4, shadowAnchorY = 62
#			)
			
			leaflet(data = species_data) %>%
#			addTiles(
#					urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#					attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#			) %>%
			  
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
	
#			addMarkers(popup = species_data[, G$SE_Species_ID], label = species_data[, G$SE_Species_Name], clusterOptions = markerClusterOptions(), icon = anglerIcon) %>%
			addMarkers(popup = species_data[, G$SE_Species_ID], label = species_data[, G$SE_Species_Name], clusterOptions = markerClusterOptions()) %>%
			setView(lng = 127.00, lat = 38.00, zoom = 6)
		}
	})  
	
	output$LD_Map <- renderLeaflet({
	  
    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, G$IMG_File, sep = ""))
    r <- raster(file)
		crs(r) <- CRS(G$Projection_Info)
		r <- as.factor(r)
		if (input$LD_Variables == "Landuse_ssp1" | input$LD_Variables == "Landuse_ssp2" | input$LD_Variables == "Landuse_ssp3") {
		  col <- colorNumeric(c("red", "orange", "forestgreen", "green", "blue", "steelblue3", "blue"), values(r), na.color = "transparent")
		  pal <- colorFactor(palette = c("red", "orange", "forestgreen", "green", "blue", "steelblue3", "blue"), domain = c("Urban", "Cropland", "Forest", "Grassland", "Water", "Wetland", "River"), na.color = "transparent", ordered = T)
	    leaflet() %>%
#		  addTiles(
#			  	urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#			  	attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#		  ) %>%
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
		  	
		  addRasterImage(r, colors = col, opacity = 0.7,) %>%
		  addLegend(position = "topright", pal = pal, values = c("Urban", "Cropland", "Forest", "Grassland", "Water", "Wetland", "River"), title = "Legend")  %>%
		  setView(lng = 127.00, lat = 36.00, zoom = 7)
		} else {
		  pal <- colorNumeric(c("deepskyblue4", "aliceblue", "firebrick4"), values(r), na.color = "transparent")	
		  leaflet() %>%
#        addTiles(
#        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#        ) %>%
		    addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
		    
		    addRasterImage(r, colors = pal, opacity = 0.8,) %>%
		    addLegend(pal = pal, values = values(r), title = "Legend")  %>%
		    setView(lng = 127.00, lat = 36.00, zoom = 7)
		}
		
	})  
	
	output$LD_Summary <- renderPrint({
	  file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, G$IMG_File, sep = ""))
	  r <- raster(file)
	  crs(r) <- CRS(G$Projection_Info)
	  summary(r)
	})
	
	output$LD_Histogram <- renderPlot({
	  file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, G$IMG_File, sep = ""))
	  x <- raster(file)
	  hist(x, # breaks = bins, 
	       col="lightskyblue3",  # skyblue",
	       border="white",
	       xlab = input$LD_Variables,
	       main = "Histogram")
	})  
	
	output$LD_Map_Landuse <- renderLeaflet({	
	    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, G$IMG_File, sep = ""))
	    r <- raster(file)
	    crs(r) <- CRS(G$Projection_Info)
	    
	    for (k in input$LD_MO_Barrier_LanduseType) {
	      r[r == as.integer(k)] <- 9999
	    }	
	    r[r != 9999] <- 0
	    r[r == 9999] <- 1
	    r <- as.factor(r)

	    pal <- colorFactor(c("gray90", "chocolate4"), values(r),
	                        na.color = "transparent")
	    
	    leaflet() %>%
#        addTiles(
#        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#        ) %>%
	      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
	        
        addRasterImage(r, colors = pal, opacity = 0.7,) %>%
        addLegend(pal = pal, values = values(r), title = "Legend")  %>%	
        setView(lng = 127.00, lat = 36.00, zoom = 7)
	})
	
	output$LD_Map_Forestfire <- renderLeaflet({	
	    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, G$IMG_File, sep = ""))
	    r <- raster(file)
	    crs(r) <- CRS(G$Projection_Info)

	    r[r >= input$LD_MO_Barrier_Forestfire_Cutoff] <- 9999
	    r[r != 9999] <- 0
	    r[r == 9999] <- 1
	    r <- as.factor(r)
	    
	    pal <- colorFactor(c("gray90", "chocolate4"), values(r),
	                        na.color = "transparent")
	    
	    leaflet() %>%
#        addTiles(
#        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#        ) %>%
	      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%      
	        
        addRasterImage(r, colors = pal, opacity = 0.7,) %>%
        addLegend(pal = pal, values = values(r), title = "Legend")  %>%	
        setView(lng = 127.00, lat = 36.00, zoom = 7)
	})
	
	output$LD_Map_Landslide <- renderLeaflet({	
	    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, G$IMG_File, sep = ""))
	    r <- raster(file)
	    crs(r) <- CRS(G$Projection_Info)
	    
	    r[r >= input$LD_MO_Barrier_Landslide_Cutoff] <- 9999
	    r[r != 9999] <- 0
	    r[r == 9999] <- 1
	    r <- as.factor(r)
	    
	    pal <- colorFactor(c("gray90", "chocolate4"), values(r),
	                        na.color = "transparent")
	    
	    leaflet() %>%
#        addTiles(
#        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#        ) %>%
	      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
	        
        addRasterImage(r, colors = pal, opacity = 0.7,) %>%
        addLegend(pal = pal, values = values(r), title = "Legend")  %>%	
        setView(lng = 127.00, lat = 36.00, zoom = 7)
	})
	
	output$CD_Variables_select <- renderUI({
	  
	  CD_Variables_Folder <- file.path(G$SE_Dir_Climate, G$Var_Current_Folder)
#	  CD_Name_Variables_list <- list.files(path = CD_Variables_Folder, full.names = FALSE, recursive = FALSE)
#	  CD_Name_Variables_list <- list.files(path = CD_Variables_Folder, pattern="\\.grd$", all.files=FALSE, full.names=FALSE)
	  CD_Name_Variables_list <- list.files(path = CD_Variables_Folder, pattern=paste("\\", G$IMG_File, "$", sep=""), all.files=FALSE, full.names=FALSE)
	  CD_Name_Variables_selected <- CD_Name_Variables_list[1]
	  
	  selectInput("CD_Variables", CD_Name_Variables,
	              choices = CD_Name_Variables_list,
	              selected = CD_Name_Variables_selected
    )
	})
	
	output$CD_Map <- renderLeaflet({
	  #		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, paste(input$CD_Variables, ".tif", sep = ""))
	  file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, input$CD_Variables)
	  r <- raster(file)
	  crs(r) <- CRS(G$Projection_Info)
	  pal <- colorNumeric(c("deepskyblue4", "aliceblue", "firebrick4"), values(r),
	                      na.color = "transparent")
	  
	  leaflet() %>%
#	    addTiles(
#	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#	    ) %>%        
	  
    addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%    
    addRasterImage(r, colors = pal, opacity = 0.7) %>%
    addLegend(pal = pal, values = values(r), title = "Legend")  %>%
    setView(lng = 128.00, lat = 36.00, zoom = 7)
	})
	
	output$CD_Summary <- renderPrint({
#		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, paste(input$CD_Variables, ".tif", sep = ""))
		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, input$CD_Variables)
		r <- raster(file)
		crs(r) <- CRS(G$Projection_Info)
		summary(r)
	})
	
	output$CD_Histogram <- renderPlot({
#		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, paste(input$CD_Variables, ".tif", sep = ""))
	  file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, input$CD_Variables)
		x <- raster(file)
		crs(x) <- CRS(G$Projection_Info)
		hist(x, # breaks = bins, 
			col="lightskyblue3",
			border="white",
			xlab = input$CD_Variables,
			main = "Histogram")
	})
   
	
	output$SDM_SP_Info <- DT::renderDataTable({
	  if (!length(input$SE_speciesindex) == 0 | !length(input$SE_specieslocation) == 0) {
	      G_FILE_speciesindex <<- read.csv(file.path(G$SE_Dir_Species, input$SE_speciesindex), header = T, sep = ",")
	      G_FILE_specieslocation <<- read.csv(file.path(G$SE_Dir_Species, input$SE_specieslocation), header = T, sep = ",")
	      G_FILE_speciesfreq <- count(G_FILE_specieslocation, !!sym(G$SE_Species_ID))
	      G_FILE_speciesinfo <<- inner_join(G_FILE_speciesfreq, G_FILE_speciesindex, by = G$SE_Species_ID)
	  } else {
	      G_FILE_speciesindex <<- read.csv(file.path(G$SE_Dir_Species, G$SE_speciesindex), header = T, sep = ",")
	      G_FILE_specieslocation <<- read.csv(file.path(G$SE_Dir_Species, G$SE_specieslocation), header = T, sep = ",")
	      G_FILE_speciesfreq <- count(G_FILE_specieslocation, !!sym(G$SE_Species_ID))
	      G_FILE_speciesinfo <<- inner_join(G_FILE_speciesfreq, G_FILE_speciesindex, by = G$SE_Species_ID)
	  }
	  DT::datatable(G_FILE_speciesinfo)
	  
	})
	
	observeEvent(input$SE_Dir_Species_List, {
	  volumes <- c(main = G$SE_Dir_Project)  #getVolumes()
	  shinyDirChoose(input, 'SE_Dir_Species_List', roots = volumes)
	  G$SE_Dir_Species_List <<- parseDirPath(volumes, input$SE_Dir_Species_List)
	  output$SE_Dir_Species_List <- renderText({G$SE_Dir_Species_List})
	})
	
	output$SE_Species_List <- renderUI({
	  if (is.null(G$SE_Dir_Species_List)) {
	    G$SE_Dir_Species_List <- G$SE_Dir_Project
	  }
	  selectInput("SE_Species_List", SDM_Name_Model_Species_File, choice = list.files(path = G$SE_Dir_Species_List, pattern="\\.csv$", all.files=FALSE, full.names=FALSE))
	})
	
	output$SDM_SP_Selection1 <- renderPrint({
	  if (input$SDM_SP_Selection == "Data_Table") {
	    s_id <<- as.character(G_FILE_speciesinfo[input$SDM_SP_Info_rows_selected, , drop = FALSE][, G$SE_Species_ID])
	  } else {
	    csv <- read.csv(file.path(G$SE_Dir_Species_List, input$SE_Species_List), header = F, sep = ",")
	    nc <- ncol(csv)
	    slist <- ""
	    for(i in 1:nc){
	      c <- as.character(csv[,i][csv[,i] != ""])
	      slist <- c(slist, c)
	    }
	    s_id <<- slist[-1]
	  }
	  if (length(s_id)) {
	    cat('Number of Speices:   ', length(s_id))
	  }
	})
	
	output$SDM_SP_Selection2 <- renderPrint({
	  if (input$SDM_SP_Selection == "Data_Table") {
	    s_id <<- as.character(G_FILE_speciesinfo[input$SDM_SP_Info_rows_selected, , drop = FALSE][, G$SE_Species_ID])
	  } else {
	    csv <- read.csv(file.path(G$SE_Dir_Species_List, input$SE_Species_List), header = F, sep = ",")
	    nc <- ncol(csv)
	    slist <- ""
	    for(i in 1:nc){
	      c <- as.character(csv[,i][csv[,i] != ""])
	      slist <- c(slist, c)
	    }
	    s_id <<- slist[-1]
	  }
	  if (length(s_id)) {
	    cat(s_id, sep = ",  ")
	  }
	})
	
	
	output$SDM_AO_MI_Dir_Folder <- renderUI({
	  SDM_AO_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  SDM_AO_MI_Dir_Folder_selected <- SDM_AO_MI_Dir_Folder_list[1]
	  selectInput("SDM_AO_MI_Dir", SDM_Name_Model_Variable_Distribution_Folder,
	              choices = c(SDM_AO_MI_Dir_Folder_list),
	              selected = SDM_AO_MI_Dir_Folder_selected
	  )
	  
	})
	
	output$SDM_HA_AO_MO_Dir_Folder <- renderUI({
	  SDM_HA_AO_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat), full.names = FALSE, recursive = FALSE)
	  SDM_HA_AO_MO_Dir_Folder_selected <- SDM_HA_AO_MO_Dir_Folder_list[1]
	  selectInput("SDM_HA_AO_MO_Dir", SDM_Name_Model_Variable_SpeciesRichness_Folder,
	              choices = c(SDM_HA_AO_MO_Dir_Folder_list),
	              selected = SDM_HA_AO_MO_Dir_Folder_selected
	  )
	  
	})

	output$SDM_AO_MI_Dir_Folder_Name <- renderUI({
	  SDM_AO_MI_Dir_Folder_Name_list <- G$DIR_NAME_SDM   # list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_MI_Dir, input$SDM_AO_Species[1]), full.names = FALSE, recursive = FALSE)
	  SDM_AO_MI_Dir_Folder_Name_selected <- SDM_AO_MI_Dir_Folder_Name_list[1]
	  selectInput("SDM_AO_MI_Dir_Folder", SDM_Name_Model_Variable_Distribution_Folder_Name,
	              choices = c(SDM_AO_MI_Dir_Folder_Name_list),
	              selected = SDM_AO_MI_Dir_Folder_Name_selected
	  )
	  
	})		
	
	
	output$SDM_AO_Species <- renderUI({
	  G$SDM_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_MI_Dir)
	  SDM_Name_Species_list <- list.dirs(path = G$SDM_AO_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  SDM_Name_Species_selected <- SDM_Name_Species_list[1]
	  selectInput("SDM_AO_Species", SDM_Name_Model_Variable_Distribution_Species,
	              choices = c(SDM_Name_Species_list),
	              selected = SDM_Name_Species_selected
	  )
	})
	
	output$SDM_AO_SDM_PROJ_model <- renderUI({
	  G$SDM_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_MI_Dir)
	  destfile <- file.path(G$SDM_AO_MI_Dir_Folder, input$SDM_AO_Species[1], G$DIR_NAME_SDM, paste(as.name(paste(input$SDM_AO_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	  all_eval <- read.csv(destfile)
	  G_FILE_species_evaluation <<- all_eval
	  SDM_Name_Models_list <- unique(as.character(G_FILE_species_evaluation$Projection))
	  SDM_Name_Models_selected <- SDM_Name_Models_list[1]
	  radioButtons("SDM_AO_SDM_PROJ_model", SDM_Name_Model_Variable_Distribution_Model,
	               choices = c(SDM_Name_Models_list),
	               selected = SDM_Name_Models_selected
	  )
	})
	
	output$SDM_HA_AO_SDM_PRED_model <- renderUI({

	  destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$SDM_HA_AO_MO_Dir, "HabitatAssessment_Options.csv")
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists[is.na(HA_Options_lists)] = ""
	  SDM_Name_Models_list <- as.character(HA_Options_lists[1, 9])
	  SDM_Name_Models_selected <- SDM_Name_Models_list[1]
	  
	  radioButtons("SDM_HA_AO_SDM_PRED_model", SDM_Name_Model_Variable_SpeciesRichness_Model,
	               choices = c(SDM_Name_Models_list),
	               selected = SDM_Name_Models_selected
	  )
	})

	output$SDM_MO_Variables_Select <- renderUI({
	  
	  SDM_MO_Variables_Folder <- file.path(G$SE_Dir_Climate, G$Var_Current_Folder)
	  SDM_Name_MO_Variables_list <- list.files(path = SDM_MO_Variables_Folder, pattern="\\.grd$", all.files=FALSE, full.names = FALSE, recursive = FALSE)
	  SDM_Name_MO_Variables_selected <- SDM_Name_MO_Variables_selected
	  
	  checkboxGroupInput("SDM_MO_Variables", SDM_Name_MO_Variables,
	                     choices = c(SDM_Name_MO_Variables_list),
	                     selected = SDM_Name_MO_Variables_selected
	  )
	})
	
	output$SDM_MO_Variables_Select_Categorical <- renderUI({
	  
#	  SDM_MO_Variables_Folder <- file.path(G$SE_Dir_Climate, G$Var_Current_Folder)
	  SDM_Name_MO_Variables_list <- input$SDM_MO_Variables
#	  SDM_Name_MO_Variables_selected <- SDM_Name_MO_Variables_selected
	  
	  checkboxGroupInput("SDM_MO_Variables_Categorical", SDM_Name_MO_Variables,
	                     choices = c(SDM_Name_MO_Variables_list)
	  )
	})
	
	observeEvent(input$SDM_MO_AO_Variables_Create, {
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  slist <- input$SDM_AO_Species
	  dlist <- input$SDM_MO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$SDM_MO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
#	  mlist <- input$SDM_AO_SDM_PROJ_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$SDM_MO_Project_year
	  
	  n <- 0
	  ld <- length(dlist)
	  lc <- length(clist)
	  ly <- length(ylist)
#	  lm <- length(mlist)
	  ls <- length(slist)
	  tl <- ld * lc * ly * ls
	  

	  withProgress(message = 'Creeating model output variables.........', value = 0, {
    for (v in input$SDM_MO_AO_Variables) {
  	  if (v == "Species.grd") {
  	    mlist <- input$SDM_AO_SDM_PROJ_model
  	    PATH_MODEL_OUTPUT <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_MI_Dir)
  	    for (s in slist) {
  	      for (d in dlist) {
  	        for (c in clist) {
  	          for (m in mlist) {
  	            org_path <- file.path(PATH_MODEL_OUTPUT, s, input$SDM_AO_MI_Dir_Folder)
  	            target_path <- file.path(G$SE_Dir_Climate, ylist[1])
  	            o_file_grd <- paste("PROJ_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
  	            o_file_gri <- paste("PROJ_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_Info, sep = "")
  	            t_file_grd <- paste("Species.grd")
  	            t_file_gri <- paste("Species.gri")
  	            ofile_path_grd <- file.path(org_path, o_file_grd)
  	            ofile_path_gri <- file.path(org_path, o_file_gri)
  	            tfile_path_grd <- file.path(target_path, t_file_grd)
  	            tfile_path_gri <- file.path(target_path, t_file_gri)
  	            if (!file.exists(ofile_path_grd)){
  	              showModal(modalDialog(
  	                title = "Error Message",
  	                paste(ofile_path_grd, "does not exist.")
  	              ))
  	            } else {
  	              if (file.exists(tfile_path_grd)) {
  	                file.remove(tfile_path_grd)
  	                file.remove(tfile_path_gri)
  	              }
  	              file.copy(from = ofile_path_grd, to = tfile_path_grd, overwrite = TRUE)
  	              file.copy(from = ofile_path_gri, to = tfile_path_gri, overwrite = TRUE)
  	            }
  	            for (y in ylist) {
  	              incProgress(1/tl, detail = paste("Doing part", "(", s, ")", "_", d, "_", c, "_", y))
  	              org_path <- file.path(PATH_MODEL_OUTPUT, s, input$SDM_AO_MI_Dir_Folder)
  	              target_path <- file.path(G$SE_Dir_Climate, d, c, y)
  	              o_file_grd <- paste("PROJ_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")
  	              o_file_gri <- paste("PROJ_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_Info, sep = "")
  	              t_file_grd <- paste("Species.grd")
  	              t_file_gri <- paste("Species.gri")
  	              ofile_path_grd <- file.path(org_path, o_file_grd)
  	              ofile_path_gri <- file.path(org_path, o_file_gri)
  	              tfile_path_grd <- file.path(target_path, t_file_grd)
  	              tfile_path_gri <- file.path(target_path, t_file_gri)
  	              if (!file.exists(ofile_path_grd)){
  	                showModal(modalDialog(
    	                  title = "Error Message",
  	                  paste(ofile_path_grd, "does not exist.")
  	                ))
  	              } else {
  	                if (file.exists(tfile_path_grd)) {
  	                  file.remove(tfile_path_grd)
  	                  file.remove(tfile_path_gri)
  	                }
  	                file.copy(from = ofile_path_grd, to = tfile_path_grd, overwrite = TRUE)
  	                file.copy(from = ofile_path_gri, to = tfile_path_gri, overwrite = TRUE)
  	              }
  	            }
  	          }
  	        }
  	      }
  	    }
  	  } else {
  	    mlist <- input$SDM_HA_AO_SDM_PRED_model
	      PATH_MODEL_OUTPUT <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$SDM_HA_AO_MO_Dir)
	      for (s in slist) {
	        for (d in dlist) {
	          for (c in clist) {
	            for (m in mlist) {
	              org_path <- PATH_MODEL_OUTPUT
	              target_path <- file.path(G$SE_Dir_Climate, ylist[1])
	              o_file_grd <- paste("HA_SR_", d, "_", c, "_", m, "_", ylist[1], G$IMG_File, sep = "")
	              o_file_gri <- paste("HA_SR_", d, "_", c, "_", m, "_", ylist[1], G$IMG_Info, sep = "")
	              t_file_grd <- paste("SpeciesRichness.grd")
	              t_file_gri <- paste("SpeciesRichness.gri")
	              ofile_path_grd <- file.path(org_path, o_file_grd)
	              ofile_path_gri <- file.path(org_path, o_file_gri)
	              tfile_path_grd <- file.path(target_path, t_file_grd)
	              tfile_path_gri <- file.path(target_path, t_file_gri)
	              if (!file.exists(ofile_path_grd)){
	                showModal(modalDialog(
	                  title = "Error Message",
	                  paste(ofile_path_grd, "does not exist.")
	                ))
	              } else {
	                if (file.exists(tfile_path_grd)) {
	                  file.remove(tfile_path_grd)
	                  file.remove(tfile_path_gri)
	                }
	                file.copy(from = ofile_path_grd, to = tfile_path_grd, overwrite = TRUE)
	                file.copy(from = ofile_path_gri, to = tfile_path_gri, overwrite = TRUE)
	              }
	              for (y in ylist) {
	                incProgress(1/tl, detail = paste("Doing part", "(", s, ")", "_", d, "_", c, "_", y))
	                org_path <- PATH_MODEL_OUTPUT
	                target_path <- file.path(G$SE_Dir_Climate, d, c, y)
	                o_file_grd <- paste("HA_SR_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")
	                o_file_gri <- paste("HA_SR_", d, "_", c, "_", m, "_", y, G$IMG_Info, sep = "")
	                t_file_grd <- paste("SpeciesRichness.grd")
	                t_file_gri <- paste("SpeciesRichness.gri")
	                ofile_path_grd <- file.path(org_path, o_file_grd)
	                ofile_path_gri <- file.path(org_path, o_file_gri)
	                tfile_path_grd <- file.path(target_path, t_file_grd)
	                tfile_path_gri <- file.path(target_path, t_file_gri)
	                if (!file.exists(ofile_path_grd)){
	                  showModal(modalDialog(
	                    title = "Error Message",
	                    paste(ofile_path_grd, "does not exist.")
	                  ))
	                } else {
	                  if (file.exists(tfile_path_grd)) {
	                    file.remove(tfile_path_grd)
	                    file.remove(tfile_path_gri)
	                  }
	                  file.copy(from = ofile_path_grd, to = tfile_path_grd, overwrite = TRUE)
	                  file.copy(from = ofile_path_gri, to = tfile_path_gri, overwrite = TRUE)
	                }
	              }
	            }
	          }
	        }
	      }
	    } 
    }
    })
	  G$SDM_MO_Variables_Folder <- file.path(G$SE_Dir_Climate, G$Var_Current_Folder)
	})
	
	
	observeEvent(input$SDM_MO_Dir_Folder, {
	  
	  PATH_PROJECT <- G$SE_Dir_Project
	  
	  # creating Species_Distribution output path
	  if (dir.exists(file.path(PATH_PROJECT, G$DIR_NAME_Species))) {
	    cat(paste(G$DIR_NAME_Species, "exists in", PATH_PROJECT, "and is a directory"))
	  } else if (file.exists(file.path(PATH_PROJECT, G$DIR_NAME_Species))) {
	    cat(paste(G$DIR_NAME_Species, "exists exists in", PATH_PROJECT, "but is a file"))
	  } else {
	    cat(paste(G$DIR_NAME_Species, "does not exist in", PATH_PROJECT, "- creating"))
	    dir.create(file.path(PATH_PROJECT, G$DIR_NAME_Species))
	  }

	  # creating Habitat output path
	  if (dir.exists(file.path(PATH_PROJECT, G$DIR_NAME_Habitat))) {
	    cat(paste(G$DIR_NAME_Habitat, "exists in", PATH_PROJECT, "and is a directory"))
	  } else if (file.exists(file.path(PATH_PROJECT, G$DIR_NAME_Habitat))) {
	    cat(paste(G$DIR_NAME_Habitat, "exists exists in", PATH_PROJECT, "but is a file"))
	  } else {
	    cat(paste(G$DIR_NAME_Habitat, "does not exist in", PATH_PROJECT, "- creating"))
	    dir.create(file.path(PATH_PROJECT, G$DIR_NAME_Habitat))
	  }
	  
	  volumes <- c(main = file.path(PATH_PROJECT, G$DIR_NAME_Species))
	  shinyDirChoose(input, 'SDM_MO_Dir_Folder', roots = volumes) # , defaultPath = "/MOTIVE_projects", defaultRoot = G$SE_Dir_Project)
	  G$SDM_MO_Dir_Folder <<- parseDirPath(volumes, input$SDM_MO_Dir_Folder)
	  output$SDM_MO_Dir_Folder <- renderText({G$SDM_MO_Dir_Folder})
	})
	
	observeEvent(input$SDM_MO_SDM_run, {
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  dlist <- input$SDM_MO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$SDM_MO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  ylist <- input$SDM_MO_Project_year  # c(G$Var_Current_Folder, "2050") # c(G$Var_Current_Folder, "2050", "2070")
#	  slist <- as.character(G_FILE_speciesinfo[input$SDM_SP_Info_rows_selected, , drop = FALSE]$ID) #c("S251") # input$SDM_MO_Species  # c("S251") # c("S015", "S134", "S145")
	  slist <- s_id  # as.character(G_FILE_speciesinfo[input$SDM_SP_Info_rows_selected, , drop = FALSE][[G$SE_Species_ID]])
	  
	  n <- 0
	  ld <- length(dlist)
	  lc <- length(clist)
	  ly <- length(ylist)
	  ls <- length(slist)
	  tl <- ld * lc * ly * ls
	  
	  withProgress(message = 'Runing SDM model.........', value = 0, {
	    
	    ##############################################################
	    ### Species Distribution Modeling
	    ### Biomod2
	    ###
	    ### by Changwan Seo
	    ##############################################################
	    
	    #####=========================================================
	    ##### Setting variables ======================================
	    
	    # setting Paths
	    PATH_SPECIES   <- G$SE_Dir_Species
	    PATH_ENV       <- G$SE_Dir_Climate
	    PATH_ENV_INPUT <- file.path(PATH_ENV, G$Var_Current_Folder, sep = "")
	    #		PATH_PROJECT   <- G$SDM_MO_Dir_Folder # G$SE_Dir_Project

	    PATH_MODEL_OUTPUT  <- G$SDM_MO_Dir_Folder
	    
	    file.copy(file.path(getwd(), "maxent.jar"), PATH_MODEL_OUTPUT)
	    setwd(PATH_MODEL_OUTPUT)
	    # Setting Column Name of species data
	    NAME_ID <- G$SE_Species_ID  # "ID"
	    NAME_SPECIES <- G$SE_Species_Name  # "K_NAME"
	    NAME_LONG <- G$SE_Species_Location_Longitude  # "Longitude"
	    NAME_LAT <- G$SE_Species_Location_Latitude  # "Latitude"
	    
	    # setting speices and environmental data
	    FILE_SPECIES_NAME <<- input$SE_speciesindex   # G$SE_speciesindex
	    FILE_SPECIES_LOCATION <<- input$SE_specieslocation  # G$SE_specieslocation
	    ENV_VARIABLES <- input$SDM_MO_Variables
	    CAT_VARIABLES <- input$SDM_MO_Variables_Categorical
	    CON_VARIABLES <- setdiff(ENV_VARIABLES,CAT_VARIABLES)
	    
	    CUR_PATH <- getwd()
	    setwd(PATH_ENV_INPUT)
	    if (length(ENV_VARIABLES) != 0) {
	      v_stack <- ""
	      if (length(CON_VARIABLES) != 0) {
	        for (i in CON_VARIABLES) {
	          v1 <- raster(i)
	          v_stack <- c(v_stack, v1)
	        }
	      }
	      if (length(CAT_VARIABLES) != 0) {
	        for (j in CAT_VARIABLES) {
	          v2 <- as.factor(raster(j))
	          v_stack <- c(v_stack, v2)
	        }
	      }
	      SDM_VARIABLES <- v_stack[-1]
	    }
	    setwd(CUR_PATH)
	    
	    # Defining Models Data Options using default options.
	    BIOMOD_eval.resp.var <- NULL #input$BIOMOD_eval.resp.var
	    BIOMOD_eval.expl.var <- NULL #input$BIOMOD_eval.expl.var
	    BIOMOD_eval.resp.xy <- NULL #input$BIOMOD_eval.resp.xy
	    BIOMOD_PA.nb.rep <- input$BIOMOD_PA.nb.rep
	    BIOMOD_PA.nb.absences <- input$BIOMOD_PA.nb.absences
	    BIOMOD_PA.strategy <- input$BIOMOD_PA.strategy
	    BIOMOD_PA.dist.min <- input$BIOMOD_PA.dist[1]
	    BIOMOD_PA.dist.max <- input$BIOMOD_PA.dist[2]
	    BIOMOD_PA.sre.quant <- input$BIOMOD_PA.sre.quant
	    BIOMOD_PA.table <- NULL #input$BIOMOD_PA.table
	    BIOMOD_na.rm <- input$BIOMOD_na.rm
	    
	    # Defining Models Options using default options.
	    BIOMOD_models <- input$SDM_MO_SDM_model # c('GAM', 'GLM')  # c('MAXENT.Phillips') 
	    BIOMOD_models.options <- BIOMOD_ModelingOptions()
	    BIOMOD_NbRunEval <- input$BIOMOD_NbRunEval
	    BIOMOD_DataSplit <- input$BIOMOD_DataSplit
	    BIOMOD_Yweights <- NULL #input$BIOMOD_Yweights
	    BIOMOD_VarImport <- input$BIOMOD_VarImport
	    BIOMOD_models.eval.meth <<- input$BIOMOD_models.eval.meth # c("ROC", "TSS", "KAPPA") # 
	    BIOMOD_SaveObj <- input$BIOMOD_SaveObj
	    BIOMOD_rescal.all.models <- input$BIOMOD_rescal.all.models
	    BIOMOD_do.full.models <- input$BIOMOD_do.full.models
	    
	    # Defining projection Options using default options.
	    BIOMOD_selected.models <- input$BIOMOD_selected.models
	    BIOMOD_binary.meth <<- input$BIOMOD_binary.meth # c("ROC", "TSS", "KAPPA") # 
	    BIOMOD_compress <- input$BIOMOD_compress
	    BIOMOD_build.clamping.mask <- input$BIOMOD_build.clamping.mask
	    BIOMOD_output.format <- input$BIOMOD_output.format
	    BIOMOD_do.stack <- input$BIOMOD_do.stack
	    
	    # Defining ensemble modelling Options using default options.
	    EM_chosen.models <- input$EM_chosen.models
	    EM_em.by <- input$EM_em.by
	    EM_eval.metric <- input$EM_eval.metric
	    EM_eval.metric.quality.threshold <- NULL #input$EM_eval.metric.quality.threshold
	    EM_models.eval.meth = input$EM_models.eval.meth
	    EM_prob.mean <- input$EM_prob.mean
	    EM_prob.cv <- input$EM_prob.cv
	    EM_prob.ci <- input$EM_prob.ci
	    EM_prob.ci.alpha <- input$EM_prob.ci.alpha
	    EM_prob.median <- input$EM_prob.median
	    EM_committee.averaging <- input$EM_committee.averaging
	    EM_prob.mean.weight <- input$EM_prob.mean.weight
	    EM_prob.mean.weight.decay <- input$EM_prob.mean.weight.decay
	    EM_VarImport <- input$EM_VarImport
	    ##### End Setting variables ==================================
	    #####=========================================================
	    
	    #####=========================================================
	    ##### Setting path and data ==================================
	    # creating working a project
	    
	    # Loading speices data
#	    DATA_SPECIES_NAME <- read.table(file.path(PATH_SPECIES, FILE_SPECIES_NAME), header = T, sep = ",")
#	    DATA_SPECIES_LOCATION <- read.table(file.path(PATH_SPECIES, FILE_SPECIES_LOCATION), header = T, sep = ",")
	    DATA_SPECIES_NAME <- G_FILE_speciesindex
	    DATA_SPECIES_LOCATION <- G_FILE_specieslocation
	    ##### End Path and Data setting =============================
	    #####=========================================================
	    
	    #####========================================================
	    #####============ Rinning models ============================
	    #####========================================================
	    
	    #####========================================================
	    ##### Modeling loop =========================================
	    
	    for (s in slist) {
	      n <- n + 1
	      ##### Setting Environmental variables ======================= 
	      SPECIES_ID <- s
	      SPECIES_NAME <- subset(DATA_SPECIES_NAME, get(NAME_ID) == SPECIES_ID, select = c(get(NAME_SPECIES)))
	      SPECIES_NAME <- as.character(SPECIES_NAME$K_NAME)
	      SPECIES_DATA <- subset(DATA_SPECIES_LOCATION, get(NAME_ID) == SPECIES_ID, select = c(NAME_ID, NAME_LONG, NAME_LAT))
	      
	      CUR_PATH <- getwd()
	      setwd(PATH_ENV_INPUT)
	      mask <- raster(input$SDM_MO_Variables[1])
	      crs(mask) <- CRS(G$Projection_Info)
	      mask[!is.na(mask)] <- 1
	      setwd(CUR_PATH)
	      
	      xy <- SPECIES_DATA[,c(2,3)]
	      r <- rasterize(xy, mask, mask=TRUE, field=1)
	      r_pts <- rasterToPoints(r, spatial=TRUE)
	      t <- r_pts@coords
	      SPECIES_DATA <- cbind(ID = 1, t)
	      colnames(SPECIES_DATA) <- c(NAME_ID, NAME_LONG, NAME_LAT)
	      
#	      myResp <- as.numeric(SPECIES_DATA[,NAME_ID] <- 1)
#	      myResp <- as.numeric(SPECIES_DATA[,NAME_ID])
	      
	      myResp <- as.numeric(SPECIES_DATA[,NAME_ID])
	      
#	      CUR_PATH <- getwd()
#	      setwd(PATH_ENV_INPUT)
	      myExpl <- stack(SDM_VARIABLES)  #stack(ENV_VARIABLES)
#	      setwd(CUR_PATH)
	      
	      myRespXY <- SPECIES_DATA[,c(NAME_LONG, NAME_LAT)]
	      myRespName <- SPECIES_NAME
	      ##### End Setting Environmental variables ===================         
	      
	      ##### BIOMOD ================================================
	      ### Formatting Data
	      myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
	                                           expl.var = myExpl,
	                                           resp.xy = myRespXY,
	                                           resp.name = myRespName,
	                                           eval.resp.var = BIOMOD_eval.resp.var,
	                                           eval.expl.var = BIOMOD_eval.expl.var,
	                                           eval.resp.xy = BIOMOD_eval.resp.xy,
	                                           PA.nb.rep = BIOMOD_PA.nb.rep,
	                                           PA.nb.absences = BIOMOD_PA.nb.absences,
	                                           PA.strategy = BIOMOD_PA.strategy,
	                                           PA.dist.min = BIOMOD_PA.dist.min,
	                                           PA.dist.max = BIOMOD_PA.dist.max,
	                                           PA.sre.quant = BIOMOD_PA.sre.quant,
	                                           PA.table = BIOMOD_PA.table,
	                                           na.rm = BIOMOD_na.rm)
	      ### End Formatting Data
	      
	      ### Modeling BIOMOD
	      # Running BIOMOD
	      myBiomodModelOut <- BIOMOD_Modeling( myBiomodData,
	                                           models = BIOMOD_models,
	                                           models.options = BIOMOD_models.options,
	                                           NbRunEval = BIOMOD_NbRunEval,
	                                           DataSplit = BIOMOD_DataSplit, 
	                                           Yweights = BIOMOD_Yweights, 
	                                           VarImport = BIOMOD_VarImport, 
	                                           models.eval.meth = BIOMOD_models.eval.meth, 
	                                           SaveObj = BIOMOD_SaveObj, 
	                                           rescal.all.models = BIOMOD_rescal.all.models, 
	                                           do.full.models = BIOMOD_do.full.models)
	      
	      # creating Biomod2 output path
	      if (dir.exists(file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM))) {
	        cat(paste(G$DIR_NAME_SDM, "exists in", PATH_MODEL_OUTPUT, "/", SPECIES_NAME, "and is a directory"))
	      } else if (file.exists(file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM))) {
	        cat(paste(G$DIR_NAME_SDM, "exists in", PATH_MODEL_OUTPUT, "/", SPECIES_NAME, "but is a file"))
	      } else {
	        cat(paste(G$DIR_NAME_SDM,"does not exist in", PATH_MODEL_OUTPUT, "/", SPECIES_NAME, "- creating"))
	        dir.create(file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM))
	      }
	      
	      # Evaluating the model
	      myBiomodModelEval <- get_evaluations(myBiomodModelOut)
	      write.csv(myBiomodModelEval, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(SPECIES_NAME, "_eval.csv", sep = "", collapse = "--")))
	      myBiomodModelImport <- get_variables_importance(myBiomodModelOut)
	      write.csv(myBiomodModelImport, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(SPECIES_NAME, "_impot.csv",  sep = "", collapse = "--")))
	      ### End Modeling BIOMOD
	      
	      ### Projection on current and future environemental conditions
	      # Projecting loop
	      for (d in dlist) {
	        for (c in clist) {
	          for (y in ylist) {
	            incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", SPECIES_NAME, ")", "_", d, "_", c, "_", y))
	            PATH_ENV_OUTPUT <- file.path(PATH_ENV, d, c, y, sep = "")
	            
	            # Setting Projection Environmenta variables
	            CUR_PATH <- getwd()
	            setwd(PATH_ENV_OUTPUT)
	            myExpl <- stack(ENV_VARIABLES)
	            setwd(CUR_PATH)
	            
	            # Defining projection Options using default options.
	            BIOMOD_proj.name = paste(d, "_", c, "_", y, sep = "")
	            
	            # Running BIOMOD projection
	            myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
	                                                    new.env = myExpl,
	                                                    proj.name = BIOMOD_proj.name,
	                                                    selected.models = BIOMOD_selected.models,
	                                                    binary.meth = BIOMOD_binary.meth,
	                                                    compress = BIOMOD_compress,
	                                                    build.clamping.mask = BIOMOD_build.clamping.mask, 
	                                                    output.format = BIOMOD_output.format,
	                                                    do.stack = BIOMOD_do.stack)
	            
	            # save projections and prodictions
	            all_proj <- get_predictions(myBiomodProjection)
	            mod_proj <- get_projected_models(myBiomodProjection)
	            sel_proj <- grep("Full", mod_proj, value = TRUE)
	            mlist <- c(sel_proj)
	            for (i in mlist) {
	              proj <- all_proj[[i]]
	              proj[proj > 1000] <- 1000
	              writeRaster(proj, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(as.name(paste("PROJ_", BIOMOD_proj.name, "_", i, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	              for (j in BIOMOD_binary.meth) {
	                if (!is.na(myBiomodModelEval[j, "Cutoff", strapplyc(i, "Full_(.*)", simplify = TRUE), "Full", "PA1"])) {
	                  cutoffvalue <- as.integer(myBiomodModelEval[j, "Cutoff", strapplyc(i, "Full_(.*)", simplify = TRUE), "Full", "PA1"])
	                  pred <- BinaryTransformation(proj, cutoffvalue)
	                  writeRaster(pred, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM,paste(as.name(paste("PRED_", BIOMOD_proj.name, "_", i, "_by", j, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                }
	              }  
	            }  
	            ### End Projection on current and future environemental conditions
	            ##### ENd BIOMOD ================================================
	            
	            ##### Modeling ensemble model ===================================
	            # Runing ensemble modelling
	            
	            if(input$SDM_MO_SDM_EMmodel) {
	              myBiomodEM <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,
	                                                    chosen.models = EM_chosen.models,
	                                                    em.by = EM_em.by,
	                                                    eval.metric = EM_eval.metric,
	                                                    eval.metric.quality.threshold = EM_eval.metric.quality.threshold,
	                                                    models.eval.meth = EM_models.eval.meth,
	                                                    prob.mean = EM_prob.mean,
	                                                    prob.cv = EM_prob.cv,
	                                                    prob.ci = EM_prob.ci,
	                                                    prob.ci.alpha = EM_prob.ci.alpha,
	                                                    prob.median = EM_prob.median,
	                                                    committee.averaging = EM_committee.averaging,
	                                                    prob.mean.weight = EM_prob.mean.weight,
	                                                    prob.mean.weight.decay = EM_prob.mean.weight.decay,
	                                                    VarImport = EM_VarImport)
	              
	              # get evaluation scores
	              myBiomodEMEval <- get_evaluations(myBiomodEM)
	              write.csv(myBiomodEMEval, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(SPECIES_NAME, "_EM_eval.csv", sep = "", collapse = "--")))
	              
	              # Ensemble Models Projections
	              myBiomodEnsembleForecasting <- BIOMOD_EnsembleForecasting(projection.output = myBiomodProjection,
	                                                                        EM.output = myBiomodEM)
	              
	              # save projections and prodictions projections and prodictions
	              EM_all_proj <- get_predictions(myBiomodEnsembleForecasting)
	              EM_mod_proj <- get_projected_models(myBiomodEnsembleForecasting)
	              EM_sel_proj <- grep(SPECIES_NAME, EM_mod_proj, value = TRUE)
	              emlist <- c(EM_sel_proj)
	              for (i in emlist) {
	                proj <- EM_all_proj[[i]]
	                proj[proj > 1000] <- 1000
	                writeRaster(proj, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(as.name(paste("PROJ_", BIOMOD_proj.name, "_", i, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                for (j in EM_models.eval.meth) {
	                  if (!is.na(eval(parse(text = as.name(paste("myBiomodEMEval$", i))))[j, "Cutoff"])) {
	                    cutoffvalue <- as.integer(eval(parse(text = as.name(paste("myBiomodEMEval$", i))))[j, "Cutoff"])
	                    pred <- BinaryTransformation(proj, cutoffvalue)
	                    writeRaster(pred, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(as.name(paste("PRED_", BIOMOD_proj.name, "_", i, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                  }
	                }
	              }
	            }
	            ##### End ensemble modelling =================================
	            
	          } # End Year loop y
	        } # End climate change Scenarios loop c
	      } # End climate data loop d
	      
	      ### Creating species evaluation information 
	      destfile <- file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(as.name(paste(SPECIES_NAME, "_eval.csv", sep = "")), sep = "", collapse = "--"))
	      if (!file.exists(destfile))
	        return
	      
	      old_eval <- read.csv(destfile)
	      lc <- length(colnames(old_eval))
	      lr <- length(row.names(old_eval))
	      nc <- (lc - 1) / 4
	      nr <- nc * lr
	      
	      new_eval <- setNames(data.frame(matrix(ncol = 6, nrow = nr)), c("Model", "Type", "Accuracy", "Cutoff", "Sensitivity", "specificity")) 
	      
	      for (i in 1:nc) {
	        k <- (2 + (i * 4)) - 4  
	        ek <- i*lr
	        sk <- ek - (lr - 1)  
	        new_eval$Model[sk:ek] <- sub(".*(Testing.data.)", "", colnames(old_eval[k]))
	        
	        n <- 0
	        for (j in sk:ek) {
	          n <- n + 1
	          new_eval$Type[j] <- as.character(old_eval[n,1])
	          new_eval$Accuracy[j] <- old_eval[n,k]
	          new_eval$Cutoff[j] <- old_eval[n,k+1]
	          new_eval$Sensitivity[j] <- old_eval[n,k+2]
	          new_eval$specificity[j] <- old_eval[n,k+3]
	        }
	      }
	      
	      if(input$SDM_MO_SDM_EMmodel) {
	        destfile <- file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(as.name(paste(SPECIES_NAME, "_EM_eval.csv", sep = "")), sep = "", collapse = "--"))
	        if (!file.exists(destfile))
	          return
	        
	        old_EM_eval <- read.csv(destfile)
	        lc <- length(colnames(old_EM_eval))
	        lr <- length(row.names(old_EM_eval))
	        nc <- (lc - 1) / 4
	        nr <- nc * lr
	        
	        new_EM_eval <- setNames(data.frame(matrix(ncol = 6, nrow = nr)), c("Model", "Type", "Accuracy", "Cutoff", "Sensitivity", "specificity")) 
	        
	        for (i in 1:nc) {
	          k <- (2 + (i * 4)) - 4  
	          ek <- i*lr
	          sk <- ek - (lr - 1)  
	          new_EM_eval$Model[sk:ek] <- sub(paste0(".*(", SPECIES_NAME, "_)"), "", sub("(.Testing.data).*", "", colnames(old_EM_eval[k])))
	          
	          n <- 0
	          for (j in sk:ek) {
	            n <- n + 1
	            new_EM_eval$Type[j] <- as.character(old_EM_eval[n,1])
	            new_EM_eval$Accuracy[j] <- old_EM_eval[n,k]
	            new_EM_eval$Cutoff[j] <- old_EM_eval[n,k+1]
	            new_EM_eval$Sensitivity[j] <- old_EM_eval[n,k+2]
	            new_EM_eval$specificity[j] <- old_EM_eval[n,k+3]
	          }
	        }
	      }
	      
	      if(exists("new_eval") && exists("new_EM_eval")) {
	        all_eval <- rbind(new_eval, new_EM_eval)
	      } else {
	        all_eval <- new_eval
	      }
	      
	      Eval_data <- all_eval
	      for (i in 1:length(all_eval[,1])) {
	        if (grepl("EM", Eval_data$Model[i])) {
	          Eval_data$Projection[i] <- Eval_data$Model[i]
	          Eval_data$Prediction[i] <- Eval_data$Model[i]
	        } else if (grepl("MAXENT", Eval_data$Model[i])) {
	          a1 <- sub("\\..*", "", Eval_data$Model[i])
	          a234 <- sub(".*?\\.", "", Eval_data$Model[i])
	          a2 <- sub("\\..*", "", a234)
	          a34 <- sub(".*?\\.", "", a234)
	          a3 <- sub("\\..*", "", a34)
	          a4 <- sub(".*\\.", "", a34)
	          Projection <- paste(a4, "_", a3, "_", a1, ".", a2, sep="")
	          Prediction <- paste(a4, "_", a3, "_", a1, ".", a2, "_by", Eval_data$Type[i], sep="")
	          Eval_data$Projection[i] <- Projection
	          Eval_data$Prediction[i] <- Prediction
	        } else{
	          a1 <- sub("\\..*", "", Eval_data$Model[i])
	          a23 <- sub(".*?\\.", "", Eval_data$Model[i])
	          a2 <- sub("\\..*", "", a23)
	          a3 <- sub(".*\\.", "", a23)
	          Projection <- paste(a3, "_", a2, "_", a1, sep="")
	          Prediction <- paste(a3, "_", a2, "_", a1, "_by", Eval_data$Type[i], sep="")
	          Eval_data$Projection[i] <- Projection
	          Eval_data$Prediction[i] <- Prediction
	        }	  
	      }
	      
	      write.csv(Eval_data, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(SPECIES_NAME, "_ALL_eval.csv", sep = "", collapse = "--")))
	      
	      #####
	      
#	      destfile <- file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(SPECIES_NAME, "_BIOMOD2_variables.csv", sep = "", collapse = "--"))

          SDM_variables <- setNames(data.frame(matrix(ncol = 44, nrow = 30)), c("input$SDM_MO_Climate_model", "input$SDM_MO_Climate_scenario", "input$SDM_MO_Project_year", "input$SDM_MO_Variables", "input$BIOMOD_eval.resp.var", 
	                                                                              "input$BIOMOD_eval.expl.var", "input$BIOMOD_eval.resp.xy", "input$BIOMOD_PA.nb.rep", "input$BIOMOD_PA.nb.absences", "input$BIOMOD_PA.strategy",
	                                                                              "input$BIOMOD_PA.dist.min", "input$BIOMOD_PA.dist.max", "input$BIOMOD_PA.sre.quant", "input$BIOMOD_PA.table", "input$BIOMOD_na.rm",
	                                                                              "input$SDM_MO_SDM_model", "input$BIOMOD_NbRunEval", "input$BIOMOD_DataSplit", "input$BIOMOD_Yweights", "input$BIOMOD_VarImport",
	                                                                              "input$BIOMOD_models.eval.meth", "input$BIOMOD_SaveObj", "input$BIOMOD_rescal.all.models", "input$BIOMOD_do.full.models",
	                                                                              "input$BIOMOD_selected.models", "input$BIOMOD_binary.meth", "input$BIOMOD_compress", "input$BIOMOD_build.clamping.mask", "input$BIOMOD_output.format",
	                                                                              "input$BIOMOD_do.stack", "input$EM_chosen.models", "input$EM_em.by", "input$EM_eval.metric", "input$EM_eval.metric.quality.threshold",
	                                                                              "input$EM_models.eval.meth", "input$EM_prob.mean", "input$EM_prob.cv", "input$EM_prob.ci", "input$EM_prob.ci.alpha",
	                                                                              "input$EM_prob.median", "input$EM_committee.averaging", "input$EM_prob.mean.weight", "input$EM_prob.mean.weight.decay", "input$EM_VarImport"
	                                                                              ))
	            
          SDM_variables[1:length(input$SDM_MO_Climate_model), "input$SDM_MO_Climate_model"] <- input$SDM_MO_Climate_model
          SDM_variables[1:length(input$SDM_MO_Climate_scenario), "input$SDM_MO_Climate_scenario"] <- input$SDM_MO_Climate_scenario
          SDM_variables[1:length(input$SDM_MO_Project_year), "input$SDM_MO_Project_year"] <- input$SDM_MO_Project_year
          SDM_variables[1:length(input$SDM_MO_Variables), "input$SDM_MO_Variables"] <- input$SDM_MO_Variables
          SDM_variables[1:1, "input$BIOMOD_eval.resp.var"] <- "NULL"
          SDM_variables[1:1, "input$BIOMOD_eval.expl.var"] <- "NULL"
          SDM_variables[1:1, "input$BIOMOD_eval.resp.xy"] <- "NULL"
          SDM_variables[1:length(input$BIOMOD_PA.nb.rep), "input$BIOMOD_PA.nb.rep"] <- input$BIOMOD_PA.nb.rep
          SDM_variables[1:length(input$BIOMOD_PA.nb.absences), "input$BIOMOD_PA.nb.absences"] <- input$BIOMOD_PA.nb.absences
          SDM_variables[1:length(input$BIOMOD_PA.strategy), "input$BIOMOD_PA.strategy"] <- input$BIOMOD_PA.strategy
          SDM_variables[1:length(input$BIOMOD_PA.dist.min), "input$BIOMOD_PA.dist.min"] <- input$BIOMOD_PA.dist[1]
          SDM_variables[1:length(input$BIOMOD_PA.dist.max), "input$BIOMOD_PA.dist.max"] <- input$BIOMOD_PA.dist[2]
          SDM_variables[1:length(input$BIOMOD_PA.sre.quant), "input$BIOMOD_PA.sre.quant"] <-  input$BIOMOD_PA.sre.quant
          SDM_variables[1:1, "input$BIOMOD_PA.table"] <- "NULL"
          SDM_variables[1:length(input$BIOMOD_na.rm), "input$BIOMOD_na.rm"] <- input$BIOMOD_na.rm
          
          SDM_variables[1:length(input$SDM_MO_SDM_model), "input$SDM_MO_SDM_model"] <- input$SDM_MO_SDM_model
          SDM_variables[1:length(input$BIOMOD_NbRunEval), "input$BIOMOD_NbRunEval"] <- input$BIOMOD_NbRunEval
          SDM_variables[1:length(input$BIOMOD_DataSplit), "input$BIOMOD_DataSplit"] <- input$BIOMOD_DataSplit
          SDM_variables[1:1, "input$BIOMOD_Yweights"] <- "NULL"
          SDM_variables[1:length(input$BIOMOD_VarImport), "input$BIOMOD_VarImport"] <- input$BIOMOD_VarImport
          SDM_variables[1:length(input$BIOMOD_models.eval.meth), "input$BIOMOD_models.eval.meth"] <- input$BIOMOD_models.eval.meth
          SDM_variables[1:length(input$BIOMOD_SaveObj), "input$BIOMOD_SaveObj"] <- input$BIOMOD_SaveObj
          SDM_variables[1:length(input$BIOMOD_rescal.all.models), "input$BIOMOD_rescal.all.models"] <- input$BIOMOD_rescal.all.models
          SDM_variables[1:length(input$BIOMOD_do.full.models), "input$BIOMOD_do.full.models"] <- input$BIOMOD_do.full.models
          SDM_variables[1:length(input$BIOMOD_selected.models), "input$BIOMOD_selected.models"] <- input$BIOMOD_selected.models
	          
          SDM_variables[1:length(input$BIOMOD_binary.meth), "input$BIOMOD_binary.meth"] <- input$BIOMOD_binary.meth
          SDM_variables[1:length(input$BIOMOD_compress), "input$BIOMOD_compress"] <- input$BIOMOD_compress
          SDM_variables[1:length(input$BIOMOD_build.clamping.mask), "input$BIOMOD_build.clamping.mask"] <- input$BIOMOD_build.clamping.mask
          SDM_variables[1:length(input$BIOMOD_output.format), "input$BIOMOD_output.format"] <- input$BIOMOD_output.format
          SDM_variables[1:length(input$BIOMOD_do.stack), "input$BIOMOD_do.stack"] <- input$BIOMOD_do.stack
          SDM_variables[1:length(input$EM_chosen.models), "input$EM_chosen.models"] <- input$EM_chosen.models
          SDM_variables[1:length(input$EM_em.by), "input$EM_em.by"] <- input$EM_em.by
          SDM_variables[1:length(input$EM_eval.metric), "input$EM_eval.metric"] <- input$EM_eval.metric
          SDM_variables[1:1, "input$EM_eval.metric.quality.threshold"] <- "NULL"
          SDM_variables[1:length(input$EM_models.eval.meth), "input$EM_models.eval.meth"] <- input$EM_models.eval.meth
	          
          SDM_variables[1:length(input$EM_prob.mean), "input$EM_prob.mean"] <- input$EM_prob.mean
          SDM_variables[1:length(input$EM_prob.cv), "input$EM_prob.cv"] <- input$EM_prob.cv
          SDM_variables[1:length(input$EM_prob.ci), "input$EM_prob.ci"] <- input$EM_prob.ci
          SDM_variables[1:length(input$EM_prob.ci.alpha), "input$EM_prob.ci.alpha"] <- input$EM_prob.ci.alpha
          SDM_variables[1:length(input$EM_prob.median), "input$EM_prob.median"] <- input$EM_prob.median
          SDM_variables[1:length(input$EM_committee.averaging), "input$EM_committee.averaging"] <- input$EM_committee.averaging
          SDM_variables[1:length(input$EM_prob.mean.weight), "input$EM_prob.mean.weight"] <- input$EM_prob.mean.weight
          SDM_variables[1:length(input$EM_prob.mean.weight.decay), "input$EM_prob.mean.weight.decay"] <- input$EM_prob.mean.weight.decay
          SDM_variables[1:length(input$EM_VarImport), "input$EM_VarImport"] <- input$EM_VarImport
	      
	        SDM_variables[is.na(SDM_variables)] <- ""
          write.csv(SDM_variables, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(SPECIES_NAME, "_", G$DIR_NAME_SDM, "_variables.csv", sep = "", collapse = "--")))
	      
          write.csv(SPECIES_DATA, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, G$DIR_NAME_SDM, paste(SPECIES_NAME, "_", G$DIR_NAME_SDM, "_species.csv", sep = "", collapse = "--")))
          
	      #####
	      
	      dir_list <- list.dirs(path = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME), full.names = FALSE, recursive = FALSE)
	      for (i in dir_list) {
	        if (i == G$DIR_NAME_SDM | i == "MIGCLIM") {
	          cat (i, "cannot be deleted")
	        } else {
	          unlink(file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, i), recursive = TRUE)
	        }
	      }
	      
	      ### End Creating species evaluation information
	      
	    } # End Speices loop s
	    
	    ##### End Modeling loop =====================================
	    #####========================================================
	    
	    #####========================================================
	    #####============ End Models Running ========================
	    #####========================================================        
	  })        
	})
	
	
	output$SDM_AO_Dir_Folder <- renderUI({
	    SDM_AO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	    SDM_AO_Dir_Folder_selected <- SDM_AO_Dir_Folder_list[1]
	    selectInput("SDM_AO_Dir", SDM_Name_Model_Out_Folder,
	                choices = c(SDM_AO_Dir_Folder_list),
	                selected = SDM_AO_Dir_Folder_selected
	    )
#	    G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
	})

	output$SDM_OU_Species <- renderUI({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		SDM_Name_Species_list <- list.dirs(path = G$SDM_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
		SDM_Name_Species_selected <- SDM_Name_Species_list[1]
		selectInput("SDM_OU_Species", SDM_Name_Model_Out_Species,
			choices = c(SDM_Name_Species_list),
			selected = SDM_Name_Species_selected
		)
	})
	
	output$SDM_OU_Projection_model <- renderUI({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM, paste(as.name(paste(input$SDM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		SDM_Name_Projection_Models_list <- unique(as.character(G_FILE_species_evaluation$Projection))
		SDM_Name_Projection_Models_selected <- SDM_Name_Projection_Models_list[1]
		selectInput("SDM_OU_Projection_model", SDM_Name_Model_Out_Model_Projection,
			choices = c(SDM_Name_Projection_Models_list),
			selected = SDM_Name_Projection_Models_selected
		)
	})
  
	output$SDM_OU_Prediction_model <- renderUI({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM, paste(as.name(paste(input$SDM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		SDM_Name_Prediction_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		SDM_Name_Prediction_Models_selected <- SDM_Name_Prediction_Models_list[1]
		selectInput("SDM_OU_Prediction_model", SDM_Name_Model_Out_Model_Prediction,
			choices = c(SDM_Name_Prediction_Models_list),
			selected = SDM_Name_Prediction_Models_selected
		)
	})

	output$SDM_OU_Validation <- DT::renderDataTable({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM, paste(as.name(paste(input$SDM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		all_eval
	})
	
	output$SDM_OU_Validation_BoxPlot <- renderPlot({
		rs <- input$SDM_OU_Validation_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			Eval_data <<- G_FILE_species_evaluation[rs, , drop = FALSE]
			ggplot(Eval_data, aes(x=Type, y=Accuracy, fill=Type)) + geom_boxplot() + scale_fill_brewer(palette = "Set2") # + facet_grid(~College) 
		}
	})
	
	output$SDM_OU_Validation_Cutoff_BoxPlot <- renderPlot({
	  rs <- input$SDM_OU_Validation_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    Eval_data <<- G_FILE_species_evaluation[rs, , drop = FALSE]
	    ggplot(Eval_data, aes(x=Type, y=Cutoff, fill=Type)) + geom_boxplot() + scale_fill_brewer(palette = "Set2") # + facet_grid(~College) 
	  }
	})
	
	output$SDM_OU_Contribution <- DT::renderDataTable({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
	  destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM, paste(as.name(paste(input$SDM_OU_Species, "_impot.csv", sep = "")), sep = "", collapse = "--"))
	  
	  if (!file.exists(destfile)) {
	    return(NULL)
	  }
	  
	  new_import <- read.csv(destfile)
	  data <- data.frame(new_import)
#	  rename(data, c("X" = "Label"))
	  
	})
	
	output$SDM_OU_Contribution_Radarchart <- renderChartJSRadar({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM, paste(as.name(paste(input$SDM_OU_Species, "_impot.csv", sep = "")), sep = "", collapse = "--"))
	
		if (!file.exists(destfile)) {
			return(NULL)
		}
	
		new_import <- read.csv(destfile)
		data <- data.frame(new_import)
#		rename(data, c("X" = "Label"))
		rs <- input$SDM_OU_Contribution_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs) > 0) {
			data <- data[rs, , drop = FALSE]
			chartJSRadar(scores = data, maxScale = 1, showToolTipLabel = TRUE)
		}
	})
  
	output$SDM_OU_Probability_map <- renderLeaflet({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM)
		Map <- paste("PROJ", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Projection_model, G$IMG_File, sep = "")
		r <- raster(file.path(dir_path, Map))
		r <- as.factor(r)
	
		MotiveEco_SDM_plot(r)
	})
	
	output$SDM_OU_PROJ_Summary <- renderPrint({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM)
		Map <- paste("PROJ", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Projection_model, G$IMG_File, sep = "")
		r <- raster(file.path(dir_path, Map))
		summary(r)
	})
	
	output$SDM_OU_PROJ_Histogram <- renderPlot({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM)
		Map <- paste("PROJ", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Projection_model, G$IMG_File, sep = "")
		x <- raster(file.path(dir_path, Map))
	
		hist(x, # breaks = bins, 
		  col="lightskyblue3",  # skyblue",
		  border="white",,
			xlab = "Projected Value",
			main = "Histogram")
	})
	
	output$SDM_OU_Predicted_map <- renderLeaflet({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM)
		Map <- paste("PRED", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Prediction_model, G$IMG_File, sep = "")
		r <- raster(file.path(dir_path, Map))
		r <- as.factor(r)
		
		MotiveEco_SDM_plot(r)
	})  
	
	output$SDM_OU_PRED_Summary <- renderPrint({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM)
		Map <- paste("PRED", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Prediction_model, G$IMG_File, sep = "")
		r <- raster(file.path(dir_path, Map))
		summary(r)
	})
	
	output$SDM_OU_PRED_Histogram <- renderPlot({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, G$DIR_NAME_SDM)
		Map <- paste("PRED", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Prediction_model, G$IMG_File, sep = "")
		x <- raster(file.path(dir_path, Map))
	
		hist(x, # breaks = bins, 
		  col = G$COL_CODE_PLOT_Ramp2, 
		  border = "white",
			xlab = "Predicted Value",
			main = "Histogram")
	})
	
	output$SRM_SDM_Dir_Folder <- renderUI({
	    SRM_SDM_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	    SRM_SDM_Dir_Folder_selected <- SRM_SDM_Dir_Folder_list[1]
	    selectInput("SRM_SDM_Dir", SRM_Name_Model_SDM_Folder,
	                choices = c(SRM_SDM_Dir_Folder_list),
	                selected = SRM_SDM_Dir_Folder_selected
	    )
	    
	})
	
	observeEvent(input$SRM_MO_Species_sel_all, {
	    G$SRM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_SDM_Dir)
	    SRM_Name_Species_list <- list.dirs(path = G$SRM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$SRM_Name_Species_selected <<- SRM_Name_Species_list
	})
	
	observeEvent(input$SRM_MO_Species_sel_none, {
	    G$SRM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_SDM_Dir)
	    SRM_Name_Species_list <- list.dirs(path = G$SRM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$SRM_Name_Species_selected <<- ""
	})
	
	output$SRM_MO_Species <- renderUI({
	    G$SRM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_SDM_Dir)
	    SRM_Name_Species_list <- list.dirs(path = G$SRM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    checkboxGroupInput("SRM_MO_Species", SRM_Name_Model_SDM_Species,
	                       choices = c(SRM_Name_Species_list),
	                       selected = G$SRM_Name_Species_selected
	    )
	})
	
	output$SRM_MO_SDM_model <- renderUI({
	    G$SRM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_SDM_Dir)
#	    SRM_Name_Species_list <- list.dirs(path = G$SRM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
#	    destfile <- file.path(G$SRM_SDM_Dir_Folder, SRM_Name_Species_list[1], G$DIR_NAME_SDM, paste(as.name(paste(SRM_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	    destfile <- file.path(G$SRM_SDM_Dir_Folder, input$SRM_MO_Species[1], G$DIR_NAME_SDM, paste(as.name(paste(input$SRM_MO_Species[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	    all_eval <- read.csv(destfile)
	    G_FILE_species_evaluation <<- all_eval
	    SRM_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
	    SRM_Name_Models_selected <- SRM_Name_Models_list[1]
	    checkboxGroupInput("SRM_MO_SDM_model", SRM_Name_Model_SDM_Model,
	                       choices = c(SRM_Name_Models_list),
	                       selected = SRM_Name_Models_selected
	    )
	})	
	
	observeEvent(input$SRM_MO_Action_run, {
	    
	    # setting Climate change scenarios, Future time, Species and current environmental path
	    dlist <- input$SRM_MO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	    clist <- input$SRM_MO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	    ylist <- input$SRM_MO_Project_year  # c(G$Var_Current_Folder, "2050") # c(G$Var_Current_Folder, "2050", "2070")
	    slist <- input$SRM_MO_Species
	    mlist <- input$SRM_MO_SDM_model
	    
	    n <- 0
	    ld <- length(dlist)
	    lc <- length(clist)
	    ly <- length(ylist)
	    ls <- length(slist)
	    lm <- length(mlist)
	    tl <- ld * lc * lm * ls * ly
	    
	    withProgress(message = 'Runing SRM model.........', value = 0, {
	        
	        ##############################################################
	        ### Species Range Modeling
	        ### Home range
	        ###
	        ### by Changwan Seo
	        ##############################################################
	        
	        #####=========================================================
	        ##### Setting variables ======================================
	        
	        # setting Paths
	        PATH_PROJECT   <- G$SE_Dir_Project
	        G$SRM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_SDM_Dir)
	        PATH_MODEL_OUTPUT <- G$SRM_SDM_Dir_Folder   # file.path(PATH_PROJECT, G$DIR_NAME_Species)
	        CUR_PATH <- getwd()
	        setwd(PATH_MODEL_OUTPUT)
	        
	        # Setting Column Name of species data
	        NAME_ID <- G$SE_Species_ID  # "ID"
	        NAME_SPECIES <- G$SE_Species_Name  # "K_NAME"
	        NAME_LONG <- G$SE_Species_Location_Longitude  # "Longitude"
	        NAME_LAT <- G$SE_Species_Location_Latitude  # "Latitude"
	        
	        # setting speices and environmental data
	        # FILE_SPECIES_NAME <<- input$SE_speciesindex   # G$SE_speciesindex
	        # FILE_SPECIES_LOCATION <<- input$SE_specieslocation  # G$SE_specieslocation
	        
	        if (!length(input$SE_speciesindex) == 0 | !length(input$SE_specieslocation) == 0) {
	            G_FILE_speciesindex <<- read.csv(file.path(G$SE_Dir_Species, input$SE_speciesindex), header = T, sep = ",")
	            G_FILE_specieslocation <<- read.csv(file.path(G$SE_Dir_Species, input$SE_specieslocation), header = T, sep = ",")
	        } else {
	            G_FILE_speciesindex <<- read.csv(file.path(G$SE_Dir_Species, G$SE_speciesindex), header = T, sep = ",")
	            G_FILE_specieslocation <<- read.csv(file.path(G$SE_Dir_Species, G$SE_specieslocation), header = T, sep = ",")
	        }
	        
	        DATA_SPECIES_NAME <- G_FILE_speciesindex
	        DATA_SPECIES_LOCATION <- G_FILE_specieslocation
	        
	        
	        #####========================================================
	        #####============ Running models ============================
	        #####========================================================
	        CUR_PATH <- getwd()
	        
	        for (s in slist) {
	            n <- n + 1
	            # creating Migclim output path
	            if (dir.exists(file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = "")))) {
	                cat(paste(paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = ""), "exists in", PATH_MODEL_OUTPUT, "/", s, "and is a directory"))
	            } else if (file.exists(file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = "")))) {
	                cat(paste(paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = ""), "exists in", PATH_MODEL_OUTPUT, "/", s, "but is a file"))
	            } else {
	                cat(paste(paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = ""), "does not exist in", PATH_MODEL_OUTPUT, "/", s, "- creating"))
	                dir.create(file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = "")))
	            }   
	            
	            
	            # SPECIES_ID <- s # "S106" # S106 분비나무,  S002 구상나무, S010 요강나물, S012 변산바람꽂, S018 매자나무
	            SPECIES_NAME <- subset(DATA_SPECIES_NAME, get(NAME_SPECIES) == s, select = c(get(NAME_ID)))
#	            SPECIES_NAME <<- SPECIES_NAME[1,]
#	            SPECIES_ID <<- as.character(SPECIES_NAME$ID)
	            SPECIES_ID <- SPECIES_NAME[1,]
	            SPECIES_DATA <- subset(DATA_SPECIES_LOCATION, get(NAME_ID) == SPECIES_ID, select = c(NAME_ID, NAME_LONG, NAME_LAT))
	            SPECIES_DATA[,NAME_ID] <- 1
	            
	            SPECIES_DATA_P <- cbind(SPECIES_DATA[, c(NAME_LONG, NAME_LAT)], z = 1)
	            names(SPECIES_DATA_P)[names(SPECIES_DATA_P) == NAME_LONG] <- "x"
	            names(SPECIES_DATA_P)[names(SPECIES_DATA_P) == NAME_LAT] <- "y"

	            # Setting working path
	            org_path <- file.path(PATH_MODEL_OUTPUT, s, G$DIR_NAME_SDM)
	            target_path <- file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = ""))
	            setwd(target_path)
	            
	            ### Projection on current and future environemental conditions
	            # Projecting loop
	            for (d in dlist) {
	                for (c in clist) {
	                    for (m in mlist) {
	                        for (y in ylist) {
	                            incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", y))
	                            
	                            Map <- paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")
	                            R_SDM <- raster(file.path(org_path, Map))
	                            R_SRM <- R_SDM
	                            
	                            file <- file.path(G$SE_Dir_Climate, G$Var_Current_Folder, SDM_Name_MO_Variables_selected[1])
	                            rm <- raster(file)
	                            rm[!is.na(rm[])] <- 0

	                            if (input$SRM_MO_Type == "Buffer") {
	                                cbuffer_dist <- input$SRM_Buffer_Distance
	                                circ <- circles(p=SPECIES_DATA_P[,c("x", "y")], d=cbuffer_dist, lonlat=TRUE, n=360, r=6378137, dissolve=TRUE)
	                                r_circ <- predict(rm, circ, mask=TRUE)
	                                r_circ[is.na(r_circ[])] <- 0
	                                r_circ <- r_circ + rm
	                                writeRaster(r_circ, file = file.path(target_path, paste(as.name(paste("SRM-PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
  
	                                R_SRM[R_SDM == 0 & r_circ == 0] <- 0
	                                R_SRM[R_SDM == 0 & r_circ == 1] <- 0
	                                R_SRM[R_SDM == 1 & r_circ == 0] <- 0
	                                R_SRM[R_SDM == 1 & r_circ == 1] <- 1
	                                writeRaster(R_SRM, file = file.path(target_path, paste(as.name(paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                                # plot(r_circ)
	                            } else if (input$SRM_MO_Type == "Hull") {
	                                occurrences <- cbind(z = SPECIES_ID, SPECIES_DATA_P[, c("x", "y")])
	                                hull_type <- input$SRM_Hull_Type # "convex"  # "concave"
	                                concave_distance_lim <- input$SRM_Hull_Concave_Distance # 5000
	                                buffer_distance <- input$SRM_Hull_Buffer_Distance # 10000
	                                cluster_method <- input$SRM_Hull_Cluster_Method # "k-means" # "hierarchical" # "k-means" 
	                                n_k_means <- input$SRM_Hull_Number_Kmeans # 3 # NULL
	                                split_distance <- input$SRM_Hull_Split_Distance # 30000
	                            
	                                hull_range <- rangemap_hull(occurrences = occurrences, hull_type = hull_type, 
	                                                            buffer_distance = buffer_distance, split = TRUE, 
	                                                            cluster_method = cluster_method, n_k_means = n_k_means, split_distance = split_distance)				
	                            
	                                hull_r <- rasterize(hull_range@species_range, rm)
	                                hull_r[hull_r > 0] <- 1
	                                hull_r[is.na(hull_r[])] <- 0
	                                hull_r <- hull_r + rm
	                                writeRaster(hull_r, file = file.path(target_path, paste(as.name(paste("SRM-PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                                
	                                R_SRM[R_SDM == 0 & hull_r == 0] <- 0
	                                R_SRM[R_SDM == 0 & hull_r == 1] <- 0
	                                R_SRM[R_SDM == 1 & hull_r == 0] <- 0
	                                R_SRM[R_SDM == 1 & hull_r == 1] <- 1
	                                
	                                writeRaster(R_SRM, file = file.path(target_path, paste(as.name(paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                                # plot(hull_r)
	                            } else {
	                                sbuffer_dist <- input$SRM_VoronoiHull_Absence_sample_Buffer_Distance
	                                circ <- circles(p=SPECIES_DATA_P[,c("x", "y")], d=sbuffer_dist, lonlat=TRUE, n=360, r=6378137, dissolve=TRUE)
	                                cm <- predict(rm, circ, mask=TRUE)
	                            
	                                cm[cm == 1] <- 0
	                                cm[is.na(cm[])] <- 1 
	                                cm[cm == 0] <- NA
	                                sm <- cm * rm
	                                
	                                if (input$SRM_VoronoiHull_sampling_Type == "ratio") {
	                                    nsp <- nrow(SPECIES_DATA)
	                                    nsa <- nsp * input$SRM_VoronoiHull_Absence_sample_ratio
	                                    
	                                } else{
	                                    nsa <- input$SRM_VoronoiHull_Absence_sample_size
	                                }
	                                
	                                sample_size <- nsa * 10
	                                s_r <- sampleRandom(sm, size=sample_size, na.rm=TRUE, xy=TRUE)
	                                SPECIES_DATA_A <- as.data.frame(s_r)
	                                names(SPECIES_DATA_A)[names(SPECIES_DATA_A) == "layer"] <- "z"
	                                SPECIES_DATA_A[,"z"] <- 0
	                            
	                                va_sample_size <- nsa
	                                va <- data.frame(SPECIES_DATA_A[sample(nrow(SPECIES_DATA_A), va_sample_size), ])
	                                vorm <- voronoiHull(p=SPECIES_DATA_P, a=va)
	                                vo <- predict(rm, vorm, mask=T)
	                                vo[is.na(vo[])] <- 0
	                                vo <- vo + rm
	                                writeRaster(vo, file = file.path(target_path, paste(as.name(paste("SRM-PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                                
	                                R_SRM[R_SDM == 0 & vo == 0] <- 0
	                                R_SRM[R_SDM == 0 & vo == 1] <- 0
	                                R_SRM[R_SDM == 1 & vo == 0] <- 0
	                                R_SRM[R_SDM == 1 & vo == 1] <- 1
	                                
	                                writeRaster(R_SRM, file = file.path(target_path, paste(as.name(paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                                # plot(vo)
	                            }
	                        }
	                    }
	                }
	            }
	            
	            SRM_variables <- setNames(data.frame(matrix(ncol = 17, nrow = 30)), c("input$SRM_SDM_Dir_Folder", "input$SRM_MO_Climate_model", "input$SRM_MO_Climate_scenario", "input$SRM_MO_Project_year", "input$SRM_MO_SDM_model",
	                                                                                  "input$SRM_MO_Type", "input$SRM_Buffer_Distance", "input$SRM_Hull_Type",  "input$SRM_Hull_Concave_Distance", "input$SRM_Hull_Buffer_Distance",
	                                                                                  "input$SRM_Hull_Cluster_Method", "input$SRM_Hull_Number_Kmeans", "input$SRM_Hull_Split_Distance", "input$SRM_VoronoiHull_Absence_sample_Buffer_Distance", "input$SRM_VoronoiHull_sampling_Type",
	                                                                                  "input$SRM_VoronoiHull_Absence_sample_ratio", "input$SRM_VoronoiHull_Absence_sample_size"
	            ))
	            
	            SRM_variables[1:length(G$SRM_SDM_Dir_Folder), "input$SRM_SDM_Dir_Folder"] <- G$SRM_SDM_Dir_Folder
	            
	            SRM_variables[1:length(input$SRM_MO_Climate_model), "input$SRM_MO_Climate_model"] <- input$SRM_MO_Climate_model
	            SRM_variables[1:length(input$SRM_MO_Climate_scenario), "input$SRM_MO_Climate_scenario"] <- input$SRM_MO_Climate_scenario
	            SRM_variables[1:length(input$SRM_MO_Project_year), "input$SRM_MO_Project_year"] <- input$SRM_MO_Project_year
	            SRM_variables[1:length(input$SRM_MO_SDM_model), "input$SRM_MO_SDM_model"] <- input$SRM_MO_SDM_model
	            
	            SRM_variables[1:length(input$SRM_MO_Type), "input$SRM_MO_Type"] <- input$SRM_MO_Type
	            SRM_variables[1:length(input$SRM_Buffer_Distance), "input$SRM_Buffer_Distance"] <- input$SRM_Buffer_Distance
	            SRM_variables[1:length(input$SRM_Hull_Type), "input$SRM_Hull_Type"] <- input$SRM_Hull_Type

	            SRM_variables[1:length(input$SRM_Hull_Concave_Distance), "input$SRM_Hull_Concave_Distance"] <- input$SRM_Hull_Concave_Distance
	            SRM_variables[1:length(input$SRM_Hull_Buffer_Distance), "input$SRM_Hull_Buffer_Distance"] <- input$SRM_Hull_Buffer_Distance
	            SRM_variables[1:length(input$SRM_Hull_Cluster_Method), "input$SRM_Hull_Cluster_Method"] <- input$SRM_Hull_Cluster_Method

	            SRM_variables[1:length(input$SRM_Hull_Number_Kmeans), "input$SRM_Hull_Number_Kmeans"] <- input$SRM_Hull_Number_Kmeans
	            SRM_variables[1:length(input$SRM_Hull_Split_Distance), "input$SRM_Hull_Split_Distance"] <- input$SRM_Hull_Split_Distance
	            
	            SRM_variables[1:length(input$SRM_VoronoiHull_Absence_sample_Buffer_Distance), "input$SRM_VoronoiHull_Absence_sample_Buffer_Distance"] <- input$SRM_VoronoiHull_Absence_sample_Buffer_Distance
	            SRM_variables[1:length(input$SRM_VoronoiHull_sampling_Type), "input$SRM_VoronoiHull_sampling_Type"] <- input$SRM_VoronoiHull_sampling_Type
	            SRM_variables[1:length(input$SRM_VoronoiHull_Absence_sample_ratio), "input$SRM_VoronoiHull_Absence_sample_ratio"] <- input$SRM_VoronoiHull_Absence_sample_ratio
	            SRM_variables[1:length(input$SRM_VoronoiHull_Absence_sample_size), "input$SRM_VoronoiHull_Absence_sample_size"] <- input$SRM_VoronoiHull_Absence_sample_size	            
	            	            
	            SRM_variables[is.na(SRM_variables)] <- ""
	            write.csv(SRM_variables, file = file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_SRM, input$SRM_MO_Dir_Folder_Name, sep = ""), paste(s, "_SRM_", input$SRM_MO_Dir_Folder_Name, "_variables.csv", sep = "", collapse = "--")))
	            
	        }

	
	
	    })
	})
	
	
	
	
	output$SRM_AO_Dir_Folder <- renderUI({
	    SRM_AO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	    SRM_AO_Dir_Folder_selected <- SRM_AO_Dir_Folder_list[1]
	    selectInput("SRM_AO_Dir", SRM_Name_Model_Out_Folder,
	                choices = c(SRM_AO_Dir_Folder_list),
	                selected = SRM_AO_Dir_Folder_selected
	    )
	    
	})
	
	output$SRM_AO_Model_Name <- renderUI({
	    SRM_AO_Model_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_AO_Dir, input$SRM_OU_Species[1]), full.names = FALSE, recursive = FALSE)
	    #	  SRM_AO_Model_Name_list <- SRM_AO_Model_Name_list[-1]
	    SRM_AO_Model_Name_selected <- SRM_AO_Model_Name_list[1]
	    selectInput("SRM_AO_Model_Name_Input", SRM_Name_Model_Out_Model,
	                choices = c(SRM_AO_Model_Name_list),
	                selected = SRM_AO_Model_Name_selected
	    )
	    
	})
	
	output$SRM_AO_Species <- renderUI({
	    G$SRM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_AO_Dir)
	    SRM_Name_Species_list <- list.dirs(path = G$SRM_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    SRM_Name_Species_selected <- SRM_Name_Species_list[1]
	    selectInput("SRM_OU_Species", SRM_Name_Model_Out_Species,
	                choices = c(SRM_Name_Species_list),
	                selected = SRM_Name_Species_selected
	    )
	})
	
	output$SRM_AO_SDM_model <- renderUI({
	    G$SRM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_AO_Dir)
	    destfile <- file.path(G$SRM_AO_Dir_Folder, input$SRM_OU_Species[1], G$DIR_NAME_SDM, paste(as.name(paste(input$SRM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	    all_eval <- read.csv(destfile)
	    G_FILE_species_evaluation <<- all_eval
	    SRM_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
	    SRM_Name_Models_selected <- SRM_Name_Models_list[1]
	    checkboxGroupInput("SRM_OU_SDM_model", SRM_Name_Model_Out_SDM,
	                       choices = c(SRM_Name_Models_list),
	                       selected = SRM_Name_Models_selected
	    )
	})
	
	output$SRM_OU_UI_plot <- renderUI({
	    # setting Climate change scenarios, Future time, Species and current environmental path
	    slist <- input$SRM_OU_Species
	    dlist <- input$SRM_OU_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	    clist <- input$SRM_OU_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	    mlist <- input$SRM_OU_SDM_model # c("PA1_Full_GLM_byROC")
	    ylist <- input$SRM_OU_Project_year
	    #	  dtlist <- input$SRM_OU_Dispersal_type
	    
	    n <- 0
	    ls <- length(slist)
	    ld <- length(dlist)
	    lc <- length(clist)
	    lm <- length(mlist)
	    ly <- length(ylist)
	    #	  ldt <- length(dtlist)
	    tl <- ls * ld * lc * lm * ly # * ldt
	    
	    nc <- 2
	    if (tl <  2) {
	        nr <- round(tl / nc) + 1
	    } else {
	        nr <- round((tl + 0.1) / nc)
	    }
	    
	    ws <- nc * 500
	    hs <- nr * 500
	    plotOutput("SRM_AO_OU_plot", width = ws, height = hs)
	})
	
	output$SRM_AO_OU_plot <- renderPlot({
	    #####========================================================
	    ##### Plot GAP output =========================================
	    
	    # setting Climate change scenarios, Future time, Species and current environmental path
	    slist <- input$SRM_OU_Species
	    dlist <- input$SRM_OU_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	    clist <- input$SRM_OU_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	    mlist <- input$SRM_OU_SDM_model # c("PA1_Full_GLM_byROC")
	    ylist <- input$SRM_OU_Project_year
	    
	    ls <- length(slist)
	    ld <- length(dlist)
	    lc <- length(clist)
	    lm <- length(mlist)
	    ly <- length(ylist)
	    tl <- ls * ld * lc * lm * ly
	    
	    nc <- 2
	    if (tl <  2) {
	        nr <- round(tl / nc) + 1
	    } else {
	        nr <- round((tl + 0.1) / nc)
	    }
	    
	    par(mfrow = c(nr,nc), cex.main = 1.2)
	    G$SRM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SRM_AO_Dir)
	    
	    for (s in slist) {
	        dir_path <- file.path(G$SRM_AO_Dir_Folder, s, input$SRM_AO_Model_Name_Input)  # paste(input$SRM_AO_Model_Name_Input, sep = ""))
	        for (d in dlist) {
	            for (c in clist) {
	                for (m in mlist) {
	                    if (ly > 0) {
	                        for (y in 1:ly) {
	                            Map1 <- paste("SRM-PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
	                            if (file.exists(file.path(dir_path, Map1))) {
	                                R_Map1 <- raster(file.path(dir_path, Map1))
	                                plot(R_Map1, main = Map1)
	                            }
	                            Map2 <- paste("PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
	                            if (file.exists(file.path(dir_path, Map2))) {
	                                R_Map2 <- raster(file.path(dir_path, Map2))
	                                plot(R_Map2, main = Map2)
	                            }
	                        }
	                    }
	                }
	            }
	        }
	    }
	    ##### End Plot output =========================================
	})		
	
	
	
	
	
	
	
	
	output$DM_SDM_Dir_Folder <- renderUI({
	  DM_SDM_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  DM_SDM_Dir_Folder_selected <- DM_SDM_Dir_Folder_list[1]
	  selectInput("DM_SDM_Dir", DM_Name_Model_SDM_Folder,
	              choices = c(DM_SDM_Dir_Folder_list),
	              selected = DM_SDM_Dir_Folder_selected
	  )

	})
	
	observeEvent(input$DM_MO_Species_sel_all, {
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_SDM_Dir)
	    DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$DM_Name_Species_selected <<- DM_Name_Species_list
	})
	
	observeEvent(input$DM_MO_Species_sel_none, {
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_SDM_Dir)
	    DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$DM_Name_Species_selected <<- ""
	})
	
	output$DM_MO_Species <- renderUI({
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_SDM_Dir)
		DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
		checkboxGroupInput("DM_MO_Species", DM_Name_Model_SDM_Species,
			choices = c(DM_Name_Species_list),
			selected = G$DM_Name_Species_selected
		)
	})
	
	output$DM_MO_SDM_model <- renderUI({
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_SDM_Dir)
	    DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
		destfile <- file.path(G$DM_SDM_Dir_Folder, DM_Name_Species_list[1], G$DIR_NAME_SDM, paste(as.name(paste(DM_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		DM_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		DM_Name_Models_selected <- DM_Name_Models_list[1]
		checkboxGroupInput("DM_MO_SDM_model", DM_Name_Model_SDM_Model,
			choices = c(DM_Name_Models_list),
			selected = DM_Name_Models_selected
		)
	})
	
	output$DM_MO_Current_UI <- renderUI({
	  
	  if (input$DM_MO_Current_Type == "SRM") {
	    uiOutput("DM_SRM_Dir_Folder")
	  } else {
	    
	  }
	})
	  
  output$DM_SRM_Dir_Folder <- renderUI({
    DM_SRM_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_SDM_Dir, input$DM_MO_Species[1]), full.names = FALSE, recursive = FALSE)
    DM_SRM_Dir_Folder_list <- DM_SRM_Dir_Folder_list[grepl(pattern = G$DIR_NAME_SRM, x = DM_SRM_Dir_Folder_list)]
    DM_SRM_Dir_Folder_selected <- DM_SRM_Dir_Folder_list[1]
    selectInput("DM_SRM_Dir", "Working Species Range Folders",
                choices = c(DM_SRM_Dir_Folder_list),
                selected = DM_SRM_Dir_Folder_selected
    )
	    
  })	
	
	output$DM_Map_Landuse <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS(G$Projection_Info)
	    
	    r_list <- NULL
	    for (y in input$DM_MO_Project_year) {
	    
	        file <- file.path(G$SE_Dir_Link, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, y, paste(input$DM_MO_Barrier_Landuse, G$IMG_File, sep = ""))
	        r <- raster(file)
	        crs(r) <- CRS(G$Projection_Info)
	    
	        for (k in input$DM_MO_Barrier_LanduseType) {
	            r[r == as.integer(k)] <- 9999
	        }	
	        r[r != 9999] <- 0
	        r[r == 9999] <- 1
	        r_list <- c(r_list, r)
	    }
	    
	    landuse_prop <- as.integer(length(input$DM_MO_Project_year) * (input$DM_MO_Barrier_Landuse_Prop / 100))
	    r_stack <- stack(r_list)
	    rlanduse <- overlay(r_stack, fun = sum)

	    rlanduse[rlanduse < landuse_prop] <- 0
	    rlanduse[rlanduse >= landuse_prop] <- 1
	    r <- projectRaster(rlanduse, mask, method = 'ngb')
	    r <- as.factor(r)
	    
	    pal <- colorFactor(c("gray90", "chocolate4"), values(r),
	                       na.color = "transparent")
	    
	    leaflet() %>%
	        addTiles(
	            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	        ) %>%        
	        
	        addRasterImage(r, colors = pal, opacity = 0.8,) %>%
	        addLegend(pal = pal, values = values(r), title = "Legend")  %>%	
	        setView(lng = 127.00, lat = 36.00, zoom = 7)
	})
	
	output$DM_Map_Forestfire <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS(G$Projection_Info)
	    
	    r_list <- NULL
	    for (y in input$DM_MO_Project_year) {
	        file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Forestfire, G$IMG_File, sep = ""))
	        r <- raster(file)
	        crs(r) <- CRS(G$Projection_Info)
	    
	        r[r >= input$DM_MO_Barrier_Forestfire_Cutoff] <- 9999
	        r[r != 9999] <- 0
	        r[r == 9999] <- 1
	        r <- as.integer(r)
	        r_list <- c(r_list, r)
	    }
	    
	    forestfire_prop <- as.integer(length(input$DM_MO_Project_year) * (input$DM_MO_Barrier_Forestfire_Prop / 100))
	    r_stack <- stack(r_list)
	    rforestfire <- overlay(r_stack, fun = sum)
	    rforestfire[rforestfire < forestfire_prop] <- 0
	    rforestfire[rforestfire >= forestfire_prop] <- 1
	    r <- projectRaster(rforestfire, mask, method = 'ngb')
	    r <- as.factor(r)
	    
	    pal <- colorFactor(c("gray90", "chocolate4"), values(r),
	                       na.color = "transparent")
	    
	    leaflet() %>%
	        addTiles(
	            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	        ) %>%        
	        
	        addRasterImage(r, colors = pal, opacity = 0.8,) %>%
	        addLegend(pal = pal, values = values(r), title = "Legend")  %>%	
	        setView(lng = 127.00, lat = 36.00, zoom = 7)
	})
	
	output$DM_Map_Landslide <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS(G$Projection_Info)
	    
	    r_list <- NULL
	    for (y in input$DM_MO_Project_year) {
	        file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Landslide, G$IMG_File, sep = ""))
	        r <- raster(file)
	        crs(r) <- CRS(G$Projection_Info)
	        
	        r[r >= input$DM_MO_Barrier_Landslide_Cutoff] <- 9999
	        r[r != 9999] <- 0
	        r[r == 9999] <- 1
	        r <- as.integer(r)
	        r_list <- c(r_list, r)
	    }
	    
	    landslide_prop <- as.integer(length(input$DM_MO_Project_year) * (input$DM_MO_Barrier_Landslide_Prop / 100))
	    r_stack <- stack(r_list)
	    rlandslide <- overlay(r_stack, fun=sum)
	    rlandslide[rlandslide < landslide_prop] <- 0
	    rlandslide[rlandslide >= landslide_prop] <- 1
	    r <- projectRaster(rlandslide, mask, method='ngb')
	    r <- as.factor(r)
	    
	    pal <- colorFactor(c("gray90", "chocolate4"), values(r),
	                       na.color = "transparent")
	    
	    leaflet() %>%
	        addTiles(
	            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	        ) %>%        
	        
	        addRasterImage(r, colors = pal, opacity = 0.8,) %>%
	        addLegend(pal = pal, values = values(r), title = "Legend")  %>%	
	        setView(lng = 127.00, lat = 36.00, zoom = 7)
	})
	
	output$DM_Map_Total <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS(G$Projection_Info)
	    
	    dm_rlist <- NULL
	    if (length(input$DM_MO_Barrier) > 0) {
	    
	    for (dm in input$DM_MO_Barrier) {
	        
	    if (dm == "Landuse") {
	        r_list <- NULL
	        for (y in input$DM_MO_Project_year) {
	            
	            file <- file.path(G$SE_Dir_Link, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, y, paste(input$DM_MO_Barrier_Landuse, G$IMG_File, sep = ""))
	            r <- raster(file)
	            crs(r) <- CRS(G$Projection_Info)
	            
	            for (k in input$DM_MO_Barrier_LanduseType) {
	                r[r == as.integer(k)] <- 9999
	            }	
	            r[r != 9999] <- 0
	            r[r == 9999] <- 1
	            r_list <- c(r_list, r)
	        }
	        
	        landuse_prop <- as.integer(length(input$DM_MO_Project_year) * (input$DM_MO_Barrier_Landuse_Prop / 100))
	        r_stack <- stack(r_list)
	        rlanduse <- overlay(r_stack, fun = sum)
	        rlanduse[rlanduse < landuse_prop] <- 0
	        rlanduse[rlanduse >= landuse_prop] <- 1
	        dm_rlanduse <- projectRaster(rlanduse, mask, method = 'ngb')
	        dm_r <- dm_rlanduse
	        dm_rlist <- c(dm_rlist, dm_rlanduse)
	    } else if (dm == "Forestfire") {
	        r_list <- NULL
	        for (y in input$DM_MO_Project_year) {
	            file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Forestfire, G$IMG_File, sep = ""))
	            r <- raster(file)
	            crs(r) <- CRS(G$Projection_Info)
	            
	            r[r >= input$DM_MO_Barrier_Forestfire_Cutoff] <- 9999
	            r[r != 9999] <- 0
	            r[r == 9999] <- 1
	            r <- as.integer(r)
	            r_list <- c(r_list, r)
	        }
	        
	        forestfire_prop <- as.integer(length(input$DM_MO_Project_year) * (input$DM_MO_Barrier_Forestfire_Prop / 100))
	        r_stack <- stack(r_list)
	        rforestfire <- overlay(r_stack, fun = sum)
	        rforestfire[rforestfire < forestfire_prop] <- 0
	        rforestfire[rforestfire >= forestfire_prop] <- 1
	        dm_rforestfire <- projectRaster(rforestfire, mask, method = 'ngb')
	        dm_r <- dm_rforestfire
	        dm_rlist <- c(dm_rlist, dm_rforestfire)
	    } else {
            r_list <- NULL
            for (y in input$DM_MO_Project_year) {
                file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Landslide, G$IMG_File, sep = ""))
                r <- raster(file)
                crs(r) <- CRS(G$Projection_Info)
                
                r[r >= input$DM_MO_Barrier_Landslide_Cutoff] <- 9999
                r[r != 9999] <- 0
                r[r == 9999] <- 1
                r <- as.integer(r)
                r_list <- c(r_list, r)
            }
            
            landslide_prop <- as.integer(length(input$DM_MO_Project_year) * (input$DM_MO_Barrier_Landslide_Prop / 100))
            r_stack <- stack(r_list)
            rlandslide <- overlay(r_stack, fun = sum)
            rlandslide[rlandslide < landslide_prop] <- 0
            rlandslide[rlandslide >= landslide_prop] <- 1
            dm_rlandslide <- projectRaster(rlandslide, mask, method = 'ngb')
            dm_r <- dm_rlandslide
            dm_rlist <- c(dm_rlist, dm_rlandslide)
        }
	    }
	    }
	    

	    if (length(dm_rlist) > 1) {    
	        r_stack <- stack(dm_rlist)
	        r <- overlay(r_stack, fun=sum)
	        r[r >= 1] <- 1
	        DM_barrier_r <<- r
	        G_DM_barrier_chk <<- TRUE
	    } else if (length(dm_rlist) == 1) {
	        r <- dm_r
	        DM_barrier_r <<- r
	        G_DM_barrier_chk <<- TRUE
	    } else {
	        mask[!is.na(mask)] <- 0
	        r <- mask
	        G_DM_barrier_chk <<- FALSE
	    }
	    
	    r <- as.factor(r)
	    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 	    
	    pal <- colorFactor(c("gray90", "chocolate4"), values(r),
	                       na.color = "transparent")
	    
	    leaflet() %>%
	      addTiles(
	        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	      ) %>%    
	    
	    addRasterImage(r, colors = pal, opacity = 0.8,) %>%
	      addLegend(pal = pal, values = values(r), title = "Legend")  %>%	
	      setView(lng = 127.00, lat = 36.00, zoom = 7)

	})
	
	output$DM_MO_DM_envChgSteps <- renderUI({
	  sliderInput("DM_MO_DM_envChgSteps", label = "DM_envChgSteps", min = 0, 
	              max = length(input$DM_MO_Project_year), step = 1, value = length(input$DM_MO_Project_year))
	})
	

	output$DM_MO_DM_dispKernel <- renderPrint({
	  G$DM_MO_DM_dispKernel <<- c(input$DM_MO_DM_dispKernel1, input$DM_MO_DM_dispKernel2, input$DM_MO_DM_dispKernel3, input$DM_MO_DM_dispKernel4, input$DM_MO_DM_dispKernel5)
	  str(G$DM_MO_DM_dispKernel)
	})

	output$DM_MO_DM_propaguleProd <- renderPrint({
	  G$DM_MO_DM_propaguleProd <<- c(input$DM_MO_DM_propaguleProd1, input$DM_MO_DM_propaguleProd2, input$DM_MO_DM_propaguleProd3, input$DM_MO_DM_propaguleProd4)
	  str(G$DM_MO_DM_propaguleProd)
	})
	
	observeEvent(input$DM_MO_Dir_Folder, {
	  textInput("DM_MO_Dir_Folder_Name", "DM Model Foler Name" ,
	            value = "insert DM Model Foler Name")
	})
	
	observeEvent(input$DM_MO_Action_run, {
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  dlist <- input$DM_MO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$DM_MO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  ylist <- input$DM_MO_Project_year  # c(G$Var_Current_Folder, "2050") # c(G$Var_Current_Folder, "2050", "2070")
	  slist <- input$DM_MO_Species
	  mlist <- input$DM_MO_SDM_model
	  
	  n <- 0
	  ld <- length(dlist)
	  lc <- length(clist)
	  ly <- length(ylist)
	  ls <- length(slist)
	  lm <- length(mlist)
	  tl <- ld * lc * lm * ls * ly
	  
	  withProgress(message = 'Runing DM model.........', value = 0, {
	    
	    ##############################################################
	    ### Species Dispersal Modeling
	    ### MIGCLIM
	    ###
	    ### by Changwan Seo
	    ##############################################################
	    
	    #####=========================================================
	    ##### Setting variables ======================================
	    
	    # setting Paths
	    PATH_PROJECT   <- G$SE_Dir_Project
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_SDM_Dir)
	    PATH_MODEL_OUTPUT <- G$DM_SDM_Dir_Folder   # file.path(PATH_PROJECT, G$DIR_NAME_Species)
	    CUR_PATH <- getwd()
	    setwd(PATH_MODEL_OUTPUT)
	    
	    # Defining Model options.
#      DM_iniDist <- "iniDist.grd"
#      DM_hsMap <- "hsMap.grd"
#      DM_rcThreshold <- cutoffvalue_S002
	    DM_envChgSteps <- input$DM_MO_DM_envChgSteps
	    DM_dispSteps <- input$DM_MO_DM_dispSteps
	    
	    if (length(input$DM_MO_Barrier) == 0) { 
	        G_DM_barrier_chk <- FALSE
	    } else {
	        G_DM_barrier_chk <- TRUE
	    }
	    if (G_DM_barrier_chk) {
	        df <- as.data.frame(DM_barrier_r, xy=TRUE)
	        df[is.na(df)] <- 0
	        DM_barrier <- df[,3:3]
	    } else {
	        DM_barrier <- ""
	    }
	    DM_barrierType <- input$DM_MO_DM_barrierType
      DM_dispKernel <- G$DM_MO_DM_dispKernel
	    DM_iniMatAge <- input$DM_MO_DM_iniMatAge
	    DM_propaguleProd <- G$DM_MO_DM_propaguleProd
	    DM_lddFreq <- input$DM_MO_DM_lddFreq
	    DM_lddMinDist <- input$DM_MO_SDM_lddDist[1]
	    DM_lddMaxDist <- input$DM_MO_SDM_lddDist[2]
	    DM_replicateNb <- input$DM_MO_DM_replicateNb
#	  	DM_simulName <- "S002_migHR4510L"
	    DM_overWrite <- input$DM_MO_DM_overWrite
	    DM_testMode <- input$DM_MO_DM_testMode
	    DM_fullOutput <- input$DM_MO_DM_fullOutput
	    DM_keepTempFiles <- input$DM_MO_DM_keepTempFiles
	    
	    #####========================================================
	    #####============ Running models ============================
	    #####========================================================
	    CUR_PATH <- getwd()

	    for (s in slist) {
	      n <- n + 1
	      # creating Migclim output path
	      if (dir.exists(file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = "")))) {
	        cat(paste(paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = ""), "exists in", PATH_MODEL_OUTPUT, "/", s, "and is a directory"))
	      } else if (file.exists(file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = "")))) {
	        cat(paste(paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = ""), "exists in", PATH_MODEL_OUTPUT, "/", s, "but is a file"))
	      } else {
	        cat(paste(paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = ""), "does not exist in", PATH_MODEL_OUTPUT, "/", s, "- creating"))
	        dir.create(file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = "")))
	      }   
	      
	      # Setting working path
	      org_path <- file.path(PATH_MODEL_OUTPUT, s, G$DIR_NAME_SDM)
	      target_path <- file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = ""))
	      setwd(target_path)
	      
	      ### Projection on current and future environemental conditions
	      # Projecting loop
	      for (d in dlist) {
	        for (c in clist) {
	          for (m in mlist) {
	            for (y in ylist[-1]) {
	              incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", y))
	              
                o_file_grd <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
                o_file_gri <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_Info, sep = "")
                if (input$DM_MO_Current_Type == "SDM") {
                  pred_path <- file.path(PATH_MODEL_OUTPUT, s, G$DIR_NAME_SDM)
	                file_path_grd <- file.path(pred_path, o_file_grd)
	                file_path_gri <- file.path(pred_path, o_file_gri)
	              } else {
	                pred_path <- file.path(PATH_MODEL_OUTPUT, s, input$DM_SRM_Dir)
	                file_path_grd <- file.path(pred_path, o_file_grd)
	                file_path_gri <- file.path(pred_path, o_file_gri)
	              }

	              if (!file.exists(file_path_grd)){
	                showModal(modalDialog(
	                  title = "Error Message",
	                  paste(file_path_grd, "is not exist.")
	                ))
	              } else {
	              
	              file.copy(file_path_grd, target_path)
	              file.copy(file_path_gri, target_path)  
	              sr_list <- ""
	              sr_list <- c(sr_list, file.path(pred_path, o_file_grd))
	              ny <- grep(y, ylist)
	              
	              for (i in 1:ny) {
	                o_file <- paste("PROJ_", d, "_", c, "_", ylist[i], "_", s, "_", sub("\\_by.*", "", m), G$IMG_File, sep = "")
	                
	                file_path <- file.path(org_path, o_file)
	                if (!file.exists(file_path)){
	                  showModal(modalDialog(
	                    title = "Error Message",
	                    paste(file_path, "is not exist.")
	                  ))
	                  chk_file <- FALSE
	                  break
	                } else {
	                  chk_file <- TRUE
	                }
	              }
	              
	              if (chk_file) {
	                
	                o_file <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
	                img <- file.path(org_path, o_file)
	                r <- raster(img)
	                
	                if (G_DM_barrier_chk) {
	                  r[DM_barrier_r == 1] <- 0 
	                  writeRaster(r, file = file.path(target_path, o_file, sep = "", collapse = "--"), overwrite = TRUE)
	                } else {
	                  
	                }
               
	                if (input$DM_MO_Future_Type == "Each_SDM") {

	                } else if (input$DM_MO_Future_Type == "Whole_SDM") {
	                  for (i in 2:ny) {
	                    o_file <- paste("PRED_", d, "_", c, "_", ylist[i], "_", s, "_", m, G$IMG_File, sep = "")
	                    img <- file.path(org_path, o_file)
	                    r1 <- raster(img)
	                    r <- r + r1
	                  }
	                  r[r >= 1] <- 1000
	                  writeRaster(r, file = file.path(target_path, paste(as.name(paste("DM_PROJ_Whole", G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                  img <- file.path(target_path, paste("DM_PROJ_Whole", G$IMG_File, sep = ""))
	                } else {
	                  o_file <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
	                  img <- file.path(org_path, o_file)
	                  r1 <- raster(img)
	                  r1[r1 >= 0] <- 1000
	                  writeRaster(r1, file = file.path(target_path, paste(as.name(paste("DM_PROJ_EVERYWHERE", G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                  img <- file.path(target_path, paste("DM_PROJ_EVERYWHERE", G$IMG_File, sep = ""))
	                }
	                
                for (i in 1:ny) {
                  if (input$DM_MO_Future_Type == "Each_SDM") {
                    o_file <- paste("PROJ_", d, "_", c, "_", ylist[i], "_", s, "_", sub("\\_by.*", "", m), G$IMG_File, sep = "")
                    img <- file.path(org_path, o_file)
                    sr_list <- c(sr_list, img)
                  } else if (input$DM_MO_Future_Type == "Whole_SDM") {
                    img <- file.path(target_path, paste("DM_PROJ_Whole", G$IMG_File, sep = ""))
                    sr_list <- c(sr_list, img)
                  } else {
                    img <- file.path(target_path, paste("DM_PROJ_EVERYWHERE", G$IMG_File, sep = ""))
                    sr_list <- c(sr_list, img)
                  }
                }
	                
	              sr_list <- grep(s, sr_list, value = TRUE)
	              sr_stack <- stack(sr_list)
	              df <- as.data.frame(sr_stack, xy=TRUE)
	              df[is.na(df)] <- 0
	              DM_iniDist <- df[,1:3]
	              nc <- ny + 3
	              DM_hsMap <- df[,4:nc]
	              DM_envChgSteps <- ny
	              DM_simulName <- paste("DM_", d, "_", c, "_", y, "_", s, "_", m, sep = "")
	              destfile <- file.path(PATH_MODEL_OUTPUT, s, G$DIR_NAME_SDM, paste(as.name(paste(s, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	              all_eval <- read.csv(destfile)
	              DM_rcThreshold <- as.integer(all_eval[all_eval$Prediction==m,]$Cutoff)
	              
	              dir_path <- file.path(target_path, DM_simulName)
	              if (file.exists(dir_path)){
	                unlink(dir_path, recursive = TRUE)
	              }
	              # Running MIGCLIM
	              MigClim.migrate(iniDist = DM_iniDist, hsMap = DM_hsMap, rcThreshold = DM_rcThreshold , envChgSteps = DM_envChgSteps, dispSteps = DM_dispSteps, 
	                              barrier = DM_barrier, barrierType = DM_barrierType, dispKernel = DM_dispKernel, 
	                              iniMatAge = DM_iniMatAge, propaguleProd = DM_propaguleProd, lddFreq = DM_lddFreq, lddMinDist = DM_lddMinDist, lddMaxDist = DM_lddMaxDist, replicateNb = DM_replicateNb, 
	                              simulName = DM_simulName, overWrite = DM_overWrite, testMode = DM_testMode, fullOutput = DM_fullOutput, keepTempFiles = DM_keepTempFiles)
	              
	              pred_file <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
	              pred_cur <- raster(file.path(org_path, pred_file))
	              path <- file.path(target_path, DM_simulName)
	              r_asc <- read.asc(file.path(path, list.files(path)[grep(".asc", list.files(path))][1]))
	              r_dm <- raster(r_asc)
#	              r_dm <- raster(file.path(path, list.files(path)[grep(G$IMG_File, list.files(path))][1]))
	              r_dm[r_dm < 0] <- 0
	              r_dm[r_dm > 0 & r_dm < 30000] <- 1
	              r_dm[r_dm == 30000] <- 0
	              r_dm[is.na(pred_cur)] <- NA
	              r_dm <- extractByMask(r_dm, msk=pred_cur, spatial=TRUE)
	              writeRaster(r_dm, file = file.path(target_path, paste(as.name(paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	              pred_sdm <- raster(file.path(org_path, paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")))
	              sdm_dm <- pred_sdm - r_dm
	              writeRaster(sdm_dm, file = file.path(target_path, paste(as.name(paste("SDM-DM_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	              }
	              }
	            } # End year loop y
            } # End Model loop m
	        } # End climate change Scenarios loop c
	      } # End climate data loop d
	      
	      #####
	      
#	      destfile <- file.path(PATH_MODEL_OUTPUT, s, "MIGCLIM", paste(s, "_MIGCLIM_variables.csv", sep = "", collapse = "--"))
	      
	      DM_variables <- setNames(data.frame(matrix(ncol = 24, nrow = 30)), c("input$DM_SDM_Dir_Folder", "input$DM_MO_SDM_model", "input$DM_MO_Climate_model", "input$DM_MO_Climate_scenario", "input$DM_MO_Project_year", "input$DM_MO_SDM_model", 
	                                                                            "input$DM_MO_Barrier", "input$DM_MO_DM_barrierType", "input$DM_MO_Barrier_Landuse", "input$DM_MO_Barrier_LanduseType", "input$DM_MO_Barrier_Forestfire_Cutoff",
	                                                                            "input$DM_MO_Barrier_Landslide_Cutoff", "input$DM_MO_Barrier_Landuse_Prop", "input$DM_MO_Barrier_Forestfire_Prop", "input$DM_MO_Barrier_Landslide_Prop",
	                                                                            "input$DM_MO_DM_dispSteps", "input$DM_MO_DM_dispKernel", "input$DM_MO_DM_iniMatAge", "input$DM_MO_DM_propaguleProd",
	                                                                            "input$DM_MO_DM_lddFreq", "input$DM_MO_SDM_lddDist", "input$DM_MO_DM_replicateNb", "input$DM_MO_Current_Type", "input$DM_MO_Future_Type"
	      ))
	      
	      DM_variables[1:length(G$DM_SDM_Dir_Folder), "input$DM_SDM_Dir_Folder"] <- G$DM_SDM_Dir_Folder
	      DM_variables[1:length(input$DM_MO_SDM_model), "input$DM_MO_SDM_model"] <- input$DM_MO_SDM_model
	      
	      DM_variables[1:length(input$DM_MO_Climate_model), "input$DM_MO_Climate_model"] <- input$DM_MO_Climate_model
	      DM_variables[1:length(input$DM_MO_Climate_scenario), "input$DM_MO_Climate_scenario"] <- input$DM_MO_Climate_scenario
	      DM_variables[1:length(input$DM_MO_Project_year), "input$DM_MO_Project_year"] <- input$DM_MO_Project_year
	      DM_variables[1:length(input$DM_MO_SDM_model), "input$DM_MO_SDM_model"] <- input$DM_MO_SDM_model
	      if (length(input$DM_MO_Barrier) > 0) {
	        DM_variables[1:length(input$DM_MO_Barrier), "input$DM_MO_Barrier"] <- input$DM_MO_Barrier
	      } else {
	        DM_variables[1:1, "input$DM_MO_Barrier"] <- "NULL"
	      }
	      DM_variables[1:length(input$DM_MO_DM_barrierType), "input$DM_MO_DM_barrierType"] <- input$DM_MO_DM_barrierType
	      DM_variables[1:length(input$DM_MO_Barrier_Landuse), "input$DM_MO_Barrier_Landuse"] <- input$DM_MO_Barrier_Landuse
	      DM_variables[1:length(input$DM_MO_Barrier_LanduseType), "input$DM_MO_Barrier_LanduseType"] <- input$DM_MO_Barrier_LanduseType
	      DM_variables[1:1, "input$DM_MO_Barrier_Forestfire_Cutoff"] <- input$DM_MO_Barrier_Forestfire_Cutoff
	      DM_variables[1:1, "input$DM_MO_Barrier_Landslide_Cutoff"] <- input$DM_MO_Barrier_Landslide_Cutoff
	      DM_variables[1:1, "input$DM_MO_Barrier_Landuse_Prop"] <- input$DM_MO_Barrier_Landuse_Prop
	      DM_variables[1:1, "input$DM_MO_Barrier_Forestfire_Prop"] <- input$DM_MO_Barrier_Forestfire_Prop
	      DM_variables[1:1, "input$DM_MO_Barrier_Landslide_Prop"] <- input$DM_MO_Barrier_Landslide_Prop
	      DM_variables[1:length(input$DM_MO_DM_dispSteps), "input$DM_MO_DM_dispSteps"] <- input$DM_MO_DM_dispSteps
	      DM_variables[1:length(G$DM_MO_DM_dispKernel), "input$DM_MO_DM_dispKernel"] <- G$DM_MO_DM_dispKernel
	      DM_variables[1:length(input$DM_MO_DM_iniMatAge), "input$DM_MO_DM_iniMatAge"] <- input$DM_MO_DM_iniMatAge
	      DM_variables[1:length(G$DM_MO_DM_propaguleProd), "input$DM_MO_DM_propaguleProd"] <- G$DM_MO_DM_propaguleProd
	      DM_variables[1:length(input$DM_MO_DM_lddFreq), "input$DM_MO_DM_lddFreq"] <- input$DM_MO_DM_lddFreq
	      DM_variables[1:length(input$DM_MO_SDM_lddDist), "input$DM_MO_SDM_lddDist"] <- input$DM_MO_SDM_lddDist
	      DM_variables[1:length(input$DM_MO_DM_replicateNb), "input$DM_MO_DM_replicateNb"] <- input$DM_MO_DM_replicateNb
	      
	      DM_variables[1:length(input$DM_MO_Current_Type), "input$DM_MO_Current_Type"] <- input$DM_MO_Current_Type
	      DM_variables[1:length(input$DM_MO_Future_Type), "input$DM_MO_Future_Type"] <- input$DM_MO_Future_Type

	      DM_variables[is.na(DM_variables)] <- ""
	      write.csv(DM_variables, file = file.path(PATH_MODEL_OUTPUT, s, paste(G$DIR_NAME_DM, input$DM_MO_Dir_Folder_Name, sep = ""), paste(s, "_MIGCLIM_", input$DM_MO_Dir_Folder_Name, "_variables.csv", sep = "", collapse = "--")))
	      
	      #####
	    } # End Speices loop s
	    setwd(CUR_PATH)
	    
	    #####========================================================
	    #####============ End Models Run ========================
	    #####========================================================        
	  })        
	})
	
	
	output$DM_AO_Dir_Folder <- renderUI({
	  DM_AO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  DM_AO_Dir_Folder_selected <- DM_AO_Dir_Folder_list[1]
	  selectInput("DM_AO_Dir", DM_Name_Model_Out_Folder,
	              choices = c(DM_AO_Dir_Folder_list),
	              selected = DM_AO_Dir_Folder_selected
	  )
	  
	})
	
	output$DM_AO_Model_Name <- renderUI({
	  DM_AO_Model_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_AO_Dir, input$DM_OU_Species[1]), full.names = FALSE, recursive = FALSE)
#	  DM_AO_Model_Name_list <- DM_AO_Model_Name_list[-1]
	  DM_AO_Model_Name_selected <- DM_AO_Model_Name_list[1]
	  selectInput("DM_AO_Model_Name_Input", DM_Name_Model_Out_Model,
	              choices = c(DM_AO_Model_Name_list),
	              selected = DM_AO_Model_Name_selected
	  )
	 
	})
	
	output$DM_OU_Species <- renderUI({
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_AO_Dir)
	  DM_Name_Species_list <- list.dirs(path = G$DM_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  DM_Name_Species_selected <- DM_Name_Species_list[1]
	  selectInput("DM_OU_Species", DM_Name_Model_Out_Species,
	                     choices = c(DM_Name_Species_list),
	                     selected = DM_Name_Species_selected
	  )
	})
	
	output$DM_OU_SDM_model <- renderUI({
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_AO_Dir)
	  destfile <- file.path(G$DM_AO_Dir_Folder, input$DM_OU_Species[1], G$DIR_NAME_SDM, paste(as.name(paste(input$DM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	  all_eval <- read.csv(destfile)
	  G_FILE_species_evaluation <<- all_eval
	  DM_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
	  DM_Name_Models_selected <- DM_Name_Models_list[1]
	  checkboxGroupInput("DM_OU_SDM_model", DM_Name_Model_Out_SDM,
	                     choices = c(DM_Name_Models_list),
	                     selected = DM_Name_Models_selected
	  )
	})
	
	
	output$DM_OU_UI_plot <- renderUI({
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  slist <- input$DM_OU_Species
	  dlist <- input$DM_OU_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$DM_OU_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$DM_OU_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$DM_OU_Project_year
#	  dtlist <- input$DM_OU_Dispersal_type
	  
	  n <- 0
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
#	  ldt <- length(dtlist)
	  tl <- ls * ld * lc * lm * ly # * ldt
	  
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  ws <- nc * 500
	  hs <- nr * 500
	  plotOutput("DM_AO_OU_plot", width = ws, height = hs)
	})
	
	
	output$DM_AO_OU_plot <- renderPlot({
	  #####========================================================
	  ##### Plot GAP output =========================================
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  slist <- input$DM_OU_Species
	  dlist <- input$DM_OU_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$DM_OU_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$DM_OU_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$DM_OU_Project_year
	  
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  tl <- ls * ld * lc * lm * ly
	  
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_AO_Dir)
	  
	  for (s in slist) {
	     dir_path <- file.path(G$DM_AO_Dir_Folder, s, input$DM_AO_Model_Name_Input)  # paste(input$DM_AO_Model_Name_Input, sep = ""))
	      for (d in dlist) {
	        for (c in clist) {
	          for (m in mlist) {
	            if (ly > 0) {
	                for (y in 1:ly) {
	                  Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
	                  if (file.exists(file.path(dir_path, Map1))) {
	                    R_Map1 <- raster(file.path(dir_path, Map1))
	                    plot(R_Map1, main = Map1)
	                  }
	                }
	              }
	            }
	          }
	        }
	    }
	  ##### End Plot output =========================================
	})
	
	output$DM_OU_SDMDM_UI_plot <- renderUI({
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  slist <- input$DM_OU_Species
	  dlist <- input$DM_OU_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$DM_OU_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$DM_OU_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$DM_OU_Project_year
	  #	  dtlist <- input$DM_OU_Dispersal_type
	  
	  n <- 0
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  #	  ldt <- length(dtlist)
	  tl <- ls * ld * lc * lm * ly # * ldt
	  
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  ws <- nc * 500
	  hs <- nr * 500
	  plotOutput("DM_AO_OU_SDMDM_plot", width = ws, height = hs)
	})
	
	
	output$DM_AO_OU_SDMDM_plot <- renderPlot({
	  #####========================================================
	  ##### Plot GAP output =========================================
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  slist <- input$DM_OU_Species
	  dlist <- input$DM_OU_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$DM_OU_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$DM_OU_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$DM_OU_Project_year
	  
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  tl <- ls * ld * lc * lm * ly
	  
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$DM_AO_Dir)
	  
	  for (s in slist) {
	    dir_path <- file.path(G$DM_AO_Dir_Folder, s, input$DM_AO_Model_Name_Input)  # paste(input$DM_AO_Model_Name_Input, sep = ""))
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          if (ly > 0) {
	            for (y in 1:ly) {
	              Map1 <- paste("SDM-DM", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
	              if (file.exists(file.path(dir_path, Map1))) {
	                R_Map1 <- raster(file.path(dir_path, Map1))
	                plot(R_Map1, main = Map1)
	              }
	            }
	          }
	        }
	      }
	    }
	  }
	  ##### End Plot output =========================================
	})
	
	
	output$SA_MO_Dir_Folder <- renderUI({
	  SA_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  SA_MO_Dir_Folder_selected <- SA_MO_Dir_Folder_list[1]
	  selectInput("SA_MO_Dir", SA_Name_Analysis_Folder,
	              choices = c(SA_MO_Dir_Folder_list),
	              selected = SA_MO_Dir_Folder_selected
	  )
	  
	})
	
	output$SA_MO_Dir_Folder_Name <- renderUI({
	    G$SA_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir)
	    SA_Name_Species_list <- list.dirs(path = G$SA_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    SA_MO_Dir_Folder_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir, SA_Name_Species_list[1]), full.names = FALSE, recursive = FALSE)
	    SA_MO_Dir_Folder_Name_selected <- SA_MO_Dir_Folder_Name_list[1]
	    selectInput("SA_MO_Dir_Folder", SA_Name_Analysis_Model,
	                choices = c(SA_MO_Dir_Folder_Name_list),
	                selected = SA_MO_Dir_Folder_Name_selected
	    )
	  
	})
	
	observeEvent(input$SA_CA_Species_Sel_All, {
	    G$SA_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir)
	    SA_Name_Species_list <- list.dirs(path = G$SA_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$SA_Name_Species_selected <<- SA_Name_Species_list
	})
	
	observeEvent(input$SA_CA_Species_Sel_None, {
	    G$SA_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir)
	    SA_Name_Species_list <- list.dirs(path = G$SA_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$SA_Name_Species_selected <<- ""  #SA_Name_Species_list[1]
	})
	
	output$SA_CA_Species <- renderUI({
	  G$SA_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir)
	  SA_Name_Species_list <- list.dirs(path = G$SA_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  checkboxGroupInput("SA_CA_Species", SA_Name_Analysis_Species,
	                     choices = c(SA_Name_Species_list),
	                     selected = G$SA_Name_Species_selected
	  )
	})
	
	output$SA_CA_SDM_model <- renderUI({
	  G$SA_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir)
	  SA_Name_Species_list <- list.dirs(path = G$SA_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  destfile <- file.path(G$SA_MO_Dir_Folder, SA_Name_Species_list[1], G$DIR_NAME_SDM, paste(as.name(paste(SA_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	  all_eval <- read.csv(destfile)
	  G_FILE_species_evaluation <<- all_eval
	  SA_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
	  SA_Name_Models_selected <- SA_Name_Models_list[1]
	  checkboxGroupInput("SA_CA_SDM_model", SA_Name_Analysis_SDM,
	                     choices = c(SA_Name_Models_list),
	                     selected = SA_Name_Models_selected
	  )
	})
	
	observeEvent(input$SA_CA_Action_change, {
		#####========================================================
		##### GAP analyzing =========================================
	
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SA_CA_Species
		dlist <- input$SA_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SA_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SA_CA_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SA_CA_Project_year
#		dtlist <- input$SA_CA_Dispersal_type
	
		n <- 0
		ls <- length(slist)
		ld <- length(dlist)
		lc <- length(clist)
		lm <- length(mlist)
		ly <- length(ylist)
#		ldt <- length(dtlist)
		tl <- ls * ld * lc * lm * ly # * ldt
		
		withProgress(message = 'Runing GAP Analysis model.........', value = 0, {
      
		for (s in slist) {
#		  for (dt in dtlist) {
		    G$SA_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir)
		    dir_path <- file.path(G$SA_MO_Dir_Folder, s, input$SA_MO_Dir_Folder)
			  n <- n + 1
			for (d in dlist) {
				for (c in clist) {
					for (m in mlist) {
						if (ly > 1) {
							if (ylist[1] == G$Var_Current_Folder) {
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
								R_gap <- raster(R_Map1)
								R_loss <- raster(R_Map1)
								R_stay <- raster(R_Map1)
								R_gain <- raster(R_Map1)
								R_gap[] <- 0
								R_loss[] <- 0
								R_stay <- raster(R_Map1)
								R_gain[] <- 0
								writeRaster(R_gap, file = file.path(dir_path, paste(as.name(paste("GAP_PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(R_loss, file = file.path(dir_path, paste(as.name(paste("LOSS_PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(R_stay, file = file.path(dir_path, paste(as.name(paste("STAY_PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(R_gain, file = file.path(dir_path, paste(as.name(paste("GAIN_PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								for (y in 2:ly) {
									incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", ylist[y]))
									Map2 <- paste("PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
									R_Map2 <- raster(file.path(dir_path, Map2))
									R_gap <- raster(R_Map1)
									R_loss <- raster(R_Map1)
									R_stay <- raster(R_Map1)
									R_gain <- raster(R_Map1)
									R_gap[R_Map1 == 0 & R_Map2 == 0] <- -2
									R_gap[R_Map1 == 0 & R_Map2 == 1] <- 1
									R_gap[R_Map1 == 1 & R_Map2 == 0] <- -1
									R_gap[R_Map1 == 1 & R_Map2 == 1] <- 0
									
									R_gain[R_Map1 == 0 & R_Map2 == 0] <- 0
									R_gain[R_Map1 == 0 & R_Map2 == 1] <- 1
									R_gain[R_Map1 == 1 & R_Map2 == 0] <- 0
									R_gain[R_Map1 == 1 & R_Map2 == 1] <- 0
									
									R_loss[R_Map1 == 0 & R_Map2 == 0] <- 0
									R_loss[R_Map1 == 0 & R_Map2 == 1] <- 0
									R_loss[R_Map1 == 1 & R_Map2 == 0] <- 1
									R_loss[R_Map1 == 1 & R_Map2 == 1] <- 0
									
									R_stay[R_Map1 == 0 & R_Map2 == 0] <- 0
									R_stay[R_Map1 == 0 & R_Map2 == 1] <- 0
									R_stay[R_Map1 == 1 & R_Map2 == 0] <- 0
									R_stay[R_Map1 == 1 & R_Map2 == 1] <- 1
									
									writeRaster(R_gap, file = file.path(dir_path, paste(as.name(paste("GAP_PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
									writeRaster(R_loss, file = file.path(dir_path, paste(as.name(paste("LOSS_PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
									writeRaster(R_stay, file = file.path(dir_path, paste(as.name(paste("STAY_PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
									writeRaster(R_gain, file = file.path(dir_path, paste(as.name(paste("GAIN_PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								}
							}
						}
					}
				}
#			}
		  }
		}
	    
		    
		})
		##### End GAP analyzing =========================================      
	})
	
	observeEvent(input$SA_CA_Action_Vindex, {
		#####========================================================
		##### GAP analyzing =========================================
	
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SA_CA_Species
		dlist <- input$SA_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SA_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SA_CA_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SA_CA_Project_year
#		dtlist <- input$SA_CA_Dispersal_type
	
		n <- 0
		ls <- length(slist)
		ld <- length(dlist)
		lc <- length(clist)
		lm <- length(mlist)
		ly <- length(ylist)
#		ldt <- length(dtlist)
		tl <- ls * ld * lc * lm * ly # * ldt
		tr <- ld * lc * lm * ly
	
		col_list <- c("Species", 
						"Climate_Model", 
						"Climate_Scenario", 
						"Model", 
						"Year", 
						"Area", 
						"Area_Loss", 
						"Area_Stay", 
						"Area_Gain", 
						"Area_Ratio", 
						"Area_Loss_Ratio", 
						"Area_Stay_Ratio", 
						"Area_Gain_Ratio",
						"Area_Gain_Ratio_Reverse",
						"Area_Gain_Ratio_Outside",
						"Area_Gain_Ratio_Outside_Reverse",
						"Vulnerability_Area_Loss_Ratio", 
						"Vulnerability_Area_LossIN_GainOUT_Ratio")
		Tab_gap_sum <- setNames(data.frame(matrix(ncol = 18, nrow = 0)), col_list)
		save_path <- G$SA_MO_Dir_Folder # file.path(isolate(G$SE_Dir_Project), "Sensitive_Species")
#		Tab_gap <- setNames(data.frame(matrix(ncol = 18, nrow = tr)), col_list)
		withProgress(message = 'Runing GAP Analysis model.........', value = 0, {
	
		
		for (s in slist) {
#		  for (dt in dtlist) {
		    G$SA_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_MO_Dir)
		    dir_path <- file.path(G$SA_MO_Dir_Folder, s, input$SA_MO_Dir_Folder)
#		    dir_path <- file.path(G$SA_MO_Dir_Folder, s, dt)
			  Tab_gap <- setNames(data.frame(matrix(ncol = 18, nrow = tr)), col_list)
			  n <- n + 1
			  n_tl <- 0
			for (d in dlist) {
				for (c in clist) {
					for (m in mlist) {
						if (ly > 0) {
							if (ly == 1 & ylist[1] == G$Var_Current_Folder) {
							  incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", ylist[1]))
								n_tl <- n_tl + 1
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
	
								T_Area0 <- freq(R_Map1, value = 0)
								T_Area1 <- freq(R_Map1, value = 1)
								T_Area <- T_Area0 + T_Area1
	
								Tab_gap$Species[n_tl] <- s
								Tab_gap$Climate_Model[n_tl] <- d
								Tab_gap$Climate_Scenario[n_tl] <- c
								Tab_gap$Model[n_tl] <- m
								Tab_gap$Year[n_tl] <- ylist[1]
								Tab_gap$Area[n_tl] <- freq(R_Map1, value = 1)
								Tab_gap$Area_Loss[n_tl] <- 0
								Tab_gap$Area_Stay[n_tl] <- Tab_gap$Area[n_tl]
								Tab_gap$Area_Gain[n_tl] <- 0
								Tab_gap$Area_Ratio[n_tl] <- 1
								Tab_gap$Area_Loss_Ratio[n_tl] <- (Tab_gap$Area_Loss[n_tl] / Tab_gap$Area[n_tl]) * 100
								Tab_gap$Area_Stay_Ratio[n_tl] <- (Tab_gap$Area_Stay[n_tl] / Tab_gap$Area[n_tl]) * 100
								Tab_gap$Area_Gain_Ratio[n_tl] <- (Tab_gap$Area_Gain[n_tl] / Tab_gap$Area[n_tl]) * 100
								Tab_gap$Area_Gain_Ratio_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio[n_tl] * -1
								Tab_gap$Area_Gain_Ratio_Outside[n_tl] <- (Tab_gap$Area_Gain[n_tl] / (T_Area - Tab_gap$Area[n_tl])) * 100
								Tab_gap$Area_Gain_Ratio_Outside_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio_Outside[n_tl] * -1
								Tab_gap$Vulnerability_Area_Loss_Ratio[n_tl] <- 1 - Tab_gap$Area_Ratio[n_tl]
								Tab_gap$Vulnerability_Area_LossIN_GainOUT_Ratio[n_tl] <- (Tab_gap$Area_Loss_Ratio[n_tl] / 100) - (Tab_gap$Area_Gain_Ratio_Outside[n_tl] / 100)
							} else if (ly > 1 & ylist[1] == G$Var_Current_Folder) {
							  incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", ylist[1]))
								n_tl <- n_tl + 1
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
								  
								T_Area0 <- freq(R_Map1, value = 0)
								T_Area1 <- freq(R_Map1, value = 1)
								T_Area <- T_Area0 + T_Area1
								  
								Tab_gap$Species[n_tl] <- s
								Tab_gap$Climate_Model[n_tl] <- d
								Tab_gap$Climate_Scenario[n_tl] <- c
								Tab_gap$Model[n_tl] <- m
								Tab_gap$Year[n_tl] <- ylist[1]
								Tab_gap$Area[n_tl] <- freq(R_Map1, value = 1)
								Tab_gap$Area_Loss[n_tl] <- 0
								Tab_gap$Area_Stay[n_tl] <- Tab_gap$Area[n_tl]
								Tab_gap$Area_Gain[n_tl] <- 0
								Tab_gap$Area_Ratio[n_tl] <- 1
								Tab_gap$Area_Loss_Ratio[n_tl] <- (Tab_gap$Area_Loss[n_tl] / Tab_gap$Area[n_tl]) * 100
								Tab_gap$Area_Stay_Ratio[n_tl] <- (Tab_gap$Area_Stay[n_tl] / Tab_gap$Area[n_tl]) * 100
								Tab_gap$Area_Gain_Ratio[n_tl] <- (Tab_gap$Area_Gain[n_tl] / Tab_gap$Area[n_tl]) * 100
								Tab_gap$Area_Gain_Ratio_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio[n_tl] * -1
								Tab_gap$Area_Gain_Ratio_Outside[n_tl] <- (Tab_gap$Area_Gain[n_tl] / (T_Area - Tab_gap$Area[n_tl])) * 100
								Tab_gap$Area_Gain_Ratio_Outside_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio_Outside[n_tl] * -1
								Tab_gap$Vulnerability_Area_Loss_Ratio[n_tl] <- 1 - Tab_gap$Area_Ratio[n_tl]
								Tab_gap$Vulnerability_Area_LossIN_GainOUT_Ratio[n_tl] <- (Tab_gap$Area_Loss_Ratio[n_tl] / 100) - (Tab_gap$Area_Gain_Ratio_Outside[n_tl] / 100)   
								for (y in 2:ly) {
									incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", ylist[y]))
									n_tl <- n_tl + 1
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
									R_gap <- raster(file.path(dir_path, Map2))
									  
									T_Area0 <- freq(R_gap, value = 0)
									T_Area1 <- freq(R_gap, value = 1)
									T_Area2 <- freq(R_gap, value = -1)
									T_Area3 <- freq(R_gap, value = -2)
									T_Area <- T_Area0 + T_Area1 + T_Area2 + T_Area3
									Map0_Area0 <- freq(R_gap, value = 0)
									Map0_Area1 <- freq(R_gap, value = -1)
									Map0_Area <- Map0_Area0 + Map0_Area1
									Map_Area0 <- freq(R_gap, value = 0)
									Map_Area1 <- freq(R_gap, value = 1)
									Map_Area <- Map_Area0 + Map_Area1
									  
									Tab_gap$Species[n_tl] <- s
									Tab_gap$Climate_Model[n_tl] <- d
									Tab_gap$Climate_Scenario[n_tl] <- c
									Tab_gap$Model[n_tl] <- m
									Tab_gap$Year[n_tl] <- ylist[y]
									Tab_gap$Area[n_tl] <- Map_Area
									Tab_gap$Area_Loss[n_tl] <- T_Area2
									Tab_gap$Area_Stay[n_tl] <- T_Area0
									Tab_gap$Area_Gain[n_tl] <- T_Area1
									Tab_gap$Area_Ratio[n_tl] <- Map_Area / Map0_Area
									Tab_gap$Area_Loss_Ratio[n_tl] <- (Tab_gap$Area_Loss[n_tl] / Map0_Area) * 100
									Tab_gap$Area_Stay_Ratio[n_tl] <- (Tab_gap$Area_Stay[n_tl] / Map0_Area) * 100
									Tab_gap$Area_Gain_Ratio[n_tl] <- (Tab_gap$Area_Gain[n_tl] / Map0_Area) * 100
									Tab_gap$Area_Gain_Ratio_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio[n_tl] * -1
									Tab_gap$Area_Gain_Ratio_Outside[n_tl] <- (Tab_gap$Area_Gain[n_tl] / (T_Area - Map0_Area)) * 100
									Tab_gap$Area_Gain_Ratio_Outside_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio_Outside[n_tl] * -1
									Tab_gap$Vulnerability_Area_Loss_Ratio[n_tl] <- 1 - Tab_gap$Area_Ratio[n_tl]
									Tab_gap$Vulnerability_Area_LossIN_GainOUT_Ratio[n_tl] <- (Tab_gap$Area_Loss_Ratio[n_tl] / 100) - (Tab_gap$Area_Gain_Ratio_Outside[n_tl] / 100)
								}
							} else {
								for (y in 1:ly) {
									incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", y))
									n_tl <- n_tl + 1
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
									R_gap <- raster(file.path(dir_path, Map2))
									
									T_Area0 <- freq(R_gap, value = 0)
									T_Area1 <- freq(R_gap, value = 1)
									T_Area2 <- freq(R_gap, value = -1)
									T_Area3 <- freq(R_gap, value = -2)
									T_Area <- T_Area0 + T_Area1 + T_Area2 + T_Area3
									Map0_Area0 <- freq(R_gap, value = 0)
									Map0_Area1 <- freq(R_gap, value = -1)
									Map0_Area <- Map0_Area0 + Map0_Area1
									Map_Area0 <- freq(R_gap, value = 0)
									Map_Area1 <- freq(R_gap, value = 1)
									Map_Area <- Map_Area0 + Map_Area1
									
									Tab_gap$Species[n_tl] <- s
									Tab_gap$Climate_Model[n_tl] <- d
									Tab_gap$Climate_Scenario[n_tl] <- c
									Tab_gap$Model[n_tl] <- m
									Tab_gap$Year[n_tl] <- ylist[y]
									Tab_gap$Area[n_tl] <- Map_Area
									Tab_gap$Area_Loss[n_tl] <- T_Area2
									Tab_gap$Area_Stay[n_tl] <- T_Area0
									Tab_gap$Area_Gain[n_tl] <- T_Area1
									Tab_gap$Area_Ratio[n_tl] <- Map_Area / Map0_Area
									Tab_gap$Area_Loss_Ratio[n_tl] <- (Tab_gap$Area_Loss[n_tl] / Map0_Area) * 100
									Tab_gap$Area_Stay_Ratio[n_tl] <- (Tab_gap$Area_Stay[n_tl] / Map0_Area) * 100
									Tab_gap$Area_Gain_Ratio[n_tl] <- (Tab_gap$Area_Gain[n_tl] / Map0_Area) * 100
									Tab_gap$Area_Gain_Ratio_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio[n_tl] * -1
									Tab_gap$Area_Gain_Ratio_Outside[n_tl] <- (Tab_gap$Area_Gain[n_tl] / (T_Area - Map0_Area)) * 100
									Tab_gap$Area_Gain_Ratio_Outside_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio_Outside[n_tl] * -1
									Tab_gap$Vulnerability_Area_Loss_Ratio[n_tl] <- 1 - Tab_gap$Area_Ratio[n_tl]
									Tab_gap$Vulnerability_Area_LossIN_GainOUT_Ratio[n_tl] <- (Tab_gap$Area_Loss_Ratio[n_tl] / 100) - (Tab_gap$Area_Gain_Ratio_Outside[n_tl] / 100)
								} 
							}
						}
					}
				}
			}
			Tab_gap_sum <- rbind(Tab_gap_sum, Tab_gap)
			write.csv(Tab_gap, file = file.path(dir_path, paste(s, "_VINDEX.csv", sep = "", collapse = "--")))
#		  }
		}
		write.csv(Tab_gap_sum, file = file.path(save_path, paste(input$SA_MO_Dir_Folder, "_Speices_VINDEX.csv", sep = "")))
		})
		##### End GAP analyzing =========================================    
	})
	
	
	output$SA_AO_Dir_Folder <- renderUI({
	  SA_AO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  SA_AO_Dir_Folder_selected <- SA_AO_Dir_Folder_list[1]
	  selectInput("SA_AO_Dir", SA_Name_Out_Folder,
	              choices = c(SA_AO_Dir_Folder_list),
	              selected = SA_AO_Dir_Folder_selected
	  )
	  
	})
	
	output$SA_AO_Model_Name <- renderUI({
	  G$SA_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir)
	  SA_Name_Species_list <- list.dirs(path = G$SA_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  SA_AO_Model_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir, SA_Name_Species_list[1]), full.names = FALSE, recursive = FALSE)
	  SA_AO_Model_Name_selected <- SA_AO_Model_Name_list[1]
	  selectInput("SA_AO_Model_Name_Input", SA_Name_Out_Model,
	              choices = c(SA_AO_Model_Name_list),
	              selected = SA_AO_Model_Name_selected
	  )
	  
	})
	
	observeEvent(input$SA_AO_Species_Sel_All, {
	  G$SA_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir)
	  SA_Name_Species_list <- list.dirs(path = G$SA_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  G$SA_Name_Species_selected <<- SA_Name_Species_list
	})
	
	observeEvent(input$SA_AO_Species_Sel_None, {
	  G$SA_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir)
	  SA_Name_Species_list <- list.dirs(path = G$SA_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  G$SA_Name_Species_selected <<- ""  #SA_Name_Species_list[1]
	})
	
	output$SA_AO_Species <- renderUI({
	  G$SA_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir)
	  SA_Name_Species_list <- list.dirs(path = G$SA_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  checkboxGroupInput("SA_AO_Species", SA_Name_Out_Species,
	                     choices = c(SA_Name_Species_list),
	                     selected = G$SA_Name_Species_selected
	  )
	})

	output$SA_AO_SDM_model <- renderUI({
	  G$SA_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir)
	  SA_Name_Species_list <- list.dirs(path = G$SA_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
		destfile <- file.path(G$SA_AO_Dir_Folder, SA_Name_Species_list[1], G$DIR_NAME_SDM, paste(as.name(paste(SA_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		SA_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		SA_Name_Models_selected <- SA_Name_Models_list[1]
		checkboxGroupInput("SA_AO_SDM_model", SA_Name_Out_SDM,
			choices = c(SA_Name_Models_list),
			selected = SA_Name_Models_selected
		)
	})
	
	output$SA_AO_UI_plot <- renderUI({
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SA_AO_Species
		dlist <- input$SA_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SA_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SA_AO_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SA_AO_Project_year
#		dtlist <- input$SA_AO_Dispersal_type
	
		n <- 0
		ls <- length(slist)
		ld <- length(dlist)
		lc <- length(clist)
		lm <- length(mlist)
		ly <- length(ylist)		
#		ldt <- length(dtlist)
		tl <- ls * ld * lc * lm * ly # * ldt
		
		nc <- 2
		if (tl <  2) {
			nr <- round(tl / nc) + 1
		} else {
			nr <- round((tl + 0.1) / nc)
		}
		
		ws <- nc * 500
		hs <- nr * 500
		plotOutput("SA_AO_OU_plot", width = ws, height = hs)
  })
	
	output$SA_AO_OU_plot <- renderPlot({
		#####========================================================
		##### Plot GAP output =========================================
		
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SA_AO_Species
		dlist <- input$SA_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SA_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SA_AO_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SA_AO_Project_year
#		dtlist <- input$SA_AO_Dispersal_type
	
		ls <- length(slist)
		ld <- length(dlist)
		lc <- length(clist)
		lm <- length(mlist)
		ly <- length(ylist)
#		ldt <- length(dtlist)
		tl <- ls * ld * lc * lm * ly  # * ldt 
		
		nc <- 2
		if (tl <  2) {
			nr <- round(tl / nc) + 1
		} else {
			nr <- round((tl + 0.1) / nc)
		}
	
		par(mfrow = c(nr,nc), cex.main = 1.2)
	
		for (s in slist) {
#		  for (dt in dtlist) {
		    G$SA_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir)
		    dir_path <- file.path(G$SA_AO_Dir_Folder, s, input$SA_AO_Model_Name_Input)
			for (d in dlist) {
				for (c in clist) {
					for (m in mlist) {
						if (ly > 0) {
							if (ly == 1 && ylist[1] == G$Var_Current_Folder) {
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
								plot(R_Map1, main = Map1)
							} else if (ly > 1 && ylist[1] == G$Var_Current_Folder) {
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, G$IMG_File, sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
								plot(R_Map1, main = Map1)
								for (y in 2:ly) {
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
									R_Map2 <- raster(file.path(dir_path, Map2))
									plot(R_Map2, main = Map2)
								}
							} else {
								for (y in 1:ly) {
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, G$IMG_File, sep = "")
									R_Map2 <- raster(file.path(dir_path, Map2))
									plot(R_Map2, main = Map2)
								}
							}
						}
					}
				}
			}
#		  }
		}
    ##### End Plot GAP output =========================================
	})
	
	output$SA_AO_IV_Table <- DT::renderDataTable({
        if (input$SA_AO_IV_Data == "Species") {
            if (length(input$SA_AO_Species) > 0) {
                if (length(input$SA_AO_Species) == 1) {
                    destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir, input$SA_AO_Species[1], input$SA_AO_Model_Name_Input, paste(as.name(paste(input$SA_AO_Species[1], "_VINDEX.csv", sep = "")), sep = "", collapse = "--"))
	                vindex <- read.csv(destfile)
	                G_FILE_species_vindex <<- vindex
	                vindex
	            } else {
	                destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir, input$SA_AO_Species[1], input$SA_AO_Model_Name_Input, paste(as.name(paste(input$SA_AO_Species[1], "_VINDEX.csv", sep = "")), sep = "", collapse = "--"))
	                vindex <- read.csv(destfile)
	                for (s in input$SA_AO_Species[-1]) {
	                    destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir, s, input$SA_AO_Model_Name_Input, paste(as.name(paste(s, "_VINDEX.csv", sep = "")), sep = "", collapse = "--"))
	                    vindex0 <- read.csv(destfile)
	                    vindex <- rbind(vindex, vindex0)
	                }
	                G_FILE_species_vindex <<- vindex
	                vindex
	            } 
            } else {
                showModal(modalDialog(
                title = "Error Message",
                paste("Vulnerable Index file doesn't exist.")
                ))
            }
        } else {
	      if (!file.exists(destfile)) {
	          showModal(modalDialog(
	              title = "Error Message",
	              paste("Vulnerable Index file doesn't exist.")
	          ))
	      } else {
	          destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$SA_AO_Dir, paste(input$SA_AO_Model_Name_Input, "_Speices_VINDEX.csv", sep = "")) # , sep = "", collapse = "--")
	          vindex <- read.csv(destfile)
	          G_FILE_species_vindex <<- vindex
	          vindex
	      }
	   }
	})

	
	output$SA_AO_IV_Plot11 <- renderPlot({
		rs <- input$SA_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
	      vindex <- filter(vindex, Species %in% input$SA_AO_Species)
	      vindex <- filter(vindex, Climate_Model %in% input$SA_AO_Climate_model)
	      vindex <- filter(vindex, Climate_Scenario %in% input$SA_AO_Climate_scenario)
	      vindex <- filter(vindex, Model %in% input$SA_AO_SDM_model)
	      vindex <- filter(vindex, Year %in% input$SA_AO_Project_year)

			Group <- vindex[, input$SA_AO_IV_UI_plot1]
			ggplot(vindex, aes(x = Area_Loss_Ratio, y = Area_Gain_Ratio_Reverse, color = Group)) +
				geom_point(size = 6) +
				geom_text(aes(label = Vulnerability_Area_Loss_Ratio), size = 3, hjust = 0.5, vjust = 3) + #, position =     "stack") +
				# horizontal
				geom_hline(yintercept = -50, color="orange", size=1) + 
				# vertical
				geom_vline(xintercept = 50, color="orange", size=1) +
				# Add arrow
				annotate("segment", x = 0, xend = 100, y = -100, yend = 0, colour = "purple", size = 2, alpha = 0.6, arrow = arrow()) +
			  labs(title= "Vulnerability Matrix", 
			     subtitle="Loss and Gain Area Ratio", caption = "Vulnerability = (Loss Ratio + Reverse Gain Ratio) / 100", x = "Loss Ratio(Loss Area/Current Area)", y = "Reverse Gain Ratio(Gain Area/Current Area)") +
			  theme(
			    plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
			    plot.subtitle = element_text(color = "mediumvioletred", size = 16, hjust = 0.5),
			    plot.caption = element_text(color = "deepskyblue4", size = 12, face = "italic"))
	  
		}   
	})
	
	output$SA_AO_IV_Plot12 <- renderPlot({
		rs <- input$SA_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
			    vindex <- filter(vindex, Species %in% input$SA_AO_Species)
			    vindex <- filter(vindex, Climate_Model %in% input$SA_AO_Climate_model)
			    vindex <- filter(vindex, Climate_Scenario %in% input$SA_AO_Climate_scenario)
			    vindex <- filter(vindex, Model %in% input$SA_AO_SDM_model)
			    vindex <- filter(vindex, Year %in% input$SA_AO_Project_year)

			Group <- vindex[, input$SA_AO_IV_UI_plot1]
			ggplot(vindex, aes(x = Area_Loss_Ratio, y = Area_Gain_Ratio_Outside_Reverse, color = Group)) +
				geom_point(size = 6) +
				geom_text(aes(label = Vulnerability_Area_LossIN_GainOUT_Ratio), size = 3, hjust = 0.5, vjust = 3) + #, position =     "stack") +
				# horizontal
				geom_hline(yintercept = -50, color="orange", size=1) + 
				# vertical
				geom_vline(xintercept = 50, color="orange", size=1) +
				# Add arrow
				annotate("segment", x = 0, xend = 100, y = -100, yend = 0, colour = "purple", size = 2, alpha = 0.6, arrow = arrow()) +
			  labs(title= "Vulnerability Matrix", 
			     subtitle="Loss and Gain Area Ratio", caption = "Vulnerability = (Loss Ratio + Reverse GainOUT Ratio) / 100", x = "Loss Ratio(Loss Area/Current Area)", y = "Reverse GainOUT Ratio(Gain Area/Total Area outside Current Area)") +
			  theme(
			    plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
			    plot.subtitle = element_text(color = "mediumvioletred", size = 16, hjust = 0.5),
			    plot.caption = element_text(color = "deepskyblue4", size = 12, face = "italic"))
			
	  }
	})
	
	output$SA_AO_IV_Plot21 <- renderPlot({
		rs <- input$SA_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
			    vindex <- filter(vindex, Species %in% input$SA_AO_Species)
			    vindex <- filter(vindex, Climate_Model %in% input$SA_AO_Climate_model)
			    vindex <- filter(vindex, Climate_Scenario %in% input$SA_AO_Climate_scenario)
			    vindex <- filter(vindex, Model %in% input$SA_AO_SDM_model)
			    vindex <- filter(vindex, Year %in% input$SA_AO_Project_year)

			Group <- vindex[, input$SA_AO_IV_UI_plot2]
			ggplot(vindex, aes(x = Year, y = Vulnerability_Area_Loss_Ratio, group = Group, color = Group, linetype = Group)) +
			  geom_line() +
				geom_point(shape = 21, color = "black", fill = "darkorange", size=6) +
#				theme_ipsum() +
			  labs(title= "Vulnerability Pattern", 
			       subtitle="Vulnerability Change by Year", caption = "Vulnerability = (Loss Ratio + Reverse Gain Ratio) / 100", x = "Year", y = "Vulnerability") +
			  theme(
			    plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
			    plot.subtitle = element_text(color = "mediumvioletred", size = 16, hjust = 0.5),
			    plot.caption = element_text(color = "deepskyblue4", size = 12, face = "italic"))
			
	  }
	})
	
	output$SA_AO_IV_Plot22 <- renderPlot({
		rs <- input$SA_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
			    vindex <- filter(vindex, Species %in% input$SA_AO_Species)
			    vindex <- filter(vindex, Climate_Model %in% input$SA_AO_Climate_model)
			    vindex <- filter(vindex, Climate_Scenario %in% input$SA_AO_Climate_scenario)
			    vindex <- filter(vindex, Model %in% input$SA_AO_SDM_model)
			    vindex <- filter(vindex, Year %in% input$SA_AO_Project_year)

			Group <- vindex[, input$SA_AO_IV_UI_plot2]
			ggplot(vindex, aes(x = Year, y = Vulnerability_Area_LossIN_GainOUT_Ratio, group = Group)) +
				geom_line(aes(color = Group, linetype = Group)) +
				geom_point(shape = 21, color = "black", fill = "dodgerblue", size=6) +
#				theme_ipsum() +
			  labs(title= "Vulnerability Pattern", 
			     subtitle="Vulnerability Change by Year", caption = "Vulnerability = (Loss Ratio + Reverse GainOUT Ratio) / 100", x = "Year", y = "Vulnerability") +
			  theme(
			    plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
			    plot.subtitle = element_text(color = "mediumvioletred", size = 16, hjust = 0.5),
			    plot.caption = element_text(color = "deepskyblue4", size = 12, face = "italic"))
			
	  }
	})
	
	output$SA_AO_IV_Plot31 <- renderPlot({
	  rs <- input$SA_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
	    vindex <- filter(vindex, Species %in% input$SA_AO_Species)
	    vindex <- filter(vindex, Climate_Model %in% input$SA_AO_Climate_model)
	    vindex <- filter(vindex, Climate_Scenario %in% input$SA_AO_Climate_scenario)
	    vindex <- filter(vindex, Model %in% input$SA_AO_SDM_model)
	    vindex <- filter(vindex, Year %in% input$SA_AO_Project_year)
	    
	    vindex$X <- ifelse(vindex$Vulnerability_Area_Loss_Ratio < 0, "below", "above")  # above / below avg flag
	    vindex <- vindex[order(vindex$Vulnerability_Area_Loss_Ratio), ]  # sort
	    vindex[, input$SA_AO_IV_UI_plot3] <- factor(vindex[, input$SA_AO_IV_UI_plot3], ordered = is.ordered(vindex)) #, levels = vindex$Species)  # convert to factor to retain sorted order in plot.
	    Group <- vindex[, input$SA_AO_IV_UI_plot3]
	    
	    # Diverging Barcharts
	    ggplot(vindex, aes(x=Group, y=Vulnerability_Area_Loss_Ratio, label=Vulnerability_Area_Loss_Ratio)) + 
	      geom_bar(stat='identity', aes(fill=X), width=.5)  +
#	      geom_text(aes(x=Group, y=Vulnerability_Area_Loss_Ratio, ymax=Vulnerability_Area_Loss_Ratio, label=Vulnerability_Area_Loss_Ratio, 
#	                    hjust=ifelse(sign(Vulnerability_Area_Loss_Ratio)>0, 1, 0)), 
#	                position = position_dodge(width=1)) +
	      scale_fill_manual(name="Vulnerability", 
	                        labels = c("Above Average", "Below Average"), 
	                        values = c("above"="#00ba38", "below"="#f8766d")) + 
	      labs(title= "Vulnerability Order", 
	           subtitle="Vulnerability Ordering by Group", caption = "Vulnerability = (Loss Ratio + Reverse Gain Ratio) / 100", x = input$SA_AO_IV_UI_plot3, y = "Vulnerability") +
	      theme(
	        plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
	        plot.subtitle = element_text(color = "mediumvioletred", size = 16, hjust = 0.5),
	        plot.caption = element_text(color = "deepskyblue4", size = 12, face = "italic")) +
	      coord_flip()
	    
	  }
	})
	
	output$SA_AO_IV_Plot32 <- renderPlot({
	  rs <- input$SA_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
	    vindex <- filter(vindex, Species %in% input$SA_AO_Species)
	    vindex <- filter(vindex, Climate_Model %in% input$SA_AO_Climate_model)
	    vindex <- filter(vindex, Climate_Scenario %in% input$SA_AO_Climate_scenario)
	    vindex <- filter(vindex, Model %in% input$SA_AO_SDM_model)
	    vindex <- filter(vindex, Year %in% input$SA_AO_Project_year)
	    
	    vindex$X <- ifelse(vindex$Vulnerability_Area_LossIN_GainOUT_Ratio < 0, "below", "above")  # above / below avg flag
	    vindex <- vindex[order(vindex$Vulnerability_Area_LossIN_GainOUT_Ratio), ]  # sort
	    vindex[, input$SA_AO_IV_UI_plot3] <- factor(vindex[, input$SA_AO_IV_UI_plot3], ordered = is.ordered(vindex)) #, levels = vindex$Species)  # convert to factor to retain sorted order in plot.
	    Group <- vindex[, input$SA_AO_IV_UI_plot3]
	    
	    # Diverging Barcharts
	    ggplot(vindex, aes(x=Group, y=Vulnerability_Area_LossIN_GainOUT_Ratio, label=Vulnerability_Area_LossIN_GainOUT_Ratio)) + 
	      geom_bar(stat='identity', aes(fill=X), width=.5)  +
	      scale_fill_manual(name="Vulnerability", 
	                        labels = c("Above Average", "Below Average"), 
	                        values = c("above"="#00ba38", "below"="#f8766d")) + 
	      labs(title= "Vulnerability Order", 
	           subtitle="Vulnerability Ordering by Group", caption = "Vulnerability = (Loss Ratio + Reverse GainOUT Ratio) / 100", x = input$SA_AO_IV_UI_plot3, y = "Vulnerability") +
	      theme(
	        plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
	        plot.subtitle = element_text(color = "mediumvioletred", size = 16, hjust = 0.5),
	        plot.caption = element_text(color = "deepskyblue4", size = 12, face = "italic")) +
	      coord_flip()
	    
	  }
	})

	
	output$HA_MI_Dir_Folder <- renderUI({
	  HA_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  HA_MI_Dir_Folder_selected <- HA_MI_Dir_Folder_list[1]
	  selectInput("HA_MI_Dir", HA_Name_Analysis_Folder,
	              choices = c(HA_MI_Dir_Folder_list),
	              selected = HA_MI_Dir_Folder_selected
	  )
	 
	})
	
	output$HA_MI_Dir_Folder_Name <- renderUI({
	    G$HA_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir)
	    HA_Name_Species_list <- list.dirs(path = G$HA_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    HA_MI_Dir_Folder_Name_list <- list.dirs(path = file.path(G$HA_MI_Dir_Folder, HA_Name_Species_list[1]), full.names = FALSE, recursive = FALSE)
	    HA_MI_Dir_Folder_Name_selected <- HA_MI_Dir_Folder_Name_list[1]
	    selectInput("HA_MI_Dir_Folder", HA_Name_Analysis_Folder,
	                choices = c(HA_MI_Dir_Folder_Name_list),
	                selected = HA_MI_Dir_Folder_Name_selected
	  )
	  
	})
	
	observeEvent(input$HA_CA_Species_Sel_All, {
	    G$HA_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir)
	    HA_Name_Species_list <- list.dirs(path = G$HA_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$HA_Name_Species_selected <<- HA_Name_Species_list
	})
	
	observeEvent(input$HA_CA_Species_Sel_None, {
	    G$HA_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir)
	    HA_Name_Species_list <- list.dirs(path = G$HA_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$HA_Name_Species_selected <<- ""  #SA_Name_Species_list[1]
	})
	
	output$HA_CA_Species <- renderUI({
	    G$HA_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir)
		HA_Name_Species_list <- list.dirs(path = G$HA_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
		checkboxGroupInput("HA_CA_Species", HA_Name_Analysis_Species,
			choices = c(HA_Name_Species_list),
			selected = G$HA_Name_Species_selected
		)
	})
	
	output$HA_CA_SDM_model <- renderUI({
	    G$HA_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir)
	    HA_Name_Species_list <- list.dirs(path = G$HA_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
		destfile <- file.path(G$HA_MI_Dir_Folder, HA_Name_Species_list[1], G$DIR_NAME_SDM, paste(as.name(paste(HA_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		HA_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		HA_Name_Models_selected <- HA_Name_Models_list[1]
		checkboxGroupInput("HA_CA_SDM_model", HA_Name_Analysis_Species,
			choices = c(HA_Name_Models_list),
			selected = HA_Name_Models_selected
		)
	})
	
	observeEvent(input$HA_MO_Dir_Folder, {
#	  showModal(modalDialog(
#	    title = "Message",
#	    "A folder path and name is recommended in english!"
#	  ))
		volumes <- c(main = file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat))
		shinyDirChoose(input, 'HA_MO_Dir_Folder', roots = volumes) # , defaultPath = "/MOTIVE_projects", defaultRoot = G$SE_Dir_Project)
		G$HA_MO_Dir_Folder <<- parseDirPath(volumes, input$HA_MO_Dir_Folder)
		output$HA_MO_Dir_Folder <- renderText({G$HA_MO_Dir_Folder})
#		G$HA_AO_MO_Dir_Folder <<- G$HA_MO_Dir_Folder
#		output$HA_AO_MO_Dir_Folder <- renderText({G$HA_AO_MO_Dir_Folder})
	})
	
	observeEvent(input$HA_VA_Action_Analysis, {
	  
		# setting Climate change scenarios, Future time, Species and current environmental path
		dlist <- input$HA_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$HA_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
#		dtlist <- input$HA_CA_Dispersal_type
		mlist <- input$HA_CA_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$HA_CA_Project_year
		slist <- input$HA_CA_Species
    
		n <- 0
		ls <- length(slist)
		ld <- length(dlist)
		lc <- length(clist)
		lm <- length(mlist)
		ly <- length(ylist)
#		ldt <- length(dtlist)
		tl <- ld * lc * lm * ly
		
		sr_list <- NULL
		loss_list <- NULL
		stay_list <- NULL
		gain_list <- NULL
		
		withProgress(message = 'Runing Habitat Impact and Vulnerability Analysis.........', value = 0, {
		G$HA_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir)  
		for (d in dlist) {
			for (c in clist) {
				for (m in mlist) {
					for (y in ylist) {
					  incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y))
							if(y == ylist[1]) {
								for (s in slist) {
									dir_path <- file.path(G$HA_MI_Dir_Folder, s, input$HA_MI_Dir_Folder)
									img <- file.path(dir_path, paste("PRED", "_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = ""))
									sr_list <- c(sr_list, img)
								}
								save_path <- G$HA_MO_Dir_Folder
								sr_list <- grep("PRED", sr_list, value = TRUE)
								sr_stack <- stack(sr_list)
								sr_raster <- overlay(sr_stack, fun=sum)
								sr_raster1 <- sr_raster
								loss_raster <- sr_raster
								loss_raster[] <- 0
								stay_raster <- sr_raster
								gain_raster <- sr_raster
								gain_raster[] <- 0
								writeRaster(sr_raster, file = file.path(save_path, paste(as.name(paste("HA_SR_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(loss_raster, file = file.path(save_path, paste(as.name(paste("HA_LOSS_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(stay_raster, file = file.path(save_path, paste(as.name(paste("HA_STAY_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(gain_raster, file = file.path(save_path, paste(as.name(paste("HA_GAIN_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								vi1_raster <- sr_raster
								vi1_raster[] <- 0
								vi2_raster <- sr_raster
								vi2_raster[] <- 0
								vi3_raster <- sr_raster
								vi3_raster[] <- 0
								writeRaster(vi1_raster, file = file.path(save_path, paste(as.name(paste("HA_VI1_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(vi2_raster, file = file.path(save_path, paste(as.name(paste("HA_VI2_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(vi3_raster, file = file.path(save_path, paste(as.name(paste("HA_VI3_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								sr_list <- NULL
								loss_list <- NULL
								stay_list <- NULL
								gain_list <- NULL
							} else {
#							    incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y))
								for (s in slist) {
								  dir_path <- file.path(G$HA_MI_Dir_Folder, s, input$HA_MI_Dir_Folder)
									img <- file.path(dir_path, paste("PRED", "_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = ""))
									sr_list <- c(sr_list, img)
									img <- file.path(dir_path, paste("LOSS_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = ""))
									loss_list <- c(loss_list, img)
									img <- file.path(dir_path, paste("STAY_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = ""))
									stay_list <- c(stay_list, img)
									img <- file.path(dir_path, paste("GAIN_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = ""))
									gain_list <- c(gain_list, img)
								}
								save_path <- G$HA_MO_Dir_Folder
								sr_list <- grep("PRED", sr_list, value = TRUE)
								sr_stack <- stack(sr_list)
								sr_raster <- overlay(sr_stack, fun=sum)
								sr_raster2 <- sr_raster
								losssr_raster <- sr_raster2 - sr_raster1
								writeRaster(sr_raster, file = file.path(save_path, paste(as.name(paste("HA_SR_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(losssr_raster, file = file.path(save_path, paste(as.name(paste("HA_VI1_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								loss_list <- grep("LOSS", loss_list, value = TRUE)
								loss_stack <- stack(loss_list)
								loss_raster <- overlay(loss_stack, fun=sum)
								writeRaster(loss_raster, file = file.path(save_path, paste(as.name(paste("HA_LOSS_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								stay_list <- grep("STAY", stay_list, value = TRUE)
								stay_stack <- stack(stay_list)
								stay_raster <- overlay(stay_stack, fun=sum)
								writeRaster(stay_raster, file = file.path(save_path, paste(as.name(paste("HA_STAY_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								gain_list <- grep("GAIN", gain_list, value = TRUE)
								gain_stack <- stack(gain_list)
								gain_raster <- overlay(gain_stack, fun=sum)
								writeRaster(gain_raster, file = file.path(save_path, paste(as.name(paste("HA_GAIN_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								vi2_raster <- sr_raster
								vi2_raster <- loss_raster / sr_raster1
								vi2_raster[sr_raster1 == 0] <- 0
								writeRaster(vi2_raster, file = file.path(save_path, paste(as.name(paste("HA_VI2_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")),overwrite = TRUE)
								
								vi3_raster <- sr_raster
								vi3_raster <- (1 - (loss_raster / sr_raster1)) + (gain_raster / (length(slist) - sr_raster1))
								vi3_raster[sr_raster1 == 0] <- 0
								writeRaster(vi3_raster, file = file.path(save_path, paste(as.name(paste("HA_VI3_", d, "_", c, "_", m, "_", y, G$IMG_File, sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								sr_list <- NULL
								loss_list <- NULL
								stay_list <- NULL
								gain_list <- NULL
							}				    
					}
				}
			}
		}
		    
		    #####
		    
#		    destfile <- file.path(G$HA_MO_Dir_Folder, "HabitatAssessment_Options.csv")
		    
		    HA_variables <- setNames(data.frame(matrix(ncol = 8, nrow = 5000)), c("input$SDM_Folder", "HA_CA_Species_Number", "input$HA_CA_Species", "input$HA_CA_Dispersal_type", "input$HA_CA_Climate_model", "input$HA_CA_Climate_scenario", 
		                                                                         "input$HA_CA_Project_year", "input$HA_CA_SDM_model"
		    ))
		    
		    HA_variables[1:length(G$HA_MI_Dir_Folder), "input$SDM_Folder"] <- G$HA_MI_Dir_Folder
		    HA_variables[1:1, "HA_CA_Species_Number"] <- length(input$HA_CA_Species)
		    HA_variables[1:length(input$HA_CA_Species), "input$HA_CA_Species"] <- input$HA_CA_Species
		    HA_variables[1:length(input$HA_CA_Dispersal_type), "input$HA_CA_Dispersal_type"] <- input$HA_MI_Dir_Folder
		    HA_variables[1:length(input$HA_CA_Climate_model), "input$HA_CA_Climate_model"] <- input$HA_CA_Climate_model
		    HA_variables[1:length(input$HA_CA_Climate_scenario), "input$HA_CA_Climate_scenario"] <- input$HA_CA_Climate_scenario
		    HA_variables[1:length(input$HA_CA_Project_year), "input$HA_CA_Project_year"] <- input$HA_CA_Project_year
		    HA_variables[1:length(input$HA_CA_SDM_model), "input$HA_CA_SDM_model"] <- input$HA_CA_SDM_model
		    
		    HA_variables[is.na(HA_variables)] <- ""
		    write.csv(HA_variables, file = file.path(G$HA_MO_Dir_Folder, "HabitatAssessment_Options.csv"))
		    shinyalert(title = "You did it!", type = "success")
		    #####		    
		    
		    
		})
	})
	
	observeEvent(input$HA_VA_Action_Admin_Species, {
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  alist <- input$HA_VA_Admin
	  dlist <- input$HA_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$HA_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$HA_CA_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$HA_CA_Project_year
	  slist <- input$HA_CA_Species
	  vlist <- c("PRED", "LOSS_PRED", "STAY_PRED", "GAIN_PRED") 
	  
	  n <- 0
	  la <- length(alist)
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  lv <- length(vlist)
	  
	  tls <- la * ls * ld * lc * lm * ly * lv
	  
	  
	  if (length(slist) > 0) {
	    withProgress(message = paste("Species Analyzing by ", input$HA_VA_Admin), value = 0, {
	      for (a in alist) {
	        for (s in slist) {
	          dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir, s, input$HA_MI_Dir_Folder)
	          dataFiles <- dir(G$SE_Dir_GIS, paste(a, ".*", sep = ""), ignore.case = TRUE, all.files = TRUE)
	          file.copy(file.path(G$SE_Dir_GIS, dataFiles), dir_path, overwrite = TRUE)
	          #	    poly <- readShapePoly(file.path(dir_path, paste(a, ".shp", sep = "")))
	          poly <- readOGR(dsn=dir_path, layer=a)
	          df <- read.dbf(file.path(G$SE_Dir_GIS, paste(a, ".dbf", sep = "")))
	          df <- cbind(df, SPECIES = s)
	          for (d in dlist) {
	            for (c in clist) {
	              for (m in mlist) {
	                for (y in ylist) {
	                  for (v in vlist) {
	                    incProgress(1/tls, detail = paste("Doing part", a, "_", s, "_", d, "_", c, "_", m, "_", y))
	                    img <- file.path(dir_path, paste(v, "_",  d, "_", c, "_", y, "_", s, "_", m, G$IMG_File, sep = ""))
	                    r <- raster(img)
	                    df1 <- raster::extract(r, poly, fun = sum, na.rm = TRUE, df=TRUE)
	                    #write to a data frame
	                    df1 <- data.frame(df1[-1])
	                    colnames(df1) <- c(paste(v, "_",  d, "_", c, "_", m, "_", y, sep = ""))
	                    df1[is.na(df1)] <- 0
	                    df <- cbind(df, df1)
	                  }
	                }
	              }
	            }
	          }
	          #write to a CSV file
	          write.csv(df, file = file.path(dir_path, paste(a, ".csv", sep="")))
	          csv <- read.csv(file.path(dir_path, paste(a, ".csv", sep="")))
	          csv <- csv[-1]
	          #	        write.dbf(df, file.path(dir_path, paste(a, ".dbf", sep = "")))
	          try1 <- paste(tempfile(), ".dbf", sep = "")
	          write.dbf(csv, try1)
	          file.copy(try1, file.path(dir_path, paste(a, ".dbf", sep="")), overwrite = TRUE)
	          file.remove(try1)
	          
	          if (s == slist[1]) {
	            df_sp0 <- df
	            df_sp <- df
	          } else {
	            df_sp <- rbind(df_sp, df)
	          }
	        }
	        #	      dir_path <- G$HA_MO_Dir_Folder
	        #	      write.csv(df_sp, file = file.path(dir_path, paste(a, ".csv", sep="")))
	        #	      write.dbf(df_sp, file.path(dir_path, paste(a, ".dbf", sep = "")))
	      }
	    })
	    shinyalert(title = "You did it!", type = "success")
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Select Species.")
	    ))
	  }
	})
	
	observeEvent(input$HA_VA_Action_Admin_Group, {
	    
	    # setting Climate change scenarios, Future time, Species and current environmental path
	  destfile <- file.path(G$HA_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_MO_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  withProgress(message = paste("Group Analyzing by ", input$HA_VA_Admin), value = 0, {
	  if (length(HA_MO_SP_List) > 0) {
	    for (a in input$HA_VA_Admin) {
	      incProgress(1/length(input$HA_VA_Admin), detail = paste("Doing part ", a))
	      dataFiles <- dir(G$SE_Dir_GIS, paste(a, ".*", sep = ""), ignore.case = TRUE, all.files = TRUE)
	      file.copy(file.path(G$SE_Dir_GIS, dataFiles), G$HA_MO_Dir_Folder, overwrite = TRUE)
	      
  	    if (length(HA_MO_SP_List) == 1) {
  	      destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir, HA_MO_SP_List[1], input$HA_MI_Dir_Folder, paste(a, ".csv", sep = ""))
  	      sindex <- read.csv(destfile)
  	      #sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SR_SIDO_SP_UI), ]
  	    } else {
  	      destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir, HA_MO_SP_List[1], input$HA_MI_Dir_Folder, paste(a, ".csv", sep = ""))
  	      sindex <- read.csv(destfile)
  	      for (s in HA_MO_SP_List[-1]) {
  	        destfile <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_MI_Dir, s, input$HA_MI_Dir_Folder, paste(a, ".csv", sep = ""))
  	        sindex0 <- read.csv(destfile)
  	        sindex <- rbind(sindex, sindex0)
  	      }
  	      #sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SR_SIDO_SP_UI), ]
  	    }
	      
	      df <- sindex
	      ps <- grep("SPECIES", colnames(df))
	      n_scol <- ps + 1
	      n_ecol <- ncol(df)
	      df[, n_scol:n_ecol][df[, n_scol:n_ecol] > 0] <- 1
	      
	      if (a == "SD") {
	        group <- c("SD_CD", "SD_KOR")
	        ID <- "SD_CD"
        } else if (a == "SGG") {
          group <- c("SGG_CD", "SD_KOR", "SGG_KOR")
          ID <- "SGG_CD"
        } else if (a == "NP") {
          group <- c("WDPA_PID", "ORIG_NAME")
          ID <- "WDPA_PID"
        } else if (a == "BR") {
          group <- c("WDPA_PID", "ORIG_NAME")
          ID <- "WDPA_PID"
        } else {
          group <- c("Region", "DMZRegion")
          ID <- "DMZRegion"
        }

	      if (length(group) == 1) {
	        df_stat <- aggregate(x = df[, n_scol:n_ecol], 
	                             by = list(df[[group]]),  
	                             FUN = sum)
	        colnames(df_stat)[1] <- group
	      } else if (length(group) == 2) {
	        group1 <- group[1]
	        group2 <- group[2]
	        df_stat <- aggregate(x = df[, n_scol:n_ecol], 
	                             by = list(df[[group1]], df[[group2]]),  
	                             FUN = sum)
	        colnames(df_stat)[1:2] <- group
	      } else {
	        group1 <- group[1]
	        group2 <- group[2]
	        group3 <- group[3]
	        df_stat <- aggregate(x = df[, n_scol:n_ecol], 
	                             by = list(df[[group1]], df[[group2]], df[[group3]]),  
	                             FUN = sum)
	        colnames(df_stat)[1:3] <- group
	      }
	      dbf <- read.dbf(file.path(G$SE_Dir_GIS, paste(a, ".dbf", sep="")))
	      ps <- ncol(dbf) + length(group)
	      pn <- ncol(dbf) + ncol(df_stat) - 1
	      dbf_j <- inner_join(dbf, df_stat, by = ID)
	      dbf_j <- dbf_j[, c(1:ncol(dbf), ps:pn)]
	      colnames(dbf_j)[1:ncol(dbf)] <- colnames(dbf)
	  
	      write.csv(dbf_j, file = file.path(G$HA_MO_Dir_Folder, paste(a, ".csv", sep="")))
	      try1 <- paste(tempfile(), ".dbf", sep = "")
	      write.dbf(dbf_j, try1)
	      file.copy(try1, file.path(G$HA_MO_Dir_Folder, paste(a, ".dbf", sep="")), overwrite = TRUE)
	      file.remove(try1)
	    }
	    shinyalert(title = "You did it!", type = "success")
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }
	  })
 
	})	
	
	observeEvent(input$HA_VA_Action_Admin_Group_org, {
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  alist <- input$HA_VA_Admin
	  dlist <- input$HA_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$HA_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$HA_CA_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$HA_CA_Project_year
	  #	    slist <- input$HA_CA_Species
	  vlist <- c("HA_SR", "HA_LOSS", "HA_STAY", "HA_GAIN", "HA_VI1", "HA_VI2", "HA_VI3") # c("HA_SR") # 
	  
	  # Species Group
	  #	    G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  #	    destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  destfile <- file.path(G$HA_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  #	    HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- read.csv(destfile)
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  slist <<- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  
	  n <- 0
	  la <- length(alist)
	  #	    ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  lv <- length(vlist)
	  
	  tlg <- la * ld * lc * lm * ly * lv
	  
	  
	  if (length(slist) > 0) {      
	    for (a in alist) {
	      dir_path <- G$HA_MO_Dir_Folder
	      dataFiles <- dir(G$SE_Dir_GIS, paste(a, ".*", sep = ""), ignore.case = TRUE, all.files = TRUE)
	      file.copy(file.path(G$SE_Dir_GIS, dataFiles), dir_path, overwrite = TRUE)
	      #	      poly <- readShapePoly(file.path(dir_path, paste(a, ".shp", sep = "")))
	      poly <- readOGR(dsn=dir_path, layer=a)
	      df <- read.dbf(file.path(G$SE_Dir_GIS, paste(a, ".dbf", sep = "")))
	      withProgress(message = paste("Species Group Analyzing by ", input$HA_VA_Admin), value = 0, {
	        for (d in dlist) {
	          for (c in clist) {
	            for (m in mlist) {
	              for (y in ylist) {
	                for (v in vlist) {
	                  incProgress(1/tlg, detail = paste("Doing part", a, "_", d, "_", c, "_", m, "_", y, "_", v))
	                  img <- file.path(dir_path, paste(v, "_",  d, "_", c, "_", m, "_", y, G$IMG_File, sep = ""))
	                  r <- raster(img)
	                  df1 <- raster::extract(r, poly, fun = max, na.rm = TRUE, df=TRUE)
	                  #write to a data frame
	                  df1 <- data.frame(df1[-1])
	                  colnames(df1) <- c(paste(v, "_",  d, "_", c, "_", m, "_", y, sep = ""))
	                  df1[is.na(df1)] <- 0
	                  df <- cbind(df, df1)
	                }
	              }
	            }
	          }
	        }
	        #write to a CSV file
	        write.csv(df, file = file.path(dir_path, paste(a, ".csv", sep="")))
	        #	                file <- "C:/MOTIVE_Projects/Proj11/Habitat_Assessment/교란종_BIOMOD/SD.dbf"
	        #	                write.dbf(df, file)
	        #	                write.dbf(df, "C:/MOTIVE_Projects/Proj11/Habitat_Assessment/교란종_BIOMOD/test.dbf")
	      })
	      shinyalert(title = "You did it!", type = "success")
	    }
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Select Species.")
	    ))
	  }
	})	

	output$HA_AO_MI_Dir_Folder <- renderUI({
	  HA_AO_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species), full.names = FALSE, recursive = FALSE)
	  HA_AO_MI_Dir_Folder_selected <- HA_AO_MI_Dir_Folder_list[1]
	  selectInput("HA_AO_MI_Dir", HA_Name_Out_Folder,
	              choices = c(HA_AO_MI_Dir_Folder_list),
	              selected = HA_AO_MI_Dir_Folder_selected
	  )
	  
	})
	
	output$HA_AO_MO_Dir_Folder <- renderUI({
	  HA_AO_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat), full.names = FALSE, recursive = FALSE)
	  HA_AO_MO_Dir_Folder_selected <- HA_AO_MO_Dir_Folder_list[1]
	  selectInput("HA_AO_MO_Dir", HA_Name_Out_Folder_Habitat,
	              choices = c(HA_AO_MO_Dir_Folder_list),
	              selected = HA_AO_MO_Dir_Folder_selected
	  )
	  
	})
	
	output$HA_AO_MI_Dir_Folder_Name <- renderUI({
	  HA_AO_MI_Dir_Folder_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_AO_MI_Dir, input$HA_AO_Species), full.names = FALSE, recursive = FALSE)
	  HA_AO_MI_Dir_Folder_Name_selected <- HA_AO_MI_Dir_Folder_Name_list[1]
	  selectInput("HA_AO_MI_Dir_Folder", HA_Name_Out_Model,
	              choices = c(HA_AO_MI_Dir_Folder_Name_list),
	              selected = HA_AO_MI_Dir_Folder_Name_selected
	  )
	  
	})		

	
	output$HA_AO_Species <- renderUI({
	  G$HA_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_AO_MI_Dir)
	  HA_Name_Species_list <- list.dirs(path = G$HA_AO_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  HA_Name_Species_selected <- HA_Name_Species_list[1]
	  selectInput("HA_AO_Species", HA_Name_Out_Species_Select,
	              choices = c(HA_Name_Species_list),
	              selected = HA_Name_Species_selected
	  )
	})
	
	
	output$HA_AO_SDM_model <- renderUI({
	  G$HA_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_AO_MI_Dir)
		destfile <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, G$DIR_NAME_SDM, paste(as.name(paste(input$HA_AO_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <- all_eval
		HA_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		HA_Name_Models_selected <- HA_Name_Models_list[1]
		radioButtons("HA_AO_SDM_model", HA_Name_Out_SDM,
			choices = c(HA_Name_Models_list),
			selected = HA_Name_Models_selected
		)
	})
	
	#-------------------------------------------------------------------		
	### HA_AO_SD_PLOT	
	#-------------------------------------------------------------------	
	
	output$HA_AO_SD_PLOT_Group_UI <- renderUI({
	  
	  if (input$HA_AO_SD_Habitat_Type == "SGG" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SD_SGG_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SD_PLOT_UI <- renderUI({
	  
	  if (input$HA_AO_SD_Habitat_Type == "Distribution" && input$HA_AO_SD_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SD_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SD_Habitat_Type == "Distribution" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    column(10, plotOutput("HA_AO_SD_Stat"))
	  } else if (input$HA_AO_SD_Habitat_Type == "SD" && input$HA_AO_SD_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SD_SIDO_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SD_Habitat_Type == "SD" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    column(10, plotOutput("HA_AO_SD_SIDO_Stat"))
	  } else if (input$HA_AO_SD_Habitat_Type == "SGG" && input$HA_AO_SD_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SD_SGG_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SD_Habitat_Type == "SGG" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    column(10, plotOutput("HA_AO_SD_SGG_Stat"))
	  } else if (input$HA_AO_SD_Habitat_Type == "NP" && input$HA_AO_SD_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SD_NP_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SD_Habitat_Type == "NP" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    column(10, plotOutput("HA_AO_SD_NP_Stat"))
	  } else if (input$HA_AO_SD_Habitat_Type == "BR" && input$HA_AO_SD_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SD_BR_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SD_Habitat_Type == "BR" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    column(10, plotOutput("HA_AO_SD_BR_Stat"))
	  } else if (input$HA_AO_SD_Habitat_Type == "DMZ" && input$HA_AO_SD_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SD_DMZ_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SD_Habitat_Type == "DMZ" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    column(10, plotOutput("HA_AO_SD_DMZ_Stat"))
	  } else if (input$HA_AO_SD_Habitat_Type == "Habitat" && input$HA_AO_SD_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SD_Habitat_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SD_Habitat_Type == "Habitat" && input$HA_AO_SD_Habitat_Plot_Type == "Statistics") {
	    column(10, plotOutput("HA_AO_SD_Habitat_Stat"))	    
	  } else {
	    
	  }
	  
	})	  
	
	
	output$HA_AO_SD_Map <- renderLeaflet({
	  G$HA_AO_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_AO_MI_Dir)
	  dir_path <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  Map <- paste("PRED", "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_Project_year, "_", input$HA_AO_Species, "_", input$HA_AO_SDM_model, G$IMG_File, sep = "")
	  r <- raster(file.path(dir_path, Map))
	  r <- as.factor(r)

	  MotiveEco_SDM_plot(r)
	})
	
	output$HA_AO_SD_Stat <- renderPlot({
	  G$HA_AO_MI_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Species, input$HA_AO_MI_Dir)
	  dir_path <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  Map <- paste("PRED", "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_Project_year, "_", input$HA_AO_Species, "_", input$HA_AO_SDM_model, G$IMG_File, sep = "")
	  r <- raster(file.path(dir_path, Map))
	  hist(r, # breaks = bins,
	       col = G$COL_CODE_PLOT_Ramp2, 
	       border = "white",
	       xlab = "Values", # Map,
	       ylab = "면적(Km2)",
	       main = "Histogram of Species Distribution")
	})
	
	
	output$HA_AO_SD_SIDO_Map <- renderLeaflet({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  poly <- readOGR(file.path(HA_AO_SD_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Km2")
	})
	
	
	output$HA_AO_SD_SIDO_Stat <- renderPlot({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  df <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 분포", "시도", "면적(Km2)")
	})
	
	output$HA_AO_SD_SGG_Map <- renderLeaflet({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  poly <- readOGR(file.path(HA_AO_SD_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Km2")
	})
	
	output$HA_AO_SD_SGG_UI <- renderUI({
	  HA_Name_SD_list <- G$SIDO_List
	  HA_Name_SD_selected <- HA_Name_SD_list[1]
	  
	  selectInput("HA_AO_SD_SGG_UI", "시도",
	              choices = c(HA_Name_SD_list),
	              selected = HA_Name_SD_selected
	  )
	})
	
	output$HA_AO_SD_SGG_Stat <- renderPlot({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  df <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$HA_AO_SD_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 분포", "시군구", "면적(Km2)")
	})
	
	output$HA_AO_SD_NP_Map <- renderLeaflet({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  poly <- readOGR(file.path(HA_AO_SD_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("NP", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Km2")
	})
	
	output$HA_AO_SD_NP_Stat <- renderPlot({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  df <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 분포", "국립공원", "면적(Km2)")
	})	
	
	output$HA_AO_SD_BR_Map <- renderLeaflet({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  poly <- readOGR(file.path(HA_AO_SD_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("BR", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Km2")
	})
	
	
	output$HA_AO_SD_BR_Stat <- renderPlot({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  df <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("BR", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 분포", "백두대간", "면적(Km2)")
	})
	
	output$HA_AO_SD_DMZ_Map <- renderLeaflet({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  poly <- readOGR(file.path(HA_AO_SD_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Km2")
	})
	
	output$HA_AO_SD_DMZ_Stat <- renderPlot({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  df <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 분포", "DMZ", "면적(Km2)")
	})
	
	output$HA_AO_SD_Habitat_Map <- renderLeaflet({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  poly <- readOGR(file.path(HA_AO_SD_Dir_Folder, paste("Habitat", ".shp", sep = "")))
	  x <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Km2")
	})
	
	output$HA_AO_SD_Habitat_Stat <- renderPlot({
	  HA_AO_SD_Dir_Folder <- file.path(G$HA_AO_MI_Dir_Folder, input$HA_AO_Species, input$HA_AO_MI_Dir_Folder)
	  df <- read.csv(file.path(HA_AO_SD_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 분포", "서식지", "면적(Km2)")
	})
	
	#-------------------------------------------------------------------		
	### HA_AO_SR_PLOT	
	#-------------------------------------------------------------------	
	
	output$HA_AO_SR_PLOT_Group_UI <- renderUI({
	  
	  if (input$HA_AO_SR_Habitat_Type == "SGG" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SR_SGG_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SR_PLOT_UI <- renderUI({
	  
	  if (input$HA_AO_SR_Habitat_Type == "Distribution" && input$HA_AO_SR_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SR_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SR_Habitat_Type == "Distribution" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "SD" && input$HA_AO_SR_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SR_SIDO_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SR_Habitat_Type == "SD" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_SIDO_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "SGG" && input$HA_AO_SR_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SR_SGG_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SR_Habitat_Type == "SGG" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_SGG_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "NP" && input$HA_AO_SR_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SR_NP_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SR_Habitat_Type == "NP" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_NP_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "BR" && input$HA_AO_SR_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SR_BR_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SR_Habitat_Type == "BR" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_BR_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "DMZ" && input$HA_AO_SR_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SR_DMZ_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SR_Habitat_Type == "DMZ" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_DMZ_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "Habitat" && input$HA_AO_SR_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SR_Habitat_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SR_Habitat_Type == "Habitat" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_Habitat_Stat"))	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SR_PLOT_SP_UI <- renderUI({
	  
	  if (input$HA_AO_SR_Habitat_Type == "SD" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SR_SIDO_SP_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SR_PLOT_SP_Table <- renderUI({
	  
	  if (input$HA_AO_SR_Habitat_Type == "SD" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SR_SIDO_SP_Table")
	  } else if (input$HA_AO_SR_Habitat_Type == "SGG" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SR_SGG_SP_Table")
	  } else if (input$HA_AO_SR_Habitat_Type == "NP" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SR_NP_SP_Table")
	  } else if (input$HA_AO_SR_Habitat_Type == "BR" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SR_BR_SP_Table")
	  } else if (input$HA_AO_SR_Habitat_Type == "DMZ" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SR_DMZ_SP_Table")
	  } else if (input$HA_AO_SR_Habitat_Type == "Habitat" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SR_Habitat_SP_Table")	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SR_PLOT_SP_Stat <- renderUI({
	  
	  if (input$HA_AO_SR_Habitat_Type == "SD" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_SIDO_SP_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "SGG" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_SGG_SP_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "NP" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_NP_SP_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "BR" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_BR_SP_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "DMZ" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_DMZ_SP_Stat"))
	  } else if (input$HA_AO_SR_Habitat_Type == "Habitat" && input$HA_AO_SR_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SR_Habitat_SP_Stat"))	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SR_Map <- renderLeaflet({
	  
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  MotiveEco_gis_plot(dir_path, "HA_SR", input$HA_AO_Climate_model, input$HA_AO_Climate_scenario, input$HA_AO_SDM_model, input$HA_AO_Project_year)
	  
	})
	
	output$HA_AO_SR_Stat <- renderPlot({
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  Map <- paste("HA_SR", "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, G$IMG_File, sep = "")
	  r <- raster(file.path(dir_path, Map))
	  crs(r) <- CRS(G$Projection_Info)
	  hist(r, # breaks = bins, 
	       col="lightskyblue3",  # skyblue",
	       border="white",
	       xlab = "Number of Species", # Map,
	       ylab = "면적(Km2)",
	       main = "Species Richness")
	})
	
	output$HA_AO_SR_SIDO_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SR_SIDO_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시도", "생물종수")
	})
	
	output$HA_AO_SR_SIDO_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SR_SIDO_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SR_SIDO_SP_List) > 0) {
	    if (length(HA_AO_SR_SIDO_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      #sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SR_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SR_SIDO_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SD.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SR_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SR_SIDO_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SR_SIDO_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[6])
	    V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시도", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SR_SGG_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SR_SGG_UI <- renderUI({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  HA_Name_SD_list <- G$SIDO_List
	  HA_Name_SD_selected <- HA_Name_SD_list[1]
	  
	  selectInput("HA_AO_SR_SGG_UI", "시도",
	              choices = c(HA_Name_SD_list),
	              selected = HA_Name_SD_selected
	  )
	})
	
	output$HA_AO_SR_SGG_Stat <- renderPlot({
	  
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$HA_AO_SR_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시군구", "생물종수")
	})
	
	output$HA_AO_SR_SGG_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SR_SGG_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SR_SGG_SP_List) > 0) {
	    if (length(HA_AO_SR_SGG_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SR_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SR_SGG_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SGG.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SR_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SR_SGG_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SR_SGG_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[9])
	    V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시군구", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SR_NP_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SR_NP_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "국립공원", "생물종수")
	})
	
	output$HA_AO_SR_NP_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SR_NP_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SR_NP_SP_List) > 0) {
	    if (length(HA_AO_SR_NP_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SR_NP_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "NP.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SR_NP_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SR_NP_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "국립공원", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SR_BR_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SR_BR_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "백두대간", "생물종수")
	})
	
	output$HA_AO_SR_BR_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SR_BR_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SR_BR_SP_List) > 0) {
	    if (length(HA_AO_SR_BR_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SR_BR_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "BR.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SR_BR_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SR_BR_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "백두대간", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SR_DMZ_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SR_DMZ_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "DMZ", "생물종수")
	})
	
	output$HA_AO_SR_DMZ_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SR_DMZ_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SR_DMZ_SP_List) > 0) {
	    if (length(HA_AO_SR_DMZ_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SR_DMZ_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SR_DMZ_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SR_DMZ_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[7])
	    V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "DMZ", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SR_Habitat_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SR_Habitat_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "서식지", "생물종수")
	})
	
	output$HA_AO_SR_NP_Habitat_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SR_Habitat_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SR_Habitat_SP_List) > 0) {
	    if (length(HA_AO_SR_Habitat_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SR_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SR_Habitat_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SR_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SR_Habitat_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SR_Habitat_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "서식지", "면적(Km2)")
	  }
	  
	})		
	
	#-------------------------------------------------------------------		
	### HA_AO_SL_PLOT	
	#-------------------------------------------------------------------	
	
	output$HA_AO_SL_PLOT_Group_UI <- renderUI({
	  
	  if (input$HA_AO_SL_Habitat_Type == "SGG" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SL_SGG_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SL_PLOT_UI <- renderUI({
	  
	  if (input$HA_AO_SL_Habitat_Type == "Distribution" && input$HA_AO_SL_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SL_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SL_Habitat_Type == "Distribution" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "SD" && input$HA_AO_SL_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SL_SIDO_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SL_Habitat_Type == "SD" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_SIDO_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "SGG" && input$HA_AO_SL_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SL_SGG_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SL_Habitat_Type == "SGG" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_SGG_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "NP" && input$HA_AO_SL_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SL_NP_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SL_Habitat_Type == "NP" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_NP_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "BR" && input$HA_AO_SL_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SL_BR_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SL_Habitat_Type == "BR" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_BR_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "DMZ" && input$HA_AO_SL_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SL_DMZ_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SL_Habitat_Type == "DMZ" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_DMZ_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "Habitat" && input$HA_AO_SL_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SL_Habitat_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SL_Habitat_Type == "Habitat" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_Habitat_Stat"))	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SL_PLOT_SP_UI <- renderUI({
	  
	  if (input$HA_AO_SL_Habitat_Type == "SD" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SL_SIDO_SP_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SL_PLOT_SP_Table <- renderUI({
	  
	  if (input$HA_AO_SL_Habitat_Type == "SD" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SL_SIDO_SP_Table")
	  } else if (input$HA_AO_SL_Habitat_Type == "SGG" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SL_SGG_SP_Table")
	  } else if (input$HA_AO_SL_Habitat_Type == "NP" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SL_NP_SP_Table")
	  } else if (input$HA_AO_SL_Habitat_Type == "BR" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SL_BR_SP_Table")
	  } else if (input$HA_AO_SL_Habitat_Type == "DMZ" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SL_DMZ_SP_Table")   
	  } else if (input$HA_AO_SL_Habitat_Type == "Habitat" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SL_Habitat_SP_Table")	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SL_PLOT_SP_Stat <- renderUI({
	  
	  if (input$HA_AO_SL_Habitat_Type == "SD" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_SIDO_SP_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "SGG" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_SGG_SP_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "NP" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_NP_SP_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "BR" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_BR_SP_Stat"))
	  } else if (input$HA_AO_SL_Habitat_Type == "DMZ" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_DMZ_SP_Stat"))	 
	  } else if (input$HA_AO_SL_Habitat_Type == "Habitat" && input$HA_AO_SL_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SL_Habitat_SP_Stat"))	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SL_Map <- renderLeaflet({
	  
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  MotiveEco_gis_plot(dir_path, "HA_LOSS", input$HA_AO_Climate_model, input$HA_AO_Climate_scenario, input$HA_AO_SDM_model, input$HA_AO_Project_year)
	  
	})
	
	output$HA_AO_SL_Stat <- renderPlot({
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  Map <- paste("HA_LOSS", "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, G$IMG_File, sep = "")
	  r <- raster(file.path(dir_path, Map))
	  crs(r) <- CRS(G$Projection_Info)
	  hist(r, # breaks = bins, 
	       col="lightskyblue3",  # skyblue",
	       border="white",
	       xlab = "Number of Species", # Map,
	       ylab = "면적(Km2)",
	       main = "Species Richness")
	})
	
	output$HA_AO_SL_SIDO_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SL_SIDO_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시도", "생물종수")
	})
	
	output$HA_AO_SL_SIDO_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SL_SIDO_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SL_SIDO_SP_List) > 0) {
	    if (length(HA_AO_SL_SIDO_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SL_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SL_SIDO_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SD.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SL_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SL_SIDO_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SL_SIDO_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[6])
	    V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시도", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SL_SGG_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SL_SGG_UI <- renderUI({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  HA_Name_SD_list <- G$SIDO_List
	  HA_Name_SD_selected <- HA_Name_SD_list[1]
	  
	  selectInput("HA_AO_SL_SGG_UI", "시도",
	              choices = c(HA_Name_SD_list),
	              selected = HA_Name_SD_selected
	  )
	})
	
	output$HA_AO_SL_SGG_Stat <- renderPlot({
	  
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$HA_AO_SL_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시군구", "생물종수")
	})
	
	output$HA_AO_SL_SGG_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SL_SGG_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SL_SGG_SP_List) > 0) {
	    if (length(HA_AO_SL_SGG_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SL_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SL_SGG_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SGG.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SL_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SL_SGG_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SL_SGG_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[9])
	    V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시군구", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SL_NP_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SL_NP_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "국립공원", "생물종수")
	})
	
	output$HA_AO_SL_NP_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SL_NP_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SL_NP_SP_List) > 0) {
	    if (length(HA_AO_SL_NP_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SL_NP_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "NP.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SL_NP_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SL_NP_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "국립공원", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SL_BR_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SL_BR_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "백두대간", "생물종수")
	})
	
	output$HA_AO_SL_BR_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SL_BR_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SL_BR_SP_List) > 0) {
	    if (length(HA_AO_SL_BR_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SL_BR_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "BR.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SL_BR_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SL_BR_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "백두대간", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SL_DMZ_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SL_DMZ_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "DMZ", "생물종수")
	})
	
	output$HA_AO_SL_DMZ_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SL_DMZ_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SL_DMZ_SP_List) > 0) {
	    if (length(HA_AO_SL_DMZ_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SL_DMZ_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SL_DMZ_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SL_DMZ_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[7])
	    V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "DMZ", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SL_Habitat_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SL_Habitat_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "서식지", "생물종수")
	})
	
	output$HA_AO_SL_NP_Habitat_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SL_Habitat_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SL_Habitat_SP_List) > 0) {
	    if (length(HA_AO_SL_Habitat_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SL_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SL_Habitat_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SL_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SL_Habitat_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SL_Habitat_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("LOSS_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "서식지", "면적(Km2)")
	  }
	  
	})
	
	#-------------------------------------------------------------------		
	### HA_AO_SS_PLOT	
	#-------------------------------------------------------------------	
	
	output$HA_AO_SS_PLOT_Group_UI <- renderUI({
	  
	  if (input$HA_AO_SS_Habitat_Type == "SGG" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SS_SGG_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SS_PLOT_UI <- renderUI({
	  
	  if (input$HA_AO_SS_Habitat_Type == "Distribution" && input$HA_AO_SS_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SS_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SS_Habitat_Type == "Distribution" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "SD" && input$HA_AO_SS_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SS_SIDO_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SS_Habitat_Type == "SD" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_SIDO_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "SGG" && input$HA_AO_SS_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SS_SGG_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SS_Habitat_Type == "SGG" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_SGG_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "NP" && input$HA_AO_SS_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SS_NP_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SS_Habitat_Type == "NP" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_NP_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "BR" && input$HA_AO_SS_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SS_BR_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SS_Habitat_Type == "BR" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_BR_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "DMZ" && input$HA_AO_SS_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SS_DMZ_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SS_Habitat_Type == "DMZ" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_DMZ_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "Habitat" && input$HA_AO_SS_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SS_Habitat_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SS_Habitat_Type == "Habitat" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_Habitat_Stat"))		    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SS_PLOT_SP_UI <- renderUI({
	  
	  if (input$HA_AO_SS_Habitat_Type == "SD" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SS_SIDO_SP_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SS_PLOT_SP_Table <- renderUI({
	  
	  if (input$HA_AO_SS_Habitat_Type == "SD" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SS_SIDO_SP_Table")
	  } else if (input$HA_AO_SS_Habitat_Type == "SGG" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SS_SGG_SP_Table")
	  } else if (input$HA_AO_SS_Habitat_Type == "NP" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SS_NP_SP_Table")
	  } else if (input$HA_AO_SS_Habitat_Type == "BR" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SS_BR_SP_Table")
	  } else if (input$HA_AO_SS_Habitat_Type == "DMZ" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SS_DMZ_SP_Table")  
	  } else if (input$HA_AO_SS_Habitat_Type == "Habitat" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SS_Habitat_SP_Table")	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SS_PLOT_SP_Stat <- renderUI({
	  
	  if (input$HA_AO_SS_Habitat_Type == "SD" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_SIDO_SP_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "SGG" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_SGG_SP_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "NP" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_NP_SP_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "BR" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_BR_SP_Stat"))
	  } else if (input$HA_AO_SS_Habitat_Type == "DMZ" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_DMZ_SP_Stat"))	  
	  } else if (input$HA_AO_SS_Habitat_Type == "Habitat" && input$HA_AO_SS_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SS_Habitat_SP_Stat"))		    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SS_Map <- renderLeaflet({
	  
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  MotiveEco_gis_plot(dir_path, "HA_STAY", input$HA_AO_Climate_model, input$HA_AO_Climate_scenario, input$HA_AO_SDM_model, input$HA_AO_Project_year)
	  
	})
	
	output$HA_AO_SS_Stat <- renderPlot({
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  Map <- paste("HA_STAY", "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, G$IMG_File, sep = "")
	  r <- raster(file.path(dir_path, Map))
	  crs(r) <- CRS(G$Projection_Info)
	  hist(r, # breaks = bins, 
	       col="lightskyblue3",  # skyblue",
	       border="white",
	       xlab = "Number of Species", # Map,
	       ylab = "면적(Km2)",
	       main = "Species Richness")
	})
	
	output$HA_AO_SS_SIDO_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SS_SIDO_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시도", "생물종수")
	})
	
	output$HA_AO_SS_SIDO_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SS_SIDO_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SS_SIDO_SP_List) > 0) {
	    if (length(HA_AO_SS_SIDO_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SS_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SS_SIDO_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SD.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SS_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SS_SIDO_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SS_SIDO_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[6])
	    V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시도", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SS_SGG_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SS_SGG_UI <- renderUI({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  HA_Name_SD_list <- G$SIDO_List
	  HA_Name_SD_selected <- HA_Name_SD_list[1]
	  
	  selectInput("HA_AO_SS_SGG_UI", "시도",
	              choices = c(HA_Name_SD_list),
	              selected = HA_Name_SD_selected
	  )
	})
	
	output$HA_AO_SS_SGG_Stat <- renderPlot({
	  
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$HA_AO_SS_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시군구", "생물종수")
	})
	
	output$HA_AO_SS_SGG_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SS_SGG_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SS_SGG_SP_List) > 0) {
	    if (length(HA_AO_SS_SGG_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SS_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SS_SGG_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SGG.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SS_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SS_SGG_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SS_SGG_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[9])
	    V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시군구", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SS_NP_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SS_NP_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "국립공원", "생물종수")
	})
	
	output$HA_AO_SS_NP_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SS_NP_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SS_NP_SP_List) > 0) {
	    if (length(HA_AO_SS_NP_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SS_NP_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "NP.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SS_NP_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SS_NP_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "국립공원", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SS_BR_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SS_BR_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "백두대간", "생물종수")
	})
	
	output$HA_AO_SS_BR_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SS_BR_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SS_BR_SP_List) > 0) {
	    if (length(HA_AO_SS_BR_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SS_BR_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "BR.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SS_BR_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SS_BR_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "백두대간", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SS_DMZ_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SS_DMZ_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "DMZ", "생물종수")
	})
	
	output$HA_AO_SS_DMZ_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SS_DMZ_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SS_DMZ_SP_List) > 0) {
	    if (length(HA_AO_SS_DMZ_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SS_DMZ_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SS_DMZ_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SS_DMZ_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[7])
	    V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "DMZ", "면적(Km2)")
	  }
	  
	})		
	
	output$HA_AO_SS_Habitat_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SS_Habitat_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "서식지", "생물종수")
	})
	
	output$HA_AO_SS_NP_Habitat_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SS_Habitat_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SS_Habitat_SP_List) > 0) {
	    if (length(HA_AO_SS_Habitat_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SS_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SS_Habitat_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SS_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SS_Habitat_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SS_Habitat_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("STAY_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "서식지", "면적(Km2)")
	  }
	  
	})	
	
	
	#-------------------------------------------------------------------		
	### HA_AO_SG_PLOT	
	#-------------------------------------------------------------------	
	
	output$HA_AO_SG_PLOT_Group_UI <- renderUI({
	  
	  if (input$HA_AO_SG_Habitat_Type == "SGG" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SG_SGG_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SG_PLOT_UI <- renderUI({
	  
	  if (input$HA_AO_SG_Habitat_Type == "Distribution" && input$HA_AO_SG_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SG_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SG_Habitat_Type == "Distribution" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "SD" && input$HA_AO_SG_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SG_SIDO_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SG_Habitat_Type == "SD" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_SIDO_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "SGG" && input$HA_AO_SG_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SG_SGG_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SG_Habitat_Type == "SGG" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_SGG_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "NP" && input$HA_AO_SG_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SG_NP_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SG_Habitat_Type == "NP" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_NP_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "BR" && input$HA_AO_SG_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SG_BR_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SG_Habitat_Type == "BR" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_BR_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "DMZ" && input$HA_AO_SG_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SG_DMZ_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SG_Habitat_Type == "DMZ" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_DMZ_Stat"))	
	  } else if (input$HA_AO_SG_Habitat_Type == "Habitat" && input$HA_AO_SG_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_SG_Habitat_Map", width = "800", height = "600")
	  } else if (input$HA_AO_SG_Habitat_Type == "Habitat" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_Habitat_Stat"))		    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SG_PLOT_SP_UI <- renderUI({
	  
	  if (input$HA_AO_SG_Habitat_Type == "SD" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_SG_SIDO_SP_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SG_PLOT_SP_Table <- renderUI({
	  
	  if (input$HA_AO_SG_Habitat_Type == "SD" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SG_SIDO_SP_Table")
	  } else if (input$HA_AO_SG_Habitat_Type == "SGG" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SG_SGG_SP_Table")
	  } else if (input$HA_AO_SG_Habitat_Type == "NP" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SG_NP_SP_Table")
	  } else if (input$HA_AO_SG_Habitat_Type == "BR" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SG_BR_SP_Table")
	  } else if (input$HA_AO_SG_Habitat_Type == "DMZ" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SG_DMZ_SP_Table")   
	  } else if (input$HA_AO_SG_Habitat_Type == "Habitat" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_SG_Habitat_SP_Table")  	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SG_PLOT_SP_Stat <- renderUI({
	  
	  if (input$HA_AO_SG_Habitat_Type == "SD" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_SIDO_SP_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "SGG" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_SGG_SP_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "NP" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_NP_SP_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "BR" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_BR_SP_Stat"))
	  } else if (input$HA_AO_SG_Habitat_Type == "DMZ" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_DMZ_SP_Stat"))	  
	  } else if (input$HA_AO_SG_Habitat_Type == "Habitat" && input$HA_AO_SG_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_SG_Habitat_SP_Stat"))	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_SG_Map <- renderLeaflet({
	  
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  MotiveEco_gis_plot(dir_path, "HA_GAIN", input$HA_AO_Climate_model, input$HA_AO_Climate_scenario, input$HA_AO_SDM_model, input$HA_AO_Project_year)
	  
	})
	
	output$HA_AO_SG_Stat <- renderPlot({
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  Map <- paste("HA_GAIN", "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, G$IMG_File, sep = "")
	  r <- raster(file.path(dir_path, Map))
	  crs(r) <- CRS(G$Projection_Info)
	  hist(r, # breaks = bins, 
	       col="lightskyblue3",  # skyblue",
	       border="white",
	       xlab = "Number of Species", # Map,
	       ylab = "면적(Km2)",
	       main = "Species Richness")
	})
	
	output$HA_AO_SG_SIDO_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SG_SIDO_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시도", "생물종수")
	})
	
	output$HA_AO_SG_SIDO_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SG_SIDO_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SG_SIDO_SP_List) > 0) {
	    if (length(HA_AO_SG_SIDO_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SG_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SG_SIDO_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SD.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SG_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SG_SIDO_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SG_SIDO_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[6])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시도", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SG_SGG_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SG_SGG_UI <- renderUI({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  HA_Name_SD_list <- G$SIDO_List
	  HA_Name_SD_selected <- HA_Name_SD_list[1]
	  
	  selectInput("HA_AO_SG_SGG_UI", "시도",
	              choices = c(HA_Name_SD_list),
	              selected = HA_Name_SD_selected
	  )
	})
	
	output$HA_AO_SG_SGG_Stat <- renderPlot({
	  
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$HA_AO_SG_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시군구", "생물종수")
	})
	
	output$HA_AO_SG_SGG_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SG_SGG_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SG_SGG_SP_List) > 0) {
	    if (length(HA_AO_SG_SGG_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SG_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SG_SGG_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SGG.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_SG_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SG_SGG_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SG_SGG_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[9])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시군구", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_SG_NP_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SG_NP_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "국립공원", "생물종수")
	})
	
	output$HA_AO_SG_NP_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SG_NP_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SG_NP_SP_List) > 0) {
	    if (length(HA_AO_SG_NP_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SG_NP_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "NP.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SG_NP_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SG_NP_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "국립공원", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SG_BR_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SG_BR_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "백두대간", "생물종수")
	})
	
	output$HA_AO_SG_BR_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SG_BR_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SG_BR_SP_List) > 0) {
	    if (length(HA_AO_SG_BR_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SG_BR_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "BR.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SG_BR_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SG_BR_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "백두대간", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_SG_DMZ_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SG_DMZ_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "DMZ", "생물종수")
	})
	
	output$HA_AO_SG_DMZ_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SG_DMZ_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SG_DMZ_SP_List) > 0) {
	    if (length(HA_AO_SG_DMZ_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SG_DMZ_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SG_DMZ_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SG_DMZ_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[7])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "DMZ", "면적(Km2)")
	  }
	  
	})		
	
	output$HA_AO_SG_Habitat_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_SG_Habitat_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "서식지", "생물종수")
	})
	
	output$HA_AO_SG_NP_Habitat_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_SG_Habitat_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_SG_Habitat_SP_List) > 0) {
	    if (length(HA_AO_SG_Habitat_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_SG_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_SG_Habitat_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_SG_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_SG_Habitat_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_SG_Habitat_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "서식지", "면적(Km2)")
	  }
	  
	})		
	
	#-------------------------------------------------------------------		
	### HA_AO_VI_PLOT	
	#-------------------------------------------------------------------	
	
	output$HA_AO_VI_TYPE_Group_UI <- renderUI({
	  
	  if (input$HA_AO_VI_Habitat_Type == "Habitat") {
	    uiOutput("HA_AO_VI_TYPE_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_VI_TYPE_UI <- renderUI({
	  
	  radioButtons("HA_AO_VI_TYPE_UI", NULL,
	               choices = c(HA_Name_VI_Types_list),
	               selected = HA_Name_VI_Types_selected,
	               inline = TRUE)
	  
	})
	
	output$HA_AO_VI_PLOT_Group_UI <- renderUI({
	  
	  if (input$HA_AO_VI_Habitat_Type == "SGG" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_VI_SGG_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_VI_PLOT_UI <- renderUI({
	  
	  if (input$HA_AO_VI_Habitat_Type == "Distribution" && input$HA_AO_VI_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_VI_Map", width = "800", height = "600")
	  } else if (input$HA_AO_VI_Habitat_Type == "Distribution" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "SD" && input$HA_AO_VI_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_VI_SIDO_Map", width = "800", height = "600")
	  } else if (input$HA_AO_VI_Habitat_Type == "SD" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_SIDO_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "SGG" && input$HA_AO_VI_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_VI_SGG_Map", width = "800", height = "600")
	  } else if (input$HA_AO_VI_Habitat_Type == "SGG" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_SGG_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "NP" && input$HA_AO_VI_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_VI_NP_Map", width = "800", height = "600")
	  } else if (input$HA_AO_VI_Habitat_Type == "NP" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_NP_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "BR" && input$HA_AO_VI_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_VI_BR_Map", width = "800", height = "600")
	  } else if (input$HA_AO_VI_Habitat_Type == "BR" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_BR_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "DMZ" && input$HA_AO_VI_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_VI_DMZ_Map", width = "800", height = "600")
	  } else if (input$HA_AO_VI_Habitat_Type == "DMZ" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_DMZ_Stat"))	
	  } else if (input$HA_AO_VI_Habitat_Type == "Habitat" && input$HA_AO_VI_Habitat_Plot_Type == "Map") {
	    tags$head(
	      # Include our custom CSS
	      includeCSS("styles.css"),
	      includeScript("gomap.js")
	    )
	    leafletOutput("HA_AO_VI_Habitat_Map", width = "800", height = "600")
	  } else if (input$HA_AO_VI_Habitat_Type == "Habitat" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_Habitat_Stat"))		    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_VI_PLOT_SP_UI <- renderUI({
	  
	  if (input$HA_AO_VI_Habitat_Type == "SD" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    uiOutput("HA_AO_VI_SIDO_SP_UI")
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_VI_PLOT_SP_Table <- renderUI({
	  
	  if (input$HA_AO_VI_Habitat_Type == "SD" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_VI_SIDO_SP_Table")
	  } else if (input$HA_AO_VI_Habitat_Type == "SGG" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_VI_SGG_SP_Table")
	  } else if (input$HA_AO_VI_Habitat_Type == "NP" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_VI_NP_SP_Table")
	  } else if (input$HA_AO_VI_Habitat_Type == "BR" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_VI_BR_SP_Table")
	  } else if (input$HA_AO_VI_Habitat_Type == "DMZ" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_VI_DMZ_SP_Table")   
	  } else if (input$HA_AO_VI_Habitat_Type == "Habitat" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    DT::dataTableOutput("HA_AO_VI_Habitat_SP_Table")  	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_VI_PLOT_SP_Stat <- renderUI({
	  
	  if (input$HA_AO_VI_Habitat_Type == "SD" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_SIDO_SP_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "SGG" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_SGG_SP_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "NP" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_NP_SP_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "BR" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_BR_SP_Stat"))
	  } else if (input$HA_AO_VI_Habitat_Type == "DMZ" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_DMZ_SP_Stat"))	  
	  } else if (input$HA_AO_VI_Habitat_Type == "Habitat" && input$HA_AO_VI_Habitat_Plot_Type == "Statistics") {
	    jqui_resizabled(plotOutput("HA_AO_VI_Habitat_SP_Stat"))	    
	  } else {
	    
	  }
	  
	})
	
	output$HA_AO_VI_Map <- renderLeaflet({
	  
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  MotiveEco_gis_plot(dir_path, input$HA_AO_VI_TYPE_UI, input$HA_AO_Climate_model, input$HA_AO_Climate_scenario, input$HA_AO_SDM_model, input$HA_AO_Project_year)
	  
	})
	
	output$HA_AO_VI_Stat <- renderPlot({
	  dir_path <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  Map <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, G$IMG_File, sep = "")
	  r <- raster(file.path(dir_path, Map))
	  crs(r) <- CRS(G$Projection_Info)
	  hist(r, # breaks = bins, 
	       col="lightskyblue3",  # skyblue",
	       border="white",
	       xlab = "Number of Species", # Map,
	       ylab = "면적(Km2)",
	       main = "Species Richness")
	})
	
	output$HA_AO_VI_SIDO_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_VI_SIDO_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시도", "생물종수")
	})
	
	output$HA_AO_VI_SIDO_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_VI_SIDO_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_VI_SIDO_SP_List) > 0) {
	    if (length(HA_AO_VI_SIDO_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_VI_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_SIDO_SP_List[1], input$HA_AO_MI_Dir_Folder, "SD.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_VI_SIDO_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SD.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_VI_SGG_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_VI_SIDO_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_VI_SIDO_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[6])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시도", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_VI_SGG_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_VI_SGG_UI <- renderUI({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  HA_Name_SD_list <- G$SIDO_List
	  HA_Name_SD_selected <- HA_Name_SD_list[1]
	  
	  selectInput("HA_AO_VI_SGG_UI", "시도",
	              choices = c(HA_Name_SD_list),
	              selected = HA_Name_SD_selected
	  )
	})
	
	output$HA_AO_VI_SGG_Stat <- renderPlot({
	  
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$HA_AO_VI_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "시군구", "생물종수")
	})
	
	output$HA_AO_VI_SGG_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_VI_SGG_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_VI_SGG_SP_List) > 0) {
	    if (length(HA_AO_VI_SGG_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_VI_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_SGG_SP_List[1], input$HA_AO_MI_Dir_Folder, "SGG.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_VI_SGG_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "SGG.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$SD_KOR==input$HA_AO_VI_SIDO_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_VI_SGG_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_VI_SGG_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[9])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "시군구", "면적(Km2)")
	  }
	  
	})
	
	output$HA_AO_VI_NP_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_VI_NP_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "국립공원", "생물종수")
	})
	
	output$HA_AO_VI_NP_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_VI_NP_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_VI_NP_SP_List) > 0) {
	    if (length(HA_AO_VI_NP_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_NP_SP_List[1], input$HA_AO_MI_Dir_Folder, "NP.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_VI_NP_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "NP.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_NP_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_VI_NP_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_VI_NP_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "국립공원", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_VI_BR_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_VI_BR_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("BR", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "백두대간", "생물종수")
	})
	
	output$HA_AO_VI_BR_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_VI_BR_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_VI_BR_SP_List) > 0) {
	    if (length(HA_AO_VI_BR_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_BR_SP_List[1], input$HA_AO_MI_Dir_Folder, "BR.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_VI_BR_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "BR.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_BR_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_VI_BR_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_VI_BR_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "백두대간", "면적(Km2)")
	  }
	  
	})	
	
	output$HA_AO_VI_DMZ_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[5])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_VI_DMZ_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "DMZ", "생물종수")
	})
	
	output$HA_AO_VI_DMZ_SP_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_VI_DMZ_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_VI_DMZ_SP_List) > 0) {
	    if (length(HA_AO_VI_DMZ_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_DMZ_SP_List[1], input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_VI_DMZ_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "DMZ.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_DMZ_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_VI_DMZ_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_VI_DMZ_SP_Table_rows_selected  # G_FILE_specieSSocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[7])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "DMZ", "면적(Km2)")
	  }
	  
	})		
	
	output$HA_AO_VI_Habitat_Map <- renderLeaflet({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  poly <- readOGR(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".shp", sep = "")))
	  x <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[3])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	  max <- max(x[V_NAME], na.rm = TRUE)
	  bins <- seq(from = 0, to = max, by = max/10)
	  
	  MotiveEco_bnd_plot(poly, X_NAME, V_NAME, bins, "Species")
	})
	
	output$HA_AO_VI_Habitat_Stat <- renderPlot({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  df <- read.csv(file.path(G$HA_AO_MO_Dir_Folder, paste("Habitat", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste(input$HA_AO_VI_TYPE_UI, "_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep="")
	  
	  MotiveEco_BND_stat(df, X_NAME, V_NAME, "생물종 풍부도", "서식지", "생물종수")
	})
	
	output$HA_AO_VI_NP_Habitat_Table <- DT::renderDataTable({
	  G$HA_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, G$DIR_NAME_Habitat, input$HA_AO_MO_Dir)
	  destfile <- file.path(G$HA_AO_MO_Dir_Folder, "HabitatAssessment_Options.csv")
	  
	  HA_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  HA_Options_lists <- HA_Options_lists[!(HA_Options_lists$input.HA_CA_Species == ""), ]
	  HA_AO_VI_Habitat_SP_List <- HA_Options_lists[,"input.HA_CA_Species"]
	  
	  if (length(HA_AO_VI_Habitat_SP_List) > 0) {
	    if (length(HA_AO_VI_Habitat_SP_List) == 1) {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } else {
	      destfile <- file.path(G$HA_AO_MI_Dir_Folder, HA_AO_VI_Habitat_SP_List[1], input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	      sindex <- read.csv(destfile)
	      for (s in HA_AO_VI_Habitat_SP_List[-1]) {
	        destfile <- file.path(G$HA_AO_MI_Dir_Folder, s, input$HA_AO_MI_Dir_Folder, "Habitat.csv")
	        sindex0 <- read.csv(destfile)
	        sindex <- rbind(sindex, sindex0)
	      }
	      #	      sindex <- sindex[which(sindex$NP_KOR==input$HA_AO_VI_Habitat_SP_UI), ]
	      G_FILE_species_sindex <<- sindex
	      sindex
	    } 
	  } else {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Species Index file doesn't exist.")
	    ))
	  }	  
	})
	
	output$HA_AO_VI_Habitat_SP_Stat <- renderPlot({
	  
	  rs <- input$HA_AO_VI_Habitat_SP_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    df <- G_FILE_species_sindex[rs, , drop = FALSE]
	    X_NAME <- names(df[5])
	    V_NAME <- paste("GAIN_PRED_", input$HA_AO_Climate_model, "_", input$HA_AO_Climate_scenario, "_", input$HA_AO_SDM_model, "_", input$HA_AO_Project_year, sep = "")
	    
	    MotiveEco_BND_SP_stat(df, X_NAME, V_NAME, "생물종 분포", "서식지", "면적(Km2)")
	  }
	  
	})	
	
	
	
	
	
	
})