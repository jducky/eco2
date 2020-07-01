
shinyServer(function(input, output) {

	output$SE_Dir_Project <- renderText({G$SE_Dir_Project})
	output$SE_Dir_Climate <- renderText({G$SE_Dir_Climate})
	output$SE_Dir_Link <- renderText({G$SE_Dir_Link})
	output$SE_Dir_GIS <- renderText({G$SE_Dir_GIS})
	output$SE_Dir_Species <- renderText({G$SE_Dir_Species})
#	output$SE_speciesindex <- renderText({G$SE_speciesindex})
#	output$SE_specieslocation <- renderText({G$SE_specieslocation})

  
#	onclick("kor_link_top", SE$Language <<- "Korean")
#	onclick("eng_link_top", SE$Language <<- "English")

#	observeEvent(input$login, {
#		showModal(modalDialog(
#			title = "You have logged in.",
#			paste0("It seems you have logged in as ",input$userid,'.'),
#			easyClose = TRUE,
#			footer = NULL
#		))
#	})

	output$SE_Project <- renderUI({
	  if (G_Project_info_CHK) {
	    Project_list <- as.character(Project_info[,1])
	    Project_selected <- as.character(Project_list[1])
	    selectInput("Project_Name", "Lists of Projects",
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
	    cat(as.character(Project_Info_Path))
	    G$SE_Dir_Project <- Project_Info_Path
	    if (!dir.exists(G$SE_Dir_Project)) { 
	      showModal(modalDialog(
	        title = "Error Message",
	        paste("Working Project folder does not exist.")
	      ))
	    }
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
	  volumes <<- getVolumes()
	  shinyDirChoose(input, 'SE_Dir_Project', roots = getVolumes())
	  G$SE_Dir_Project <<- parseDirPath(volumes, input$SE_Dir_Project)
	})
	
#	output$SE_Project_New_Path <- renderText({G$SE_Dir_Project})
	
	output$SE_Project_New_Path <- renderUI({

	  if (!dir.exists(G$SE_Dir_Project)) {
	    showModal(modalDialog(
	      title = "Error Message",
	      paste("Working Project folder does not exist.")
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
	    textInput("Project_New_Name", "Name",
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
	    textInput("Project_New_Manager", "Manager",
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
	    textInput("Project_New_Institute", "Institute",
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
	    dateInput("Project_New_Date", "Date",
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
	    textInput("Project_New_Description", "Description",
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
	        title = "Message",
	        "information is updated and saved!"
	    ))
	})

	output$SE_Dir_Project_SDM <- renderUI({
	  Dir_Project_SDM_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  Dir_Project_SDM_selected <- Dir_Project_SDM_list[1]
	  selectInput("Dir_Project_SDM", "Working Species Distribution Folders",
	              choices = c(Dir_Project_SDM_list),
	              selected = Dir_Project_SDM_selected
	  )
	})
	
	output$SE_Dir_Project_SDM_Species <- renderUI({
	  Dir_Project_SDM_Species_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$Dir_Project_SDM), full.names = FALSE, recursive = FALSE)
	  Dir_Project_SDM_Species_selected <- Dir_Project_SDM_Species_list[1]
	  selectInput("Dir_Project_SDM_Species", "Select a species",
	              choices = c(Dir_Project_SDM_Species_list),
	              selected = Dir_Project_SDM_Species_selected
	  )
	})
	
	output$SE_Dir_Project_SDM_Species_Model <- renderUI({
	  Dir_Project_SDM_Species_Model_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$Dir_Project_SDM, input$Dir_Project_SDM_Species), full.names = FALSE, recursive = FALSE)
	  Dir_Project_SDM_Species_Model_selected <- Dir_Project_SDM_Species_Model_list[1]
	  selectInput("Dir_Project_SDM_Species_Model", "Select a Model",
	              choices = c(Dir_Project_SDM_Species_Model_list),
	              selected = Dir_Project_SDM_Species_Model_selected
	  )
	})
	
	output$SE_Dir_Project_SDM_Species_Model_Options <- renderTable({
	    
	    destfile <- file.path(G$SE_Dir_Project, "Species_Distribution", input$Dir_Project_SDM, input$Dir_Project_SDM_Species, input$Dir_Project_SDM_Species_Model, paste(input$Dir_Project_SDM_Species, "_", input$Dir_Project_SDM_Species_Model, "_variables.csv", sep = ""))
	    if (file.exists(destfile)) { 
	        SDM_variables_lists <- read.csv(destfile, header = T, sep = ",")
	        SDM_variables_lists[is.na(SDM_variables_lists)] = ""
	        
	        SDM_variables_lists_T <- data.frame(t(SDM_variables_lists))
	        rownames(SDM_variables_lists_T) <- colnames(SDM_variables_lists)
	        SDM_variables_lists_T[-1,]
	    } 
	    }, rownames = TRUE, colnames = FALSE)
	
	output$SE_Dir_Project_SDM_Species_Model_Output <- renderPrint({
      Dir_Project_SDM_Species_Model_list_csv <- list.files(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$Dir_Project_SDM, input$Dir_Project_SDM_Species, input$Dir_Project_SDM_Species_Model), pattern="\\.csv$", all.files=FALSE, full.names=FALSE)
      Dir_Project_SDM_Species_Model_list_grd <- list.files(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$Dir_Project_SDM, input$Dir_Project_SDM_Species, input$Dir_Project_SDM_Species_Model), pattern="\\.grd$", all.files=FALSE, full.names=FALSE)
      cat(as.character(Dir_Project_SDM_Species_Model_list_csv))
      cat(as.character(Dir_Project_SDM_Species_Model_list_grd))
	})
	
	output$SE_Dir_Project_IS <- renderUI({
	  Dir_Project_IS_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Invasive_Species"), full.names = FALSE, recursive = FALSE)
	  Dir_Project_IS_selected <<- Dir_Project_IS_list[1]
	  selectInput("Dir_Project_IS", "Working Invasive Species Folders",
	               choices = c(Dir_Project_IS_list),
	               selected = Dir_Project_IS_selected
	  )
	})
	
	output$SE_Dir_Project_IS_Species_Model_Options <- renderTable({
	    
	    destfile <- file.path(G$SE_Dir_Project, "Invasive_Species", input$Dir_Project_IS, "InvasiveSpecies_Options.csv")
	    
	    IS_Options_lists <- read.csv(destfile, header = T, sep = ",")
	    IS_Options_lists[is.na(IS_Options_lists)] = ""
	    
	    IS_Options_lists_T <- data.frame(t(IS_Options_lists))
	    rownames(IS_Options_lists_T) <- colnames(IS_Options_lists)
	    IS_Options_lists_T[-1,]
	    
	}, rownames = TRUE, colnames = FALSE)
	
	output$SE_Dir_Project_IS_Species_Model_Output <- renderPrint({
	  Dir_Project_IS_list_csv <- list.files(path = file.path(G$SE_Dir_Project, "Invasive_Species", input$Dir_Project_IS), pattern="\\.csv$", all.files=FALSE, full.names=FALSE)
	  Dir_Project_IS_list_dbf <- list.files(path = file.path(G$SE_Dir_Project, "Invasive_Species", input$Dir_Project_IS), pattern="\\.dbf$", all.files=FALSE, full.names=FALSE)
	  Dir_Project_IS_list_grd <- list.files(path = file.path(G$SE_Dir_Project, "Invasive_Species", input$Dir_Project_IS), pattern="\\.grd$", all.files=FALSE, full.names=FALSE)
	  cat(as.character(Dir_Project_IS_list_csv))
	  cat('\n\n')
	  cat(as.character(Dir_Project_IS_list_dbf))
	  cat('\n\n')
	  cat(as.character(Dir_Project_IS_list_grd))
	})
	
	output$SE_Dir_Project_VH <- renderUI({
	  Dir_Project_VH_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Vulnerable_Habitat"), full.names = FALSE, recursive = FALSE)
	  Dir_Project_VH_selected <<- Dir_Project_VH_list[1]
	  selectInput("Dir_Project_VH", "Working Vulnerable Habitat Folders",
	               choices = c(Dir_Project_VH_list),
	               selected = Dir_Project_VH_selected
	  )
	})
	
	output$SE_Dir_Project_VH_Species_Model_Options <- renderTable({
	    
	    destfile <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$Dir_Project_VH, "VulnerableHabitat_Options.csv")
	    
	    VH_Options_lists <<- read.csv(destfile, header = T, sep = ",")
	    VH_Options_lists[is.na(VH_Options_lists)] = ""
	    
	    VH_Options_lists_T <- data.frame(t(VH_Options_lists))
	    rownames(VH_Options_lists_T) <- colnames(VH_Options_lists)
	    VH_Options_lists_T[-1,]
	    
	}, rownames = TRUE, colnames = FALSE)
	
	output$SE_Dir_Project_VH_Species_Model_Output <- renderPrint({
	  Dir_Project_VH_list_csv <- list.files(path = file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$Dir_Project_VH), pattern="\\.csv$", all.files=FALSE, full.names=FALSE)
	  Dir_Project_VH_list_dbf <- list.files(path = file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$Dir_Project_VH), pattern="\\.dbf$", all.files=FALSE, full.names=FALSE)
	  Dir_Project_VH_list_grd <- list.files(path = file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$Dir_Project_VH), pattern="\\.grd$", all.files=FALSE, full.names=FALSE)
	  cat(as.character(Dir_Project_VH_list_csv))
	  cat('\n\n')
	  cat(as.character(Dir_Project_VH_list_dbf))
	  cat('\n\n')
	  cat(as.character(Dir_Project_VH_list_grd))	  
	})
		  
	observeEvent(input$SE_Dir_Climate, {
		volumes <- getVolumes()
		shinyDirChoose(input, 'SE_Dir_Climate', roots = volumes)
		G$SE_Dir_Climate <<- parseDirPath(volumes, input$SE_Dir_Climate)
		output$SE_Dir_Climate <- renderText({G$SE_Dir_Climate})
	})
  
	observeEvent(input$SE_Dir_Link, {
		volumes <- getVolumes()
		shinyDirChoose(input, 'SE_Dir_Link', roots = volumes)
		G$SE_Dir_Link <<- parseDirPath(volumes, input$SE_Dir_Link)
		output$SE_Dir_Link <- renderText({G$SE_Dir_Link})
	})
	
	observeEvent(input$SE_Dir_GIS, {
	  volumes <- getVolumes()
	  shinyDirChoose(input, 'SE_Dir_GIS', roots = volumes)
	  G$SE_Dir_GIS <<- parseDirPath(volumes, input$SE_Dir_GIS)
	  output$SE_Dir_GIS <- renderText({G$SE_Dir_GIS})
	})
  
	observeEvent(input$SE_Dir_Species, {
		volumes <- getVolumes()
		shinyDirChoose(input, 'SE_Dir_Species', roots = volumes)
		G$SE_Dir_Species <<- parseDirPath(volumes, input$SE_Dir_Species)
		output$SE_Dir_Species <- renderText({G$SE_Dir_Species})
	})
	
	output$SE_speciesindex <- renderUI({
	    selectInput('SE_speciesindex','Select a Species Index .csv File', selected = G$SE_speciesindex, choice = list.files(G$SE_Dir_Species))
	})
	
	output$SE_specieslocation <- renderUI({
	    selectInput('SE_specieslocation','Select a Species Location .csv File', selected = G$SE_specieslocation, choice = list.files(G$SE_Dir_Species))
	})

	output$SP_Info <- DT::renderDataTable ({
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
			leaflet(data = species_data) %>%
			addTiles(
					urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
					attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
			) %>%

			addMarkers(lng = species_data[, G$SE_Species_Location_Longitude], lat = species_data[, G$SE_Species_Location_Latitude], popup = species_data[, G$SE_Species_ID], label = species_data[, G$SE_Species_ID]) %>%
			setView(lng = 127.00, lat = 38.00, zoom = 6)
		}
	})

	output$SP_LOC_Info <- DT::renderDataTable(inner_join(G_FILE_specieslocation, G_FILE_speciesinfo[input$SP_Info_rows_selected, , drop = FALSE], by = G$SE_Species_ID), server = TRUE)
	
	output$SP_LOC_Map <- renderLeaflet({
		rs <- input$SP_LOC_Info_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			species_data <- G_FILE_specieslocation[rs, , drop = FALSE]
			leaflet(data = species_data) %>%
			addTiles(
					urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
					attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
			) %>%
	
			  addMarkers(lng = species_data[, G$SE_Species_Location_Longitude], lat = species_data[, G$SE_Species_Location_Latitude], popup = species_data[, G$SE_Species_ID], label = species_data[, G$SE_Species_ID]) %>%
			setView(lng = 127.00, lat = 38.00, zoom = 6)
		}
	})  
	
	output$LD_Summary <- renderPrint({
	  file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, ".asc", sep = ""))
	  r <- raster(file)
	  crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
		summary(r)
	})
	
	output$LD_Histogram <- renderPlot({
	  file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, ".asc", sep = ""))
	  x <- raster(file)
		hist(x, # breaks = bins, 
			col="orange",
			border="brown",
			xlab = input$CD_Variables,
			main = "Histogram")
	})  
	
	output$LD_Map <- renderLeaflet({	
	    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, ".asc", sep = ""))
        r <- raster(file)
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
	})  
	
	
	output$LD_Map_Landuse <- renderLeaflet({	
	    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, ".asc", sep = ""))
	    r <- raster(file)
	    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
	    for (k in input$LD_MO_Barrier_LanduseType) {
	        r[r == as.integer(k)] <- 9999
	    }	
	    r[r < 9999] <- 0
	    r[r == 9999] <- 1
	    
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
	})
	
	output$LD_Map_Forestfire <- renderLeaflet({	
	    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, ".asc", sep = ""))
	    r <- raster(file)
	    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

	    r[r >= input$LD_MO_Barrier_Forestfire_Cutoff] <- 9999
	    r[r < 9999] <- 0
	    r[r == 9999] <- 1
	    r <- as.integer(r)	
	    
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
	})
	
	output$LD_Map_Landslide <- renderLeaflet({	
	    file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, input$LD_Project_year, paste(input$LD_Variables, ".asc", sep = ""))
	    r <- raster(file)
	    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
	    r[r >= input$LD_MO_Barrier_Landslide_Cutoff] <- 9999
	    r[r < 9999] <- 0
	    r[r == 9999] <- 1
	    r <- as.integer(r)	
	    
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
	})
	
	output$CD_Variables_select <- renderUI({
	  
	  CD_Variables_Folder <- file.path(G$SE_Dir_Climate, "2000")
	  CD_Name_Variables_list <- list.files(path = CD_Variables_Folder, full.names = FALSE, recursive = FALSE)
	  CD_Name_Variables_selected <- CD_Name_Variables_list[1]
	  
	  selectInput("CD_Variables", CD_Name_Variables,
	              choices = CD_Name_Variables_list,
	              selected = CD_Name_Variables_selected
    )
	})
	
	output$CD_Summary <- renderPrint({
#		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, paste(input$CD_Variables, ".tif", sep = ""))
		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, input$CD_Variables)
		r <- raster(file)
		crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
		summary(r)
	})
	
	output$CD_Histogram <- renderPlot({
#		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, paste(input$CD_Variables, ".tif", sep = ""))
	  file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, input$CD_Variables)
		x <- raster(file)
		crs(x) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
		hist(x, # breaks = bins, 
			col="orange",
			border="brown",
			xlab = input$CD_Variables,
			main = "Histogram")
	})
   
	
	output$CD_Map <- renderLeaflet({
#		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, paste(input$CD_Variables, ".tif", sep = ""))
		file <- file.path(G$SE_Dir_Climate, input$CD_Climate_model, input$CD_Climate_scenario, input$CD_Project_year, input$CD_Variables)
		r <- raster(file)
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
	
	output$SDM_SP_Selection <- renderPrint({
		s_id <- as.character(G_FILE_speciesinfo[input$SDM_SP_Info_rows_selected, , drop = FALSE][, G$SE_Species_ID])
#		s_kname <- as.character(G_FILE_speciesinfo[input$SDM_SP_Info_rows_selected, , drop = FALSE][, G$SE_Species_Name])
		if (length(s_id)) {
			cat('Speices ID:\n\n')
			cat(s_id, sep = ', ')
#			cat('\n\n')
#			cat("Species Name:\n\n")
#			cat(s_kname, sep = ', ')
		}
	})
	
	
	output$SDM_AO_MI_Dir_Folder <- renderUI({
	  SDM_AO_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  SDM_AO_MI_Dir_Folder_selected <- SDM_AO_MI_Dir_Folder_list[1]
	  selectInput("SDM_AO_MI_Dir", "Working Species Distribution Folders",
	              choices = c(SDM_AO_MI_Dir_Folder_list),
	              selected = SDM_AO_MI_Dir_Folder_selected
	  )
	  
	})
	
	output$SDM_IS_AO_MO_Dir_Folder <- renderUI({
	  SDM_IS_AO_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Invasive_Species"), full.names = FALSE, recursive = FALSE)
	  SDM_IS_AO_MO_Dir_Folder_selected <- SDM_IS_AO_MO_Dir_Folder_list[1]
	  selectInput("SDM_IS_AO_MO_Dir", "Working Invasive Species Folders",
	              choices = c(SDM_IS_AO_MO_Dir_Folder_list),
	              selected = SDM_IS_AO_MO_Dir_Folder_selected
	  )
	  
	})
	
	output$SDM_VH_AO_MO_Dir_Folder <- renderUI({
	  SDM_VH_AO_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Vulnerable_Habitat"), full.names = FALSE, recursive = FALSE)
	  SDM_VH_AO_MO_Dir_Folder_selected <- SDM_VH_AO_MO_Dir_Folder_list[1]
	  selectInput("SDM_VH_AO_MO_Dir", "Working Vulberable Habitat Folders",
	              choices = c(SDM_VH_AO_MO_Dir_Folder_list),
	              selected = SDM_VH_AO_MO_Dir_Folder_selected
	  )
	  
	})
	
	output$SDM_AO_MI_Dir_Folder_Name <- renderUI({
	  SDM_AO_MI_Dir_Folder_Name_list <- "BIOMOD2"   # list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_MI_Dir, input$SDM_AO_Species[1]), full.names = FALSE, recursive = FALSE)
	  SDM_AO_MI_Dir_Folder_Name_selected <- SDM_AO_MI_Dir_Folder_Name_list[1]
	  selectInput("SDM_AO_MI_Dir_Folder", "Working SDM Types",
	              choices = c(SDM_AO_MI_Dir_Folder_Name_list),
	              selected = SDM_AO_MI_Dir_Folder_Name_selected
	  )
	  
	})		
	
	
	output$SDM_AO_Species <- renderUI({
	  G$SDM_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_MI_Dir)
	  IS_Name_Species_list <- list.dirs(path = G$SDM_AO_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  IS_Name_Species_selected <- IS_Name_Species_list[1]
	  selectInput("SDM_AO_Species", "Select a species",
	              choices = c(IS_Name_Species_list),
	              selected = IS_Name_Species_selected
	  )
	})
	
	output$SDM_AO_SDM_PROJ_model <- renderUI({
	  G$SDM_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_MI_Dir)
	  destfile <- file.path(G$SDM_AO_MI_Dir_Folder, input$SDM_AO_Species[1], "BIOMOD2", paste(as.name(paste(input$SDM_AO_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	  all_eval <- read.csv(destfile)
	  G_FILE_species_evaluation <<- all_eval
	  SDM_Name_Models_list <- as.character(G_FILE_species_evaluation$Projection)
	  SDM_Name_Models_selected <- SDM_Name_Models_list[1]
	  radioButtons("SDM_AO_SDM_PROJ_model", "Select models",
	               choices = c(SDM_Name_Models_list),
	               selected = SDM_Name_Models_selected
	  )
	})
	
	output$SDM_IS_AO_SDM_PRED_model <- renderUI({

	  destfile <- file.path(G$SE_Dir_Project, "Invasive_Species", input$SDM_IS_AO_MO_Dir, "InvasiveSpecies_Options.csv")
	  IS_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  IS_Options_lists[is.na(IS_Options_lists)] = ""
	  SDM_Name_Models_list <- as.character(IS_Options_lists[1, 8])
	  SDM_Name_Models_selected <- SDM_Name_Models_list[1]
	  
	  radioButtons("SDM_IS_AO_SDM_PRED_model", "Select models",
	               choices = c(SDM_Name_Models_list),
	               selected = SDM_Name_Models_selected
	  )
	})
	
	
	output$SDM_VH_AO_SDM_PRED_model <- renderUI({

	  destfile <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$SDM_VH_AO_MO_Dir, "VulnerableHabitat_Options.csv")
	  VH_Options_lists <- read.csv(destfile, header = T, sep = ",")
	  VH_Options_lists[is.na(VH_Options_lists)] = ""
	  SDM_Name_Models_list <- as.character(VH_Options_lists[1, 8])
	  SDM_Name_Models_selected <- SDM_Name_Models_list[1]
	  
	  radioButtons("SDM_VH_AO_SDM_PRED_model", "Select models",
	               choices = c(SDM_Name_Models_list),
	               selected = SDM_Name_Models_selected
	  )
	})
	
	output$SDM_MO_Variables_Select <- renderUI({
	  
	  SDM_MO_Variables_Folder <- file.path(G$SE_Dir_Climate, "2000")
	  SDM_Name_MO_Variables_list <- list.files(path = SDM_MO_Variables_Folder, full.names = FALSE, recursive = FALSE)
	  SDM_Name_MO_Variables_selected <- SDM_Name_MO_Variables_selected
	  
	  checkboxGroupInput("SDM_MO_Variables", SDM_Name_MO_Variables,
	                     choices = c(SDM_Name_MO_Variables_list),
	                     selected = SDM_Name_MO_Variables_selected
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
  	    PATH_MODEL_OUTPUT <- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_MI_Dir)
  	    for (s in slist) {
  	      for (d in dlist) {
  	        for (c in clist) {
  	          for (m in mlist) {
  	            org_path <- file.path(PATH_MODEL_OUTPUT, s, input$SDM_AO_MI_Dir_Folder)
  	            target_path <- file.path(G$SE_Dir_Climate, ylist[1])
  	            o_file_grd <- paste("PROJ_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
  	            o_file_gri <- paste("PROJ_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".gri", sep = "")
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
  	              o_file_grd <- paste("PROJ_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = "")
  	              o_file_gri <- paste("PROJ_", d, "_", c, "_", y, "_", s, "_", m, ".gri", sep = "")
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
  	  } else if (v == "Invasive.grd") {
  	    mlist <- input$SDM_IS_AO_SDM_PRED_model
	      PATH_MODEL_OUTPUT <- file.path(G$SE_Dir_Project, "Invasive_Species", input$SDM_IS_AO_MO_Dir)
	      for (s in slist) {
	        for (d in dlist) {
	          for (c in clist) {
	            for (m in mlist) {
	              org_path <- PATH_MODEL_OUTPUT
	              target_path <- file.path(G$SE_Dir_Climate, ylist[1])
	              o_file_grd <- paste("IS_SR_", d, "_", c, "_", m, "_", ylist[1], ".grd", sep = "")
	              o_file_gri <- paste("IS_SR_", d, "_", c, "_", m, "_", ylist[1], ".gri", sep = "")
	              t_file_grd <- paste("Invasive.grd")
	              t_file_gri <- paste("Invasive.gri")
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
	                o_file_grd <- paste("IS_SR_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	                o_file_gri <- paste("IS_SR_", d, "_", c, "_", m, "_", y, ".gri", sep = "")
	                t_file_grd <- paste("Invasive.grd")
	                t_file_gri <- paste("Invasive.gri")
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
	      mlist <- input$SDM_VH_AO_SDM_PRED_model
	      PATH_MODEL_OUTPUT <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$SDM_VH_AO_MO_Dir)
	      for (s in slist) {
	        for (d in dlist) {
	          for (c in clist) {
	            for (m in mlist) {
	              org_path <- PATH_MODEL_OUTPUT
	              target_path <- file.path(G$SE_Dir_Climate, ylist[1])
	              o_file_grd <- paste("VH_SR_", d, "_", c, "_", m, "_", ylist[1], ".grd", sep = "")
	              o_file_gri <- paste("VH_SR_", d, "_", c, "_", m, "_", ylist[1], ".gri", sep = "")
	              t_file_grd <- paste("Vulnerable.grd")
	              t_file_gri <- paste("Vulnerable.gri")
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
	                o_file_grd <- paste("VH_SR_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	                o_file_gri <- paste("VH_SR_", d, "_", c, "_", m, "_", y, ".gri", sep = "")
	                t_file_grd <- paste("Vulnerable.grd")
	                t_file_gri <- paste("Vulnerable.gri")
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
	  G$SDM_MO_Variables_Folder <- file.path(G$SE_Dir_Climate, "2000")
	})
	
	
	observeEvent(input$SDM_MO_Dir_Folder, {
	  
	  PATH_PROJECT <- G$SE_Dir_Project
	  
	  # creating Species_Distribution output path
	  if (dir.exists(file.path(PATH_PROJECT, "Species_Distribution"))) {
	    cat(paste("Species_Distribution exists in", PATH_PROJECT, "and is a directory"))
	  } else if (file.exists(file.path(PATH_PROJECT, "Species_Distribution"))) {
	    cat(paste("Species_Distribution exists exists in", PATH_PROJECT, "but is a file"))
	  } else {
	    cat(paste("Species_Distribution does not exist in", PATH_PROJECT, "- creating"))
	    dir.create(file.path(PATH_PROJECT, "Species_Distribution"))
	  }
	  
#	  # creating Sensitive_Species output path
#	  if (dir.exists(file.path(PATH_PROJECT, "Sensitive_Species"))) {
#	    cat(paste("Sensitive_Species exists in", PATH_PROJECT, "and is a directory"))
#	  } else if (file.exists(file.path(PATH_PROJECT, "Sensitive_Species"))) {
#	    cat(paste("Sensitive_Species exists exists in", PATH_PROJECT, "but is a file"))
#	  } else {
#	    cat(paste("Sensitive_Species does not exist in", PATH_PROJECT, "- creating"))
#	    dir.create(file.path(PATH_PROJECT, "Sensitive_Species"))
#	  }
	  
	  # creating Invasive_Species output path
	  if (dir.exists(file.path(PATH_PROJECT, "Invasive_Species"))) {
	    cat(paste("Invasive_Species exists in", PATH_PROJECT, "and is a directory"))
	  } else if (file.exists(file.path(PATH_PROJECT, "Invasive_Species"))) {
	    cat(paste("Invasive_Species exists exists in", PATH_PROJECT, "but is a file"))
	  } else {
	    cat(paste("Invasive_Species does not exist in", PATH_PROJECT, "- creating"))
	    dir.create(file.path(PATH_PROJECT, "Invasive_Species"))
	  }
	  
	  # creating Vulnerable_Habitat output path
	  if (dir.exists(file.path(PATH_PROJECT, "Vulnerable_Habitat"))) {
	    cat(paste("Vulnerable_Habitat exists in", PATH_PROJECT, "and is a directory"))
	  } else if (file.exists(file.path(PATH_PROJECT, "Vulnerable_Habitat"))) {
	    cat(paste("Vulnerable_Habitat exists exists in", PATH_PROJECT, "but is a file"))
	  } else {
	    cat(paste("Vulnerable_Habitat does not exist in", PATH_PROJECT, "- creating"))
	    dir.create(file.path(PATH_PROJECT, "Vulnerable_Habitat"))
	  }
	  
	  
	  volumes <- c(main = file.path(PATH_PROJECT, "Species_Distribution"))
	  shinyDirChoose(input, 'SDM_MO_Dir_Folder', roots = volumes) # , defaultPath = "/MOTIVE_projects", defaultRoot = G$SE_Dir_Project)
	  G$SDM_MO_Dir_Folder <<- parseDirPath(volumes, input$SDM_MO_Dir_Folder)
	  output$SDM_MO_Dir_Folder <- renderText({G$SDM_MO_Dir_Folder})
	})
	
	observeEvent(input$SDM_MO_SDM_run, {
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  dlist <- input$SDM_MO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$SDM_MO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  ylist <- input$SDM_MO_Project_year  # c("2000", "2050") # c("2000", "2050", "2070")
	  slist <- as.character(G_FILE_speciesinfo[input$SDM_SP_Info_rows_selected, , drop = FALSE]$ID) #c("S251") # input$SDM_MO_Species  # c("S251") # c("S015", "S134", "S145")
	  
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
	    PATH_ENV_INPUT <- file.path(PATH_ENV, "2000", sep = "")
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
	    ENV_VARIABLES <- input$SDM_MO_Variables   # c("bio01.asc", "bio02.asc", "bio03.asc", "bio12.asc", "bio13.asc", "bio14.asc")
	    
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
	      
	      myResp <- as.numeric(SPECIES_DATA[,NAME_ID] <- 1)
	      myResp <- as.numeric(SPECIES_DATA[,NAME_ID])
	      
	      CUR_PATH <- getwd()
	      setwd(PATH_ENV_INPUT)
	      myExpl <- stack(ENV_VARIABLES)
	      setwd(CUR_PATH)
	      
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
	      if (dir.exists(file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2"))) {
	        cat(paste("BIOMOD2 exists in", PATH_MODEL_OUTPUT, "/", SPECIES_NAME, "and is a directory"))
	      } else if (file.exists(file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2"))) {
	        cat(paste("BIOMOD2 exists in", PATH_MODEL_OUTPUT, "/", SPECIES_NAME, "but is a file"))
	      } else {
	        cat(paste("BIOMOD2 does not exist in", PATH_MODEL_OUTPUT, "/", SPECIES_NAME, "- creating"))
	        dir.create(file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2"))
	      }
	      
	      # Evaluating the model
	      myBiomodModelEval <- get_evaluations(myBiomodModelOut)
	      write.csv(myBiomodModelEval, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(SPECIES_NAME, "_eval.csv", sep = "", collapse = "--")))
	      myBiomodModelImport <- get_variables_importance(myBiomodModelOut)
	      write.csv(myBiomodModelImport, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(SPECIES_NAME, "_impot.csv",  sep = "", collapse = "--")))
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
	              writeRaster(proj, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(as.name(paste("PROJ_", BIOMOD_proj.name, "_", i, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	              for (j in BIOMOD_binary.meth) {
	                if (!is.na(myBiomodModelEval[j, "Cutoff", strapplyc(i, "Full_(.*)", simplify = TRUE), "Full", "PA1"])) {
	                  cutoffvalue <- as.integer(myBiomodModelEval[j, "Cutoff", strapplyc(i, "Full_(.*)", simplify = TRUE), "Full", "PA1"])
	                  pred <- BinaryTransformation(proj, cutoffvalue)
	                  writeRaster(pred, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2",paste(as.name(paste("PRED_", BIOMOD_proj.name, "_", i, "_by", j, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
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
	              write.csv(myBiomodEMEval, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(SPECIES_NAME, "_EM_eval.csv", sep = "", collapse = "--")))
	              
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
	                writeRaster(proj, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(as.name(paste("PROJ_", BIOMOD_proj.name, "_", i, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                for (j in EM_models.eval.meth) {
	                  if (!is.na(eval(parse(text = as.name(paste("myBiomodEMEval$", i))))[j, "Cutoff"])) {
	                    cutoffvalue <- as.integer(eval(parse(text = as.name(paste("myBiomodEMEval$", i))))[j, "Cutoff"])
	                    pred <- BinaryTransformation(proj, cutoffvalue)
	                    writeRaster(pred, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(as.name(paste("PRED_", BIOMOD_proj.name, "_", i, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                  }
	                }
	              }
	            }
	            ##### End ensemble modelling =================================
	            
	          } # End Year loop y
	        } # End climate change Scenarios loop c
	      } # End climate data loop d
	      
	      ### Creating species evaluation information 
	      destfile <- file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(as.name(paste(SPECIES_NAME, "_eval.csv", sep = "")), sep = "", collapse = "--"))
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
	        destfile <- file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(as.name(paste(SPECIES_NAME, "_EM_eval.csv", sep = "")), sep = "", collapse = "--"))
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
	      
	      write.csv(Eval_data, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(SPECIES_NAME, "_ALL_eval.csv", sep = "", collapse = "--")))
	      
	      #####
	      
#	      destfile <- file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(SPECIES_NAME, "_BIOMOD2_variables.csv", sep = "", collapse = "--"))

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
          write.csv(SDM_variables, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(SPECIES_NAME, "_BIOMOD2_variables.csv", sep = "", collapse = "--")))
	      
          write.csv(SPECIES_DATA, file = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME, "BIOMOD2", paste(SPECIES_NAME, "_BIOMOD2_species.csv", sep = "", collapse = "--")))
          
	      #####
	      
	      dir_list <- list.dirs(path = file.path(PATH_MODEL_OUTPUT, SPECIES_NAME), full.names = FALSE, recursive = FALSE)
	      for (i in dir_list) {
	        if (i == "BIOMOD2" | i == "MIGCLIM") {
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
	    SDM_AO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	    SDM_AO_Dir_Folder_selected <- SDM_AO_Dir_Folder_list[1]
	    selectInput("SDM_AO_Dir", "Working Species Distribution Folders",
	                choices = c(SDM_AO_Dir_Folder_list),
	                selected = SDM_AO_Dir_Folder_selected
	    )
#	    G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
	})

	output$SDM_OU_Species <- renderUI({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		SDM_Name_Species_list <- list.dirs(path = G$SDM_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
		SDM_Name_Species_selected <- SDM_Name_Species_list[1]
		selectInput("SDM_OU_Species", "Select a species",
			choices = c(SDM_Name_Species_list),
			selected = SDM_Name_Species_selected
		)
	})
	
	output$SDM_OU_Projection_model <- renderUI({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2", paste(as.name(paste(input$SDM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		SDM_Name_Projection_Models_list <- unique(as.character(G_FILE_species_evaluation$Projection))
		SDM_Name_Projection_Models_selected <- SDM_Name_Projection_Models_list[1]
		selectInput("SDM_OU_Projection_model", "Select Projection models",
			choices = c(SDM_Name_Projection_Models_list),
			selected = SDM_Name_Projection_Models_selected
		)
	})
  
	output$SDM_OU_Prediction_model <- renderUI({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2", paste(as.name(paste(input$SDM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		SDM_Name_Prediction_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		SDM_Name_Prediction_Models_selected <- SDM_Name_Prediction_Models_list[1]
		selectInput("SDM_OU_Prediction_model", "Select Prediction models",
			choices = c(SDM_Name_Prediction_Models_list),
			selected = SDM_Name_Prediction_Models_selected
		)
	})

	output$SDM_OU_Validation <- DT::renderDataTable({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2", paste(as.name(paste(input$SDM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		all_eval
	})
	
	output$SDM_OU_Validation_BoxPlot <- renderPlot({
		rs <- input$SDM_OU_Validation_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			Eval_data <- G_FILE_species_evaluation[rs, , drop = FALSE]
	
			boxplot(Accuracy~Type,
				data=Eval_data,
				main="Boxplots by Type",
				xlab="Type",
				ylab="Value",
				varwidth = TRUE,
				col="orange",
				border="brown"
			)
		}
	})
	
	output$SDM_OU_Contribution <- DT::renderDataTable({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2", paste(as.name(paste(input$SDM_OU_Species, "_impot.csv", sep = "")), sep = "", collapse = "--"))
	
		if (!file.exists(destfile)) {
			return(NULL)
		}
	
		new_import <- read.csv(destfile)
		data <- data.frame(t(new_import[-1]))
		colnames(data) <- new_import[, 1]
		# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
		data <- rbind(rep(1,length(colnames(data))) , rep(0,length(colnames(data))) , data)
		data[-c(1,2),]
	})
	
	output$SDM_OU_Contribution_Radarchart <- renderPlot({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		destfile <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2", paste(as.name(paste(input$SDM_OU_Species, "_impot.csv", sep = "")), sep = "", collapse = "--"))
	
		if (!file.exists(destfile)) {
			return(NULL)
		}
	
		new_import <- read.csv(destfile)
		data <- data.frame(t(new_import[-1]))
		colnames(data) <- new_import[, 1]
		# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
#		data <- rbind(rep(1,length(colnames(data))) , rep(0,length(colnames(data))) , data)
#		data <- data[-c(1,2),]
		rs <- input$SDM_OU_Contribution_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs) > 0) {
			data <- data[rs, , drop = FALSE]
			data <- rbind(rep(1,length(colnames(data))) , rep(0,length(colnames(data))) , data)
			coul <- brewer.pal(length(rs), "BuPu")
			colors_border <- coul
			colors_in <- alpha(coul,0.3)
			radarchart(data, axistype=0 , maxmin=F,
			           #custom polygon
			           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
			           #custom the grid
			           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
			           #custom labels
			           vlcex=0.8 
      )
			legend(x=1.2, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch = 20, col = colors_in, text.col = "grey", cex = 1.2, pt.cex = 3)
		}
	})
  
	output$SDM_OU_Probability_map <- renderLeaflet({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2")
		Map <- paste("PROJ", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Projection_model, ".grd", sep = "")
		r <- raster(file.path(dir_path, Map))
	
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
	})
	
	output$SDM_OU_PROJ_Summary <- renderPrint({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2")
		Map <- paste("PROJ", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Projection_model, ".grd", sep = "")
		r <- raster(file.path(dir_path, Map))
		summary(r)
	})
	
	output$SDM_OU_PROJ_Histogram <- renderPlot({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2")
		Map <- paste("PROJ", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Projection_model, ".grd", sep = "")
		x <- raster(file.path(dir_path, Map))
	
		hist(x, # breaks = bins, 
			col="orange",
			border="brown",
			xlab = "Projected Value",
			main = "Histogram")
	})
	
	output$SDM_OU_Predicted_map <- renderLeaflet({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2")
		Map <- paste("PRED", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Prediction_model, ".grd", sep = "")
		r <- raster(file.path(dir_path, Map))
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
	})  
	
	output$SDM_OU_PRED_Summary <- renderPrint({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2")
		Map <- paste("PRED", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Prediction_model, ".grd", sep = "")
		r <- raster(file.path(dir_path, Map))
		summary(r)
	})
	
	output$SDM_OU_PRED_Histogram <- renderPlot({
	  G$SDM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SDM_AO_Dir)
		dir_path <- file.path(G$SDM_AO_Dir_Folder, input$SDM_OU_Species, "BIOMOD2")
		Map <- paste("PRED", "_", input$SDM_OU_Climate_model, "_", input$SDM_OU_Climate_scenario, "_", input$SDM_OU_Project_year, "_", input$SDM_OU_Species, "_", input$SDM_OU_Prediction_model, ".grd", sep = "")
		x <- raster(file.path(dir_path, Map))
	
		hist(x, # breaks = bins, 
			col="orange",
			border="brown",
			xlab = "Predicted Value",
			main = "Histogram")
	})
	
	
	output$DM_SDM_Dir_Folder <- renderUI({
	  DM_SDM_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  DM_SDM_Dir_Folder_selected <- DM_SDM_Dir_Folder_list[1]
	  selectInput("DM_SDM_Dir", "Working Species Distribution Folders",
	              choices = c(DM_SDM_Dir_Folder_list),
	              selected = DM_SDM_Dir_Folder_selected
	  )

	})
	
	observeEvent(input$DM_MO_Species_sel_all, {
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_SDM_Dir)
	    DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$DM_Name_Species_selected <<- DM_Name_Species_list
	})
	
	observeEvent(input$DM_MO_Species_sel_none, {
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_SDM_Dir)
	    DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$DM_Name_Species_selected <<- ""
	})
	
	output$DM_MO_Species <- renderUI({
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_SDM_Dir)
		DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
		checkboxGroupInput("DM_MO_Species", "Select a species",
			choices = c(DM_Name_Species_list),
			selected = G$DM_Name_Species_selected
		)
	})
	
	output$DM_MO_SDM_model <- renderUI({
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_SDM_Dir)
	    DM_Name_Species_list <- list.dirs(path = G$DM_SDM_Dir_Folder, full.names = FALSE, recursive = FALSE)
		destfile <- file.path(G$DM_SDM_Dir_Folder, DM_Name_Species_list[1], "BIOMOD2", paste(as.name(paste(DM_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		DM_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		DM_Name_Models_selected <- DM_Name_Models_list[1]
		checkboxGroupInput("DM_MO_SDM_model", "Select models",
			choices = c(DM_Name_Models_list),
			selected = DM_Name_Models_selected
		)
	})
	
	output$DM_Map_Landuse <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
	    r_list <- NULL
	    for (y in input$DM_MO_Project_year) {
	    
	        file <- file.path(G$SE_Dir_Link, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, y, paste(input$DM_MO_Barrier_Landuse, ".asc", sep = ""))
	        r <- raster(file)
	        crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
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
	})
	
	output$DM_Map_Forestfire <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
	    r_list <- NULL
	    for (y in input$DM_MO_Project_year) {
	        file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Forestfire, ".asc", sep = ""))
	        r <- raster(file)
	        crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
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
	})
	
	output$DM_Map_Landslide <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
	    r_list <- NULL
	    for (y in input$DM_MO_Project_year) {
	        file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Landslide, ".asc", sep = ""))
	        r <- raster(file)
	        crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	        
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
	})
	
	output$DM_Map_Total <- renderLeaflet({
	    
	    file <- file.path(G$SE_Dir_Climate, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, input$DM_MO_Project_year[1], SDM_Name_MO_Variables_selected[1])
	    mask <- raster(file)
	    crs(mask) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	    
	    dm_rlist <- NULL
	    if (length(input$DM_MO_Barrier) > 0) {
	    
	    for (dm in input$DM_MO_Barrier) {
	        
	    if (dm == "Landuse") {
	        r_list <- NULL
	        for (y in input$DM_MO_Project_year) {
	            
	            file <- file.path(G$SE_Dir_Link, input$DM_MO_Climate_model, input$DM_MO_Climate_scenario, y, paste(input$DM_MO_Barrier_Landuse, ".asc", sep = ""))
	            r <- raster(file)
	            crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	            
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
	            file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Forestfire, ".asc", sep = ""))
	            r <- raster(file)
	            crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	            
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
                file <- file.path(G$SE_Dir_Link, input$LD_Climate_model, input$LD_Climate_scenario, y, paste(DM_Name_DM_MO_Barrier_Landslide, ".asc", sep = ""))
                r <- raster(file)
                crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
                
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
	    
	    
	    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 	    
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
	  ylist <- input$DM_MO_Project_year  # c("2000", "2050") # c("2000", "2050", "2070")
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
	    G$DM_SDM_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_SDM_Dir)
	    PATH_MODEL_OUTPUT <- G$DM_SDM_Dir_Folder   # file.path(PATH_PROJECT, "Species_Distribution")
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
	      if (dir.exists(file.path(PATH_MODEL_OUTPUT, s, paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = "")))) {
	        cat(paste(paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = ""), "exists in", PATH_MODEL_OUTPUT, "/", s, "and is a directory"))
	      } else if (file.exists(file.path(PATH_MODEL_OUTPUT, s, paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = "")))) {
	        cat(paste(paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = ""), "exists in", PATH_MODEL_OUTPUT, "/", s, "but is a file"))
	      } else {
	        cat(paste(paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = ""), "does not exist in", PATH_MODEL_OUTPUT, "/", s, "- creating"))
	        dir.create(file.path(PATH_MODEL_OUTPUT, s, paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = "")))
	      }   
	      
	      # Setting working path
	      org_path <- file.path(PATH_MODEL_OUTPUT, s, "BIOMOD2")
	      target_path <- file.path(PATH_MODEL_OUTPUT, s, paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = ""))
	      setwd(target_path)
	      
	      ### Projection on current and future environemental conditions
	      # Projecting loop
	      for (d in dlist) {
	        for (c in clist) {
	          for (m in mlist) {
	            for (y in ylist[-1]) {
	              incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", y))
	              o_file_grd <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
	              o_file_gri <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".gri", sep = "")
	              file_path_grd <- file.path(org_path, o_file_grd)
	              file_path_gri <- file.path(org_path, o_file_gri)
	              if (!file.exists(file_path_grd)){
	                showModal(modalDialog(
	                  title = "Error Message",
	                  paste(file_path_grd, "is not exist.")
	                ))
	              } else {
	              
	              file.copy(file_path_grd, target_path)
	              file.copy(file_path_gri, target_path)  
	              sr_list <- ""
	              sr_list <- c(sr_list, file.path(org_path, o_file_grd))
	              ny <- grep(y, ylist)
	              
	              for (i in 1:ny) {
	                o_file <- paste("PROJ_", d, "_", c, "_", ylist[i], "_", s, "_", sub("\\_by.*", "", m), ".grd", sep = "")
	                
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
	              
                for (i in 1:ny) {
                  o_file <- paste("PROJ_", d, "_", c, "_", ylist[i], "_", s, "_", sub("\\_by.*", "", m), ".grd", sep = "")
                  sr_list <- c(sr_list, file.path(org_path, o_file))
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
	              destfile <- file.path(PATH_MODEL_OUTPUT, s, "BIOMOD2", paste(as.name(paste(s, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
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
	              
	              pred_file <- paste("PRED_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
	              pred_cur <- raster(file.path(org_path, pred_file))
	              path <- file.path(target_path, DM_simulName)
	              r_asc <- read.asc(file.path(path, list.files(path)[grep(".asc", list.files(path))][1]))
	              r_dm <- raster(r_asc)
	              r_dm[r_dm < 0] <- 0
	              r_dm[r_dm > 0 & r_dm < 30000] <- 1
	              r_dm[r_dm == 30000] <- 0
	              r_dm[is.na(pred_cur)] <- NA
	              r_dm <- extractByMask(r_dm, msk=pred_cur, spatial=TRUE)
	              writeRaster(r_dm, file = file.path(target_path, paste(as.name(paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	              pred_sdm <- raster(file.path(org_path, paste("PRED_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = "")))
	              sdm_dm <- pred_sdm - r_dm
	              writeRaster(sdm_dm, file = file.path(target_path, paste(as.name(paste("SDM-DM_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	              }
	              }
	            } # End year loop y
            } # End Model loop m
	        } # End climate change Scenarios loop c
	      } # End climate data loop d
	      
	      #####
	      
#	      destfile <- file.path(PATH_MODEL_OUTPUT, s, "MIGCLIM", paste(s, "_MIGCLIM_variables.csv", sep = "", collapse = "--"))
	      
	      DM_variables <- setNames(data.frame(matrix(ncol = 20, nrow = 30)), c("input$DM_MO_Climate_model", "input$DM_MO_Climate_scenario", "input$DM_MO_Project_year", "input$DM_MO_SDM_model", 
	                                                                            "input$DM_MO_Barrier", "input$DM_MO_DM_barrierType", "input$DM_MO_Barrier_Landuse", "input$DM_MO_Barrier_LanduseType", "input$DM_MO_Barrier_Forestfire_Cutoff",
	                                                                            "input$DM_MO_Barrier_Landslide_Cutoff", "input$DM_MO_Barrier_Landuse_Prop", "input$DM_MO_Barrier_Forestfire_Prop", "input$DM_MO_Barrier_Landslide_Prop",
	                                                                            "input$DM_MO_DM_dispSteps", "input$DM_MO_DM_dispKernel", "input$DM_MO_DM_iniMatAge", "input$DM_MO_DM_propaguleProd",
	                                                                            "input$DM_MO_DM_lddFreq", "input$DM_MO_SDM_lddDist", "input$DM_MO_DM_replicateNb"
	      ))
	      
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

	      DM_variables[is.na(DM_variables)] <- ""
	      write.csv(DM_variables, file = file.path(PATH_MODEL_OUTPUT, s, paste("MIGCLIM_", input$DM_MO_Dir_Folder_Name, sep = ""), paste(s, "_MIGCLIM_", input$DM_MO_Dir_Folder_Name, "_variables.csv", sep = "", collapse = "--")))
	      
	      #####
	    } # End Speices loop s
	    setwd(CUR_PATH)
	    
	    #####========================================================
	    #####============ End Models Run ========================
	    #####========================================================        
	  })        
	})
	
	
	output$DM_AO_Dir_Folder <- renderUI({
	  DM_AO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  DM_AO_Dir_Folder_selected <- DM_AO_Dir_Folder_list[1]
	  selectInput("DM_AO_Dir", "Working Species Distribution Folders",
	              choices = c(DM_AO_Dir_Folder_list),
	              selected = DM_AO_Dir_Folder_selected
	  )
	  
	})
	
	output$DM_AO_Model_Name <- renderUI({
	  DM_AO_Model_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_AO_Dir, input$DM_OU_Species[1]), full.names = FALSE, recursive = FALSE)
#	  DM_AO_Model_Name_list <- DM_AO_Model_Name_list[-1]
	  DM_AO_Model_Name_selected <- DM_AO_Model_Name_list[1]
	  selectInput("DM_AO_Model_Name_Input", "Working Dispersal Modeling Folders",
	              choices = c(DM_AO_Model_Name_list),
	              selected = DM_AO_Model_Name_selected
	  )
	 
	})
	
	output$DM_OU_Species <- renderUI({
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_AO_Dir)
	  DM_Name_Species_list <- list.dirs(path = G$DM_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  DM_Name_Species_selected <- DM_Name_Species_list[1]
	  selectInput("DM_OU_Species", "Select a species",
	                     choices = c(DM_Name_Species_list),
	                     selected = DM_Name_Species_selected
	  )
	})
	
	output$DM_OU_SDM_model <- renderUI({
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_AO_Dir)
	  destfile <- file.path(G$DM_AO_Dir_Folder, input$DM_OU_Species[1], "BIOMOD2", paste(as.name(paste(input$DM_OU_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	  all_eval <- read.csv(destfile)
	  G_FILE_species_evaluation <<- all_eval
	  DM_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
	  DM_Name_Models_selected <- DM_Name_Models_list[1]
	  checkboxGroupInput("DM_OU_SDM_model", "Select models",
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
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_AO_Dir)
	  
	  for (s in slist) {
	     dir_path <- file.path(G$DM_AO_Dir_Folder, s, input$DM_AO_Model_Name_Input)  # paste(input$DM_AO_Model_Name_Input, sep = ""))
	      for (d in dlist) {
	        for (c in clist) {
	          for (m in mlist) {
	            if (ly > 0) {
	                for (y in 1:ly) {
	                  Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, ".grd", sep = "")
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
	  G$DM_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$DM_AO_Dir)
	  
	  for (s in slist) {
	    dir_path <- file.path(G$DM_AO_Dir_Folder, s, input$DM_AO_Model_Name_Input)  # paste(input$DM_AO_Model_Name_Input, sep = ""))
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          if (ly > 0) {
	            for (y in 1:ly) {
	              Map1 <- paste("SDM-DM", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, ".grd", sep = "")
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
	
	
	output$SS_MO_Dir_Folder <- renderUI({
	  SS_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  SS_MO_Dir_Folder_selected <- SS_MO_Dir_Folder_list[1]
	  selectInput("SS_MO_Dir", "Working Species Distribution Folders",
	              choices = c(SS_MO_Dir_Folder_list),
	              selected = SS_MO_Dir_Folder_selected
	  )
	  
	})
	
	output$SS_MO_Dir_Folder_Name <- renderUI({
	    G$SS_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir)
	    SS_Name_Species_list <- list.dirs(path = G$SS_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    SS_MO_Dir_Folder_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir, SS_Name_Species_list[1]), full.names = FALSE, recursive = FALSE)
	    SS_MO_Dir_Folder_Name_selected <- SS_MO_Dir_Folder_Name_list[1]
	    selectInput("SS_MO_Dir_Folder", "Working SDM Types",
	                choices = c(SS_MO_Dir_Folder_Name_list),
	                selected = SS_MO_Dir_Folder_Name_selected
	    )
	  
	})
	
	observeEvent(input$SS_CA_Species_Sel_All, {
	    G$SS_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir)
	    SS_Name_Species_list <- list.dirs(path = G$SS_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$SS_Name_Species_selected <<- SS_Name_Species_list
	})
	
	observeEvent(input$SS_CA_Species_Sel_None, {
	    G$SS_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir)
	    SS_Name_Species_list <- list.dirs(path = G$SS_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$SS_Name_Species_selected <<- ""  #SS_Name_Species_list[1]
	})
	
	output$SS_CA_Species <- renderUI({
	  G$SS_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir)
	  SS_Name_Species_list <- list.dirs(path = G$SS_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  checkboxGroupInput("SS_CA_Species", "Select a species",
	                     choices = c(SS_Name_Species_list),
	                     selected = G$SS_Name_Species_selected
	  )
	})
	
	output$SS_CA_SDM_model <- renderUI({
	  G$SS_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir)
	  SS_Name_Species_list <- list.dirs(path = G$SS_MO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  destfile <- file.path(G$SS_MO_Dir_Folder, SS_Name_Species_list[1], "BIOMOD2", paste(as.name(paste(SS_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	  all_eval <- read.csv(destfile)
	  G_FILE_species_evaluation <<- all_eval
	  SS_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
	  SS_Name_Models_selected <- SS_Name_Models_list[1]
	  checkboxGroupInput("SS_CA_SDM_model", "Select models",
	                     choices = c(SS_Name_Models_list),
	                     selected = SS_Name_Models_selected
	  )
	})
	
	observeEvent(input$SS_CA_Action_change, {
		#####========================================================
		##### GAP analyzing =========================================
	
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SS_CA_Species
		dlist <- input$SS_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SS_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SS_CA_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SS_CA_Project_year
#		dtlist <- input$SS_CA_Dispersal_type
	
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
		    G$SS_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir)
		    dir_path <- file.path(G$SS_MO_Dir_Folder, s, input$SS_MO_Dir_Folder)
			  n <- n + 1
			for (d in dlist) {
				for (c in clist) {
					for (m in mlist) {
						if (ly > 1) {
							if (ylist[1] == "2000") {
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
								for (y in 2:ly) {
									incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", ylist[y]))
									Map2 <- paste("PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, ".grd", sep = "")
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
									
									writeRaster(R_gap, file = file.path(dir_path, paste(as.name(paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
									writeRaster(R_loss, file = file.path(dir_path, paste(as.name(paste("LOSS_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
									writeRaster(R_stay, file = file.path(dir_path, paste(as.name(paste("STAY_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
									writeRaster(R_gain, file = file.path(dir_path, paste(as.name(paste("GAIN_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_",m, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
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
	
	observeEvent(input$SS_CA_Action_Vindex, {
		#####========================================================
		##### GAP analyzing =========================================
	
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SS_CA_Species
		dlist <- input$SS_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SS_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SS_CA_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SS_CA_Project_year
#		dtlist <- input$SS_CA_Dispersal_type
	
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
		save_path <- G$SS_MO_Dir_Folder # file.path(isolate(G$SE_Dir_Project), "Sensitive_Species")
#		Tab_gap <- setNames(data.frame(matrix(ncol = 18, nrow = tr)), col_list)
		withProgress(message = 'Runing GAP Analysis model.........', value = 0, {
	
		
		for (s in slist) {
#		  for (dt in dtlist) {
		    G$SS_MO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_MO_Dir)
		    dir_path <- file.path(G$SS_MO_Dir_Folder, s, input$SS_MO_Dir_Folder)
#		    dir_path <- file.path(G$SS_MO_Dir_Folder, s, dt)
			  Tab_gap <- setNames(data.frame(matrix(ncol = 18, nrow = tr)), col_list)
			  n <- n + 1
			  n_tl <- 0
			for (d in dlist) {
				for (c in clist) {
					for (m in mlist) {
						if (ly > 0) {
							if (ly == 1 & ylist[1] == "2000") {
							  incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", ylist[1]))
								n_tl <- n_tl + 1
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
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
							} else if (ly > 1 & ylist[1] == "2000") {
							  incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", ylist[1]))
								n_tl <- n_tl + 1
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
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
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, ".grd", sep = "")
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
									Tab_gap$Area_Gain_Ratio_Outside[n_tl] <- (Tab_gap$Area_Gain[n_tl] / (T_Area - Tab_gap$Area[n_tl])) * 100
									Tab_gap$Area_Gain_Ratio_Outside_Reverse[n_tl] <- Tab_gap$Area_Gain_Ratio_Outside[n_tl] * -1
									Tab_gap$Vulnerability_Area_Loss_Ratio[n_tl] <- 1 - Tab_gap$Area_Ratio[n_tl]
									Tab_gap$Vulnerability_Area_LossIN_GainOUT_Ratio[n_tl] <- (Tab_gap$Area_Loss_Ratio[n_tl] / 100) - (Tab_gap$Area_Gain_Ratio_Outside[n_tl] / 100)
								}
							} else {
								for (y in 1:ly) {
									incProgress(1/tl, detail = paste("Doing part", n, "/", ls, "(", s, ")", "_", d, "_", c, "_", m, "_", y))
									n_tl <- n_tl + 1
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, ".grd", sep = "")
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
									Tab_gap$Area_Gain_Ratio_Outside[n_tl] <- (Tab_gap$Area_Gain[n_tl] / (T_Area - Tab_gap$Area[n_tl])) * 100
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
		write.csv(Tab_gap_sum, file = file.path(save_path, paste(input$SS_MO_Dir_Folder, "_Speices_VINDEX.csv", sep = "")))
		})
		##### End GAP analyzing =========================================    
	})
	
	
	output$SS_AO_Dir_Folder <- renderUI({
	  SS_AO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  SS_AO_Dir_Folder_selected <- SS_AO_Dir_Folder_list[1]
	  selectInput("SS_AO_Dir", "Working Species Distribution Folders",
	              choices = c(SS_AO_Dir_Folder_list),
	              selected = SS_AO_Dir_Folder_selected
	  )
	  
	})
	
	output$SS_AO_Model_Name <- renderUI({
	  G$SS_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir)
	  SS_Name_Species_list <- list.dirs(path = G$SS_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  SS_AO_Model_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir, SS_Name_Species_list[1]), full.names = FALSE, recursive = FALSE)
	  SS_AO_Model_Name_selected <- SS_AO_Model_Name_list[1]
	  selectInput("SS_AO_Model_Name_Input", "Working SDM Types",
	              choices = c(SS_AO_Model_Name_list),
	              selected = SS_AO_Model_Name_selected
	  )
	  
	})
	
	observeEvent(input$SS_AO_Species_Sel_All, {
	  G$SS_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir)
	  SS_Name_Species_list <- list.dirs(path = G$SS_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  G$SS_Name_Species_selected <<- SS_Name_Species_list
	})
	
	observeEvent(input$SS_AO_Species_Sel_None, {
	  G$SS_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir)
	  SS_Name_Species_list <- list.dirs(path = G$SS_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  G$SS_Name_Species_selected <<- ""  #SS_Name_Species_list[1]
	})
	
	output$SS_AO_Species <- renderUI({
	  G$SS_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir)
	  SS_Name_Species_list <- list.dirs(path = G$SS_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  checkboxGroupInput("SS_AO_Species", "Select a species",
	                     choices = c(SS_Name_Species_list),
	                     selected = G$SS_Name_Species_selected
	  )
	})
	
	output$SS_AO_Species_old <- renderUI({
	  G$SS_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir)
		SS_Name_Species_list <- list.dirs(path = G$SS_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
		SS_Name_Species_selected <- SS_Name_Species_list[1]
		selectInput("SS_AO_Species", "Select a species",
			choices = c(SS_Name_Species_list),
			selected = SS_Name_Species_selected
		)
	})

	output$SS_AO_SDM_model <- renderUI({
	  G$SS_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir)
	  SS_Name_Species_list <- list.dirs(path = G$SS_AO_Dir_Folder, full.names = FALSE, recursive = FALSE)
		destfile <- file.path(G$SS_AO_Dir_Folder, SS_Name_Species_list[1], "BIOMOD2", paste(as.name(paste(SS_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		SS_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		SS_Name_Models_selected <- SS_Name_Models_list[1]
		checkboxGroupInput("SS_AO_SDM_model", "Select models",
			choices = c(SS_Name_Models_list),
			selected = SS_Name_Models_selected
		)
	})
	
	output$SS_AO_UI_plot <- renderUI({
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SS_AO_Species
		dlist <- input$SS_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SS_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SS_AO_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SS_AO_Project_year
#		dtlist <- input$SS_AO_Dispersal_type
	
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
		plotOutput("SS_AO_OU_plot", width = ws, height = hs)
  })
	
	output$SS_AO_OU_plot <- renderPlot({
		#####========================================================
		##### Plot GAP output =========================================
		
		# setting Climate change scenarios, Future time, Species and current environmental path
		slist <- input$SS_AO_Species
		dlist <- input$SS_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$SS_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
		mlist <- input$SS_AO_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$SS_AO_Project_year
#		dtlist <- input$SS_AO_Dispersal_type
	
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
		    G$SS_AO_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir)
		    dir_path <- file.path(G$SS_AO_Dir_Folder, s, input$SS_AO_Model_Name_Input)
			for (d in dlist) {
				for (c in clist) {
					for (m in mlist) {
						if (ly > 0) {
							if (ly == 1 && ylist[1] == "2000") {
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
								plot(R_Map1, main = Map1)
							} else if (ly > 1 && ylist[1] == "2000") {
								Map1 <- paste("PRED", "_", d, "_", c, "_", ylist[1], "_", s, "_", m, ".grd", sep = "")
								R_Map1 <- raster(file.path(dir_path, Map1))
								plot(R_Map1, main = Map1)
								for (y in 2:ly) {
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, ".grd", sep = "")
									R_Map2 <- raster(file.path(dir_path, Map2))
									plot(R_Map2, main = Map2)
								}
							} else {
								for (y in 1:ly) {
									Map2 <- paste("GAP_", "PRED", "_", d, "_", c, "_", ylist[y], "_", s, "_", m, ".grd", sep = "")
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
	
	output$SS_AO_IV_Table <- DT::renderDataTable({
        if (input$SS_AO_IV_Data == "Species") {
            if (length(input$SS_AO_Species) > 0) {
                if (length(input$SS_AO_Species) == 1) {
                    destfile <- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir, input$SS_AO_Species[1], input$SS_AO_Model_Name_Input, paste(as.name(paste(input$SS_AO_Species[1], "_VINDEX.csv", sep = "")), sep = "", collapse = "--"))
	                vindex <- read.csv(destfile)
	                G_FILE_species_vindex <<- vindex
	                vindex
	            } else {
	                destfile <- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir, input$SS_AO_Species[1], input$SS_AO_Model_Name_Input, paste(as.name(paste(input$SS_AO_Species[1], "_VINDEX.csv", sep = "")), sep = "", collapse = "--"))
	                vindex <- read.csv(destfile)
	                for (s in input$SS_AO_Species[-1]) {
	                    destfile <- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir, s, input$SS_AO_Model_Name_Input, paste(as.name(paste(s, "_VINDEX.csv", sep = "")), sep = "", collapse = "--"))
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
	          destfile <- file.path(G$SE_Dir_Project, "Species_Distribution", input$SS_AO_Dir, paste(input$SS_AO_Model_Name_Input, "_Speices_VINDEX.csv", sep = "")) # , sep = "", collapse = "--")
	          vindex <- read.csv(destfile)
	          G_FILE_species_vindex <<- vindex
	          vindex
	      }
	   }
	})

	
	output$SS_AO_IV_Plot11 <- renderPlot({
		rs <- input$SS_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
	      vindex <- filter(vindex, Species %in% input$SS_AO_Species)
	      vindex <- filter(vindex, Climate_Model %in% input$SS_AO_Climate_model)
	      vindex <- filter(vindex, Climate_Scenario %in% input$SS_AO_Climate_scenario)
	      vindex <- filter(vindex, Model %in% input$SS_AO_SDM_model)
	      vindex <- filter(vindex, Year %in% input$SS_AO_Project_year)

			Group <- vindex[, input$SS_AO_IV_UI_plot1]
			ggplot(vindex, aes(x = Area_Loss_Ratio, y = Area_Gain_Ratio_Reverse, color = Group)) +
				geom_point(size = 6) +
				labs(title = "Vulnerability (Area Loss Ratio)", x = "Loss", y = "Gain") +
				geom_text(aes(label = Vulnerability_Area_Loss_Ratio), size = 3, hjust = 0.5, vjust = 3) + #, position =     "stack") +
				# horizontal
				geom_hline(yintercept = -50, color="orange", size=1) + 
				# vertical
				geom_vline(xintercept = 50, color="orange", size=1) +
				# Add arrow
				annotate("segment", x = 0, xend = 100, y = -100, yend = 0, colour = "purple", size = 2, alpha = 0.6, arrow = arrow())
	  
		}   
	})
	
	output$SS_AO_IV_Plot12 <- renderPlot({
		rs <- input$SS_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
			    vindex <- filter(vindex, Species %in% input$SS_AO_Species)
			    vindex <- filter(vindex, Climate_Model %in% input$SS_AO_Climate_model)
			    vindex <- filter(vindex, Climate_Scenario %in% input$SS_AO_Climate_scenario)
			    vindex <- filter(vindex, Model %in% input$SS_AO_SDM_model)
			    vindex <- filter(vindex, Year %in% input$SS_AO_Project_year)

			Group <- vindex[, input$SS_AO_IV_UI_plot1]
			ggplot(vindex, aes(x = Area_Loss_Ratio, y = Area_Gain_Ratio_Outside_Reverse, color = Group)) +
				geom_point(size = 6) +
				labs(title = "Vulnerability (Area LossIN GainOUT Ratio)", x = "Loss", y = "Gain") +
				geom_text(aes(label = Vulnerability_Area_LossIN_GainOUT_Ratio), size = 3, hjust = 0.5, vjust = 3) + #, position =     "stack") +
				# horizontal
				geom_hline(yintercept = -50, color="orange", size=1) + 
				# vertical
				geom_vline(xintercept = 50, color="orange", size=1) +
				# Add arrow
				annotate("segment", x = 0, xend = 100, y = -100, yend = 0, colour = "purple", size = 2, alpha = 0.6, arrow = arrow())
			
	  }
	})
	
	output$SS_AO_IV_Plot21 <- renderPlot({
		rs <- input$SS_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
			    vindex <- filter(vindex, Species %in% input$SS_AO_Species)
			    vindex <- filter(vindex, Climate_Model %in% input$SS_AO_Climate_model)
			    vindex <- filter(vindex, Climate_Scenario %in% input$SS_AO_Climate_scenario)
			    vindex <- filter(vindex, Model %in% input$SS_AO_SDM_model)
			    vindex <- filter(vindex, Year %in% input$SS_AO_Project_year)

			Group <- vindex[, input$SS_AO_IV_UI_plot2]
			ggplot(vindex, aes(x = Year, y = Vulnerability_Area_Loss_Ratio, group = Group, color = Group, linetype = Group)) +
			  geom_line() +
				geom_point(shape = 21, color = "black", fill = "#69b3a2", size=6) +
				theme_ipsum() +
				labs(title = "Vulnerability (Area Loss Ratio)", x = "Year", y = "Vulnerability")
			
	  }
	})
	
	output$SS_AO_IV_Plot22 <- renderPlot({
		rs <- input$SS_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
		if (length(rs)) {
			vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
			    vindex <- filter(vindex, Species %in% input$SS_AO_Species)
			    vindex <- filter(vindex, Climate_Model %in% input$SS_AO_Climate_model)
			    vindex <- filter(vindex, Climate_Scenario %in% input$SS_AO_Climate_scenario)
			    vindex <- filter(vindex, Model %in% input$SS_AO_SDM_model)
			    vindex <- filter(vindex, Year %in% input$SS_AO_Project_year)

			Group <- vindex[, input$SS_AO_IV_UI_plot2]
			ggplot(vindex, aes(x = Year, y = Vulnerability_Area_LossIN_GainOUT_Ratio, group = Group)) +
				geom_line(aes(color = Group, linetype = Group)) +
				geom_point(shape = 21, color = "black", fill = "#69b3a2", size=6) +
				theme_ipsum() +
				labs(title = "Vulnerability (Area Loss Ratio)", x = "Year", y = "Vulnerability")
			
	  }
	})
	
	output$SS_AO_IV_Plot31 <- renderPlot({
	  rs <- input$SS_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
	    vindex <- filter(vindex, Species %in% input$SS_AO_Species)
	    vindex <- filter(vindex, Climate_Model %in% input$SS_AO_Climate_model)
	    vindex <- filter(vindex, Climate_Scenario %in% input$SS_AO_Climate_scenario)
	    vindex <- filter(vindex, Model %in% input$SS_AO_SDM_model)
	    vindex <- filter(vindex, Year %in% input$SS_AO_Project_year)
	    
	    Group <- vindex[, input$SS_AO_IV_UI_plot3]
	    vindex$X <- ifelse(vindex$Vulnerability_Area_Loss_Ratio < 0, "below", "above")  # above / below avg flag
	    vindex <- vindex[order(vindex$Vulnerability_Area_Loss_Ratio), ]  # sort
#	    vindex$Species <- factor(vindex$Species, ordered = is.ordered(vindex)) #, levels = vindex$Species)  # convert to factor to retain sorted order in plot.
	    vindex[, input$SS_AO_IV_UI_plot35] <- factor(vindex[, input$SS_AO_IV_UI_plot35], ordered = is.ordered(vindex)) #, levels = vindex$Species)  # convert to factor to retain sorted order in plot.
	    
	    # Diverging Barcharts
	    ggplot(vindex, aes(x=Group, y=Vulnerability_Area_Loss_Ratio, label=Vulnerability_Area_Loss_Ratio)) + 
	      geom_bar(stat='identity', aes(fill=X), width=.5)  +
	      scale_fill_manual(name="Vulnerability", 
	                        labels = c("Above Average", "Below Average"), 
	                        values = c("above"="#00ba38", "below"="#f8766d")) + 
	      labs(subtitle="Species Vulnerability Index", 
	           title= "Diverging Bars") + 
	      coord_flip()
	    
	  }
	})
	
	output$SS_AO_IV_Plot32 <- renderPlot({
	  rs <- input$SS_AO_IV_Table_rows_selected  # G_FILE_specieslocation   # st_read("species.shp")
	  if (length(rs)) {
	    vindex <- G_FILE_species_vindex[rs, , drop = FALSE]
	  
	    vindex <- filter(vindex, Species %in% input$SS_AO_Species)
	    vindex <- filter(vindex, Climate_Model %in% input$SS_AO_Climate_model)
	    vindex <- filter(vindex, Climate_Scenario %in% input$SS_AO_Climate_scenario)
	    vindex <- filter(vindex, Model %in% input$SS_AO_SDM_model)
	    vindex <- filter(vindex, Year %in% input$SS_AO_Project_year)
	    
	    Group <- vindex[, input$SS_AO_IV_UI_plot3]
	    vindex$X <- ifelse(vindex$Vulnerability_Area_LossIN_GainOUT_Ratio < 0, "below", "above")  # above / below avg flag
	    vindex <- vindex[order(vindex$Vulnerability_Area_LossIN_GainOUT_Ratio), ]  # sort
	    vindex[, input$SS_AO_IV_UI_plot35] <- factor(vindex[, input$SS_AO_IV_UI_plot35], ordered = is.ordered(vindex)) #, levels = vindex$Species)  # convert to factor to retain sorted order in plot.
	    
	    # Diverging Barcharts
	    ggplot(vindex, aes(x=Group, y=Vulnerability_Area_LossIN_GainOUT_Ratio, label=Vulnerability_Area_LossIN_GainOUT_Ratio)) + 
	      geom_bar(stat='identity', aes(fill=X), width=.5)  +
	      scale_fill_manual(name="Vulnerability", 
	                        labels = c("Above Average", "Below Average"), 
	                        values = c("above"="#00ba38", "below"="#f8766d")) + 
	      labs(subtitle="Species Vulnerability Index", 
	           title= "Diverging Bars") + 
	      coord_flip()
	    
	  }
	})

	
	output$IS_MI_Dir_Folder <- renderUI({
	  IS_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  IS_MI_Dir_Folder_selected <- IS_MI_Dir_Folder_list[1]
	  selectInput("IS_MI_Dir", "Working Species Distribution Folders",
	              choices = c(IS_MI_Dir_Folder_list),
	              selected = IS_MI_Dir_Folder_selected
	  )
	 
	})
	
	output$IS_MI_Dir_Folder_Name <- renderUI({
	    G$IS_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir)
	    IS_Name_Species_list <- list.dirs(path = G$IS_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    IS_MI_Dir_Folder_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir, IS_Name_Species_list[1]), full.names = FALSE, recursive = FALSE)
	    IS_MI_Dir_Folder_Name_selected <- IS_MI_Dir_Folder_Name_list[1]
	    selectInput("IS_MI_Dir_Folder", "Working SDM Types",
	                choices = c(IS_MI_Dir_Folder_Name_list),
	                selected = IS_MI_Dir_Folder_Name_selected
	  )
	  
	})
	
	observeEvent(input$IS_CA_Species_Sel_All, {
	    G$IS_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir)
	    IS_Name_Species_list <- list.dirs(path = G$IS_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$IS_Name_Species_selected <<- IS_Name_Species_list
	})
	
	observeEvent(input$IS_CA_Species_Sel_None, {
	    G$IS_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir)
	    IS_Name_Species_list <- list.dirs(path = G$IS_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$IS_Name_Species_selected <<- ""  #SS_Name_Species_list[1]
	})
	
	output$IS_CA_Species <- renderUI({
	    G$IS_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir)
		IS_Name_Species_list <- list.dirs(path = G$IS_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
		checkboxGroupInput("IS_CA_Species", "Select a species",
			choices = c(IS_Name_Species_list),
			selected = G$IS_Name_Species_selected
		)
	})
	
	output$IS_CA_SDM_model <- renderUI({
	    G$IS_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir)
	    IS_Name_Species_list <- list.dirs(path = G$IS_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
		destfile <- file.path(G$IS_MI_Dir_Folder, IS_Name_Species_list[1], "BIOMOD2", paste(as.name(paste(IS_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		IS_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		IS_Name_Models_selected <- IS_Name_Models_list[1]
		checkboxGroupInput("IS_CA_SDM_model", "Select models",
			choices = c(IS_Name_Models_list),
			selected = IS_Name_Models_selected
		)
	})
	
	observeEvent(input$IS_MO_Dir_Folder, {
#	  showModal(modalDialog(
#	    title = "Message",
#	    "A folder path and name is recommended in english!"
#	  ))
		volumes <- c(main = file.path(G$SE_Dir_Project, "Invasive_Species"))
		shinyDirChoose(input, 'IS_MO_Dir_Folder', roots = volumes) # , defaultPath = "/MOTIVE_projects", defaultRoot = G$SE_Dir_Project)
		G$IS_MO_Dir_Folder <- parseDirPath(volumes, input$IS_MO_Dir_Folder)
		output$IS_MO_Dir_Folder <- renderText({G$IS_MO_Dir_Folder})
#		G$IS_AO_MO_Dir_Folder <<- G$IS_MO_Dir_Folder
#		output$IS_AO_MO_Dir_Folder <- renderText({G$IS_AO_MO_Dir_Folder})
	})
	
	observeEvent(input$IS_VA_Action_Analysis, {
	  
		# setting Climate change scenarios, Future time, Species and current environmental path
		dlist <- input$IS_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
		clist <- input$IS_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
#		dtlist <- input$IS_CA_Dispersal_type
		mlist <- input$IS_CA_SDM_model # c("PA1_Full_GLM_byROC")
		ylist <- input$IS_CA_Project_year
		slist <- input$IS_CA_Species
    
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
		
		withProgress(message = 'Runing Invasive Species Impact and Vulnerability Analysis.........', value = 0, {
		G$IS_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir)  
		for (d in dlist) {
			for (c in clist) {
				for (m in mlist) {
					for (y in ylist) {
					  incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y))
							if(y == ylist[1]) {
								for (s in slist) {
									dir_path <- file.path(G$IS_MI_Dir_Folder, s, input$IS_MI_Dir_Folder)
									img <- file.path(dir_path, paste("PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
									sr_list <- c(sr_list, img)
								}
								save_path <- G$IS_MO_Dir_Folder
								sr_list <- grep("PRED", sr_list, value = TRUE)
								sr_stack <- stack(sr_list)
								sr_raster <- overlay(sr_stack, fun=sum)
								sr_raster1 <- sr_raster
								writeRaster(sr_raster, file = file.path(save_path, paste(as.name(paste("IS_SR_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								vi1_raster <- sr_raster
								vi1_raster[] <- 0
								vi2_raster <- sr_raster
								vi2_raster[] <- 0
								vi3_raster <- sr_raster
								vi3_raster[] <- 0
								writeRaster(vi1_raster, file = file.path(save_path, paste(as.name(paste("IS_VI1_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(vi2_raster, file = file.path(save_path, paste(as.name(paste("IS_VI2_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(vi3_raster, file = file.path(save_path, paste(as.name(paste("IS_VI3_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								sr_list <- NULL
							} else {
#							    incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y))
								for (s in slist) {
								  dir_path <- file.path(G$IS_MI_Dir_Folder, s, input$IS_MI_Dir_Folder)
									img <- file.path(dir_path, paste("PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
									sr_list <- c(sr_list, img)
									img <- file.path(dir_path, paste("LOSS_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
									loss_list <- c(loss_list, img)
									img <- file.path(dir_path, paste("STAY_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
									stay_list <- c(stay_list, img)
									img <- file.path(dir_path, paste("GAIN_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
									gain_list <- c(gain_list, img)
								}
								save_path <- G$IS_MO_Dir_Folder
								sr_list <- grep("PRED", sr_list, value = TRUE)
								sr_stack <- stack(sr_list)
								sr_raster <- overlay(sr_stack, fun=sum)
								sr_raster2 <- sr_raster
								losssr_raster <- sr_raster2 - sr_raster1
								writeRaster(sr_raster, file = file.path(save_path, paste(as.name(paste("IS_SR_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								writeRaster(losssr_raster, file = file.path(save_path, paste(as.name(paste("IS_VI1_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								loss_list <- grep("LOSS", loss_list, value = TRUE)
								loss_stack <- stack(loss_list)
								loss_raster <- overlay(loss_stack, fun=sum)
								writeRaster(loss_raster, file = file.path(save_path, paste(as.name(paste("IS_LOSS_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								stay_list <- grep("STAY", stay_list, value = TRUE)
								stay_stack <- stack(stay_list)
								stay_raster <- overlay(stay_stack, fun=sum)
								writeRaster(stay_raster, file = file.path(save_path, paste(as.name(paste("IS_STAY_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								gain_list <- grep("GAIN", gain_list, value = TRUE)
								gain_stack <- stack(gain_list)
								gain_raster <- overlay(gain_stack, fun=sum)
								writeRaster(gain_raster, file = file.path(save_path, paste(as.name(paste("IS_GAIN_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
								vi2_raster <- sr_raster
								vi2_raster <- loss_raster / sr_raster1
								vi2_raster[sr_raster1 == 0] <- 0
								writeRaster(vi2_raster, file = file.path(save_path, paste(as.name(paste("IS_VI2_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")),overwrite = TRUE)
								
								vi3_raster <- sr_raster
								vi3_raster <- (1 - (loss_raster / sr_raster1)) + (gain_raster / (length(slist) - sr_raster1))
								vi3_raster[sr_raster1 == 0] <- 0
								writeRaster(vi3_raster, file = file.path(save_path, paste(as.name(paste("IS_VI3_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
								
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
		    
#		    destfile <- file.path(G$IS_MO_Dir_Folder, "InvasiveSpecies_Options.csv")
		    
		    IS_variables <- setNames(data.frame(matrix(ncol = 8, nrow = 5000)), c("input$SDM_Folder", "IS_CA_Species_Number", "input$IS_CA_Species", "input$IS_CA_Dispersal_type", "input$IS_CA_Climate_model", "input$IS_CA_Climate_scenario", 
		                                                                         "input$IS_CA_Project_year", "input$IS_CA_SDM_model"
		    ))
		    
		    IS_variables[1:length(G$IS_MI_Dir_Folder), "input$SDM_Folder"] <- G$IS_MI_Dir_Folder
		    IS_variables[1:1, "IS_CA_Species_Number"] <- length(input$IS_CA_Species)
		    IS_variables[1:length(input$IS_CA_Species), "input$IS_CA_Species"] <- input$IS_CA_Species
		    IS_variables[1:length(input$IS_CA_Dispersal_type), "input$IS_CA_Dispersal_type"] <- input$IS_MI_Dir_Folder
		    IS_variables[1:length(input$IS_CA_Climate_model), "input$IS_CA_Climate_model"] <- input$IS_CA_Climate_model
		    IS_variables[1:length(input$IS_CA_Climate_scenario), "input$IS_CA_Climate_scenario"] <- input$IS_CA_Climate_scenario
		    IS_variables[1:length(input$IS_CA_Project_year), "input$IS_CA_Project_year"] <- input$IS_CA_Project_year
		    IS_variables[1:length(input$IS_CA_SDM_model), "input$IS_CA_SDM_model"] <- input$IS_CA_SDM_model
		    
		    IS_variables[is.na(IS_variables)] <- ""
		    write.csv(IS_variables, file = file.path(G$IS_MO_Dir_Folder, "InvasiveSpecies_Options.csv"))
		    
		    #####		    
		    
		    
		})
	})
	
	observeEvent(input$IS_VA_Action_Admin, {

	  # setting Climate change scenarios, Future time, Species and current environmental path
	  alist <- input$IS_VA_Admin
	  dlist <- input$IS_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$IS_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
#	  dtlist <- input$IS_CA_Dispersal_type
	  mlist <- input$IS_CA_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$IS_CA_Project_year
	  slist <- input$IS_CA_Species
	  vlist <- c("IS_SR", "IS_LOSS", "IS_STAY", "IS_GAIN", "IS_VI1", "IS_VI2", "IS_VI3") # c("IS_SR") # 
	  
	  n <- 0
	  la <- length(alist)
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  lv <- length(vlist)
	  
	  tls <- la * ld * lc * lm * ly * lv
	  tlg <- la * ld * lc * lm * ly * ls 

	  
	  if(FALSE) { 
      # Individual Species
      
	  for (a in alist) {
	    withProgress(message = paste("Species Analyzing by ", input$IS_VA_Admin), value = 0, {
	    for (s in slist) {
	        dir_path <- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_MI_Dir, s, input$IS_MI_Dir_Folder)
	        dataFiles <- dir(G$SE_Dir_GIS, paste(a, ".*", sep = ""), ignore.case = TRUE, all.files = TRUE)
	        file.copy(file.path(G$SE_Dir_GIS, dataFiles), dir_path, overwrite = TRUE)
	        #	    poly <- readShapePoly(file.path(dir_path, paste(a, ".shp", sep = "")))
	        poly <- readOGR(dsn=dir_path, layer=a)
	        df <- read.dbf(file.path(G$SE_Dir_GIS, paste(a, ".dbf", sep = "")))
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
#	            if (length(ylist) > 1) {
#	              if(y == ylist[1]) {
#	                incProgress(1/tls, detail = paste("Doing part", d, "_", c, "_", m, "_", y, "_", vlist[1]))
#                    img <- file.path(dir_path, paste(vlist[1], "_",  d, "_", c, "_", m, "_", y, ".grd", sep = ""))
#                    r <- raster(img)
#                    df1 <- extract(r, poly, fun = max, na.rm = TRUE, df = TRUE)
#                    #write to a data frame
#                    df1 <- data.frame(df1[-1])
#                    colnames(df1) <- c(paste(vlist[1], "_",  d, "_", c, "_", m, "_", y, sep = ""))
#                    df1[is.na(df1)] <- 0
#                    df <- cbind(df, df1)
#	              } else {
	                for (v in "PRED") {
	                  incProgress(1/tls, detail = paste("Doing part", d, "_", c, "_", y, "_", s, "_", m))
	                  img <- file.path(dir_path, paste(v, "_",  d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
	                  r <- raster(img)
	                  df1 <- extract(r, poly, fun = max, na.rm = TRUE, df=TRUE)
	                  #write to a data frame
	                  df1 <- data.frame(df1[-1])
	                  colnames(df1) <- c(paste(v, "_",  d, "_", c, "_", y, "_", s, "_", m, sep = ""))
	                  df1[is.na(df1)] <- 0
	                  df <- cbind(df, df1)
	                }
#	              }
#	            }
#	            else {
#	                showModal(modalDialog(
#	                    title = "Message",
#	                    "Please select projection years more than one!"
#	                ))
#	            }  
	          }
	        }
	      }
	    }
	    
	    #write to a CSV file
	    write.csv(df, file = file.path(dir_path, paste("IS_", a, ".csv", sep="")))
	    write.dbf(df, file.path(dir_path, paste(a, ".dbf", sep = "")))
	    
	    }
	    })
	  }
	  
	  } else { 
	  # Species Group
	  dir_path <- G$IS_MO_Dir_Folder
	  for (a in alist) {
	        
	    dataFiles <- dir(G$SE_Dir_GIS, paste(a, ".*", sep = ""), ignore.case = TRUE, all.files = TRUE)
	    file.copy(file.path(G$SE_Dir_GIS, dataFiles), dir_path, overwrite = TRUE)
	    #	      poly <- readShapePoly(file.path(dir_path, paste(a, ".shp", sep = "")))
	    poly <- readOGR(dsn=dir_path, layer=a)
	    df <- read.dbf(file.path(G$SE_Dir_GIS, paste(a, ".dbf", sep = "")))
	    withProgress(message = paste("Species Group Analyzing by ", input$IS_VA_Admin), value = 0, {
	    for (d in dlist) {
	        for (c in clist) {
	            for (m in mlist) {
	                for (y in ylist) {
#	                    if (length(ylist) > 1) {
#	                        if(y == ylist[1]) {
#	                            incProgress(1/tlg, detail = paste("Doing part", d, "_", c, "_", m, "_", y, "_", vlist[1]))
#	                            img <- file.path(dir_path, paste(vlist[1], "_",  d, "_", c, "_", m, "_", y, ".grd", sep = ""))
#	                            r <- raster(img)
#	                            df1 <- extract(r, poly, fun = max, na.rm = TRUE, df = TRUE)
#	                            #write to a data frame
#	                            df1 <- data.frame(df1[-1])
#	                            colnames(df1) <- c(paste(vlist[1], "_",  d, "_", c, "_", m, "_", y, sep = ""))
#	                            df1[is.na(df1)] <- 0
#	                            df <- cbind(df, df1)
#	                        } else {
	                            for (v in vlist) {
	                                incProgress(1/tlg, detail = paste("Doing part", d, "_", c, "_", m, "_", y, "_", v))
	                                img <- file.path(dir_path, paste(v, "_",  d, "_", c, "_", m, "_", y, ".grd", sep = ""))
	                                r <- raster(img)
	                                df1 <- extract(r, poly, fun = max, na.rm = TRUE, df=TRUE)
	                                #write to a data frame
	                                df1 <- data.frame(df1[-1])
	                                colnames(df1) <- c(paste(v, "_",  d, "_", c, "_", m, "_", y, sep = ""))
	                                df1[is.na(df1)] <- 0
	                                df <- cbind(df, df1)
	                            }
#	                        }
#	                    }
#	                    else {
#	                        showModal(modalDialog(
#	                            title = "Message",
#	                            "Please select projection years more than one!"
#	                        ))
#	                    }  
	                }
	            }
	        }
	    }
	    
	    #write to a CSV file
	    write.csv(df, file = file.path(dir_path, paste("IS_", a, ".csv", sep="")))
	    write.dbf(df, file.path(dir_path, paste(a, ".dbf", sep = "")))
	    
	  })
	  }
	  }
	})
	

	output$IS_AO_MI_Dir_Folder <- renderUI({
	  IS_AO_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  IS_AO_MI_Dir_Folder_selected <- IS_AO_MI_Dir_Folder_list[1]
	  selectInput("IS_AO_MI_Dir", "Working Species Distribution Folders",
	              choices = c(IS_AO_MI_Dir_Folder_list),
	              selected = IS_AO_MI_Dir_Folder_selected
	  )
	  
	})
	
	output$IS_AO_MO_Dir_Folder <- renderUI({
	  IS_AO_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Invasive_Species"), full.names = FALSE, recursive = FALSE)
	  IS_AO_MO_Dir_Folder_selected <- IS_AO_MO_Dir_Folder_list[1]
	  selectInput("IS_AO_MO_Dir", "Working Invasive Species Folders",
	              choices = c(IS_AO_MO_Dir_Folder_list),
	              selected = IS_AO_MO_Dir_Folder_selected
	  )
	  
	})
	
	output$IS_AO_MI_Dir_Folder_Name <- renderUI({
	  IS_AO_MI_Dir_Folder_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_AO_MI_Dir, input$IS_AO_Species[1]), full.names = FALSE, recursive = FALSE)
	  IS_AO_MI_Dir_Folder_Name_selected <- IS_AO_MI_Dir_Folder_Name_list[1]
	  selectInput("IS_AO_MI_Dir_Folder", "Working SDM Types",
	              choices = c(IS_AO_MI_Dir_Folder_Name_list),
	              selected = IS_AO_MI_Dir_Folder_Name_selected
	  )
	  
	})		

	
	output$IS_AO_Species <- renderUI({
	  G$IS_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_AO_MI_Dir)
	  IS_Name_Species_list <- list.dirs(path = G$IS_AO_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	  IS_Name_Species_selected <- IS_Name_Species_list[1]
	  selectInput("IS_AO_Species", "Select a species",
	              choices = c(IS_Name_Species_list),
	              selected = IS_Name_Species_selected
	  )
	})
	
	
	output$IS_AO_SDM_model <- renderUI({
	  G$IS_AO_MI_Dir_Folder <<- file.path(G$SE_Dir_Project, "Species_Distribution", input$IS_AO_MI_Dir)
		destfile <- file.path(G$IS_AO_MI_Dir_Folder, input$IS_AO_Species[1], "BIOMOD2", paste(as.name(paste(input$IS_AO_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		IS_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		IS_Name_Models_selected <- IS_Name_Models_list[1]
		radioButtons("IS_AO_SDM_model", "Select models",
			choices = c(IS_Name_Models_list),
			selected = IS_Name_Models_selected
		)
	})
	
	output$IS_AO_SD_Map <- renderLeaflet({
	  dir_path <- file.path(G$IS_AO_MI_Dir_Folder, input$IS_AO_Species, input$IS_AO_MI_Dir_Folder)
	  Map <- paste("PRED", "_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_Project_year, "_", input$IS_AO_Species, "_", input$IS_AO_SDM_model, ".grd", sep = "")
	  r <- raster(file.path(dir_path, Map))
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
	})  
	
	output$IS_AO_SD_Summary <- renderPrint({
	  dir_path <- file.path(G$IS_AO_MI_Dir_Folder, input$IS_AO_Species, "BIOMOD2")
	  Map <- paste("PRED", "_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_Project_year, "_", input$IS_AO_Species, "_", input$IS_AO_SDM_model, ".grd", sep = "")
	  r <- raster(file.path(dir_path, Map))
	  summary(r)
	})
	
	output$IS_AO_SD_Histogram <- renderPlot({
	  dir_path <- file.path(G$IS_AO_MI_Dir_Folder, input$IS_AO_Species, "BIOMOD2")
	  Map <- paste("PRED", "_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_Project_year, "_", input$IS_AO_Species, "_", input$IS_AO_SDM_model, ".grd", sep = "")
	  x <- raster(file.path(dir_path, Map))
	  
	  hist(x, # breaks = bins, 
	       col="orange",
	       border="brown",
	       xlab = "Predicted Value",
	       main = "Histogram")
	})
	
  output$IS_AO_SR_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$IS_AO_Species
	  olist <- c("IS_SR")  # input$IS_AO_Output_option1
	  dlist <- input$IS_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$IS_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$IS_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$IS_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$IS_AO_Dir_Folder), Map1))
	              r <- raster(file.path(G$IS_AO_MO_Dir_Folder, Map1))
	              #                plot(R_Map1, main = Map1)
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
	})

  output$IS_AO_SR_Map_OLD <- renderLeaflet({
      
      # setting Climate change scenarios, Future time, Species and current environmental path
      #    slist <- input$IS_AO_Species
      olist <- c("IS_SR")  # input$IS_AO_Output_option1
      dlist <- input$IS_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
      clist <- input$IS_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
      mlist <- input$IS_AO_SDM_model # c("PA1_Full_GLM_byROC")
      ylist <- input$IS_AO_Project_year
      #	dtlist <- input$SS_AO_Dispersal_type
      
      #    ls <- length(slist)
      lo <- length(olist)
      ld <- length(dlist)
      lc <- length(clist)
      lm <- length(mlist)
      ly <- length(ylist)
      
      tl <- lo * ld * lc * lm * ly
      nc <- 2
      if (tl <  2) {
          nr <- round(tl / nc) + 1
      } else {
          nr <- round((tl + 0.1) / nc)
      }
      
      #    par(mfrow = c(nr,nc), cex.main = 1.2)
      
      for (o in olist) {
          for (d in dlist) {
              for (c in clist) {
                  for (m in mlist) {
                      for (y in ylist) {
                          if (ly > 0) {
                              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
                              #                R_Map1 <- raster(file.path(isolate(G$IS_AO_Dir_Folder), Map1))
                              r <- raster(file.path(isolate(G$IS_AO_MO_Dir_Folder), Map1))
                              #                plot(R_Map1, main = Map1)
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
  })  
  

	output$IS_AO_SR_SIDO_Map <- renderLeaflet({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  poly <- readOGR(file.path(G$IS_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("IS_SR_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep = "")
	  
		bins <- c(0, 2, 4, 6, 8, 10, Inf)
		pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
		
		labels <- sprintf(
		  "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
		  poly[[X_NAME]], poly[[V_NAME]]
		) %>% lapply(htmltools::HTML)
		
		leaflet(poly) %>%
		  setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
		  addProviderTiles("MapBox", options = providerTileOptions(
		    id = "mapbox.light",
		    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%   
		  addPolygons(
		    fillColor = ~pal(poly[[V_NAME]]),
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
		  addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
		            position = "bottomright")
	})
	
	output$IS_AO_SR_SIDO_Stat <- renderPlot({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  df <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("IS_SR_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 외래종 분포") + labs(x = "시도") + labs(y = "외래종수")
	})
	
	output$IS_AO_SR_SGG_Map <- renderLeaflet({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  poly <- readOGR(file.path(G$IS_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("IS_SR_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$IS_AO_SR_SGG_UI <- renderUI({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  df <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SGG", ".csv", sep = "")))
	  IS_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  IS_Name_SD_selected <- IS_Name_SD_list[1]
	  
	  selectInput("IS_AO_SR_SGG_UI", "시도",
	              choices = c(IS_Name_SD_list),
	              selected = IS_Name_SD_selected
	  )
	})
	
	output$IS_AO_SR_SGG_Stat <- renderPlot({
	  
#	  df <- read.csv(file.path(isolate(G$IS_AO_Dir_Folder), paste("IS_", input$IS_VA_Admin, ".csv", sep = "")))
#	  df <- read.dbf(file.path(isolate(G$SE_Dir_GIS), paste("O_SGG", ".dbf", sep = "")))
#	  df[df[, 2] == "gun",]
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  df <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$IS_AO_SR_SGG_UI), ]
#	  names(df) <- c(names(x[-1]))
    X_NAME <- names(df[8])
	  V_NAME <- paste("IS_SR_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep="")

	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 외래종 분포") + labs(x = "시군구") + labs(y = "외래종수")
	})
	
	output$IS_AO_SI_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$IS_AO_Species
	  olist <- c("IS_GAIN")  # input$IS_AO_Output_option1
	  dlist <- input$IS_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$IS_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$IS_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$IS_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	                Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	                #                R_Map1 <- raster(file.path(isolate(G$IS_AO_Dir_Folder), Map1))
	                r <- raster(file.path(G$IS_AO_MO_Dir_Folder, Map1))
	                #                plot(R_Map1, main = Map1)
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
	})
	
	
	output$IS_AO_SI_SIDO_Map <- renderLeaflet({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  poly <- readOGR(file.path(G$IS_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("IS_GAIN_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", #people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$IS_AO_SI_SIDO_Stat <- renderPlot({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  df <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("IS_GAIN_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 외래종 유입") + labs(x = "시도") + labs(y = "외래종수")
	})
	#	geom_text(aes(label = Vulnerability_Area_Loss_Ratio), size = 3, hjust = 0.5, vjust = 3) + 
	
	output$IS_AO_SI_SGG_Map <- renderLeaflet({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  poly <- readOGR(file.path(G$IS_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("IS_GAIN_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$IS_AO_SI_SGG_UI <- renderUI({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  df <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SGG", ".csv", sep = "")))
	  IS_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  IS_Name_SD_selected <- IS_Name_SD_list[1]
	  
	  selectInput("IS_AO_SI_SGG_UI", "시도",
	              choices = c(IS_Name_SD_list),
	              selected = IS_Name_SD_selected
	  )
	})
	
	output$IS_AO_SI_SGG_Stat <- renderPlot({
	  G$IS_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Invasive_Species", input$IS_AO_MO_Dir)
	  df <- read.csv(file.path(G$IS_AO_MO_Dir_Folder, paste("IS_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$IS_AO_SI_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("IS_GAIN_", input$IS_AO_Climate_model, "_", input$IS_AO_Climate_scenario, "_", input$IS_AO_SDM_model, "_", input$IS_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 외래종 유입") + labs(x = "시군구") + labs(y = "외래종수")
	})
	
	
	output$VH_MI_Dir_Folder <- renderUI({
	  VH_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  VH_MI_Dir_Folder_selected <- VH_MI_Dir_Folder_list[1]
	  selectInput("VH_MI_Dir", "Working Species Distribution Folders",
	              choices = c(VH_MI_Dir_Folder_list),
	              selected = VH_MI_Dir_Folder_selected
	  )
	  
	})
	
	output$VH_MI_Dir_Folder_Name <- renderUI({
	    G$VH_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_MI_Dir)
	    VH_Name_Species_list <- list.dirs(path = G$VH_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    VH_MI_Dir_Folder_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_MI_Dir, VH_Name_Species_list[1]), full.names = FALSE, recursive = FALSE)
	    VH_MI_Dir_Folder_Name_selected <- VH_MI_Dir_Folder_Name_list[1]
	    selectInput("VH_MI_Dir_Folder", "Working SDM Types",
	                choices = c(VH_MI_Dir_Folder_Name_list),
	                selected = VH_MI_Dir_Folder_Name_selected
	    )
	    
	})
	
	observeEvent(input$VH_CA_Species_Sel_All, {
	    G$VH_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_MI_Dir)
	    VH_Name_Species_list <- list.dirs(path = G$VH_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$VH_Name_Species_selected <<- VH_Name_Species_list
	})
	
	observeEvent(input$VH_CA_Species_Sel_None, {
	    G$IS_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_MI_Dir)
	    VH_Name_Species_list <- list.dirs(path = G$VH_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    G$VH_Name_Species_selected <<- ""  #SS_Name_Species_list[1]
	})
	
	output$VH_CA_Species <- renderUI({
	    G$VH_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_MI_Dir)
	    VH_Name_Species_list <- list.dirs(path = G$VH_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    checkboxGroupInput("VH_CA_Species", "Select a species",
	                       choices = c(VH_Name_Species_list),
	                       selected = G$VH_Name_Species_selected
	    )
	})
	
	output$VH_CA_SDM_model <- renderUI({
	    G$VH_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_MI_Dir)
	    VH_Name_Species_list <- list.dirs(path = G$VH_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
	    destfile <- file.path(G$VH_MI_Dir_Folder, VH_Name_Species_list[1], "BIOMOD2", paste(as.name(paste(VH_Name_Species_list[1], "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
	    all_eval <- read.csv(destfile)
	    G_FILE_species_evaluation <<- all_eval
	    VH_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
	    VH_Name_Models_selected <- VH_Name_Models_list[1]
	    checkboxGroupInput("VH_CA_SDM_model", "Select models",
	                       choices = c(VH_Name_Models_list),
	                       selected = VH_Name_Models_selected
	    )
	})
	
	observeEvent(input$VH_MO_Dir_Folder, {
	  showModal(modalDialog(
	    title = "Message",
	    "A folder path and name is recommended in english!"
	  ))
	  volumes <- c(main = file.path(G$SE_Dir_Project, "Vulnerable_Habitat"))
	  shinyDirChoose(input, 'VH_MO_Dir_Folder', roots = volumes) # , defaultPath = "/MOTIVE_projects", defaultRoot = G$SE_Dir_Project)
	  G$VH_MO_Dir_Folder <<- parseDirPath(volumes, input$VH_MO_Dir_Folder)
	  output$VH_MO_Dir_Folder <- renderText({G$VH_MO_Dir_Folder})
	  G$VH_AO_MO_Dir_Folder <<- G$VH_MO_Dir_Folder
	  output$VH_AO_MO_Dir_Folder <- renderText({G$VH_AO_MO_Dir_Folder})
	})
	
	
	observeEvent(input$VH_VA_Action_Analysis, {
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  dlist <- input$VH_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  #	dtlist <- input$VH_CA_Dispersal_type
	  mlist <- input$VH_CA_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_CA_Project_year
	  slist <- input$VH_CA_Species
	  
	  n <- 0
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- ld * lc * lm * ly
	  
	  sr_list <- NULL
	  loss_list <- NULL
	  stay_list <- NULL
	  gain_list <- NULL
	  
	  withProgress(message = 'Runing Vulnerable Habitat Impact and Vulnerability Analysis.........', value = 0, {
	    G$VH_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_MI_Dir)  
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y))
#	            if (length(ylist) > 1) {
	              if(y == ylist[1]) {
	                for (s in slist) {
	                  dir_path <- file.path(G$VH_MI_Dir_Folder, s, input$VH_MI_Dir_Folder)
	                  img <- file.path(dir_path, paste("PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
	                  sr_list <- c(sr_list, img)
	                }
	                save_path <- G$VH_MO_Dir_Folder
	                sr_list <- grep("PRED", sr_list, value = TRUE)
	                sr_stack <- stack(sr_list)
	                sr_raster <- overlay(sr_stack, fun=sum)
	                sr_raster1 <- sr_raster
	                writeRaster(sr_raster, file = file.path(save_path, paste(as.name(paste("VH_SR_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                vi1_raster <- sr_raster
	                vi1_raster[] <- 0
	                vi2_raster <- sr_raster
	                vi2_raster[] <- 0
	                vi3_raster <- sr_raster
	                vi3_raster[] <- 0
	                writeRaster(vi1_raster, file = file.path(save_path, paste(as.name(paste("VH_VI1_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                writeRaster(vi2_raster, file = file.path(save_path, paste(as.name(paste("VH_VI2_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                writeRaster(vi3_raster, file = file.path(save_path, paste(as.name(paste("VH_VI3_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                sr_list <- NULL
	              } else {					  
	                for (s in slist) {
#	                  incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y))
	                  dir_path <- file.path(G$VH_MI_Dir_Folder, s, input$VH_MI_Dir_Folder)
	                  img <- file.path(dir_path, paste("PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
	                  sr_list <- c(sr_list, img)
	                  img <- file.path(dir_path, paste("LOSS_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
	                  loss_list <- c(loss_list, img)
	                  img <- file.path(dir_path, paste("STAY_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
	                  stay_list <- c(stay_list, img)
	                  img <- file.path(dir_path, paste("GAIN_", "PRED", "_", d, "_", c, "_", y, "_", s, "_", m, ".grd", sep = ""))
	                  gain_list <- c(gain_list, img)
	                }
	                save_path <- G$VH_MO_Dir_Folder
	                sr_list <- grep("PRED", sr_list, value = TRUE)
	                sr_stack <- stack(sr_list)
	                sr_raster <- overlay(sr_stack, fun=sum)
	                sr_raster2 <- sr_raster
	                losssr_raster <- sr_raster2 - sr_raster1
	                writeRaster(sr_raster, file = file.path(save_path, paste(as.name(paste("VH_SR_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                writeRaster(losssr_raster, file = file.path(save_path, paste(as.name(paste("VH_VI1_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                
	                loss_list <- grep("LOSS", loss_list, value = TRUE)
	                loss_stack <- stack(loss_list)
	                loss_raster <- overlay(loss_stack, fun=sum)
	                writeRaster(loss_raster, file = file.path(save_path, paste(as.name(paste("VH_LOSS_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                
	                stay_list <- grep("STAY", stay_list, value = TRUE)
	                stay_stack <- stack(stay_list)
	                stay_raster <- overlay(stay_stack, fun=sum)
	                writeRaster(stay_raster, file = file.path(save_path, paste(as.name(paste("VH_STAY_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                
	                gain_list <- grep("GAIN", gain_list, value = TRUE)
	                gain_stack <- stack(gain_list)
	                gain_raster <- overlay(gain_stack, fun=sum)
	                writeRaster(gain_raster, file = file.path(save_path, paste(as.name(paste("VH_GAIN_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                
	                vi2_raster <- sr_raster
	                vi2_raster <- loss_raster / sr_raster1
	                writeRaster(vi2_raster, file = file.path(save_path, paste(as.name(paste("VH_VI2_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                vi3_raster <- sr_raster
	                vi3_raster <- (1 - (loss_raster / sr_raster1)) + (gain_raster / (length(slist) - sr_raster1))
	                writeRaster(vi3_raster, file = file.path(save_path, paste(as.name(paste("VH_VI3_", d, "_", c, "_", m, "_", y, ".grd", sep = "")), sep = "", collapse = "--")), overwrite = TRUE)
	                
	                sr_list <- NULL
	                loss_list <- NULL
	                stay_list <- NULL
	                gain_list <- NULL
	              }				    
#	            } 
	          }
	        }
	      }
	    }
	      
	      #		    destfile <- file.path(G$VH_MO_Dir_Folder, "VulnerableHabitat_Options.csv")
	      
	      VH_variables <- setNames(data.frame(matrix(ncol = 7, nrow = 5000)), c("input$SDM_Folder", "input$VH_CA_Species", "input$VH_CA_Dispersal_type", "input$VH_CA_Climate_model", "input$VH_CA_Climate_scenario", 
	                                                                          "input$VH_CA_Project_year", "input$VH_CA_SDM_model"
	      ))
	      
	      VH_variables[1:length(G$VH_MI_Dir_Folder), "input$SDM_Folder"] <- G$VH_MI_Dir_Folder
	      VH_variables[1:length(input$VH_CA_Species), "input$VH_CA_Species"] <- input$VH_CA_Species
	      VH_variables[1:length(input$VH_CA_Dispersal_type), "input$VH_CA_Dispersal_type"] <- input$VH_MI_Dir_Folder
	      VH_variables[1:length(input$VH_CA_Climate_model), "input$VH_CA_Climate_model"] <- input$VH_CA_Climate_model
	      VH_variables[1:length(input$VH_CA_Climate_scenario), "input$VH_CA_Climate_scenario"] <- input$VH_CA_Climate_scenario
	      VH_variables[1:length(input$VH_CA_Project_year), "input$VH_CA_Project_year"] <- input$VH_CA_Project_year
	      VH_variables[1:length(input$VH_CA_SDM_model), "input$VH_CA_SDM_model"] <- input$VH_CA_SDM_model
	      
	      VH_variables[is.na(VH_variables)] <- ""
	      write.csv(VH_variables, file = file.path(G$VH_MO_Dir_Folder, "VulnerableHabitat_Options.csv"))
	      
	      #####			      
	      
	  })
	})
	
	observeEvent(input$VH_VA_Action_Habitat, {
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  alist <- input$VH_VA_Habitat
	  dlist <- input$VH_CA_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_CA_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  #	dtlist <- input$VH_CA_Dispersal_type
	  mlist <- input$VH_CA_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_CA_Project_year
	  slist <- input$VH_CA_Species
	  vlist <- c("VH_SR", "VH_LOSS", "VH_STAY", "VH_GAIN", "VH_VI1", "VH_VI2", "VH_VI3") # c("VH_SR") # 
	  
	  n <- 0
	  la <- length(alist)
	  ls <- length(slist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  lv <- length(vlist)
	  
	  tl <- la * ld * lc * lm * ly * lv
	  
	  withProgress(message = paste("Grouping by ", input$VH_VA_Habitat), value = 0, {
	    
	    dir_path <- G$VH_MO_Dir_Folder
	    
	    for (a in alist) {
	      
	      dataFiles <- dir(G$SE_Dir_GIS, paste(a, ".*", sep = ""), ignore.case = TRUE, all.files = TRUE)
	      file.copy(file.path(G$SE_Dir_GIS, dataFiles), dir_path, overwrite = TRUE)
	      poly <- readShapePoly(file.path(G$SE_Dir_GIS, paste(a, ".shp", sep = "")))
	      df <- read.dbf(file.path(G$SE_Dir_GIS, paste(a, ".dbf", sep = "")))
	      
	      for (d in dlist) {
	        for (c in clist) {
	          for (m in mlist) {
	            for (y in ylist) {
	              if (length(ylist) > 1) {
	                if(y == ylist[1]) {
	                  incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y, "_", vlist[1]))
	                  img <- file.path(dir_path, paste(vlist[1], "_",  d, "_", c, "_", m, "_", y, ".grd", sep = ""))
	                  r <- raster(img)
	                  df1 <- extract(r, poly, fun = max, na.rm = FALSE, df = TRUE)
	                  #write to a data frame
	                  df1 <- data.frame(df1)
	                  df1[is.na(df1)] <- 0
	                  df <- cbind(df, df1[-1])
	                } else {
	                  for (v in vlist) {
	                    incProgress(1/tl, detail = paste("Doing part", d, "_", c, "_", m, "_", y, "_", v))
	                    img <- file.path(dir_path, paste(v, "_",  d, "_", c, "_", m, "_", y, ".grd", sep = ""))
	                    r <- raster(img)
	                    df1 <- extract(r, poly, fun = max, na.rm = FALSE, df=TRUE)
	                    #write to a data frame
	                    df1 <- data.frame(df1[-1])
	                    df1[is.na(df1)] <- 0
	                    df <- cbind(df, df1)
	                  }
	                }
	              } 
	            }
	          }
	        }
	      }
	      
	      #write to a CSV file
	      write.csv(df, file = file.path(dir_path, paste("VH_", a, ".csv", sep="")))
	      write.dbf(df, file.path(dir_path, paste(a, ".dbf", sep = "")))
	    }
	    
	  })
	})
	
	
	output$VH_AO_MI_Dir_Folder <- renderUI({
	  VH_AO_MI_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution"), full.names = FALSE, recursive = FALSE)
	  VH_AO_MI_Dir_Folder_selected <- VH_AO_MI_Dir_Folder_list[1]
	  selectInput("VH_AO_MI_Dir", "Working Species Distribution Folders",
	              choices = c(VH_AO_MI_Dir_Folder_list),
	              selected = VH_AO_MI_Dir_Folder_selected
	  )
	})
	
	output$VH_AO_MO_Dir_Folder <- renderUI({
	  VH_AO_MO_Dir_Folder_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Vulnerable_Habitat"), full.names = FALSE, recursive = FALSE)
	  VH_AO_MO_Dir_Folder_selected <- VH_AO_MO_Dir_Folder_list[1]
	  selectInput("VH_AO_MO_Dir", "Working Vulnerable Habitat Folders",
	              choices = c(VH_AO_MO_Dir_Folder_list),
	              selected = VH_AO_MO_Dir_Folder_selected
	  )
	})
	
	output$VH_AO_MI_Dir_Folder_Name <- renderUI({
	  VH_AO_MI_Dir_Folder_Name_list <- list.dirs(path = file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_AO_MI_Dir, input$VH_AO_Species[1]), full.names = FALSE, recursive = FALSE)
	  VH_AO_MI_Dir_Folder_Name_selected <- VH_AO_MI_Dir_Folder_Name_list[1]
	  selectInput("VH_AO_MI_Dir_Folder", "Working SDM Types",
	              choices = c(VH_AO_MI_Dir_Folder_Name_list),
	              selected = VH_AO_MI_Dir_Folder_Name_selected
	  )
	})		
	
	output$VH_AO_Species <- renderUI({
	    G$VH_AO_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_AO_MI_Dir)
		VH_Name_Species_list <- list.dirs(path = G$VH_AO_MI_Dir_Folder, full.names = FALSE, recursive = FALSE)
		VH_Name_Species_selected <- VH_Name_Species_list[1]
		selectInput("VH_AO_Species", "Select a species",
			choices = c(VH_Name_Species_list),
			selected = VH_Name_Species_selected
		)
	})
	
	
	output$VH_AO_SDM_model <- renderUI({
	    G$VH_AO_MI_Dir_Folder <- file.path(G$SE_Dir_Project, "Species_Distribution", input$VH_AO_MI_Dir)
		destfile <- file.path(G$VH_AO_MI_Dir_Folder, input$VH_AO_Species[1], "BIOMOD2", paste(as.name(paste(input$VH_AO_Species, "_ALL_eval.csv", sep = "")), sep = "", collapse = "--"))
		all_eval <- read.csv(destfile)
		G_FILE_species_evaluation <<- all_eval
		VH_Name_Models_list <- as.character(G_FILE_species_evaluation$Prediction)
		VH_Name_Models_selected <- VH_Name_Models_list[1]
		radioButtons("VH_AO_SDM_model", "Select models",
			choices = c(VH_Name_Models_list),
			selected = VH_Name_Models_selected
		)
	})
	
	output$VH_AO_SD_Map <- renderLeaflet({
	    
	    dir_path <- file.path(G$VH_AO_MI_Dir_Folder, input$VH_AO_Species, input$VH_AO_MI_Dir_Folder)
	    Map <- paste("PRED", "_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_Project_year, "_", input$VH_AO_Species, "_", input$VH_AO_SDM_model, ".grd", sep = "")
	    r <- raster(file.path(dir_path, Map))
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
	})  
	
	output$VH_AO_SD_Summary <- renderPrint({
	    dir_path <- file.path(G$VH_AO_MI_Dir_Folder, input$VH_AO_Species, "BIOMOD2")
	    Map <- paste("PRED", "_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_Project_year, "_", input$VH_AO_Species, "_", input$VH_AO_SDM_model, ".grd", sep = "")
	    r <- raster(file.path(dir_path, Map))
	    summary(r)
	})
	
	output$VH_AO_SD_Histogram <- renderPlot({
	    dir_path <- file.path(G$VH_AO_MI_Dir_Folder, input$VH_AO_Species, "BIOMOD2")
	    Map <- paste("PRED", "_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_Project_year, "_", input$VH_AO_Species, "_", input$VH_AO_SDM_model, ".grd", sep = "")
	    x <- raster(file.path(dir_path, Map))
	    
	    hist(x, # breaks = bins, 
	         col="orange",
	         border="brown",
	         xlab = "Predicted Value",
	         main = "Histogram")
	})	

	output$VH_AO_SR_Map <- renderLeaflet({
	   
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$VH_AO_Species
	  olist <- c("VH_SR")  # input$VH_AO_Output_option1
	  dlist <- input$VH_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$VH_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              r <- raster(file.path(G$VH_AO_MO_Dir_Folder, Map1))
	              #                plot(R_Map1, main = Map1)
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
	})
	
	output$VH_AO_SR_SIDO_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("VH_SR_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SR_SIDO_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("VH_SR_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 생물종 분포") + labs(x = "시도") + labs(y = "생물종수")
	})
	
	output$VH_AO_SR_SGG_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("VH_SR_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SR_SGG_UI <- renderUI({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  VH_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  VH_Name_SD_selected <- VH_Name_SD_list[1]
	  
	  selectInput("VH_AO_SR_SGG_UI", "시도",
	              choices = c(VH_Name_SD_list),
	              selected = VH_Name_SD_selected
	  )
	})
	
	output$VH_AO_SR_SGG_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$VH_AO_SR_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("VH_SR_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")

	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 생물종 분포") + labs(x = "시군구") + labs(y = "생물종수")
	})
	
	output$VH_AO_SR_NP_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("VH_SR_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SR_NP_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("VH_SR_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "국립공원 생물종 분포") + labs(x = "국립공원") + labs(y = "생물종수")
	})
	
	output$VH_AO_SR_BR_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[2])
	  V_NAME <- paste("VH_SR_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SR_BR_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  X_NAME <- names(df[3])
	  V_NAME <- paste("VH_SR_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "백두대간 생물종 분포") + labs(x = "백두대간") + labs(y = "생물종수")
	})
	
	output$VH_AO_SR_DMZ_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("VH_SR_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SR_DMZ_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("VH_SR_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")

	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "DMZ 생물종 분포") + labs(x = "DMZ") + labs(y = "생물종수")
	})
	
	output$VH_AO_SL_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$VH_AO_Species
	  olist <- c("VH_LOSS")  # input$VH_AO_Output_option1
	  dlist <- input$VH_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$VH_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              r <- raster(file.path(G$VH_AO_MO_Dir_Folder, Map1))
	              #                plot(R_Map1, main = Map1)
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
	})
	
	output$VH_AO_SL_SIDO_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("VH_LOSS_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SL_SIDO_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("VH_LOSS_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 생물종 분포") + labs(x = "시도") + labs(y = "생물종수")
	})
	
	output$VH_AO_SL_SGG_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("VH_LOSS_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SL_SGG_UI <- renderUI({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)  
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  VH_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  VH_Name_SD_selected <- VH_Name_SD_list[1]
	  
	  selectInput("VH_AO_SL_SGG_UI", "시도",
	              choices = c(VH_Name_SD_list),
	              selected = VH_Name_SD_selected
	  )
	})
	
	output$VH_AO_SL_SGG_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$VH_AO_SL_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("VH_LOSS_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 생물종 분포") + labs(x = "시군구") + labs(y = "생물종수")
	})
	
	output$VH_AO_SL_NP_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("VH_LOSS_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SL_NP_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("VH_LOSS_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "국립공원 생물종 분포") + labs(x = "국립공원") + labs(y = "생물종수")
	})
	
	output$VH_AO_SL_BR_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[2])
	  V_NAME <- paste("VH_LOSS_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SL_BR_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  X_NAME <- names(df[3])
	  V_NAME <- paste("VH_LOSS_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "백두대간 생물종 분포") + labs(x = "백두대간") + labs(y = "생물종수")
	})
	
	output$VH_AO_SL_DMZ_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("VH_LOSS_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SL_DMZ_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("VH_LOSS_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "DMZ 생물종 분포") + labs(x = "DMZ") + labs(y = "생물종수")
	})
	
	output$VH_AO_SS_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$VH_AO_Species
	  olist <- c("VH_STAY")  # input$VH_AO_Output_option1
	  dlist <- input$VH_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$VH_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              r <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              #                plot(R_Map1, main = Map1)
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
	})
	
	output$VH_AO_SS_SIDO_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("VH_STAY_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SS_SIDO_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("VH_STAY_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 생물종 분포") + labs(x = "시도") + labs(y = "생물종수")
	})
	
	output$VH_AO_SS_SGG_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("VH_STAY_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SS_SGG_UI <- renderUI({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  VH_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  VH_Name_SD_selected <- VH_Name_SD_list[1]
	  
	  selectInput("VH_AO_SS_SGG_UI", "시도",
	              choices = c(VH_Name_SD_list),
	              selected = VH_Name_SD_selected
	  )
	})
	
	output$VH_AO_SS_SGG_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$VH_AO_SS_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("VH_STAY_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 생물종 분포") + labs(x = "시군구") + labs(y = "생물종수")
	})
	
	output$VH_AO_SS_NP_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("VH_STAY_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SS_NP_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("VH_STAY_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "국립공원 생물종 분포") + labs(x = "국립공원") + labs(y = "생물종수")
	})
	
	output$VH_AO_SS_BR_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[2])
	  V_NAME <- paste("VH_STAY_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SS_BR_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  X_NAME <- names(df[3])
	  V_NAME <- paste("VH_STAY_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "백두대간 생물종 분포") + labs(x = "백두대간") + labs(y = "생물종수")
	})
	
	output$VH_AO_SS_DMZ_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("VH_STAY_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SS_DMZ_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("VH_STAY_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "DMZ 생물종 분포") + labs(x = "DMZ") + labs(y = "생물종수")
	})
	
	output$VH_AO_SI_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$VH_AO_Species
	  olist <- c("VH_GAIN")  # input$VH_AO_Output_option1
	  dlist <- input$VH_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$VH_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              r <- raster(file.path(G$VH_AO_MO_Dir_Folder, Map1))
	              #                plot(R_Map1, main = Map1)
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
	})
	
	output$VH_AO_SI_SIDO_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("VH_GAIN_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SI_SIDO_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("VH_GAIN_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 생물종 분포") + labs(x = "시도") + labs(y = "생물종수")
	})
	
	output$VH_AO_SI_SGG_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)  
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("VH_GAIN_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SI_SGG_UI <- renderUI({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  VH_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  VH_Name_SD_selected <- VH_Name_SD_list[1]
	  
	  selectInput("VH_AO_SI_SGG_UI", "시도",
	              choices = c(VH_Name_SD_list),
	              selected = VH_Name_SD_selected
	  )
	})
	
	output$VH_AO_SI_SGG_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$VH_AO_SI_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("VH_GAIN_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 생물종 분포") + labs(x = "시군구") + labs(y = "생물종수")
	})
	
	output$VH_AO_SI_NP_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("VH_GAIN_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SI_NP_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("VH_GAIN_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "국립공원 생물종 분포") + labs(x = "국립공원") + labs(y = "생물종수")
	})
	
	output$VH_AO_SI_BR_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[2])
	  V_NAME <- paste("VH_GAIN_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SI_BR_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  X_NAME <- names(df[3])
	  V_NAME <- paste("VH_GAIN_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "백두대간 생물종 분포") + labs(x = "백두대간") + labs(y = "생물종수")
	})
	
	output$VH_AO_SI_DMZ_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("VH_GAIN_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 2, 4, 6, 8, 10, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_SI_DMZ_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("VH_GAIN_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "DMZ 생물종 분포") + labs(x = "DMZ") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI1_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$VH_AO_Species
	  olist <- c("VH_VI1")  # input$VH_AO_Output_option1
	  dlist <- input$VH_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$VH_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              r <- raster(file.path(G$VH_AO_MO_Dir_Folder, Map1))
	              #                plot(R_Map1, main = Map1)
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
	})
	
	output$VH_AO_VI1_SIDO_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("VH_VI1_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(-10, -5, 0, 5, 10, 15, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI1_SIDO_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("VH_VI1_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 생물종 분포") + labs(x = "시도") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI1_SGG_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("VH_VI1_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(-10, -5, 0, 5, 10, 15, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI1_SGG_UI <- renderUI({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  VH_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  VH_Name_SD_selected <- VH_Name_SD_list[1]
	  
	  selectInput("VH_AO_VI1_SGG_UI", "시도",
	              choices = c(VH_Name_SD_list),
	              selected = VH_Name_SD_selected
	  )
	})
	
	output$VH_AO_VI1_SGG_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$VH_AO_VI1_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("VH_VI1_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 생물종 분포") + labs(x = "시군구") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI1_NP_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("VH_VI1_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(-10, -5, 0, 5, 10, 15, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI1_NP_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("VH_VI1_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "국립공원 생물종 분포") + labs(x = "국립공원") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI1_BR_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[2])
	  V_NAME <- paste("VH_VI1_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(-10, -5, 0, 5, 10, 15, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI1_BR_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  X_NAME <- names(df[3])
	  V_NAME <- paste("VH_VI1_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "백두대간 생물종 분포") + labs(x = "백두대간") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI1_DMZ_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("VH_VI1_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(-10, -5, 0, 5, 10, 15, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI1_DMZ_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("VH_VI1_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "DMZ 생물종 분포") + labs(x = "DMZ") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI2_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$VH_AO_Species
	  olist <- c("VH_VI2")  # input$VH_AO_Output_option1
	  dlist <- input$VH_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$VH_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              r <- raster(file.path(G$VH_AO_MO_Dir_Folder, Map1))
	              #                plot(R_Map1, main = Map1)
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
	})
	
	output$VH_AO_VI2_SIDO_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("VH_VI2_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI2_SIDO_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("VH_VI2_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 생물종 분포") + labs(x = "시도") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI2_SGG_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("VH_VI2_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI2_SGG_UI <- renderUI({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  VH_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  VH_Name_SD_selected <- VH_Name_SD_list[1]
	  
	  selectInput("VH_AO_VI2_SGG_UI", "시도",
	              choices = c(VH_Name_SD_list),
	              selected = VH_Name_SD_selected
	  )
	})
	
	output$VH_AO_VI2_SGG_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$VH_AO_VI2_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("VH_VI2_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 생물종 분포") + labs(x = "시군구") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI2_NP_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("VH_VI2_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI2_NP_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("VH_VI2_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "국립공원 생물종 분포") + labs(x = "국립공원") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI2_BR_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[2])
	  V_NAME <- paste("VH_VI2_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI2_BR_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  X_NAME <- names(df[3])
	  V_NAME <- paste("VH_VI2_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "백두대간 생물종 분포") + labs(x = "백두대간") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI2_DMZ_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("VH_VI2_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI2_DMZ_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("VH_VI2_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "DMZ 생물종 분포") + labs(x = "DMZ") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI3_Map <- renderLeaflet({
	  
	  # setting Climate change scenarios, Future time, Species and current environmental path
	  #    slist <- input$VH_AO_Species
	  olist <- c("VH_VI3")  # input$VH_AO_Output_option1
	  dlist <- input$VH_AO_Climate_model  # c("KMA") # c("KMA", "KEI", "WORLDCLIM")
	  clist <- input$VH_AO_Climate_scenario  # c("RCP4.5") # c("RCP4.5", "RCP8.5")
	  mlist <- input$VH_AO_SDM_model # c("PA1_Full_GLM_byROC")
	  ylist <- input$VH_AO_Project_year
	  #	dtlist <- input$SS_AO_Dispersal_type
	  
	  #    ls <- length(slist)
	  lo <- length(olist)
	  ld <- length(dlist)
	  lc <- length(clist)
	  lm <- length(mlist)
	  ly <- length(ylist)
	  
	  tl <- lo * ld * lc * lm * ly
	  nc <- 2
	  if (tl <  2) {
	    nr <- round(tl / nc) + 1
	  } else {
	    nr <- round((tl + 0.1) / nc)
	  }
	  
	  #    par(mfrow = c(nr,nc), cex.main = 1.2)
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  for (o in olist) {
	    for (d in dlist) {
	      for (c in clist) {
	        for (m in mlist) {
	          for (y in ylist) {
	            if (ly > 0) {
	              Map1 <- paste(o, "_", d, "_", c, "_", m, "_", y, ".grd", sep = "")
	              #                R_Map1 <- raster(file.path(isolate(G$VH_AO_MO_Dir_Folder), Map1))
	              r <- raster(file.path(G$VH_AO_MO_Dir_Folder, Map1))
	              #                plot(R_Map1, main = Map1)
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
	})
	
	output$VH_AO_VI3_SIDO_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SD", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  names(poly) <- c(names(x)[-1])
	  X_NAME <- names(poly[4])
	  V_NAME <- paste("VH_VI3_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.4, 0.8, 1.2, 1.6, 2.0, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI3_SIDO_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SD", ".csv", sep = "")))
	  X_NAME <- names(df[5])
	  V_NAME <- paste("VH_VI3_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시도 생물종 분포") + labs(x = "시도") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI3_SGG_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("SGG", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[7])
	  V_NAME <- paste("VH_VI3_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.4, 0.8, 1.2, 1.6, 2.0, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = 7)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI3_SGG_UI <- renderUI({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  VH_Name_SD_list <- c("강원도", "경기도", "경상남도", "경상북도", "광주시",  "대구시",  "대전시",  "부산시",  "서울시",  "세종시",  "울산시",  "인천시",  "전라남도", "전라북도", "제주도",  "충청남도", "충청북도") # unique(df$SD_KOR)
	  VH_Name_SD_selected <- VH_Name_SD_list[1]
	  
	  selectInput("VH_AO_VI3_SGG_UI", "시도",
	              choices = c(VH_Name_SD_list),
	              selected = VH_Name_SD_selected
	  )
	})
	
	output$VH_AO_VI3_SGG_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_SGG", ".csv", sep = "")))
	  df <- df[which(df$SD_KOR==input$VH_AO_VI3_SGG_UI), ]
	  X_NAME <- names(df[8])
	  V_NAME <- paste("VH_VI3_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "시군구 생물종 분포") + labs(x = "시군구") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI3_NP_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("NP", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[3])
	  V_NAME <- paste("VH_VI3_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.4, 0.8, 1.2, 1.6, 2.0, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI3_NP_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_NP", ".csv", sep = "")))
	  X_NAME <- names(df[4])
	  V_NAME <- paste("VH_VI3_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "국립공원 생물종 분포") + labs(x = "국립공원") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI3_BR_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("BR", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[2])
	  V_NAME <- paste("VH_VI3_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.4, 0.8, 1.2, 1.6, 2.0, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI3_BR_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_BR", ".csv", sep = "")))
	  X_NAME <- names(df[3])
	  V_NAME <- paste("VH_VI3_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "백두대간 생물종 분포") + labs(x = "백두대간") + labs(y = "생물종수")
	})
	
	output$VH_AO_VI3_DMZ_Map <- renderLeaflet({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  poly <- readOGR(file.path(G$VH_AO_MO_Dir_Folder, paste("DMZ", ".shp", sep = "")))
	  x <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  names(poly) <- c(names(x[-1]))
	  X_NAME <- names(poly[5])
	  V_NAME <- paste("VH_VI3_", input$VH_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep = "")
	  
	  bins <- c(0, 0.4, 0.8, 1.2, 1.6, 2.0, Inf)
	  pal <- colorBin("YlOrRd", domain = poly[[V_NAME]], bins = bins)
	  
	  labels <- sprintf(
	    "<strong>%s</strong><br/>%g Species", # people / mi<sup>2</sup>",
	    poly[[X_NAME]], poly[[V_NAME]]
	  ) %>% lapply(htmltools::HTML)
	  
	  leaflet(poly) %>%
	    setView(lng = 128.00, lat = 36.00, zoom = 7) %>%
	    addProviderTiles("MapBox", options = providerTileOptions(
	      id = "mapbox.light",
	      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
	    addTiles(
	      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
	      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	    ) %>%   
	    addPolygons(
	      fillColor = ~pal(poly[[V_NAME]]),
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
	    addLegend(pal = pal, values = ~poly[[V_NAME]], opacity = 0.7, title = NULL,
	              position = "bottomright")
	})
	
	output$VH_AO_VI3_DMZ_Stat <- renderPlot({
	  G$VH_AO_MO_Dir_Folder <- file.path(G$SE_Dir_Project, "Vulnerable_Habitat", input$VH_AO_MO_Dir)
	  df <- read.csv(file.path(G$VH_AO_MO_Dir_Folder, paste("VH_DMZ", ".csv", sep = "")))
	  X_NAME <- names(df[6])
	  V_NAME <- paste("VH_VI3_", input$IS_AO_Climate_model, "_", input$VH_AO_Climate_scenario, "_", input$VH_AO_SDM_model, "_", input$VH_AO_Project_year, sep="")
	  
	  ggplot(data=df, aes(x=df[[X_NAME]], y=df[[V_NAME]])) + #, fill=df[[X_NAME]])) +
	    geom_bar(stat="identity", position=position_dodge()) +
	    geom_text(aes(label=df[[V_NAME]]), vjust=1.6, color="white",
	              position = position_dodge(0.9), size=3.5) +
	    scale_fill_brewer(palette="Paired") +
	    theme_minimal() +
	    labs(title = "DMZ 생물종 분포") + labs(x = "DMZ") + labs(y = "생물종수")
	})

	
})