### JD Edition
library(shinythemes)
#ST_Name <- "success"


shinyUI(

  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = tags$a(tags$img(src="MOTIVE_Ecosystem.png", height=30, width=200),)
#      tags$li(class = "dropdown", 
#              style = "padding: 10px 1200px 0px 0px;",
#              tags$p("foo"))
      
#      title = "MOTIVE Ecosystem",
#      img(src="MOTIVE.png", height=25, width=125),
#      img(src="Ecosystem.png", height=50, width=160, style="margin-top:7px;")
      
      ),
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem(SE_NAME_Project_Finished, 
          tags$hr(),                 
          uiOutput("SE_Project"),
          tags$hr(),
          tags$p(SE_NAME_Project_Manager),
          verbatimTextOutput("SE_Project_Info_Manager"),
          tags$p(SE_NAME_Project_Institute),
          verbatimTextOutput("SE_Project_Info_Institute"),
          tags$p(SE_NAME_Project_Date),
          verbatimTextOutput("SE_Project_Info_Date"),
          tags$p(SE_NAME_Project_Path),
          verbatimTextOutput("SE_Project_Info_Path"),
          tags$p(SE_NAME_Project_Description),
          verbatimTextOutput("SE_Project_Info_Description"),
          tags$hr()
          ),
        menuItem(SE_NAME_Project_Working,
          tags$hr(),
          shinyDirButton("SE_Dir_Project", SE_NAME_Project_Path, SE_NAME_Project_Path),
#          verbatimTextOutput("SE_Project_New_Path"),
          uiOutput("SE_Project_New_Path"),
          uiOutput("SE_Project_New_Info"),
          uiOutput("SE_Project_New_Name"),
          uiOutput("SE_Project_New_Manager"),
          uiOutput("SE_Project_New_Institute"),
          uiOutput("SE_Project_New_Date"),
          uiOutput("SE_Project_New_Description"),
          br(),
          actionButton("SE_Dir_Project_Action", label = SE_Name_WE_Project_Action),
          tags$hr()
          ),
        menuItem(SE_NAME_Project_Data,
          tags$hr(),
          shinyDirButton("SE_Dir_Climate", SE_Name_DE_Climate, SE_Name_DE_Climate),
          verbatimTextOutput("SE_Dir_Climate"),
                 
          shinyDirButton("SE_Dir_Link", SE_Name_DE_Link, SE_Name_DE_Link),
          verbatimTextOutput("SE_Dir_Link"),
                 
          shinyDirButton("SE_Dir_GIS", SE_Name_DE_GIS, SE_Name_DE_GIS),
          verbatimTextOutput("SE_Dir_GIS"),
                 
          shinyDirButton("SE_Dir_Species", SE_Name_DE_Species, SE_Name_DE_Species),
          verbatimTextOutput("SE_Dir_Species"),
          tags$hr(),
          uiOutput("SE_speciesindex"),
          uiOutput("SE_specieslocation"),
          tags$hr()
        )
      )
    ),
    
    dashboardBody(
      shinyjs::useShinyjs(),
      
      fluidPage( div(
#        h4(SE_Name_System, style = "display: inline-block; color: white; font-size: 200%; margin-left: 20px; position: absolute; line-height: 8vh;"), 
        div( style = "display: inline-block; margin-left: 80%; margin-top: 10px; font-size: 110%; color: white;",
                  
#             a("   한국어", style = "cursor:pointer; margin-right: 5px; color: white;" ),
#             a("English", style = "cursor:pointer; margin-right: 5px; color: white; " ),
#             a("CONTACT US", style = "cursor:pointer; margin-right: 5px; color: white; " ),
#             a("LOGOUT", style = "cursor:pointer; margin-right: 5px; color: white;" )
            a(SE_Name_System_institute, style = "cursor:pointer; margin-right: 5px; color: azure;" )

        ),
#        style = "background-image: url(eco_title.png); height: 10vh; position: relative;"),


#        img(src="MOTIVE.png", height=25, width=125),
#        img(src="Ecosystem.png", height=50, width=160, style="margin-top:7px;"),
#        style = "background:#17366e; color:#fff; height:70px; width:120%; margin-left:-30px; margin-top:-15px; padding-left:30px; padding-top:12px;"
#        ),
        style = "background-image: url(eco_title.png); height: 7vh; position: center;"
        ),

        tags$script(HTML(
          'document.querySelector("body").classList.add("sidebar-collapse");'
        )),


tags$head(tags$style(HTML(
  '.myClass { 
        font-size: 15px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),

tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Climate Change Impact and Vulnerability Assessment for Ecosystem </span>\');
      })
     ')),
        
                
        theme = shinytheme("yeti"),
        # shinythemes::themeSelector(),
        
        tags$head(tags$style(HTML('
        
          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          background-color: rgb(0,0,0);
          color: rgb(255,255,255);font-weight: bold;font-size: 18px;
          }  

          #SE_Dir_Project {
          display: inline;
          }
                                  
          #SDM_MO_SDM_run {
          width: 200px;
          height: 70px;
          font-size: 3rem;
          }
                                  
          #DM_MO_Action {
          width: 200px;
          height: 70px;
          }
                                  
          #CM_UI, #CS_UI, #PY_UI, #SYNC_UI {
          display: inline-block;
          }
                                  
          #CM_btn, #CS_btn, #PY_btn {
          color: #fff;
          background-color: #0080ff;
          }
                                  
          #SS_Analy_Box > div > div {
          padding-left: 0;
          padding-right: 15px;
          }
                                  
          #SS_Analy_Box > div > div:first-child,
          #SS_Analy_Box > div > div:last-child {
          width: 20%;
          }
                                  
          #SS_Analy_Box > div > div:nth-child(2),
          #SS_Analy_Box > div > div:nth-child(3) {
          width: 13%;
          }
                                  
          #SS_Analy_Box > div > div:nth-child(4) {
          width: 15%;
          }
                                  
          #SS_Analy_Box > div > div:nth-child(5) {
          width: 18%;
          }
                                  
          #CD_Summary, #CD_Histogram {
          display: inline-block;
          width: 40%;
          }
                                  
          ##CD_Histogram {
          width: 40%;
          }
                                  
          .fa-folder-open {
          color: #3498db;
          }
                                  
          .fa-pie-chart {
          color: #9b59b6;
          }
                                  
          .fa-table {
          color: #e74c3c;
          }
                                  
          .container-fluid > .tabbable > .nav-tabs {
          font-weight: bold;
          }
          '))),

#	fluidPage(h4(SE_Name_System),
#	tags$hr(),
  setBackgroundColor("ghostwhite"),

#	tabsetPanel(
	  navbarPage(NULL, theme = shinytheme("flatly"),
		tabPanel(SE_Name,
#      tabsetPanel(
        tabBox(title = NULL, width = 12,
        tabPanel(SE_Name_Dir_Project_SDM,
#          tags$hr(),
				  sidebarLayout(
				    sidebarPanel(width = 3, Fluid = TRUE,
				      uiOutput("SE_Dir_Project_SDM"),
				      uiOutput("SE_Dir_Project_SDM_Species"),
              uiOutput("SE_Dir_Project_SDM_Species_Model"),
              br()
				    ),
            mainPanel(
				      tabsetPanel(
#				      navbarPage(NULL, theme = shinytheme("flatly"),
#              tabBox(title = NULL, width = 12,           
                tabPanel(SE_Name_Dir_Project_SDM_OPTION, icon = icon("list-ul"), 
                  tags$br(),
                  tableOutput("SE_Dir_Project_SDM_Species_Model_Options")
                ),
                tabPanel(SE_Name_Dir_Project_SDM_OUTPUT, icon = icon("file"), 
                  tags$br(),
                  textOutput("SE_Dir_Project_SDM_Species_Model_Output")
                )
              )
            )
				  )
        ),
        tabPanel(SE_Name_Dir_Project_HA,
#          tags$hr(),
          sidebarLayout(
            sidebarPanel(width = 3, Fluid = TRUE,                     
				      uiOutput("SE_Dir_Project_HA"),
				      br()
            ),
				    mainPanel(
#				      tabsetPanel(
#				      navbarPage(NULL, theme = shinytheme("flatly"),
				      tabsetPanel(          
				        tabPanel(SE_Name_Dir_Project_HA_OPTION, icon = icon("list-ul"),
				          tags$br(),
				          tableOutput("SE_Dir_Project_HA_Species_Model_Options")
				        ),
				        tabPanel(SE_Name_Dir_Project_HA_OUTPUT, icon = icon("file"),
				          tags$br(),
				          textOutput("SE_Dir_Project_HA_Species_Model_Output")
				        )
				      )
				    )
          )
        )
      )
		),       

		tabPanel(SP_Name,
				tabsetPanel(
#		    tabBox(title = NULL, width = 12,
				tabPanel(SP_Name_Info, icon = icon("tree"),
					tags$head(
						# Include our custom CSS
						includeCSS("styles.css"),
						includeScript("gomap.js")
					),
					tags$br(),
					fluidRow(
						column(6, DT::dataTableOutput("SP_Info"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
						column(6, leafletOutput("SP_Map", width = "500", height = "600"))
					)
				),
				tabPanel(SP_Name_Location, icon = icon("globe-asia"),
					tags$head(
						# Include our custom CSS
						includeCSS("styles.css"),
						includeScript("gomap.js")
					),
					tags$br(),
					column(6, DT::dataTableOutput("SP_LOC_Info"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
					column(6, leafletOutput("SP_LOC_Map", width = "500", height = "600"))
				)             
			)
		),  

		tabPanel(LD_Name, fluid = TRUE,
#			tags$hr(),
			sidebarLayout(
				sidebarPanel(width = 3, Fluid = TRUE,
					selectInput("LD_Variables", LD_Name_Variables,
						choices = LD_Name_Variables_list,
						selected = LD_Name_Variables_selected),

					# Input: Checkbox if file has header ----
					radioButtons("LD_Climate_model", LD_Name_Models,
						choices = LD_Name_Models_list,
						selected = LD_Name_Models_selected),

					# Input: Checkbox if file has header ----
					radioButtons("LD_Climate_scenario", LD_Name_Scenarios,
						choices = LD_Name_Scenarios_list,
						selected = LD_Name_Scenarios_selected),
					
					# Input: Checkbox if file has header ----
					radioButtons("LD_Project_year", LD_Name_Year,
						choices = LD_Name_Year_list,
						selected = LD_Name_Year_selected),
					
					# Input: Checkbox if file has header ----
					tags$br(),
					checkboxGroupInput("LD_MO_Barrier_LanduseType", DM_Name_DM_MO_Barrier_LanduseType,
					                   choices = c(DM_Name_DM_MO_Barrier_LanduseType_list),
					                   selected = as.integer(DM_Name_DM_MO_Barrier_LanduseType_selected)
					),
					sliderInput("LD_MO_Barrier_Forestfire_Cutoff", label = LD_Name_Cutoff_Forestfire, min = 0.01, 
					            max = 1.0, step = 0.01, value = 0.955),
					sliderInput("LD_MO_Barrier_Landslide_Cutoff", label = LD_Name_Cutoff_Landslide, min = 0.01, 
					            max = 1.0, step = 0.01, value = 0.85)
				),

				# Main panel for displaying outputs ----
				mainPanel(
					tabsetPanel(
#					tabBox(title = NULL, width = 12,  
						tabPanel(LD_Name_Map, icon = icon("layer-group"),
							tags$head(
							# Include our custom CSS
							includeCSS("styles.css"),
							includeScript("gomap.js")
							),
							tags$br(),
							column(6, leafletOutput("LD_Map", width = "800", height = "650"))
						),
						tabPanel(LD_Name_Summary, icon = icon("list-alt"),
							tags$br(),
							tags$br(),
							tags$br(),
#							column(10, verbatimTextOutput("LD_Summary")),
							column(10, plotOutput("LD_Histogram"))
						),
						tabPanel(LD_Name_Map_Landuse, icon = icon("layer-group"), 
						         tags$head(
						             # Include our custom CSS
						             includeCSS("styles.css"),
						             includeScript("gomap.js")
						         ),
						  tags$br(),
						  column(6, leafletOutput("LD_Map_Landuse", width = "800", height = "650"))
						),
						tabPanel(LD_Name_Map_Forestfire, icon = icon("layer-group"), 
						         tags$head(
						             # Include our custom CSS
						             includeCSS("styles.css"),
						             includeScript("gomap.js")
						         ),
						  tags$br(),
						  column(6, leafletOutput("LD_Map_Forestfire", width = "800", height = "650"))
						),
						tabPanel(LD_Name_Map_Landslide, icon = icon("layer-group"), 
						         tags$head(
						             # Include our custom CSS
						             includeCSS("styles.css"),
						             includeScript("gomap.js")
						         ),
						  tags$br(),
						  column(6, leafletOutput("LD_Map_Landslide", width = "800", height = "650"))
						)
					)
				)
			)
		),  

		tabPanel(CD_Name, fluid = TRUE,
#			tags$hr(),
      sidebarLayout(
				sidebarPanel(width = 3, Fluid = TRUE,
          uiOutput("CD_Variables_select"),

					# Input: Checkbox if file has header ----
					radioButtons("CD_Climate_model", CD_Name_Models,
						choices = CD_Name_Models_list,
						selected = CD_Name_Models_selected),
	
					# Input: Checkbox if file has header ----
					radioButtons("CD_Climate_scenario", CD_Name_Scenarios,
						choices = CD_Name_Scenarios_list,
						selected = CD_Name_Scenarios_selected),
	
					# Input: Checkbox if file has header ----
					radioButtons("CD_Project_year", CD_Name_Year,
						choices = CD_Name_Year_list,
						selected = CD_Name_Year_selected)
				),
	
				# Main panel for displaying outputs ----
				mainPanel(
					tabsetPanel(
						tabPanel(CD_Name_Map, icon = icon("layer-group"),
							tags$head(
								# Include our custom CSS
								includeCSS("styles.css"),
								includeScript("gomap.js")
							),
							tags$br(),
							column(6, leafletOutput("CD_Map", width = "800", height = "650"))
						),
						tabPanel(CD_Name_Summary, icon = icon("list-alt"),
						  tags$br(),
						  tags$br(),
						  tags$br(),
#							column(10, verbatimTextOutput("CD_Summary")),
							column(10, plotOutput("CD_Histogram"))
						)
					)
				)
            )
        ),  
          
		tabPanel(SDM_Name,
#			tabsetPanel(
		  tabBox(title = NULL, width = 12, 
				tabPanel(SDM_Name_Model,
					tabsetPanel(
						tabPanel(SDM_Name_Model_Species, icon = icon("tree"),
						  tags$br(),
							fluidRow(
								column(6, DT::dataTableOutput("SDM_SP_Info"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
								column(4, verbatimTextOutput("SDM_SP_Selection"))
							)
						),
						tabPanel(SDM_Name_Model_Variable, icon = icon("list-ul"),
							tags$br(),
							fluidRow(
								# Sidebar panel for inputs ----
								sidebarPanel(width = 3,
									# Input: Checkbox if file has header ----
									checkboxGroupInput("SDM_MO_Climate_model", SDM_Name_CD_Models,
										choices = c(SDM_Name_CD_Models_list),
										selected = SDM_Name_CD_Models_selected),

									# Input: Checkbox if file has header ----
									checkboxGroupInput("SDM_MO_Climate_scenario", SDM_Name_CD_Scenarios,
										choices = c(SDM_Name_CD_Scenarios_list),
										selected = SDM_Name_CD_Scenarios_selected),

									# Input: Checkbox if file has header ----
									checkboxGroupInput("SDM_MO_Project_year", SDM_Name_CD_Year,
										choices = c(SDM_Name_CD_Year_list),
										selected = SDM_Name_CD_Year_selected)
								),	
								sidebarPanel(width = 3,
                  tags$p(SDM_Name_Model_Variable_Option),
                  checkboxGroupInput("SDM_MO_AO_Variables", NULL, # SDM_Name_MO_AO_Variables,
                                     choices = c(SDM_Name_MO_AO_Variables_list),
                                     selected = SDM_Name_MO_AO_Variables_selected),
                  useShinyalert(),  # Set up shinyalert
                  actionButton("SDM_MO_AO_Variables_Create", label = SDM_Name_MO_Variables_Create, icon = icon("layer-group")),
                  tags$hr(),
                  tags$p(SDM_Name_Model_Variable_Distribution),
								  uiOutput("SDM_AO_MI_Dir_Folder"),
								  uiOutput("SDM_AO_MI_Dir_Folder_Name"),
								  uiOutput("SDM_AO_Species"),
								  uiOutput("SDM_AO_SDM_PROJ_model"),
								  tags$hr(),
								  tags$p(SDM_Name_Model_Variable_SpeciesRichness),
								  uiOutput("SDM_HA_AO_MO_Dir_Folder"),
								  uiOutput("SDM_HA_AO_SDM_PRED_model")
								),
								sidebarPanel(width = 3,
                  tags$p(SDM_Name_Model_Variable_Data),
                  uiOutput("SDM_MO_Variables_Select")
								)
							)
						),
						tabPanel(SDM_Name_Model_SDM, icon = icon("list-ul"),  
							tags$br(),
							fluidRow(
								# Sidebar panel for inputs ----
								sidebarPanel(width = 3,
                  tags$p(SDM_Name_Model_SDM_Data),
#                  uiOutput("DM_MO_DM_envChgSteps"),
                  selectInput("BIOMOD_eval.resp.var", "BIOMOD_eval.resp.var",
                    choices = NULL,
                    selected = NULL),
                  selectInput("BIOMOD_eval.expl.var", "BIOMOD_eval.expl.var",
                    choices = NULL,
                    selected = NULL),
                  selectInput("BIOMOD_eval.resp.xy", "BIOMOD_eval.resp.xy",
                    choices = NULL,
                    selected = NULL),
                  sliderInput("BIOMOD_PA.nb.rep", label = "BIOMOD_PA.nb.rep", min = 0, 
                    max = 10, step = 1, value = 1),
                  sliderInput("BIOMOD_PA.nb.absences", label = "BIOMOD_PA.nb.absences", min = 0, 
                    max = 10000, step = 1000, value = 1000),
                  selectInput("BIOMOD_PA.strategy", "BIOMOD_PA.strategy",
                    choices = c("random", "sre", "disk", "user.defined"),
                    selected = "random"),
                  sliderInput("BIOMOD_PA.dist", label = "BIOMOD_PA.dist Range (m) for PA disk option", min = 10000, 
                    max = 200000, step = 10000, value = c(10000, 100000)),
                  sliderInput("BIOMOD_PA.sre.quant", label = "BIOMOD_PA.sre.quant", min = 0, 
                    max = 1, step = 0.025, value = 0.025),
                  selectInput("BIOMOD_PA.table", "BIOMOD_PA.table for PA sre option",
                    choices = NULL,
                    selected = NULL),
                  checkboxInput("BIOMOD_na.rm", "BIOMOD_na.rm", TRUE)
                ),

								sidebarPanel(width = 3,
								  tags$p(SDM_Name_Model_SDM_Modeling),
								  sliderInput("BIOMOD_NbRunEval", label = "BIOMOD_NbRunEval", min = 0, 
                    max = 10, step = 1, value = 1),
                  sliderInput("BIOMOD_DataSplit", label = "BIOMOD_DataSplit", min = 0, 
                    max = 100, step = 10, value = 100),
								  selectInput("BIOMOD_Yweights", "BIOMOD_Yweights",
                    choices = NULL,
                    selected = NULL),
								  sliderInput("BIOMOD_VarImport", label = "BIOMOD_VarImport", min = 0, 
                    max = 10, step = 1, value = 5),
								  checkboxGroupInput("BIOMOD_models.eval.meth", "BIOMOD_models.eval.meth",
                    choices = c("ROC", "TSS", "KAPPA"),  #, "FAR", "SR", "ACCURANCY", "BIAS", "POD", "CSI", "ETS"),
                    selected = "ROC"),
								  br(),
								  checkboxInput("BIOMOD_SaveObj", "BIOMOD_SaveObj", TRUE),
								  checkboxInput("BIOMOD_rescal.all.models", "BIOMOD_rescal.all.models", TRUE),
								  checkboxInput("BIOMOD_do.full.models", "BIOMOD_do.full.models", TRUE),
								  tags$br(),
								  tags$p(SDM_Name_Model_SDM_Projection),
								  selectInput("BIOMOD_selected.models", "BIOMOD_selected.models",
								               choices = "all",
								               selected = "all"),
								  checkboxGroupInput("BIOMOD_binary.meth", "BIOMOD_binary.meth",
								                     choices = c("ROC", "TSS", "KAPPA"),  #, "FAR", "SR", "ACCURANCY", "BIAS", "POD", "CSI", "ETS"),
								                     selected = "ROC"),
								  br(),
								  checkboxInput("BIOMOD_compress", "BIOMOD_compress", FALSE),
								  checkboxInput("BIOMOD_build.clamping.mask", "BIOMOD_build.clamping.mask", FALSE),
								  selectInput("BIOMOD_output.format", "BIOMOD_output.format",
								              choices = ".img",
								              selected = ".img"),
								  checkboxInput("BIOMOD_do.stack", "BIOMOD_do.stack", TRUE)
								             
								),
								sidebarPanel(width = 3,
								  tags$p(SDM_Name_Model_SDM_Ensemble),
                  selectInput("EM_chosen.models", "EM_chosen.models",
                    choices = "all",
                    selected = "all"),
                  selectInput("EM_em.by", "EM_em.by",
                    choices = c("PA_dataset+repet", "PA_dataset+algo", "PA_dataset", "algo", "all"),
                    selected = "PA_dataset+repet"),
                  selectInput("EM_eval.metric", "EM_eval.metric",
                    choices = "all",
                    selected = "all"),
                  selectInput("EM_eval.metric.quality.threshold", "EM_eval.metric.quality.threshold",
                              choices = NULL,
                              selected = NULL),
                  checkboxGroupInput("EM_models.eval.meth", "EM_models.eval.meth",
                                     choices = c("ROC", "TSS", "KAPPA"),  #, "FAR", "SR", "ACCURANCY", "BIAS", "POD", "CSI", "ETS"),
                                     selected = "ROC"),
                  br(),
                  checkboxInput("EM_prob.mean", "EM_prob.mean", TRUE),
                  checkboxInput("EM_prob.cv", "EM_prob.cv", FALSE),
                  checkboxInput("EM_prob.ci", "EM_prob.ci", FALSE),
                  sliderInput("EM_prob.ci.alpha", label = "EM_prob.ci.alpha", min = 0, 
                              max = 1, step = 0.05, value = 0.05),
                  checkboxInput("EM_prob.median", "EM_prob.median", FALSE),
                  checkboxInput("EM_committee.averaging", "EM_committee.averaging", FALSE),
                  checkboxInput("EM_prob.mean.weight", "EM_prob.mean.weight", TRUE),
                  selectInput("EM_prob.mean.weight.decay", "EM_prob.mean.weight.decay",
                              choices = "proportional",
                              selected = "proportional"),
                  sliderInput("EM_VarImport", label = "EM_VarImport", min = 0, 
                              max = 10, step = 1, value = 0)
								),
								sidebarPanel(width = 3,
									checkboxGroupInput("SDM_MO_SDM_model", SDM_Name_models,
										choices = c(SDM_Name_models_list),
										selected = c(SDM_Name_models_selected)
									),
									tags$br(), 
									checkboxInput("SDM_MO_SDM_EMmodel", label = SDM_Name_EMmodels, value = FALSE),
									tags$br(),
									tags$br(),
									tags$br(),
									shinyDirButton("SDM_MO_Dir_Folder", SDM_Name_Dir, NULL, icon = icon("folder-open")),
									verbatimTextOutput("SDM_MO_Dir_Folder", placeholder = TRUE),
									tags$br(),
									useShinyalert(),  # Set up shinyalert
									actionButton("SDM_MO_SDM_run", label = SDM_Name_models_run, icon = icon("running"))
								)
							)
						)
					)
				),

				tabPanel(SDM_Name_Model_Out, fluid = TRUE,
#					tags$hr(),
					sidebarLayout(
						sidebarPanel(width = 3, Fluid = TRUE,
						  uiOutput("SDM_AO_Dir_Folder"),
							uiOutput("SDM_OU_Species"),
							tags$br(),

							uiOutput("SDM_OU_Projection_model"),
							tags$br(),

							uiOutput("SDM_OU_Prediction_model"),
							tags$hr(),

							# Input: Checkbox if file has header ----
							radioButtons("SDM_OU_Climate_model", SDM_Name_CD_Models_out,
								choices = c(SDM_Name_CD_Models_out_list),
								selected = SDM_Name_CD_Models_out_selected),

							# Input: Checkbox if file has header ----
							radioButtons("SDM_OU_Climate_scenario", SDM_Name_CD_Scenarios_out,
								choices = c(SDM_Name_CD_Scenarios_out_list),
								selected = SDM_Name_CD_Scenarios_out_selected),

							# Input: Checkbox if file has header ----
							radioButtons("SDM_OU_Project_year", SDM_Name_CD_Year_out,
								choices = c(SDM_Name_CD_Year_out_list),
								selected = SDM_Name_CD_Year_out_selected)
						),

                        # Main panel for displaying outputs ----
						mainPanel(
							tabsetPanel(
								tabPanel(SDM_Name_Model_Out_Validation, icon = icon("file-signature"),
									tags$br(),
									fluidRow(
										column(8, DT::dataTableOutput("SDM_OU_Validation"), style = "overflow-y: scroll;overflow-x: scroll;")
									),
									tags$br(),
                  br(),
									fluidRow(
										column(8, plotOutput("SDM_OU_Validation_BoxPlot"))
									),
									tags$br(),
									br(),
									fluidRow(
									  column(8, plotOutput("SDM_OU_Validation_Cutoff_BoxPlot"))
									)
								),
								tabPanel(SDM_Name_Model_Out_Contribution, icon = icon("file-signature"),
									tags$br(),
									fluidRow(
										column(8, DT::dataTableOutput("SDM_OU_Contribution"), style = "overflow-y: scroll;overflow-x: scroll;")
									),
									tags$br(),
									br(),
									fluidRow(
										column(8,chartJSRadarOutput("SDM_OU_Contribution_Radarchart", width = "450", height = "300"))
									)
								),
								tabPanel(SDM_Name_Model_Out_Probability, icon = icon("layer-group"), 
									tags$head(
										# Include our custom CSS
										includeCSS("styles.css"),
										includeScript("gomap.js")
									),
									tags$br(),
									leafletOutput("SDM_OU_Probability_map", width = "800", height = "600"),
									tags$hr(),
#									column(10, verbatimTextOutput("SDM_OU_PROJ_Summary")),
									column(9, plotOutput("SDM_OU_PROJ_Histogram"))
								),
								tabPanel(SDM_Name_Model_Out_Prediction, icon = icon("layer-group"), 
									tags$head(
										# Include our custom CSS
										includeCSS("styles.css"),
										includeScript("gomap.js")
									),
									tags$br(),
									leafletOutput("SDM_OU_Predicted_map", width = "800", height = "600"),
									tags$hr(),
#									column(10, verbatimTextOutput("SDM_OU_PRED_Summary")),
									column(9, plotOutput("SDM_OU_PRED_Histogram"))
								)
							)
						)
					)
				)
			)
		),  
	
		tabPanel(SRM_Name,
		         #			tabsetPanel(
		         tabBox(title = NULL, width = 12,
		           tabPanel(SRM_Name_Model, fluid = TRUE,
		             tabsetPanel(
		               tabPanel(SRM_Name_Model_SDM, icon = icon("list-ul"),
		                  tags$br(),      
                        fluidRow(
                        sidebarPanel(width = 3, Fluid = TRUE,
                            uiOutput("SRM_SDM_Dir_Folder"),
                            tags$br(),
                            actionButton("SRM_MO_Species_sel_all", label = "Select All", icon = icon("check-circle")),
                            actionButton("SRM_MO_Species_sel_none", label = "Unselect All", icon = icon("circle")),
                            uiOutput("SRM_MO_Species")
		                  ),
                        sidebarPanel(width = 3, Fluid = TRUE,             
                            # Input: Checkbox if file has header ----
                            radioButtons("SRM_MO_Climate_model", SRM_Name_CD_Models,
                                choices = c(SRM_Name_CD_Models_list),
                                selected = SRM_Name_CD_Models_selected),
                            # Input: Checkbox if file has header ----
                            radioButtons("SRM_MO_Climate_scenario", SRM_Name_CD_Scenarios,
                                choices = c(SRM_Name_CD_Scenarios_list),
                                selected = SRM_Name_CD_Scenarios_selected),
                            # Input: Checkbox if file has header ----
                            radioButtons("SRM_MO_Project_year", SRM_Name_CD_Year,
                                choices = c(SRM_Name_CD_Year_list),
                                selected = SRM_Name_CD_Year_selected)
                          ),
                        sidebarPanel(width = 3, Fluid = TRUE, 
                            uiOutput("SRM_MO_SDM_model")
                        )
		                )
		              ),
                      tabPanel(SRM_Name_Model_SRM, icon = icon("list-ul"),
                        tags$br(),      
                        fluidRow(
                        sidebarPanel(width = 3,
                            # Input: Checkbox if file has header ----
                            radioButtons("SRM_MO_Type", SRM_Name_MO_Type,
                                        choices = c(SRM_Name_MO_Type_list),
                                        selected = SRM_Name_MO_Type_selected)
                        ),
                        sidebarPanel(width = 3,
                            # Input: Checkbox if file has header ----
                            tags$p("Buffer:"),
                            sliderInput("SRM_Buffer_Distance", label = "Bufffer Distance", min = 1000, 
                                        max = 100000, step = 1000, value = 10000),
                            tags$br(),
                            tags$p("Hull:"),
                            radioButtons("SRM_Hull_Type", SRM_Name_Hull_Type,
                                        choices = c(SRM_Name_Hull_Type_list),
                                        selected = SRM_Name_Hull_Type_selected),
                            sliderInput("SRM_Hull_Concave_Distance", label = "Hull Concave Distance", min = 1000, 
                                        max = 100000, step = 1000, value = 10000),
                            sliderInput("SRM_Hull_Buffer_Distance", label = "Hull Bufffer Distance", min = 1000, 
                                        max = 100000, step = 1000, value = 10000),
                            radioButtons("SRM_Hull_Cluster_Method", SRM_Name_Hull_Cluster_Method,
                                        choices = c(SRM_Name_Hull_Cluster_Method_list),
                                        selected = SRM_Name_Hull_Cluster_Method_selected),
                            sliderInput("SRM_Hull_Number_Kmeans", label = "Hull Number of Kmeans", min = 1, 
                                        max = 10, step = 1, value = 3),
                            sliderInput("SRM_Hull_Split_Distance", label = "Hull Split Distance", min = 1000, 
                                        max = 100000, step = 1000, value = 10000)
                        ),
                        sidebarPanel(width = 3,
                            # Input: Checkbox if file has header ----
                            tags$p("VoronoiHull:"),
                            sliderInput("SRM_VoronoiHull_Absence_sample_Buffer_Distance", label = "VoronoiHull Absence sampling Bufffer Distance", min = 1000, 
                                        max = 100000, step = 1000, value = 10000),
                            radioButtons("SRM_VoronoiHull_sampling_Type", SRM_Name_VoronoiHull_sampling_Type,
                                        choices = c(SRM_Name_VoronoiHull_sampling_Type_list),
                                        selected = SRM_Name_VoronoiHull_sampling_Type_selected),
                            sliderInput("SRM_VoronoiHull_Absence_sample_ratio", label = "VoronoiHull Absence sampling ratio", min = 1, 
                                        max = 10, step = 0.1, value = 1),
                            sliderInput("SRM_VoronoiHull_Absence_sample_size", label = "VoronoiHull Absence sampling size", min = 10, 
                                        max = 1000, step = 10, value = 100),
                            br(),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            textInput("SRM_MO_Dir_Folder_Name", SRM_Name_SRM_MO_Folder,
                                      value = ""),
                            tags$br(),
                            useShinyalert(),  # Set up shinyalert
                            actionButton("SRM_MO_Action_run", label = SRM_Name_SRM_MO_Action, icon = icon("running"))    
                        )
		              )
		             )
		             )
		           ),
		           
		           tabPanel(SRM_Name_Model_Out, fluid = TRUE,
#		                    tags$hr(),
		                    sidebarLayout(
		                      sidebarPanel(width = 3, Fluid = TRUE,
		                                   uiOutput("SRM_AO_Dir_Folder"),
                                        uiOutput("SRM_AO_Model_Name"),
		                                   uiOutput("SRM_AO_Species"),
		                                   tags$br(),
		                                   
		                                   uiOutput("SRM_AO_SDM_model"),
		                                   
		                                   # Input: Checkbox if file has header ----
		                                   checkboxGroupInput("SRM_OU_Climate_model", SRM_Name_CD_Models,
		                                                      choices = c(SRM_Name_CD_Models_list),
		                                                      selected = SRM_Name_CD_Models_selected
		                                   ),
		                                   
		                                   # Input: Checkbox if file has header ----
		                                   checkboxGroupInput("SRM_OU_Climate_scenario", SRM_Name_CD_Scenarios,
		                                                      choices = c(SRM_Name_CD_Scenarios_list),
		                                                      selected = SRM_Name_CD_Scenarios_selected
		                                   ),
		                                   
		                                   # Input: Checkbox if file has header ----
		                                   radioButtons("SRM_OU_Project_year", SRM_Name_CD_Year,
		                                                      choices = c(SRM_Name_CD_Year_list),
		                                                      selected = SRM_Name_CD_Year_selected
		                                   )
		                      ),
		                      
		                      # Main panel for displaying outputs ----
		                      mainPanel(
		                        tabsetPanel(
		                          tabPanel(SRM_Name_Out_Plot, icon = icon("layer-group"),
		                                  tags$br(),
		                                   uiOutput("SRM_OU_UI_plot")
		                          )
		                        )
		                      )
		           )
		         )
		      )

		),


tabPanel(DM_Name,
         #			tabsetPanel(
         tabBox(title = NULL, width = 12,
           tabPanel(DM_Name_Model, fluid = TRUE,
                    tabsetPanel(
                      tabPanel(DM_Name_Model_SDM, icon = icon("list-ul"),
                               tags$br(),      
                               fluidRow(
                                 sidebarPanel(width = 3, Fluid = TRUE,
                                              uiOutput("DM_SDM_Dir_Folder"),
                                              tags$hr(),
                                              actionButton("DM_MO_Species_sel_all", label = DM_Name_Model_SDM_Select_All, icon = icon("check-circle")),
                                              actionButton("DM_MO_Species_sel_none", label = DM_Name_Model_SDM_Select_None, icon = icon("circle")),
                                              uiOutput("DM_MO_Species")
                                 ),
                                 sidebarPanel(width = 3, Fluid = TRUE,             
                                              # Input: Checkbox if file has header ----
                                              radioButtons("DM_MO_Climate_model", DM_Name_CD_Models,
                                                           choices = c(DM_Name_CD_Models_list),
                                                           selected = DM_Name_CD_Models_selected),
                                              # Input: Checkbox if file has header ----
                                              radioButtons("DM_MO_Climate_scenario", DM_Name_CD_Scenarios,
                                                           choices = c(DM_Name_CD_Scenarios_list),
                                                           selected = DM_Name_CD_Scenarios_selected),
                                              # Input: Checkbox if file has header ----
                                              checkboxGroupInput("DM_MO_Project_year", DM_Name_CD_Year,
                                                                 choices = c(DM_Name_CD_Year_list),
                                                                 selected = DM_Name_CD_Year_selected)
                                 ),
                                 sidebarPanel(width = 3,
                                              uiOutput("DM_MO_SDM_model"),
                                              tags$hr(),
                                              tags$hr(),
                                              tags$p(DM_Name_Model_SDM_Current_Distribution),
                                              radioButtons("DM_MO_Current_Type", NULL,
                                                           choices = c("SDM", "SRM"),
                                                           selected = "SDM",
                                                           inline = TRUE),
                                              uiOutput("DM_MO_Current_UI"),
                                              tags$hr(),
                                              tags$hr(),
                                              tags$p(DM_Name_Model_SDM_Suitable_Distribution),
                                              radioButtons("DM_MO_Future_Type", NULL,
                                                           choices = c("Each_SDM", "Whole_SDM", "Everywhere"),
                                                           selected = "Each_SDM",
                                                           inline = TRUE)
                                 )
                               )
                      ),
                      tabPanel(DM_Name_Model_DM_Barrier, icon = icon("list-ul"),
                               tags$br(),
                               sidebarLayout(
                                 sidebarPanel(width = 3, Fluid = TRUE,
                                              tags$p("DM_Barrier:"),
                                              checkboxGroupInput("DM_MO_Barrier", DM_Name_DM_MO_Barriers,
                                                                 choices = c(DM_Name_DM_MO_Barriers_list),
                                                                 selected = DM_Name_DM_MO_Barriers_selected
                                              ),
                                              radioButtons("DM_MO_DM_barrierType", "DM_barrierType",
                                                           choices = c("strong" = "strong","weak" = "weak"),
                                                           selected = "weak"
                                              ),
                                              tags$hr(),
                                              radioButtons("DM_MO_Barrier_Landuse", DM_Name_DM_MO_Barrier_Landuse,
                                                           choices = c(DM_Name_DM_MO_Barrier_Landuse_list),
                                                           selected = DM_Name_DM_MO_Barrier_Landuse_selected
                                              ),
                                              checkboxGroupInput("DM_MO_Barrier_LanduseType", DM_Name_DM_MO_Barrier_LanduseType,
                                                                 choices = c(DM_Name_DM_MO_Barrier_LanduseType_list),
                                                                 selected = as.integer(DM_Name_DM_MO_Barrier_LanduseType_selected)
                                              ),
                                              sliderInput("DM_MO_Barrier_Forestfire_Cutoff", label = DM_Name_Model_DM_Barrier_Cutoff_Forestfire, min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.955),
                                              sliderInput("DM_MO_Barrier_Landslide_Cutoff", label = DM_Name_Model_DM_Barrier_Cutoff_Landslide, min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.85),
                                              tags$br(),
                                              sliderInput("DM_MO_Barrier_Landuse_Prop", label = DM_Name_Model_DM_Barrier_Proportion_Landuse, min = 10, 
                                                          max = 100, step = 10, value = 50),
                                              sliderInput("DM_MO_Barrier_Forestfire_Prop", label = DM_Name_Model_DM_Barrier_Proportion_Forestfire, min = 10, 
                                                          max = 100, step = 10, value = 50),
                                              sliderInput("DM_MO_Barrier_Landslide_Prop", label = DM_Name_Model_DM_Barrier_Proportion_Landslide, min = 10, 
                                                          max = 100, step = 10, value = 50)
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel(DM_Name_Map_Landuse, icon = icon("layer-group"), 
                                              tags$head(
                                                # Include our custom CSS
                                                includeCSS("styles.css"),
                                                includeScript("gomap.js")
                                              ),
                                              tags$br(),
                                              column(6, leafletOutput("DM_Map_Landuse", width = "800", height = "650"))
                                     ),
                                     tabPanel(DM_Name_Map_Forestfire, icon = icon("layer-group"), 
                                              tags$head(
                                                # Include our custom CSS
                                                includeCSS("styles.css"),
                                                includeScript("gomap.js")
                                              ),
                                              tags$br(),
                                              column(6, leafletOutput("DM_Map_Forestfire", width = "800", height = "650"))
                                     ),
                                     tabPanel(DM_Name_Map_Landslide, icon = icon("layer-group"), 
                                              tags$head(
                                                # Include our custom CSS
                                                includeCSS("styles.css"),
                                                includeScript("gomap.js")
                                              ),
                                              tags$br(),
                                              column(6, leafletOutput("DM_Map_Landslide", width = "800", height = "650"))
                                     ),
                                     tabPanel(DM_Name_Map_Total, icon = icon("layer-group"), 
                                              tags$head(
                                                # Include our custom CSS
                                                includeCSS("styles.css"),
                                                includeScript("gomap.js")
                                              ),
                                              tags$br(),
                                              column(6, leafletOutput("DM_Map_Total", width = "800", height = "650"))
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel(DM_Name_Model_DM, icon = icon("list-ul"),
                               tags$br(),
                               fluidRow(
                                 sidebarPanel(width = 3,
                                              sliderInput("DM_MO_DM_dispSteps", label = "DM_dispSteps", min = 0, 
                                                          max = 99, value = 10),
                                              tags$br(),
                                              tags$p("DM_dispKernel:"),
                                              verbatimTextOutput("DM_MO_DM_dispKernel"),
                                              sliderInput("DM_MO_DM_dispKernel1", label = "Dispersal Proportion of 1st Pixel", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 1.0),
                                              sliderInput("DM_MO_DM_dispKernel2", label = "Dispersal Proportion of 2nd Pixel", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.4),
                                              sliderInput("DM_MO_DM_dispKernel3", label = "Dispersal Proportion of 3rd Pixel", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.16),
                                              sliderInput("DM_MO_DM_dispKernel4", label = "Dispersal Proportion of 4th Pixel", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.06),
                                              sliderInput("DM_MO_DM_dispKernel5", label = "Dispersal Proportion of 5th Pixel", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.03)
                                 ),
                                 sidebarPanel(width = 3,
                                              tags$p("DM_Succession:"),
                                              sliderInput("DM_MO_DM_iniMatAge", label = "DM_iniMatAge (the initial maturity age)", min = 0, 
                                                          max = 10, value = 5),
                                              tags$br(),
                                              tags$p("DM_propaguleProd:"),
                                              verbatimTextOutput("DM_MO_DM_propaguleProd"),
                                              sliderInput("DM_MO_DM_propaguleProd1", label = "Propagule Production Proportion 1st year after iniMatAge", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.01),
                                              sliderInput("DM_MO_DM_propaguleProd2", label = "Propagule Production Proportion 2nd year after iniMatAge", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.08),
                                              sliderInput("DM_MO_DM_propaguleProd3", label = "Propagule Production Proportion 3rd year after iniMatAge", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.5),
                                              sliderInput("DM_MO_DM_propaguleProd4", label = "Propagule Production Proportion 4th year after iniMatAge", min = 0.01, 
                                                          max = 1.0, step = 0.01, value = 0.92)
                                 ),
                                 sidebarPanel(width = 3,
                                              tags$p("DM_Dispersal Distance:"),
                                              sliderInput("DM_MO_DM_lddFreq", label = "DM_lddFreq", min = 0.001, 
                                                          max = 1.0, step = 0.001, value = 0.001),
                                              sliderInput("DM_MO_SDM_lddDist", label = "LDD Distance Range (pixel)", min = 0, 
                                                          max = 10, value = c(6, 10)),
                                              sliderInput("DM_MO_DM_replicateNb", label = "DM_replicateNb", min = 0, 
                                                          max = 10, value = 1),
                                              checkboxInput("DM_MO_DM_overWrite", "DM_overWrite", TRUE),
                                              checkboxInput("DM_MO_DM_testMode", "DM_testMode", FALSE),
                                              checkboxInput("DM_MO_DM_fullOutput", "DM_fullOutput", FALSE),
                                              checkboxInput("DM_MO_DM_keepTempFiles", "DM_keepTempFiles", FALSE),
                                              br(),
                                              tags$br(),
                                              textInput("DM_MO_Dir_Folder_Name", DM_Name_Model_DM_Folder,
                                                        value = ""),
                                              tags$br(),
                                              useShinyalert(),  # Set up shinyalert
                                              actionButton("DM_MO_Action_run", label = DM_Name_DM_MO_Action, icon = icon("running"))
                                 )
                               )
                      )
                    )
           ),
           
           tabPanel(DM_Name_Model_Out, fluid = TRUE,
#                    tags$hr(),
                    sidebarLayout(
                      sidebarPanel(width = 3, Fluid = TRUE,
                                   uiOutput("DM_AO_Dir_Folder"),
                                   uiOutput("DM_AO_Model_Name"),
                                   uiOutput("DM_OU_Species"),
                                   tags$br(),
                                   
                                   uiOutput("DM_OU_SDM_model"),
                                   
                                   # Input: Checkbox if file has header ----
                                   checkboxGroupInput("DM_OU_Climate_model", DM_Name_CD_Models,
                                                      choices = c(DM_Name_CD_Models_list),
                                                      selected = DM_Name_CD_Models_selected
                                   ),
                                   
                                   # Input: Checkbox if file has header ----
                                   checkboxGroupInput("DM_OU_Climate_scenario", DM_Name_CD_Scenarios,
                                                      choices = c(DM_Name_CD_Scenarios_list),
                                                      selected = DM_Name_CD_Scenarios_selected
                                   ),
                                   
                                   # Input: Checkbox if file has header ----
                                   checkboxGroupInput("DM_OU_Project_year", DM_Name_CD_Year,
                                                      choices = c(DM_Name_CD_Year_list),
                                                      selected = DM_Name_CD_Year_selected
                                   )
                      ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        tabsetPanel(
                          tabPanel(DM_Name_Out_Plot, icon = icon("layer-group"), 
                                   tags$br(),
                                   uiOutput("DM_OU_UI_plot")
                          ),
                          tabPanel(DM_Name_Out_SDMDM_Plot, icon = icon("layer-group"), 
                                   tags$br(),
                                   uiOutput("DM_OU_SDMDM_UI_plot")
                          )
                        )
                      )
                    )
           )
         )
         
), 
     
		tabPanel(SA_Name,
		         #			tabsetPanel(
		         tabBox(title = NULL, width = 12,
				tabPanel(SA_Name_Analysis, fluid = TRUE,
                    tags$br(),
					fluidRow(
						sidebarPanel(width = 3, Fluid = TRUE,
              uiOutput("SA_MO_Dir_Folder"),
              uiOutput("SA_MO_Dir_Folder_Name"),
              tags$br(),
              actionButton("SA_CA_Species_Sel_All", label = SA_Name_Analysis_Select_All, icon = icon("check-circle")),
              actionButton("SA_CA_Species_Sel_None", label = SA_Name_Analysis_Select_None, icon = icon("circle")),
							uiOutput("SA_CA_Species")
						),
						sidebarPanel(width = 3, Fluid = TRUE,             
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SA_CA_Climate_model", SA_Name_CD_Models,
								choices = c(SA_Name_CD_Models_list),
								selected = SA_Name_CD_Models_selected),
                                   
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SA_CA_Climate_scenario", SA_Name_CD_Scenarios,
								choices = c(SA_Name_CD_Scenarios_list),
								selected = SA_Name_CD_Scenarios_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SA_CA_Project_year", SA_Name_CD_Year,
								choices = c(SA_Name_CD_Year_list),
								selected = SA_Name_CD_Year_selected)
						),
						sidebarPanel(width = 4,
							uiOutput("SA_CA_SDM_model"),
							tags$br(),
							tags$br(),
							br(),
							actionButton("SA_CA_Action_change", label = SA_Name_Analysis_Change, icon = icon("running")),
							br(),
							tags$br(),
							tags$br(),
							tags$br(),
							actionButton("SA_CA_Action_Vindex", label = SA_Name_Analysis_Vulnerability, icon = icon("running"))
						)
					)
				),
	
				tabPanel(SA_Name_Out, fluid = TRUE,
					tags$br(),
					sidebarLayout(
						sidebarPanel(width = 3, Fluid = TRUE,
                            uiOutput("SA_AO_Dir_Folder"),
                            uiOutput("SA_AO_Model_Name"),	
#							uiOutput("SA_AO_Species"),
                            tags$br(),
                            actionButton("SA_AO_Species_Sel_All", label = SA_Name_Out_Select_All, icon = icon("check-circle")),
                            actionButton("SA_AO_Species_Sel_None", label = SA_Name_Out_Select_None, icon = icon("circle")),
                            uiOutput("SA_AO_Species"),
							tags$br(),
							uiOutput("SA_AO_SDM_model"),
							
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SA_AO_Climate_model", SA_Name_CD_Models,
								choices = c(SA_Name_CD_Models_list),
								selected = SA_Name_CD_Models_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SA_AO_Climate_scenario", SA_Name_CD_Scenarios,
								choices = c(SA_Name_CD_Scenarios_list),
								selected = SA_Name_CD_Scenarios_selected),
                                   
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SA_AO_Project_year", SA_Name_CD_Year,
								choices = c(SA_Name_CD_Year_list),
								selected = SA_Name_CD_Year_selected)
						),
                      
						# Main panel for displaying outputs ----
						mainPanel(
							tabsetPanel(
								tabPanel(SA_Name_Out_ChangePlot, icon = icon("layer-group"),
									tags$br(),
									uiOutput("SA_AO_UI_plot")
								),
								tabPanel(SA_Name_Out_IV_Table, icon = icon("file-signature"),
                                tags$br(),
                  radioButtons("SA_AO_IV_Data", "Vulnerable Index File",
                                choices = c("Target Species" = "Species","Total Species" = "Total"),
                                selected = "Species"
                  ),
									tags$br(),
									fluidRow(
										column(12, DT::dataTableOutput("SA_AO_IV_Table"), style = "overflow-y: scroll;overflow-x: scroll;")
									)
                ),
                tabPanel(SA_Name_Out_IV_GainLoss, icon = icon("chart-area"),
									fluidRow(
										tags$br(),
                    selectInput("SA_AO_IV_UI_plot1", "Select the 5th group",
                                choices = c(SA_Name_Group1_list),
                                selected = SA_Name_Group1_selected
                    ),
										tags$br(),
										column(6, plotOutput("SA_AO_IV_Plot11", height = '500px')),
										column(6, plotOutput("SA_AO_IV_Plot12", height = '500px'))
									)
                ),
								tabPanel(SA_Name_Out_IV_Pattern, icon = icon("chart-line"),
									fluidRow(
										tags$br(),
									  selectInput("SA_AO_IV_UI_plot2", "Select the 4th group",
									              choices = c(SA_Name_Group2_list),
									              selected = SA_Name_Group2_selected
									  ),
										tags$br(),
										column(6, plotOutput("SA_AO_IV_Plot21", height = '500px')),
										column(6, plotOutput("SA_AO_IV_Plot22", height = '500px'))
									)
								),
								tabPanel(SA_Name_Out_IV_Order, icon = icon("bar-chart-o"),
                  fluidRow(
                    tags$br(),
                    selectInput("SA_AO_IV_UI_plot3", "Select the 5th group",
                                choices = c(SA_Name_Group3_list),
                                selected = SA_Name_Group3_selected
                    ),
                    tags$br(),
                    column(6, plotOutput("SA_AO_IV_Plot31", height = '500px')),
                    column(6, plotOutput("SA_AO_IV_Plot32", height = '500px'))
                  )
                )
							)
						)
					)
				)
			)
		),      

		tabPanel(HA_Name,
		         #			tabsetPanel(
		         tabBox(title = NULL, width = 12,
				tabPanel(HA_Name_Anlayis, fluid = TRUE,
					tags$br(),
					fluidRow(
						sidebarPanel(width = 3, Fluid = TRUE,
                            uiOutput("HA_MI_Dir_Folder"),
                            uiOutput("HA_MI_Dir_Folder_Name"),
						    tags$br(),
						    actionButton("HA_CA_Species_Sel_All", label = HA_Name_Analysis_Select_All, icon = icon("check-circle")),
						    actionButton("HA_CA_Species_Sel_None", label = HA_Name_Analysis_Select_None, icon = icon("circle")),
							uiOutput("HA_CA_Species")
						),
						sidebarPanel(width = 3, Fluid = TRUE,                                                      
							# Input: Checkbox if file has header ----
							checkboxGroupInput("HA_CA_Climate_model", HA_Name_CD_Models,
								choices = c(HA_Name_CD_Models_list),
								selected = HA_Name_CD_Models_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("HA_CA_Climate_scenario", HA_Name_CD_Scenarios,
								choices = c(HA_Name_CD_Scenarios_list),
								selected = HA_Name_CD_Scenarios_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("HA_CA_Project_year", HA_Name_CD_Year,
								choices = c(HA_Name_CD_Year_list),
								selected = HA_Name_CD_Year_selected)
						),
						sidebarPanel(width = 4,
							uiOutput("HA_CA_SDM_model"),
							tags$br(),
							tags$br(),
							br(),
							shinyDirButton("HA_MO_Dir_Folder", HA_Name_AO_Dir, NULL, icon = icon("folder-open")),
							verbatimTextOutput("HA_MO_Dir_Folder", placeholder = TRUE),
#							tags$br(),
							actionButton("HA_VA_Action_Analysis", label = HA_Name_Action, icon = icon("running")),
              br(),
							tags$br(),
							tags$br(),
              tags$br(),
						    checkboxGroupInput("HA_VA_Admin", HA_Name_Admin,
                                    choices = c(HA_Name_Admin_list),
                                    selected = HA_Name_Admin_selected),
              actionButton("HA_VA_Action_Admin_Species", label = HA_Name_Action_Admin_Species, icon = icon("running")),
              br(),
							tags$br(),
              tags$br(),
              tags$br(),
							actionButton("HA_VA_Action_Admin_Group", label = HA_Name_Action_Admin_Group, icon = icon("running"))
						)
					)
				),
	
				tabPanel(HA_Name_Out, fluid = TRUE,
#					tags$br(),
					sidebarLayout(
						sidebarPanel(width = 3, Fluid = TRUE,

							uiOutput("HA_AO_MI_Dir_Folder"),
							uiOutput("HA_AO_MI_Dir_Folder_Name"),
							uiOutput("HA_AO_Species"),
							tags$br(),
							uiOutput("HA_AO_MO_Dir_Folder"),
							tags$br(),
							uiOutput("HA_AO_SDM_model"),
	
							# Input: Checkbox if file has header ----
              radioButtons("HA_AO_Climate_model", HA_Name_CD_Models,
								choices = c(HA_Name_CD_Models_list),
								selected = HA_Name_CD_Models_selected),
	
							# Input: Checkbox if file has header ----
              radioButtons("HA_AO_Climate_scenario", HA_Name_CD_Scenarios,
								choices = c(HA_Name_CD_Scenarios_list),
								selected = HA_Name_CD_Scenarios_selected),
	
							# Input: Checkbox if file has header ----
              radioButtons("HA_AO_Project_year", HA_Name_CD_Year,
								choices = c(HA_Name_CD_Year_list),
								selected = HA_Name_CD_Year_selected)
						),
	
	# Main panel for displaying outputs ----
						mainPanel(
							tabsetPanel(
                tabPanel(HA_Name_Out_Species, icon = icon("layer-group"),
                  tags$br(),
                  radioButtons("HA_AO_SD_Habitat_Type", NULL,
                    choices = c(HA_Name_SD_Habitat_Types_list),
                    selected = HA_Name_SD_Habitat_Types_selected,
                    inline = TRUE),
                  tags$br(),
                  radioButtons("HA_AO_SD_Habitat_Plot_Type", NULL,
                               choices = c(HA_Name_SD_Habitat_Plot_Types_list),
                               selected = HA_Name_SD_Habitat_Plot_Types_selected,
                               inline = TRUE),
                  tags$br(),
                  uiOutput("HA_AO_SD_PLOT_Group_UI"),
                  uiOutput("HA_AO_SD_PLOT_UI")
                ),
								tabPanel(HA_Name_Out_SR, icon = icon("layer-group"),
                  tags$br(),
                  radioButtons("HA_AO_SR_Habitat_Type", NULL,
                              choices = c(HA_Name_SD_Habitat_Types_list),
								              selected = HA_Name_SD_Habitat_Types_selected,
								              inline = TRUE),
								  tags$br(),
								  radioButtons("HA_AO_SR_Habitat_Plot_Type", NULL,
								              choices = c(HA_Name_SD_Habitat_Plot_Types_list),
								              selected = HA_Name_SD_Habitat_Plot_Types_selected,
								              inline = TRUE),
								  tags$br(),
								  uiOutput("HA_AO_SR_PLOT_Group_UI"),
								  uiOutput("HA_AO_SR_PLOT_UI"),
                  tags$br(),
                  uiOutput("HA_AO_SR_PLOT_SP_Table"),
                  tags$br(),
                  uiOutput("HA_AO_SR_PLOT_SP_Stat")
								),
								tabPanel(HA_Name_Out_SL, icon = icon("layer-group"),
								         tags$br(),
								         radioButtons("HA_AO_SL_Habitat_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Types_list),
								                      selected = HA_Name_SD_Habitat_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         radioButtons("HA_AO_SL_Habitat_Plot_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Plot_Types_list),
								                      selected = HA_Name_SD_Habitat_Plot_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         uiOutput("HA_AO_SL_PLOT_Group_UI"),
								         uiOutput("HA_AO_SL_PLOT_UI"),
								         tags$br(),
								         uiOutput("HA_AO_SL_PLOT_SP_Table"),
								         tags$br(),
								         uiOutput("HA_AO_SL_PLOT_SP_Stat")
								),
								tabPanel(HA_Name_Out_SS, icon = icon("layer-group"),
								         tags$br(),
								         radioButtons("HA_AO_SS_Habitat_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Types_list),
								                      selected = HA_Name_SD_Habitat_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         radioButtons("HA_AO_SS_Habitat_Plot_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Plot_Types_list),
								                      selected = HA_Name_SD_Habitat_Plot_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         uiOutput("HA_AO_SS_PLOT_Group_UI"),
								         uiOutput("HA_AO_SS_PLOT_UI"),
								         tags$br(),
								         uiOutput("HA_AO_SS_PLOT_SP_Table"),
								         tags$br(),
								         uiOutput("HA_AO_SS_PLOT_SP_Stat")
								),								
								tabPanel(HA_Name_Out_SG, icon = icon("layer-group"),
								         tags$br(),
								         radioButtons("HA_AO_SG_Habitat_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Types_list),
								                      selected = HA_Name_SD_Habitat_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         radioButtons("HA_AO_SG_Habitat_Plot_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Plot_Types_list),
								                      selected = HA_Name_SD_Habitat_Plot_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         uiOutput("HA_AO_SG_PLOT_Group_UI"),
								         uiOutput("HA_AO_SG_PLOT_UI"),
								         tags$br(),
								         uiOutput("HA_AO_SG_PLOT_SP_Table"),
								         tags$br(),
								         uiOutput("HA_AO_SG_PLOT_SP_Stat")
								),
								tabPanel(HA_Name_Out_VI, icon = icon("layer-group"),
								         tags$br(),
								         radioButtons("HA_AO_VI_Habitat_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Types_list),
								                      selected = HA_Name_SD_Habitat_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         radioButtons("HA_AO_VI_Habitat_Plot_Type", NULL,
								                      choices = c(HA_Name_SD_Habitat_Plot_Types_list),
								                      selected = HA_Name_SD_Habitat_Plot_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         radioButtons("HA_AO_VI_TYPE_UI", NULL,
								                      choices = c(HA_Name_VI_Types_list),
								                      selected = HA_Name_VI_Types_selected,
								                      inline = TRUE),
								         tags$br(),
								         uiOutput("HA_AO_VI_PLOT_Group_UI"),
								         uiOutput("HA_AO_VI_PLOT_UI"),
#								         tags$br(),
#								         uiOutput("HA_AO_VI_PLOT_SP_Table"),
#								         tags$br(),
#								         uiOutput("HA_AO_VI_PLOT_SP_Stat")
								)								
								
							)
						)
					)
				)
			)    
		),     
	
		tabPanel(HELP_Name, fluid = TRUE,
			tags$br(),
			sidebarPanel(width = 5,
        helpText(h5(HELP_Name_Content))
			)
		)
	)
	)
)
)
)

