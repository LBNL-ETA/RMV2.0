#-------------------------------------------------------------------------------
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
# ui.R
#-------------------------------------------------------------------------------

#===============================================================================
#                               SHINY_UI                                       #
#===============================================================================

dashboardPage(skin = "red",
  dashboardHeader(title = "RM&V 2.0"),
  dashboardSidebar(
    conditionalPanel(condition = " input.next_init_1 == 0",
      sidebarMenu(id = "tbar",
        menuItem("Home",
                 tabName = "home",
                 icon = icon("home")),
        menuItem("Project Setup",
                 tabName = "initialization",
                 icon = icon("tasks"))

      )#end sidebarMenu
    ),#end conditional panel

    conditionalPanel(condition = "input.next_init_1 &&
                                 input.type_init == 1 &&
                                 input.new_init == 1",
      sidebarMenu(id = "tbar",
        menuItem("Home",
                 tabName = "home",
                 icon = icon("home")),
        menuItem("Project Setup",
                 tabName = "initialization",
                 icon = icon("tasks")),
        menuItem("___ Screening Analysis ___",
                 tabName = "screening",
                 icon = icon("paint-brush "),
                 conditionalPanel(condition = "input.next_init_sc",
                   menuSubItem("Input Data Overview",
                               icon = icon("table"),
                               tabName = "visuProject_sc"),
                   menuSubItem("Train Baseline Models",
                               icon = icon("gears"),
                               tabName = "trainModels_sc")
                 ),
                 conditionalPanel(condition = "input.Train_go_sc",
                   menuSubItem("Baseline Modeling Results",
                               icon = icon("pie-chart"),
                               tabName = "resBaseline_sc"),
                   ##  -----  Deprecated  -----
                   ## exclude the uncertainty calculation
                   # menuSubItem("Uncertainty Estimation",
                   #             icon = icon("stats", lib = "glyphicon"),
                   #             tabName = "uncertEstimation_sc"),
                   menuSubItem("Summary",
                               icon = icon("dashboard"),
                               tabName = "summary_sc")
                 ),

                 startExpanded = T

        ),
        uiOutput("save_sc_Ui"),

        menuItem("Help",
                 tabName = "help",
                 icon = icon("question-circle"))
      )#end sidebarMenu
    ),#end conditional panel

    conditionalPanel(condition = "input.next_init_1 &&
                                 input.type_init == 1 &&
                                 input.new_init == 2",
      sidebarMenu(id = "tbar",
        menuItem("Home",
                 tabName = "home",
                 icon = icon("home")),
        menuItem("Project Setup",
                 tabName = "initialization",
                 icon = icon("tasks")),
        menuItem("___ Screening Analysis ___",
                 tabName = "screening_lo",
                 icon = icon("paint-brush "),
                 conditionalPanel(condition = "input.next_init_sc_lo",
                   menuSubItem("Input Data Overview",
                               icon = icon("table"),
                               tabName = "visuProject_sc"),
                   menuSubItem("Baseline Modeling Results",
                               icon = icon("pie-chart"),
                               tabName = "resBaseline_sc"),
                  ##  -----  Deprecated  -----
                  ## exclude the uncertainty calculation
                  # menuSubItem("Uncertainty Estimation",
                  #             icon = icon("stats", lib = "glyphicon"),
                  #             tabName = "uncertEstimation_sc"),
                  menuSubItem("Summary",
                              icon = icon("dashboard"),
                              tabName = "summary_sc")
                 ),
                 startExpanded = T
         ),

        menuItem("Help",
                 tabName = "help",
                 icon = icon("question-circle"))
      )#end sidebarMenu
    ),#end conditional panel

    conditionalPanel(condition = "input.next_init_1 &&
                                 input.type_init == 2 &&
                                 input.new_init == 1",
      sidebarMenu(id = "tbar",
        menuItem("Home",
                 tabName = "home",
                 icon = icon("home")),
        menuItem("Project Setup",
                 tabName = "initialization",
                 icon = icon("tasks")),
        menuItem("____ Savings Analysis ____",
                 tabName = "savings",
                 icon = icon("leaf"),
                 conditionalPanel(condition = "input.next_init_sav",
                   menuSubItem("Input Data Overview",
                               icon = icon("table"),
                               tabName = "visuProject_sav"),
                   menuSubItem("Train Baseline Models",
                               icon = icon("gears"),
                               tabName = "trainModels_sav")
                 ),
                 conditionalPanel(condition = "input.Train_go_sav",
                   menuSubItem("Baseline Modeling Results",
                               icon = icon("pie-chart"),
                               tabName = "resBaseline_sav"),
                   menuSubItem("Savings Estimations",
                               icon = icon("stats", lib = "glyphicon"),
                               tabName = "savAnalysis_sav")
                 ),
                 # menuSubItem("Savings & CUSUM",
                 #             icon = icon("line-chart"),
                 #             tabName = "visSavAnalysis_sav"),
                 conditionalPanel(condition = "input.sav_est_go",
                   menuSubItem("Non Routine Events",
                               icon = icon("line-chart"),
                               tabName = "nrEvents_sav"),
                   menuSubItem("Summary",
                               icon = icon("dashboard"),
                               tabName = "summary_sav")
                 ),

                 startExpanded = T
        ),
        uiOutput("save_sav_Ui"),

        menuItem("Help",
                 tabName = "help",
                 icon = icon("question-circle"))
      )#end sidebarMenu
    ),#end conditional panel

    conditionalPanel(condition = "input.next_init_1 &&
                                 input.type_init == 2 &&
                                 input.new_init == 2",
      sidebarMenu(id = "tbar",
        menuItem("Home",
                 tabName = "home",
                 icon = icon("home")),
        menuItem("Project Setup",
                 tabName = "initialization",
                 icon = icon("tasks")),
        menuItem("____ Savings Analysis ____",
                 tabName = "savings_lo",
                 icon = icon("leaf"),
                 conditionalPanel(condition = "input.next_init_sav_lo",
                   menuSubItem("Input Data Overview",
                               icon = icon("table"),
                               tabName = "visuProject_sav"),
                   menuSubItem("Baseline Modeling Results",
                               icon = icon("pie-chart"),
                               tabName = "resBaseline_sav"),
                   menuSubItem("Savings Estimations",
                               icon = icon("stats", lib = "glyphicon"),
                               tabName = "savAnalysis_sav")
                 ),
                 # menuSubItem("Savings & CUSUM",
                 #             icon = icon("line-chart"),
                 #             tabName = "visSavAnalysis_sav"),
                 conditionalPanel(condition = "input.sav_est_go",
                   menuSubItem("Non Routine Events",
                               icon = icon("line-chart"),
                               tabName = "nrEvents_sav_lo"),
                   menuSubItem("Summary",
                               icon = icon("dashboard"),
                               tabName = "summary_sav")
                 ),
                 startExpanded = T
        ),

        menuItem("Help",
                 tabName = "help",
                 icon = icon("question-circle"))
      )#end sidebarMenu
    )#end conditional panel

  ),#end of dashboardSidebar
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    tabItems(
      tabItem(tabName = "home",
        fluidPage(title = "Home",
          fluidRow(
            column(width = 4),
            column(width = 4,
              box(#title = h1("LBNL MV 2.0 Toolbox"),
                  status ="info",
                  width = 12,
                  #background = "red",
                  solidHeader = F,
                  collapsible = F,
                  collapsed = F,
                  div(img(src='logo.svg'),style="text-align: center;")
                )# end of box
            ),# end of column
            column(width = 4)
            )# end of fluidRow
        )# end of fluidPage
      ),#end of tabItem

#===============================================================================
#                           Initialization                                     #
#===============================================================================

      tabItem(tabName = "initialization",
        fluidPage(title = "initialization",
          fluidRow(
            column(width = 3,
              box(title = "Project Type",
                  width = 12,
                  solidHeader = T,
                  status = "info",
                 box(title = "Select the project type",
                     width = 12,
                     solidHeader = F,
                     status ="info",
                     radioButtons("type_init",
                                  label = h3(""),
                                  choices = list("Screening" = 1,
                                                 "Savings Analysis" = 2),
                                  selected = 1),
                     radioButtons("new_init",
                                  label = h3(""),
                                  choices = list("Create a New Project" = 1,
                                                 "Open an Existing Project" = 2),
                                  selected = 1)
                 ),#end of box
                 column(width = 12,
                        conditionalPanel(condition = "input.next_init_1 == 0",
                                         actionButton("next_init_1",
                                                      label = "Next Step",
                                                      icon = icon("arrow-right"),
                                                      width ="100%")
                        )#end conditionalPanel
                 )#end of column
              )# end of box
            ),# end of column
            conditionalPanel(condition = "input.next_init_1 && input.type_init == 1 && input.new_init == 1",
              column(width = 4,
                box(title = "Screening Mode",
                    width = 12,
                    solidHeader = T,
                    status = "info",
                   box(title = "Project Name",
                       width = 12,
                       solidHeader =F,
                       status ="info",
                       textInput("p_name_sc",
                                 label = NULL,
                                 value = paste0("Project_",
                                 format(Sys.time(), "%m.%d")))
                   ),#end of box
                   box(id = "save_dir_sc_box",
                       title = "Select the directory where the results will be saved",
                       width = 12,
                       solidHeader =F,
                       status ="warning",
                       column(4,
                         shinyDirButton("save_dir_sc",
                                         "Browse",
                                         "Browse")
                              ),
                       column(8,
                          textOutput("save_dir_sc_out"),
                              tags$style(type='text/css',
                                "#save_dir_sc_out { width:100%; margin-top: 5px;}")
                              )
                   ),#end of box
                   box(id = "pre_dir_sc_box",
                       title = "Select pre-installation data directory",
                       width = 12,
                       solidHeader =F,
                       status ="warning",
                       column(4,
                         shinyDirButton("pre_dir_sc",
                                         "Browse",
                                         "Browse")
                              ),
                       column(8,
                          textOutput("pre_dir_sc_out"),
                              tags$style(type='text/css',
                                "#pre_dir_sc_out { width:100%; margin-top: 5px;}")
                              )
                   ),#end of box
                   conditionalPanel(
                     "output.fields_sc_new",
                     column(width = 12,
                            actionButton("next_init_sc",
                                         label = "Next Step",
                                         icon = icon("arrow-right"),
                                         width ="100%")
                     )#end of column
                   )
                )#end of box
              ),# end of column
              column(width = 4,
                     infoBoxOutput("intEndBox_sc",width = 12)
              )#end of column
            ),# end of conditionalPanel
            conditionalPanel(condition = "input.next_init_1 && input.type_init == 1 && input.new_init == 2",
              column(width = 4,
                box(title = "Screening Mode",
                    width = 12,
                    solidHeader = T,
                    status = "info",
                   box(id = "load_sc_box",
                       title = "Choose the project file",
                       width = 12,
                       solidHeader =F,
                       status = "warning",
                       column(4,
                         shinyFilesButton("load_sc",
                                         "Browse",
                                         "Browse",
                                         multiple=F)
                              ),
                       column(8,
                          textOutput("load_sc_out"),
                              tags$style(type='text/css',
                                "#load_sc_out { width:100%; margin-top: 5px;}")
                              )
                   ),#end of box
                   conditionalPanel(
                     "output.fields_sc_load",
                     column(width = 12,
                            actionButton("next_init_sc_lo",
                                         label = "Next Step",
                                         icon = icon("arrow-right"),
                                         width ="100%")
                     )#end of column
                   )#end of cp
                )#end of box
              ),# end of column
              column(width = 4,
                     infoBoxOutput("intEndBox_sc_lo",width = 12)
              )#end of column
            ),# end of conditionalPanel
            conditionalPanel(condition = "input.next_init_1 && input.type_init == 2 && input.new_init == 1",
              column(width = 4,
                box(title = "Savings Analysis Mode",
                    width = 12,
                    solidHeader = T,
                    status = "info",
                   box(title = "Project Name",
                       width = 12,
                       solidHeader =F,
                       status ="info",
                       textInput("p_name_sav",
                                 label = NULL,
                                 value = paste0("Project_",
                                 format(Sys.time(), "%m.%d")))
                   ),#end of box
                   box(id = "save_dir_sav_box",
                       title = "Select the directory where the results will be saved",
                       width = 12,
                       solidHeader =F,
                       status ="warning",
                       column(4,
	                        shinyDirButton("save_dir_sav",
                                         "Browse",
                                         "Browse")
                              ),
                       column(8,
                          textOutput("save_dir_sav_out"),
                              tags$style(type='text/css',
                                "#save_dir_sav_out { width:100%; margin-top: 5px;}")
                              )
                   ),#end of box
                   box(id = "pre_dir_sav_box",
                       title = "Select pre-installation data directory",
                       width = 12,
                       solidHeader =F,
                       status ="warning",
                       column(4,
                         shinyDirButton("pre_dir_sav",
                                        "Browse",
                                        "Browse")
                              ),
                       column(8,
                          textOutput("pre_dir_sav_out"),
                              tags$style(type='text/css',
                                "#pre_dir_sav_out { width:100%; margin-top: 5px;}")
                              )
                   ),#end of box
                   box(id = "post_dir_sav_box",
                       title = "Select post-installation data directory",
                       width = 12,
                       solidHeader =F,
                       status ="warning",
                       column(4,
                         shinyDirButton("post_dir_sav",
                                        "Browse",
                                        "Browse")
                              ),
                       column(8,
                          textOutput("post_dir_sav_out"),
                              tags$style(type='text/css',
                                "#post_dir_sav_out { width:100%; margin-top: 5px;}")
                              )
                       #br(),
                       #tags$b(textOutput("post_dir_sav"))
                   ),#end of box
                   conditionalPanel(
                     "output.fields_sav_new",
                     column(width = 12,
                            actionButton("next_init_sav",
                                         label = "Next Step",
                                         icon = icon("arrow-right"),
                                         width ="100%")
                     )#end of column
                 )#end of cp
                )#end of box
              ),# end of column
              column(width = 4,
                     infoBoxOutput("intEndBox_sav",width = 12)
              )#end of column
            ),# end of conditionalPanel
            conditionalPanel(condition = "input.next_init_1 && input.type_init == 2 && input.new_init == 2",
              column(width = 4,
                box(title = "Savings Analysis Mode",
                    width = 12,
                    solidHeader = T,
                    status = "info",
                   box(id = "load_sav_box",
                       title = "Choose the project file",
                       width = 12,
                       solidHeader =F,
                       status ="warning",
                       column(4,
                         shinyFilesButton("load_sav",
                                          "Browse",
                                          "Browse", multiple=F)
                              ),
                       column(8,
                          textOutput("load_sav_out"),
                              tags$style(type='text/css',
                                "#load_sav_out { width:100%; margin-top: 5px;}")
                              )
                   ),#end of box
                   conditionalPanel(
                     "output.fields_sav_load",
                     column(width = 12,
                            actionButton("next_init_sav_lo",
                                         label = "Next Step",
                                         icon = icon("arrow-right"),
                                         width ="100%")
                     )#end of column
                   )#end of cp
                )#end of box
              ),# end of column
              column(width = 4,
                     infoBoxOutput("intEndBox_sav_lo",width = 12)
              )#end of column
            )# end of conditionalPanel
          )# end of fluidRow
        )# end of fluidPage
      ),#end of tabItem

#===============================================================================
#                              Screening                                       #
#===============================================================================

      tabItem(tabName = "visuProject_sc",
        fluidPage(
          conditionalPanel("input.next_init_sc || input.next_init_sc_lo",
            fluidRow(
              column(width = 8,
                box(title = "Pre-Installation data summary",
                    width =12,
                    solidHeader = T,
                    status = "info",
                    dataTableOutput("pre_summary_tab_sc"),
                    downloadButton("pre_summary_tab_sc_dl", "Download")
                )# end of box
              )#end of column
            ),# end of fluidrow
            fluidRow(
              column(width = 12,
                box(title = "Pre-Installation data plots",
                    width = 12,
                    solidHeader = T,
                    status = "info",
                   box(title = "Choose Data to Visualize",
                       width =3,
                       solidHeader = F,
                       status = "info",
                       selectInput('Data_vis_sc', 'Data', ""),
                       actionButton("pre_plot_go_sc",
                                    label = "Plot",
                                    width ="100%")
                   ),# end of box
                   conditionalPanel(condition = "input.pre_plot_go_sc",
                     tabBox(title = tagList(shiny::icon("line-chart"), ""),
                            width = 9,
                       tabPanel("Time Serie Pre-Installation",
                                width =12,
                                solidHeader = F,
                                status = "info",
                                dygraphs::dygraphOutput("ts_plot_pre_sc")
                       ),#end of tabPanel
                       tabPanel("Scatter plot eload Vs. input",
                                width =6,
                                solidHeader = F,
                                status = "info",
                                uiOutput('radio_plot_pre_sc'),
                                plotOutput("scatter_plot_pre_sc")
                       ),#end of tabPanel
                       tabPanel("Heatmap",
                                width =6,
                                solidHeader = F,
                                status = "info",
                                plotly::plotlyOutput("heatmap_pre_sc",
                                           height = "auto",
                                           width = "auto")
                       )#end of tabPanel
                     )#end of tabBox
                   )#end of conditionalPanel
                )#end of box
              )#end of column
            )#end of fluidRow
          )#end of conditionalPanel
        )# end of fluidPage
      ),#end of tabItem

      tabItem(tabName = "trainModels_sc",
       fluidPage(
         conditionalPanel(condition = "input.next_init_sc",
           fluidRow(
             column(width = 12,
               box(title = "Model Set Up",
                   width = 12,
                   solidHeader = F,
                   status = "info",
                  box(title = "Select the Baseline Model",
                      width =3,
                      solidHeader = T,
                      status = "info",
                      selectInput('Model_sc',
                                  'Model',
                                  c("TOWT","GBM")),
                      conditionalPanel(condition = "input.Model_sc == 'GBM'",
                        radioButtons("d_off_GBM_sc",
                                     "Include Days Off as independent variable:",
                                     c("No" = "no_sc",
                                       "Default US Holidays
                                       (from 2007 to 2017)" = "def_d_off_sc",
                                       "Select a new Days Off file" = "yes_sc")),
                        conditionalPanel(condition = "input.d_off_GBM_sc == 'yes_sc'",
                          fileInput("d_off_path_sc",
                                    "Choose Days Off CSV File",
                                    accept = ".csv")
                        )# end of conditionalPanel
                      ),# end of conditionalPanel
                      actionButton("Train_go_sc",
                                   label = "Train the Baseline Models",
                                   width ="100%")

                  ),# end of box
                  box(title = "Baseline Model Description",
                      width = 5,
                      solidHeader = F,
                      status = "info",
                      h3(textOutput("Model_Name_sc")),
                      textOutput("Model_Desc_sc")
                  ),#end of box
                  conditionalPanel(condition = "input.Model_sc == 'TOWT'",
                    box(title = "TOWT Hyper-parameter Setup",
                        width = 4,
                        solidHeader = F,
                        status = "info",
                        sliderInput("timescaleDays_sc",
                                    "timescale for weighting function (in days):",
                                    min = 1,
                                    max = 90,
                                    value = 15,
                                    step = 1)
                    )#end of box
                  ),# end of conditionalPanel
                  conditionalPanel(condition = "input.Model_sc == 'GBM'",
                    box(title = "GBM Hyper-parameters Setup",
                        width = 4,
                        solidHeader = F,
                        status = "info",
                        sliderInput("ncores_sc",
                                    "number of cores (parallelization):",
                                    min = 1,
                                    max = parallel::detectCores(logical =F),
                                    value = parallel::detectCores(logical =F),
                                    step = 1),
                        sliderInput("kfolds_sc",
                                    "number of cross validations (k-folds):",
                                    min = 3, max = 10,
                                    value = 5, step = 1),
                        sliderInput("gbm_iter_sc",
                                    "number of GBM iterations:",
                                    min = 50, max = 1000,
                                    value = c(50,500), step = 50),
                         sliderInput("depth_sc",
                                     "depth of trees:",
                                     min = 1, max = 10,
                                     value = c(3,7), step = 1),
                         checkboxGroupInput("lr_sc",
                                            "Choose the learning rates:",
                                            choices = c("0.5" = "0.5",
                                                        "0.1" = "0.1",
                                                        "0.05" = "0.05",
                                                        "0.01" = "0.01",
                                                        "0.005" = "0.005",
                                                        "0.001" = "0.001"),
                                             selected = "0.05")

                    )#end of box
                  )# end of conditionalPanel
               )#end of box
             )# end of column
           ),# end of fluidRow
           fluidRow(
             column(width = 12,
               column(width = 4),#end of column
               column(width = 4,
                      infoBoxOutput("trainEndBox_sc",width = 12)
               ),#end of column
               column(width = 4)#end of column
             )# end of column
           )# end of fluidRow
         )#end of conditionalPanel
       )#end of fluidPage
      ),#end of tabItem

      tabItem(tabName = "resBaseline_sc",
        fluidPage(
          conditionalPanel(condition = "input.Train_go_sc || input.next_init_sc_lo",
            column(width = 6,
              box(title = "Goodness of Fit",
                  width = 12,
                  solidHeader = T,
                  status = "info",
                  dataTableOutput("model_metrics_tab_sc"),
                  downloadButton("model_metrics_tab_sc_dl", "Download")
              )#end of box
            ),#end of column
            column(width = 3,
              box(title = " Screening Thresholds",
                  width =12,
                  solidHeader = T,
                  status = "info",
                  numericInput("R2_tresh_sc",
                               "R2 (in %)",
                               65, min = 0, max = 100, step =1,
                               width = NULL),
                  br(),
                  numericInput("CVRMSE_tresh_sc",
                               "CVRMSE (in %)",
                               25, min = 0, max = 100, step =1,
                               width = NULL),
                  br(),
                  numericInput("NMBE_tresh_sc",
                               "NMBE (in %)",
                               0.5, min = -100, max = 100,
                               width = NULL),
                  actionButton("screen_go_sc",
                               label = "screen",
                               width ="100%")
              )# end of box
            ),#end of column
            column(width = 3,
              box(title ="screening Results",
                  width =12,
                  solidHeader = T,
                  status = "info",
                  plotly::plotlyOutput("pie_plot_screen_sc",
                               width = "auto",
                               height = "auto")
              )#end of box
            ),#end of column
            column(width = 12,
              box(title = "Plots Baseline Model Results",
                  width = 12,
                  solidHeader = T,
                  status = "info",
                 box(title = "Choose Data to Visualize",
                     width = 3,
                     solidHeader = F,
                     status = "info",
                     selectInput('Res_vis_sc', 'Data', ""),
                     actionButton("base_plot_go_sc",
                                  label = "Plot",
                                  width ="100%")
                 ),# end of box
                 conditionalPanel(condition = "input.base_plot_go_sc",
                   tabBox(title = tagList(icon("line-chart"), ""),
                          width = 9,
                     tabPanel("Time Serie Plot",
                              width =12,
                              solidHeader = F,
                              status = "info",
                              dygraphs::dygraphOutput("ts_plot_mod_sc")
                     ),#end of tabPanel
                     tabPanel("Scatter plot fitting vs. actual",
                              width =8,
                              solidHeader = F,
                              status = "info",
                              plotOutput("scatter_plot_act_fit_sc")
                     ),#end of tabPanel
                     tabPanel("Scatter plot of errors vs. input",
                              width =8,
                              solidHeader = F,
                              status = "info",
                              uiOutput('radio_plot_err_sc'),
                              plotOutput("scatter_plot_err_input_sc")
                     ),#end of tabPanel
                     tabPanel("Residual Autocorrelation Plot",
                              width =8,
                              solidHeader = F,
                              status = "info",
                              numericInput("lag_max_sc",
                                           label = "Select the Maximum Lag to Plot",
                                           value = 12,
                                           min = 1,
                                           step = 1,
                                           width = '20%'),
                              plotOutput("acf_plot_sc")
                     )#end of tabPanel
                   )#end of tabBox
                 )#end of conditionalPanel
             )# end of box
            )#end of column
          )#end of conditionalPanel
        )#end of fluidPage
      ),#end of tabItem
##  -----  Deprecated  -----
## exclude the uncertainty calculation
      # tabItem(tabName = "uncertEstimation_sc",
      #   fluidPage(
      #     conditionalPanel(condition = "input.Train_go_sc || input.next_init_sc_lo",
      #       fluidRow(
      #         column(width = 8,
      #           box(title = "Fractional savings uncertainty for a given fractional savings",
      #               width = 12,
      #               solidHeader = T,
      #               status = "info",
      #              box(title = " ",
      #                  width =3,
      #                  solidHeader = F,
      #                  status = "info",
      #                  numericInput("Frac_Sav_sc",
      #                               "Provide the Expected Fractional Savings (in %)",
      #                               5, min = 0, max = 100, step =1,
      #                               width = NULL),
      #                  br(),
      #                  numericInput("d_post_sc",
      #                               "Provide The Expected Length Of The Post Period (in days)",
      #                               365, min = 0, max = 3650, step =1,
      #                               width = NULL),
      #                  actionButton("fsu_est_go_sc",
      #                               label = "Estimate",
      #                               width ="100%")
      #              ),# end of box
      #              conditionalPanel(condition = "input.fsu_est_go_sc",
      #                box(title = "",
      #                    width = 9,
      #                    solidHeader = F,
      #                    status = "info",
      #                    dataTableOutput("fsu_est_tab_sc")
      #                )# end of box
      #              ) #end of conditionalPanel
      #           )# end of box
      #         )#end of column
      #       )#end of fluidRow
      #     )#end of conditionalPanel
      #   )#end of fluidPage
      # ),#end of tabItem

      tabItem(tabName = "summary_sc",
        fluidPage(
          conditionalPanel(condition = "input.Train_go_sc || input.next_init_sc_lo",
            column(width = 4),
            column(width = 4,
              box(title = "Choose the Building",
                  width = 12,
                  solidHeader = T,
                  status ="info",
                  selectInput('Res_summ_sc', 'Data', ""),
                  actionButton("summary_go_sc",
                               label = "Summarize",
                               width ="100%")
              )# end of box
            ),#end of column
            column(width = 4),
            column(width = 12,
                   valueBoxOutput("R2BoxSc",width = 4),
                   valueBoxOutput("CVRMSEBoxSc",width = 4),
                   valueBoxOutput("NMBEBoxSc",width = 4)
            ),#end column
            column(width = 2),
            column(width = 8,
              box("Pre-Installation",
                  width =12,
                  solidHeader = F,
                  status = "info",
                  dygraphs::dygraphOutput("pre_ts_plot_mod_sc_2")
              )#end of box
            ),#end column
            column(width = 2)
          )#end of conditionalPanel
        )#end of fluidPage
      ),#end of tabItem

#===============================================================================
#                              Savings Analysis                                #
#===============================================================================

      tabItem(tabName = "visuProject_sav",
        fluidPage(
          conditionalPanel("input.next_init_sav || input.next_init_sav_lo",
            column(width = 12,
              box(title = "Pre-Installation data summary",
                  width =6,
                  solidHeader = T,
                  status = "info",
                  dataTableOutput("pre_summary_tab_sav"),
                  downloadButton("pre_summary_tab_sav_dl", "Download")
              ),# end of box
              box(title = "Post-Installation data summary",
                  width = 6,
                  solidHeader = T,
                  status = "info",
                  dataTableOutput("post_summary_tab_sav"),
                  downloadButton("post_summary_tab_sav_dl", "Download")
              )# end of box
            ),#end of column
            column(width = 4),
            column(width = 4,
              box(title = "Choose Data to Visualize",
                  width =12,
                  solidHeader = T,
                  status = "info",
                  selectInput("Data_vis_sav",
                              "Data", ""),
                  actionButton("plot_go_sav",
                               label = "Plot",
                               width ="100%")
              )# end of box
            ),#end of column
            column(width = 12,
              conditionalPanel(condition = "input.plot_go_sav",
                tabBox(title = tagList(shiny::icon("line-chart"),
                                       "Pre-Installation plots"),
                       width = 6,
                  tabPanel("Time Serie Pre-Installation",
                           width =12,
                           solidHeader = F,
                           status = "info",
                           dygraphs::dygraphOutput("ts_plot_pre_sav")
                  ),#end of tabPanel
                  tabPanel("Scatter plot eload Vs. input",
                           width =12,
                           solidHeader = F,
                           status = "info",
                           uiOutput('radio_plot_pre_sav'),
                           plotOutput("scatter_plot_pre_sav")
                  ),#end of tabPanel
                  tabPanel("Heatmap",
                           width =6,
                           solidHeader = F,
                           status = "info",
                           plotly::plotlyOutput("heatmap_pre_sav",
                                        height = "auto",
                                        width = "auto")
                  )#end of tabPanel
                )#end of tabBox
              ),#end of conditionalPanel
              conditionalPanel(condition = "input.plot_go_sav",
                tabBox(title = tagList(shiny::icon("line-chart"),
                                       "Post-Installation plots"),
                       width = 6,
                  tabPanel("Time Serie Post-Installation",
                           width =12,
                           solidHeader = F,
                           status = "info",
                           dygraphs::dygraphOutput("ts_plot_post_sav")
                  ),#end of tabPanel
                  tabPanel("Scatter plot eload Vs. input",
                           width =12,
                           solidHeader = F,
                           status = "info",
                           uiOutput('radio_plot_post_sav'),
                           plotOutput("scatter_plot_post_sav")
                  ),#end of tabPanel
                  tabPanel("Heatmap",
                           width =6,
                           solidHeader = F,
                           status = "info",
                           plotly::plotlyOutput("heatmap_post_sav",
                                        height = "auto",
                                        width = "auto")
                  )#end of tabPanel
                )#end of tabBox
              )#end of conditionalPanel
            )#end of column
          )# end of conditional panel
        )# end of fluidPage
      ),#end of tabItem

      tabItem(tabName = "trainModels_sav",
        fluidPage(
          conditionalPanel(condition = "input.next_init_sav",
            fluidRow(
              column(width = 12,
                box(title = "Model Set Up",
                    width = 12,
                    solidHeader = T,
                    status ="info",
                   box(title = "Select the Baseline Model",
                       width =3,
                       solidHeader = F,
                       status = "info",
                       selectInput('Model_sav',
                                   'Model',
                                   c("TOWT","GBM")),
                       conditionalPanel(condition = "input.Model_sav == 'GBM'",
                         radioButtons("d_off_GBM_sav",
                                      "Include Days Off as independent variable:",
                                      c("No" = "no_sav",
                                        "Default US Holidays
                                        (from 2007 to 2017)" = "def_d_off_sav",
                                        "Select a new Days Off file" = "yes_sav")),
                         conditionalPanel(condition = "input.d_off_GBM_sav == 'yes_sav'",
                           fileInput("d_off_path_sav",
                                     "Choose Days Off CSV File",
                                     accept = ".csv")
                         )# end of conditionalPanel
                       ),# end of conditionalPanel
                       actionButton("Train_go_sav",
                                    label = "Train the Baseline Model",
                                    width ="100%")
                   ),# end of box
                   box(title = "Baseline Model Description",
                       width = 5,
                       solidHeader = F,
                       status = "info",
                       h3(textOutput("Model_Name_sav")),
                       textOutput("Model_Desc_sav")
                   ),#end of box
                   conditionalPanel(condition = "input.Model_sav == 'TOWT'",
                     box(title = "TOWT Hyper-parameter Setup",
                         width = 4,
                         solidHeader = F,
                         status = "info",
                         sliderInput("timescaleDays_sav",
                                     "timescale for weighting function (in days):",
                                     min = 1,
                                     max = 90,
                                     value = 15,
                                     step = 1)
                     )#end of box
                   ),# end of conditionalPanel
                   conditionalPanel(condition = "input.Model_sav == 'GBM'",
                     box(title = "GBM Hyper-parameters Setup",
                         width = 4,
                         solidHeader = F,
                         status = "info",
                         sliderInput("ncores_sav",
                                     "number of cores (parallelization):",
                                     min = 1,
                                     max = parallel::detectCores(logical =F),
                                     value = parallel::detectCores(logical =F),
                                     step = 1),
                         sliderInput("kfolds_sav",
                                     "number of cross validations (k-folds):",
                                     min = 3, max = 10,
                                     value = 5, step = 1),
                         sliderInput("gbm_iter_sav",
                                     "number of GBM iterations:",
                                     min = 50, max = 1000,
                                     value = c(50,500), step = 50),
                          sliderInput("depth_sav",
                                      "depth of trees:",
                                      min = 1, max = 10,
                                      value = c(3,7), step = 1),
                          checkboxGroupInput("lr_sav",
                                             "learning rates:",
                                             choices = c("0.5" = "0.5",
                                                         "0.1" = "0.1",
                                                         "0.05" = "0.05",
                                                         "0.01" = "0.01",
                                                         "0.005" = "0.005",
                                                         "0.001" = "0.001"),
                                              selected = "0.05")

                     )#end of box
                   )# end of conditionalPanel
                )#end of box
              )# end of column
            ),# end of fluidRow
            fluidRow(
              column(width = 12,
                column(width = 4),#end of column
                column(width = 4,
                       infoBoxOutput("trainEndBox_sav",width = 12)
                ),#end of column
                column(width = 4)#end of column
              )# end of column
            )# end of fluidRow
          )# end of conditionalPanel
        )#end of fluidPage
      ),#end of tabItem

      tabItem(tabName = "resBaseline_sav",
        fluidPage(
          conditionalPanel(condition = "input.Train_go_sav || input.next_init_sav_lo",
            fluidRow(
              column(width = 6,
                box(title = "Goodness of Fit",
                    width = 12,
                    solidHeader = T,
                    status = "info",
                    dataTableOutput("model_metrics_tab_sav"),
                    downloadButton("model_metrics_tab_sav_dl", "Download")
                )#end of box
              ),#end of column
              column(width = 3,
                box(title = " Screening Thresholds",
                    width =12,
                    solidHeader = T,
                    status = "info",
                    numericInput("R2_tresh_sav",
                                 "R2 (in %)",
                                 65, min = 0, max = 100, step =1,
                                 width = NULL),
                    br(),
                    numericInput("CVRMSE_tresh_sav",
                                 "CVRMSE (in %)",
                                 25, min = 0, max = 100, step =1,
                                 width = NULL),
                    br(),
                    numericInput("NMBE_tresh_sav",
                                 "NMBE (in %)",
                                 0.5, min = -100, max = 100,
                                 width = NULL),
                    actionButton("screen_go_sav",
                                 label = "screen",
                                 width ="100%")
                )# end of box
              ),#end of column
              column(width = 3,
                box(title ="Screening results",
                    width =12,
                    solidHeader = T,
                    status = "info",
                    plotly::plotlyOutput("pie_plot_screen_sav",
                                 width = "auto",
                                 height = "auto")
                )#end of box
              )#end of column
            ),#end of fluidRow
            column(width = 4),
            column(width = 4,
             box(title = "Choose Data to Visualize",
                 width = 12,
                 solidHeader = T,
                 status = "info",
                 selectInput('Res_vis_sav', 'Data', ""),
                 actionButton("base_plot_go_sav",
                              label = "Plot",
                              width ="100%")
             )# end of box
            ),#end of column
            column(width =4),
            column(width = 12,
              conditionalPanel(condition = "input.base_plot_go_sav",
                tabBox(title = tagList(icon("line-chart"), "Plots Baseline Model Results"),
                       width = 12,
                  tabPanel("Time Serie Plot",
                           width =12,
                           solidHeader = F,
                           status = "info",
                    fluidRow(
                      column(width = 6,
                             dygraphs::dygraphOutput("pre_ts_plot_mod_sav")
                      ),#end of column
                      column(width = 6,
                             dygraphs::dygraphOutput("post_ts_plot_mod_sav")
                     )#end of column
                    )#end of fluidRow
                  ),#end of tabPanel
                  tabPanel("Scatter plot fitting vs. actual",
                           width =12,
                           solidHeader = F,
                           status = "info",
                    fluidRow(
                      column(width = 8,
                             plotOutput("scatter_plot_act_fit_sav")
                      )#end of column
                    )#end of fluidRow
                  ),#end of tabPanel
                  tabPanel("Scatter plot residual vs. input",
                           width =12,
                           solidHeader = F,
                           status = "info",
                    fluidRow(
                      column(width = 8,
                             uiOutput('radio_plot_err_sav'),
                             plotOutput("scatter_plot_err_input_sav")
                      )#end of column
                    )#end of fluidRow
                  ),#end of tabPanel
                  tabPanel("Residual Autocorrelation Plot",
                           width =12,
                           solidHeader = F,
                           status = "info",
                    fluidRow(
                      column(width = 8,
                             numericInput("lag_max_sav",
                                          label = "Select the Maximum Lag to Plot",
                                          value = 12,
                                          min = 1,
                                          step = 1,
                                          width = '20%'),
                             plotOutput("acf_plot_sav")
                      )#end of column
                    )#end of fluidRow
                  )#end of tabPanel
                )#end of tabBox
              )#end of conditionalPanel
            )#end of column
          )#end of conditionalPanel
        )#end of fluidPage
      ),#end of tabItem

      tabItem(tabName = "savAnalysis_sav",
        fluidPage(
          conditionalPanel(condition = "input.Train_go_sav || input.next_init_sav_lo",
            fluidRow(
              column(width = 12,
                box(title = "Savings Estimations",
                    width = 12,
                    solidHeader = T,
                    status ="info",
                   box(title = " ",
                       width =3,
                       solidHeader = F,
                       status = "info",
                       paste("The savings are estimated by subtracting the
                              actual energy consumption from the baseline
                              model predicted energy consumption of the post
                              period."),
                       br(),
                       hr(),
                       ##  -----  Deprecated  -----
                       ## exclude the uncertainty calculation
                       # numericInput("inCL",
                       #              "The Confidence Level at which the Savings Uncertainty will be Estimated (in %)",
                       #              68, min = 0, max = 100, step =1,
                       #              width = NULL),
                       # br(),
                       actionButton("sav_est_go",
                                    label = "Estimate Savings",
                                    width ="100%"),
                       br(),
                       hr(),
                       HTML("<p> <strong> FS:</strong> Fractional Savings </p>")#,
                       #HTML("<p> <strong> FSU:</strong> Fractional Savings Uncertainty</p>")
                   ),# end of box
                   conditionalPanel(condition = "input.sav_est_go",
                                    box(title = "",
                                        width = 9,
                                        solidHeader =F,
                                        status ="info",
                                        dataTableOutput("sav_est_tab"),
                                        downloadButton("sav_est_tab_dl", "Download")
                                    )# end of box
                   ) #end of conditionalPanel
                )# end of box
              )#end of column
            ),#end of fluidRow
            fluidRow(
              conditionalPanel(condition = "input.sav_est_go",
                column(width = 12,
                  box(title ="Savings Results",
                      width =12,
                      solidHeader = F,
                      status = "info",
                      plotly::plotlyOutput("bar_plot_sav_error")
                  )#end of box
                )#end of column
              )#end of conditionalPanel
            ),#end of fluidRow
            fluidRow(
              conditionalPanel(condition = "input.sav_est_go",
                column(width = 12,
                 column(width = 4),
                 column(width = 4,
                  box(title = "Choose Data to Visualize",
                      width = 12,
                      solidHeader = T,
                      status = "info",
                      selectInput('Res_vis_sav_2', 'Data', ""),
                      actionButton("plot_gran_go_sav",
                                   label = "Plot",
                                   width ="100%")
                  )# end of box
                 ),#end of column
                 column(width =4)
                ),# end of column
                conditionalPanel(condition = "input.plot_gran_go_sav",
                  column(width =2),
                  column(width = 8,
                   tabBox(title = tagList(shiny::icon("line-chart"),
                                          "Savings plots"),
                          width = 12,
                     tabPanel("Savings Time Serie",
                              width =12,
                              solidHeader = F,
                              status = "info",
                              radioButtons(inputId = 'Plot_gran_sav',
                                           label = 'Select Time Granularity',
                                           choices = c("Original Granularity",
                                                       "Daily",
                                                       "Weekly",
                                                       "Monthly"),
                                           selected = "Original Granularity",
                                           inline = T),
                              dygraphs::dygraphOutput("ts_savings_plot_sav")
                     ),#end of tabPanel
                     tabPanel("Savings CUSUM Plot",
                              width =12,
                              solidHeader = F,
                              status = "info",
                              radioButtons(inputId = 'Plot_cusum_gran_sav',
                                           label = 'Select Time Granularity',
                                           choices = c("Original Granularity",
                                                       "Daily",
                                                       "Weekly",
                                                       "Monthly"),
                                           selected = "Original Granularity",
                                           inline = T),
                              dygraphs::dygraphOutput("ts_cusum_plot_sav")
                     ),#end of tabPanel
                     tabPanel("Savings Heatmap Plot",
                              width =6,
                              solidHeader = F,
                              status = "info",
                              plotly::plotlyOutput("heatmap_savings_sav",
                                           height = "auto",
                                           width = "auto")
                     )#end of tabPanel
                   )#end of tabBox
                  ),#end of column
                  column(width =2)
                )#end of conditionalPanel
              )#end of conditionalPanel
            )# end of fluidRow
          )#end of conditionalPanel
        )#end of fluidPage
      ),#end of tabItem
      ##  -----  Deprecated  -----
      # tabItem(tabName = "visSavAnalysis_sav",
      #   fluidPage(
      #     conditionalPanel(condition = "input.Train_go_sav || input.next_init_sav_lo",
      #       column(width = 3,
      #         box(title = "Plots Settings",
      #             width = 12,
      #             solidHeader = T,
      #             status ="info",
      #            box(title = "Choose Data to Visualize",
      #                width =12,
      #                solidHeader = F,
      #                status = "info",
      #                selectInput('Res_vis_sav_2', 'Data', "")
      #            ),# end of box
      #            box(title = "Choose Time Horizon",
      #                width =12,
      #                solidHeader = F,
      #                status = "info",
      #                selectInput('Plot_gran_sav',
      #                            'Model',
      #                            c("Original Granularity",
      #                              "daily",
      #                              "weekly",
      #                              "monthly"))
      #            ),# end of box
      #             actionButton("plot_gran_go_sav",
      #                          label = "Plot",
      #                          width ="100%")
      #         )#end of box
      #       ),#end of column
      #       conditionalPanel(condition = "input.plot_gran_go_sav",
      #         column(width = 9,
      #           box(title = "Plots",
      #               width = 12,
      #               solidHeader = T,
      #               status ="info",
      #              box(title = "Post Period Plot",
      #                  width = 12,
      #                  solidHeather = F,
      #                  status = "info",
      #                  dygraphs::dygraphOutput("ts_post_plot_sav")
      #              ),#end of box
      #              box(title = "Savings Heatmap Plot",
      #                  width = 12,
      #                  solidHeather = F,
      #                  status = "info",
      #                  plotly::plotlyOutput("heatmap_savings_sav",
      #                               height = "auto",
      #                               width = "auto")
      #              ),#end of box
      #              box(title = "Plot of the Difference Between the Actual and the Prediction",
      #                  width = 12,
      #                  solidHeather = F,
      #                  status = "info",
      #                  dygraphs::dygraphOutput("ts_savings_plot_sav")
      #              ),#end of box
      #              box(title = "CUSUM Plot",
      #                  width = 12,
      #                  solidHeather = F,
      #                  status = "info",
      #                  dygraphs::dygraphOutput("ts_cusum_plot_sav")
      #              )#end of box
      #           )#end of box
      #         )#end of column
      #       )#end of conditionalPanel
      #     )#end of conditionalPanel
      #   )#end of fluidPage
      # ),#end of tabItem

      tabItem(tabName = "nrEvents_sav",
        fluidPage(
          conditionalPanel(condition = "input.Train_go_sav && input.sav_est_go",
            fluidRow(
              column(width = 12,
                box(title = "Potential Non-Routine Events Identification",
                    width = 12,
                    solidHeader = T,
                    status ="info",
                   box(title = " ",
                       width =3,
                       solidHeader = F,
                       status = "info",
                       paste("The Potential Non-Routine Events are identified using
                              statistical multiple change points algorithm.
                              Change points are considered to be the points
                              in the time series where a change in the
                              statistical properties (i.e. change in mean
                              and/or variance) is observed."),
                       br(),
                       hr(),
                       actionButton("nre_go_sav",
                                    label = "Check for Potential Non-Routine Events",
                                    width ="100%"),
                       br(),
                       hr(),
                       HTML("<p> <strong> cpts:</strong> Number of detected change points </p>")#,
                   ),# end of box
                   conditionalPanel(condition = "input.nre_go_sav",
                                    box(title = "",
                                        width = 9,
                                        solidHeader =F,
                                        status ="info",
                                        dataTableOutput("nre_test_tab_sav"),
                                        downloadButton("nre_test_tab_sav_dl", "Download")
                                    )# end of box
                   ) #end of conditionalPanel
                )# end of box
              )#end of column
            ),#end of fluidRow
            conditionalPanel(condition = "input.nre_go_sav",
              fluidRow(
                column(width = 12,
                  column(width = 4),#end of column
                  column(width = 4,
                         infoBoxOutput("nreEndBox_sav",width = 12)
                  ),#end of column
                  column(width = 4)#end of column
                )# end of column
              ),# end of fluidRow
              fluidRow(
                column(width = 12,
                 column(width = 4),
                 column(width = 4,
                  box(title = "Choose Data to Visualize",
                      width = 12,
                      solidHeader = T,
                      status = "info",
                      selectInput('nre_vis_sav', 'Data', ""),
                      actionButton("nre_plot_go_sav",
                                   label = "Plot",
                                   width ="100%")
                  )# end of box
                 ),#end of column
                 column(width =4)
                ),# end of column
                conditionalPanel(condition = "input.nre_plot_go_sav",
                  column(width =2),
                  column(width = 8,
                    box(title = "NRE Plots",
                        width = 12,
                        solidHeader = T,
                        status ="info",
                        dygraphs::dygraphOutput("nre_ts_plot_sav")
                    )#end of box
                  ),#end of column
                  column(width =2)
                )#end of conditionalPanel
              )# end of fluidRow
            )#end of conditionalPanel

          )#end of conditionalPanel
        )#end of fluidPage
      ),#end of tabItem

      tabItem(tabName = "nrEvents_sav_lo",
        fluidPage(
          conditionalPanel(condition = "input.next_init_sav_lo && input.sav_est_go",
            fluidRow(
              column(width = 12,
                box(title = "Potential Non-Routine Events Identification",
                    width = 12,
                    solidHeader = T,
                    status ="info",
                    box(title = " ",
                        width =3,
                        solidHeader = F,
                        status = "info",
                        paste("The Potential Non-Routine Events are identified using
                               statistical multiple change points algorithm.
                               Change points are considered to be the points
                               in the time series where a change in the
                               statistical properties (i.e. change in mean
                               and/or variance) is observed."),
                        br(),
                        hr(),
                        actionButton("nre_go_sav_lo",
                                     label = "Show Potential Non-Routine Events Identification Results",
                                     width ="100%"),
                        br(),
                        hr(),
                        HTML("<p> <strong> cpts:</strong> Number of detected change points </p>")#,

                    ),# end of box
                    conditionalPanel(condition = "input.nre_go_sav_lo",
                                     box(title = "",
                                         width = 9,
                                         solidHeader =F,
                                         status ="info",
                                         dataTableOutput("nre_test_tab_sav_lo"),
                                         downloadButton("nre_test_tab_sav_lo_dl", "Download")
                                     )# end of box
                    ) #end of conditionalPanel
                 )# end of box
              )#end of column
            ),#end of fluidRow
            conditionalPanel(condition = "input.nre_go_sav_lo",
              fluidRow(
                column(width = 12,
                  column(width = 4),#end of column
                  column(width = 4,
                         infoBoxOutput("nreEndBox_sav_lo",width = 12)
                  ),#end of column
                  column(width = 4)#end of column
                )# end of column
              ),# end of fluidRow
              fluidRow(
                column(width = 12,
                 column(width = 4),
                 column(width = 4,
                  box(title = "Choose Data to Visualize",
                      width = 12,
                      solidHeader = T,
                      status = "info",
                      selectInput('nre_vis_sav_lo', 'Data', ""),
                      actionButton("nre_plot_go_sav_lo",
                                   label = "Plot",
                                   width ="100%")
                  )# end of box
                 ),#end of column
                 column(width =4)
                ),# end of column
                conditionalPanel(condition = "input.nre_plot_go_sav_lo",
                  column(width =2),
                  column(width = 8,
                    box(title = "NRE Plots",
                        width = 12,
                        solidHeader = T,
                        status ="info",
                        dygraphs::dygraphOutput("nre_ts_plot_sav_lo")
                    )#end of box
                  ),#end of column
                  column(width =2)
                )#end of conditionalPanel
              )# end of fluidRow
            ) #end of conditionalPanel
          )#end of conditionalPanel
        )#end of fluidPage
      ),#end of tabItem

      tabItem(tabName = "summary_sav",
        fluidPage(
          conditionalPanel(condition = "input.Train_go_sav || input.next_init_sav_lo",
            column(width = 3),
            column(width = 6,
              box(title = "Summary setup",
                  width = 12,
                  solidHeader = T,
                  status ="info",
                 box(title = "Summary level",
                     width =6,
                     solidHeader = F,
                     status = "info",
                     radioButtons("type_summ",
                                  label = h3(""),
                                  choices = list("Portfolio" = 1,
                                                 "Building" = 2),
                                  selected = 1)
                 ),# end of box
                 conditionalPanel(condition = "input.type_summ == 1",
                   box(title = "Building to include in the summary",
                       width =6,
                       solidHeader = F,
                       status = "info",
                       radioButtons("level_summ",
                                    label = h3(""),
                                    choices = list("All" = 1,
                                                   "Screened" = 2),
                                    selected = 1)
                   )# end of box
                 ), #end of conditionalPanel
                 conditionalPanel(condition = "input.type_summ == 2",
                   box(title = "Choose the Building",
                       width =6,
                       solidHeader = F,
                       status = "info",
                       selectInput('Res_summ_sav', 'Data', "")
                   )# end of box
                 ), #end of conditionalPanel
                 column(width = 12,
                   column(width = 4),
                   column(width = 4,
                     actionButton("summary_go_sav",
                                  label = "Summarize",
                                  width ="100%")
                   ),
                   column(width = 4)
                 )#en column
              )# end of box
            ),#end of column
            column(width = 3),
            conditionalPanel(condition = "input.type_summ == 2 && input.summary_go_sav",
              column(width = 12,
                     valueBoxOutput("R2BoxSav",width = 4),
                     valueBoxOutput("CVRMSEBoxSav",width = 4),
                     valueBoxOutput("NMBEBoxSav",width = 4),
                     ##  -----  Deprecated  -----
                     ## exclude the uncertainty calculation
                     # valueBoxOutput("FsuBoxSav",width = 3),
                     box("Pre-Installation",
                         width =6,
                         solidHeader = F,
                         status = "info",
                         dygraphs::dygraphOutput("pre_ts_plot_mod_sav_2")
                     ),#end of box
                     box("Post-Installation",
                         width =6,
                         solidHeader = F,
                         status = "info",
                         dygraphs::dygraphOutput("post_ts_plot_mod_sav_2")
                     ),#end of box
                     conditionalPanel(condition = "input.sav_est_go",
                       valueBoxOutput("SavingsBoxSav",width = 6),
                       valueBoxOutput("FsBoxSav",width = 6)
                     )
                     ##  -----  Deprecated  -----
                     ## exclude the uncertainty calculation
                     # valueBoxOutput("SavRangeBoxSav",width = 6),
                     # valueBoxOutput("FsRangeBoxSav",width = 6)
              ),#end column
              column(width = 3),
              column(width = 6,
                     conditionalPanel(condition = "input.nre_go_sav || input.nre_go_sav_lo",
                       valueBoxOutput("NreBoxSav",width = 12)
                     )
              ),#end column
              column(width = 3)
            ),#end of conditionalPanel
            conditionalPanel(condition = "input.type_summ == 1 &&
                                          input.level_summ == 1 &&
                                          input.summary_go_sav",
              column(width = 12,
                     ##  -----  Deprecated  -----
                     ## exclude the uncertainty calculation
                     # column(width = 4),
                     # valueBoxOutput("PortFsuBoxSav",width = 4),
                     # column(width = 4),
                     box("Savings Results",
                         width =12,
                         solidHeader = F,
                         status = "info",
                         plotly::plotlyOutput("bar_plot_sav_error_2")
                     ),#end of box
                     conditionalPanel(condition = "input.sav_est_go",
                       valueBoxOutput("PortSavingsBoxSav",width = 6),
                       valueBoxOutput("PortFsBoxSav",width = 6)
                     )
                     ##  -----  Deprecated  -----
                     ## exclude the uncertainty calculation
                     # valueBoxOutput("PortSavRangeBoxSav",width = 6),
                     # valueBoxOutput("PortFsRangeBoxSav",width = 6)
              )#end column
            ),#end of conditionalPanel
            conditionalPanel(condition = "input.type_summ == 1 &&
                                          input.level_summ == 2 &&
                                          input.summary_go_sav",
              column(width = 12,
                     ##  -----  Deprecated  -----
                     ## exclude the uncertainty calculation
                     # column(width = 4),
                     # valueBoxOutput("PortFsuBoxSav_2",width = 4),
                     # column(width = 4),
                     box("Savings Results",
                         width =12,
                         solidHeader = F,
                         status = "info",
                         plotly::plotlyOutput("bar_plot_sav_error_3")
                     ),#end of box
                     conditionalPanel(condition = "input.sav_est_go",
                       valueBoxOutput("PortSavingsBoxSav_2",width = 6),
                       valueBoxOutput("PortFsBoxSav_2",width = 6)
                     )
                     ##  -----  Deprecated  -----
                     ## exclude the uncertainty calculation
                     # valueBoxOutput("PortSavRangeBoxSav_2",width = 6),
                     # valueBoxOutput("PortFsRangeBoxSav_2",width = 6)
              )#end column
            )#end of conditionalPanel
          )#end of conditionalPanel
        )#end of fluidPage
      )#end of tabItem
    )# end of tabItems
  )# end of dashboardBody
 )#end of dashboardPage
