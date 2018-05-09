#-------------------------------------------------------------------------------
# RMV2.0 (version 1.1.0)
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
#-------------------------------------------------------------------------------

# change_box <- function(box, success_condition) {
#   browser()
#     selector <- paste0("$('#",box,"').parent()")
#     if (success_condition) {
#       shinyjs::removeClass(
#         selector = selector, class = "box-warning")
#       shinyjs::addClass(
#         selector = selector, class = "box-success")
#     } else {
#       shinyjs::removeClass(
#         selector = selector, class = "box-success")
#       shinyjs::addClass(
#         selector = selector, class = "box-warning")
#     }
# }

#===============================================================================
#                               SHINY_SERVER                                   #
#===============================================================================
shinyServer(function(input, output, session) {
  #=============================================================================
  #                        Menu SideBar                                        #
  #=============================================================================

  output$save_sc_Ui <- renderUI({
    actionButton("save_sc",
                  label = "Save Project",
                  icon = icon("floppy-o"),
                  width='85%')
  })

  output$save_pred_sc_Ui <- renderUI({
    actionButton("save_pred_sc",
                  label = "Save Predictions",
                  icon = icon("floppy-o"),
                  width='60%')
  })

  output$save_sav_Ui <- renderUI({
      actionButton("save_sav",
                  label = "Save Project",
                  icon = icon("floppy-o"),
                  width='85%')
  })

  output$save_pred_sav_Ui <- renderUI({
      actionButton("save_pred_sav",
                  label = "Save Predictions",
                  icon = icon("floppy-o"),
                  width='60%')
  })

  #=============================================================================
  #                        Screening Server Functions  New Project             #
  #=============================================================================

  # Set the reactive var where the different variables will be stored
  screen_out <- reactiveValues()


  ########################  Create a new Project ###############################

  # set the project name
  p_name_sc <- renderText({input$p_name_sc})
  screen_out$p_name_sc <- p_name_sc

  observe({change_box("save_dir_sc_box", !(is.null(input$save_dir_sc)))})

  # set the directory where the project will be stored
  shinyDirChoose(input,
                 'save_dir_sc',
                 roots = volumes,
                 filetypes = c('', 'csv'))
  save_dir_sc <- reactive(input$save_dir_sc)
  path_save_dir_sc <- reactive({
    home <- normalizePath("~")
    file.path(volumes[save_dir_sc()$root], paste(unlist(save_dir_sc()$path[-1]),
              collapse = .Platform$file.sep))
  })
  output$save_dir_sc_out <- renderText({basename(path_save_dir_sc())})

  ########################  Set Pre-instatllation Data ###########################

  observe({change_box("pre_dir_sc_box", !(is.null(input$pre_dir_sc)))})

  observe({
    if (input$next_init_1!=0 & input$type_init == 1 & input$new_init == 1){
      # set the directory from where the pre data will be read
      shinyDirChoose(input,
                     'pre_dir_sc',
                     roots = volumes,
                     filetypes = c('csv'))
      # define home directory
      home <- normalizePath("~")
      # get the pre data directory paths
      screen_out$pre_dir_sc <- file.path(volumes[input$pre_dir_sc$root],
                                         paste(unlist(input$pre_dir_sc$path[-1]),
                                         collapse = .Platform$file.sep))
      # get the pre data files paths
      screen_out$files_path_sc <- list.files(screen_out$pre_dir_sc,
                                             "*\\.csv",
                                             full.names = T,
                                             include.dirs =F)
      # get the pre data files names
      screen_out$files_names <- list.files(screen_out$pre_dir_sc,
                                              "*\\.csv",
                                              full.names = F,
                                              include.dirs =F)
      #Render the pre path
      output$pre_dir_sc_out <- renderText({basename(screen_out$pre_dir_sc)})
    }
  })

  output$fields_sc_new <- reactive(!(is.null(input$save_dir_sc)) &&
                                    !(is.null(input$pre_dir_sc)))
  outputOptions(output, "fields_sc_new", suspendWhenHidden = FALSE)


  # Extract the summary of the pre data files
  observeEvent(input$next_init_sc,{
    if (length(screen_out$files_names)!=0){
      screen_out <- data_load(screen_out$files_path_sc,
                              screen_out$files_names,
                              screen_out, Post =F, clean = T)
      screen_out$Data_pre_summary <- screen_out$Data_pre_summary_0[,c(1:6,9)]
      output$intEndBox_sc <- renderInfoBox({
        infoBox(h4("The project setup is completed"),
                "Proceed to the screening analysis",
                icon = icon("thumbs-o-up"),
                fill = T,
                color = "navy")
      })
    }
    else{
      output$intEndBox_sc <- renderInfoBox({
        infoBox(h4("No csv files found"),
                "Please select a valid directory",
                icon = icon("times-circle-o"),
                fill = T,
                color = "red")
      })
    }
  })

  #Render the summary table
  output$pre_summary_tab_sc <- renderDataTable({
    summary_tab <- screen_out$Data_pre_summary
    return(summary_tab)
  }, options = list(pageLength = 5))

  #Download the table
  output$pre_summary_tab_sc_dl <- downloadHandler(
    filename = function () {
      paste0(input$p_name_sc, "_pre_summary", ".csv")
    },
    content = function(file) {
      write.csv(screen_out$Data_pre_summary, file, row.names = FALSE)
    }
  )

  # output$pre_summary_tab_sc <- DT::renderDataTable(
  #  Data_pre_summary(), extensions = 'Buttons',
  #                         options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf'))
  # )

  ###########################  Data Visualization #############################

  #update the pre-data files names for the visualization
  observe({
    updateSelectInput(session,
                      "Data_vis_sc",
                      choices = screen_out$files_names)
  })

  # plot time serie for the selected building
  observeEvent(input$pre_plot_go_sc,{
    Data_list <- screen_out$Data_pre
    pre_name <- input$Data_vis_sc
    Data <- Data_list[[pre_name]]
    variables_i <- c(c("Temp"),
                     names(Data)[names(Data) %nin%
                                     c("time","eload","Temp")])
    output$radio_plot_pre_sc <- renderUI({
      radioButtons(inputId = 'var_pre_sc',
                   label = 'Select the Input Variable to Plot',
                   choices = variables_i,
                   selected = "Temp",
                   inline = T)
    })
    # Render the Time series plot
    output$ts_plot_pre_sc <- dygraphs::renderDygraph({
     p <- act_plot(Data)
     return(p)
    })

    # plot eload vs Temp for the selected building
    output$scatter_plot_pre_sc <- renderPlot({
     p <- eload_vs_input_plot(Data, input$var_pre_sc)
     return(p)
     })

    # heatmap for the selected building
    output$heatmap_pre_sc <- plotly::renderPlotly({
     p <- eload_heatmap(Data)
     return(p)
     })
  })




 ###########################  Train Baseline Models ############################

 # generate UI corresponding to the baseline Models
 output$Model_Desc_sc <- renderText({
    switch(input$Model_sc,
      "TOWT" = paste("TOWT is a piecewise linear model and where the predicted
      energy consumption is a combination of two terms that relate the energy
      consumption to the time of the week and the piecewise-continuous effect
      of the temperature. Each time of the week has a different predicted energy
      consumption, and the temperature effect is estimated separately
      for periods of the day with high and low energy consumption in order to
      capture the pattern for occupied and unoccupied building periods."),
      "GBM" = paste("The GBM baseline model is based on the Gradient Boosting
      Machine (GBM) algorithm that is an ensemble trees based machine learning
      method. The GBM generate a model of the energy consumption using time and
      temperature as independent vaiables. However, he practical advantage of using
      the GBM model, in comparison to TOWT model is that it is capable of handle
      additional independent variables, such as holidays indicator, humidity,
      or solar radiation. GBM model has several hyper-parameters that needs to be
      tuned in order to produce an accurate model. These parameters are tuned
      automaticaly using a search grid and a k-folds cross validation procedure.
      It is possible to change the definition of the search grid using the
      Hyper-parameters Setup tab, however for a user that is not familiar with these
      hyper-paprameters, we advise using the default values.")
    )
   })
 output$Model_Name_sc <- renderText({
    switch(input$Model_sc,
      "TOWT" = paste("Time-of-Week-and-Temperature model"),
      "GBM" = paste("Gradient Boosting Machine model")

    )
 })

 # Train the baseline model and format the results
 observeEvent(input$Train_go_sc,{
   switch(input$Model_sc,
     "TOWT" = pam_list <- list(timescaleDays = input$timescaleDays_sc),
     "GBM" = pam_list <- list(k_folds = input$kfolds_sc,
                              ncores = input$ncores_sc,
                              iter = input$gbm_iter_sc,
                              depth = input$depth_sc,
                              lr = input$lr_sc)
   )
   if (input$Model_sc == "GBM"){
     switch(input$d_off_GBM_sc,
       "no_sc" = days_off_path <- NULL,
       "def_d_off_sc" = days_off_path <- system.file("extdata",
                                                     "USA_Fed_Holidays.csv",
                                                      package = "RMV2.0"),
       "yes_sc" = {path_obj <- input$d_off_path_sc
                   days_off_path <- path_obj$datapath}
     )
   }
   res_base <- suppressWarnings(train_model(screen_out, screen = T,
                                            Model = input$Model_sc,
                                            pam_list = pam_list,
                                            days_off_path = days_off_path))
   if (length(res_base$failures)==0){
     screen_out$files_names_mod <- screen_out$files_names
     models_list <- res_base$res_list
     results_summary <- train_model_summary(models_list,
                                            screen_out$files_names)
     screen_out$model_obj_list <- list(models_list = models_list,
                                       results_summary = results_summary)
     output$trainEndBox_sc <- renderInfoBox({
       infoBox(h4("The Baseline models training is completed"),
               "No failures reported",
               icon = icon("thumbs-o-up"),
               fill = T,
               color = "navy")
     })
   }
   else{
     failures <- screen_out$Data_pre_summary[res_base$failures,1]
     screen_out$files_names_mod <- screen_out$files_names[screen_out$files_names %nin% failures]
     models_list <- res_base$res_list
     results_summary <- train_model_summary(models_list,
                                            screen_out$files_names_mod)
     screen_out$model_obj_list <- list(models_list = models_list,
                                       results_summary = results_summary)
     output$trainEndBox_sc <- renderInfoBox({
       infoBox(h4("Baseline modeling failed for:"),
               paste0(failures, collapse = "; "),
               icon = icon("times-circle-o"),
               fill = T,
               color = "red")
     })
   }
   screen_out$Model <- input$Model_sc
 })


 ###########################  Visualize Baseline Results #######################

 #Extract and analyse the pre-installation data files
 output$model_metrics_tab_sc <- renderDataTable({
   results_obj <- screen_out$model_obj_list
   summary_tab <- results_obj$results_summary
   return(summary_tab)
 }, options = list(pageLength = 5))

 #Download the table
 output$model_metrics_tab_sc_dl <- downloadHandler(
   filename = function () {
     paste0(input$p_name_sc, "_model_metrics", ".csv")
   },
   content = function(file) {
     write.csv(screen_out$model_obj_list$results_summary,
       file, row.names = FALSE)
   }
 )

 #Perform the Screening
 pie_plot_screen_sc <- eventReactive(input$screen_go_sc,{
   results_obj <- screen_out$model_obj_list
   summary_tab <- results_obj$results_summary
   screen_summary_list <- screen_summary(summary_tab,
                                         input$R2_tresh_sc,
                                         input$CVRMSE_tresh_sc,
                                         input$NMBE_tresh_sc)
   p <- screen_pie_plot(screen_summary_list)
   return(p)
 })

 output$pie_plot_screen_sc <- plotly::renderPlotly({
  p <- pie_plot_screen_sc()
  return(p)
 })

 # update the pre-installation files names for the visualization
 observe({
   updateSelectInput(session, "Res_vis_sc",choices = screen_out$files_names_mod)
 })

 observeEvent(input$base_plot_go_sc,{
   name_i <- input$Res_vis_sc

   # Pre data
   pre_Data_list <- screen_out$Data_pre
   pre_Data <- pre_Data_list[[name_i]]

   # Data variables
   if (screen_out$Model == "GBM"){
     variables_i <- c(c("Temp"),
                      names(pre_Data)[names(pre_Data) %nin%
                                      c("time","eload","Temp")])
   }
   if (screen_out$Model == "TOWT"){
     variables_i <- "Temp"
   }
   output$radio_plot_err_sc <- renderUI({
     radioButtons(inputId = 'var_err_sc',
                  label = 'Select the Input Variable to Plot',
                  choices = variables_i,
                  selected = "Temp",
                  inline = T)
   })

   idx_i <-  which(input$Res_vis_sc == screen_out$files_names_mod)
   model_obj_list <- screen_out$model_obj_list
   model_list <- model_obj_list$models_list
   model_obj <- model_list[[idx_i]]

   # plot time serie for the selected building
   output$ts_plot_mod_sc <- dygraphs::renderDygraph({
     p <- pre_plot(model_obj)
     return(p)
   })

   # plot actual vs fit for the selected building
   output$scatter_plot_act_fit_sc <- renderPlot({
     p <- act_vs_fit_plot(model_obj)
     return(p)
   })

   # plot error vs selected input for the selected building
   output$scatter_plot_err_input_sc <- renderPlot({
     p <- errors_vs_input_plot(model_obj, input$var_err_sc)
     return(p)
   })
   # Residual autocorrelation plot for the selected building
   output$acf_plot_sc <- renderPlot({
     p <- acf_plot(model_obj, lag_max = input$lag_max_sc)
     return(p)
   })

 })



 # # plot time serie for the selected building
 # model_obj_to_plot <- eventReactive(input$base_plot_go_sc,{
 #   idx_i <-  which(input$Res_vis_sc == screen_out$files_names_mod)
 #   model_obj_list <- screen_out$model_obj_list
 #   model_list <- model_obj_list$models_list
 #   model_obj <- model_list[[idx_i]]
 #   return(model_obj)
 # })
 #
 # output$ts_plot_mod_sc <- dygraphs::renderDygraph({
 #   p <- pre_plot(model_obj_to_plot())
 #   return(p)
 # })
 #
 # # plot actual vs fit for the selected building
 # output$scatter_plot_act_fit_sc <- renderPlot({
 #   p <- act_vs_fit_plot(model_obj_to_plot())
 #   return(p)
 # })
 #
 # # plot actual vs fit for the selected building
 # output$scatter_plot_err_input_sc <- renderPlot({
 #   p <- errors_vs_input_plot(model_obj_to_plot())
 #   return(p)
 # })

 ##  -----  Deprecated  -----
 ## exclude the uncertainty calculation
 #
 # ###########################  Uncertainty Estimation ###########################
 #
 # # Calculate the uncertainties estimation tab
 # observeEvent(input$fsu_est_go_sc,{
 #   model_obj_list <- screen_out$model_obj_list
 #   model_obj_list <- model_obj_list$models_list
 #   screen_out$fsu_est_tab <- fsu_estimation_sc(screen_out, input$d_post_sc,
 #                                               input$Frac_Sav_sc)
 # })
 #
 # # Render uncertianties estimation tab
 # output$fsu_est_tab_sc <- renderDataTable({
 #   fsu_tab <- screen_out$fsu_est_tab
 #   return(fsu_tab)
 #   }, options = list(pageLength = 5))
 #

 ############################  Results summary ################################

 # update the pre-data files names for the visualization
  observe({
    updateSelectInput(session, "Res_summ_sc",choices = screen_out$files_names_mod)
  })

 observeEvent(input$summary_go_sc,{

     results_obj <- screen_out$model_obj_list
     summary_tab <- results_obj$results_summary
     idx_i <- which(input$Res_summ_sc == summary_tab$Name)
     output$R2BoxSc <- renderValueBox({
       valueBox(
         paste0("R2"),
         h4(strong(paste0(summary_tab$R2[idx_i], "%"))),
         icon = icon("area-chart"),
         color = "aqua"
       )
     })
     output$CVRMSEBoxSc <- renderValueBox({
       valueBox(
         paste0("CV(RMSE)"),
         h4(strong(paste0(summary_tab$CVRMSE[idx_i], "%"))),
         icon = icon("area-chart"),
         color = "aqua"
       )
     })
     output$NMBEBoxSc <- renderValueBox({
       valueBox(
         paste0("NMBE"),
         h4(strong(paste0(summary_tab$NMBE[idx_i], "%"))),
         icon = icon("area-chart"),
         color = "aqua"
       )
     })
     output$pre_ts_plot_mod_sc_2 <- dygraphs::renderDygraph({
       model_obj_list <- screen_out$model_obj_list
       model_list <- model_obj_list$models_list
       model_obj <- model_list[[idx_i]]
       p <- pre_plot(model_obj)
       return(p)
     })
 })

  ########################  Save the new Project ###############################

  observeEvent(input$save_sc,{
   save_session(path_save_dir_sc(),input$p_name_sc,screen_out)

  })

  ########################  Save the predictions ###############################

  observeEvent(input$save_pred_sc,{
   save_predictions(screen_out$model_obj_list,path_save_dir_sc(),post = FALSE)
  })

 #=============================================================================
 #                   Screening Server Functions loaded project                #
 #=============================================================================


 ###########################  Load a Project ##################################

  observe({change_box("load_sc_box", !(is.null(input$load_sc)))})

 # Extract the project file path
 shinyFileChoose(input,
                 'load_sc',
                 roots = volumes,
                 filetypes = c('', 'rds'))

  observeEvent(input$load_sc, {
    load_sc <- parseFilePaths(roots=volumes, input$load_sc)
    screen_out$load_sc <- as.character(load_sc$datapath)
    output$load_sc_out <- renderText({basename(screen_out$load_sc)})
  })

  output$fields_sc_load <- reactive({!(is.null(input$load_sc))})
  outputOptions(output, "fields_sc_load", suspendWhenHidden = FALSE)

  # Finalize the Project loading
  observeEvent(input$next_init_sc_lo,{
    load_res <- load_session(screen_out$load_sc)

    #Populate screen_out variable
    screen_out$p_name_sc <- load_res$p_name_sc
    screen_out$pre_dir_sc <- load_res$pre_dir_sc
    screen_out$files_names <- load_res$files_names
    screen_out$Data_pre <- load_res$Data_pre
    screen_out$Data_pre_summary <- load_res$Data_pre_summary
    screen_out$Data_pre_summary_0 <- load_res$Data_pre_summary_0
    screen_out$Model <- load_res$Model
    screen_out$model_obj_list <- load_res$model_obj_list
    screen_out$files_names_mod <- load_res$files_names_mod
    screen_out$fsu_est_tab <- load_res$fsu_est_tab

    output$intEndBox_sc_lo <- renderInfoBox({
      infoBox(h4("The project is loaded"),
              "Proceed to the screening analysis",
              icon = icon("thumbs-o-up"),
              fill = T, color = "navy")
    })
  })

  #==============================================================================
  #                        Savings Analysis Server Functions                    #
  #==============================================================================

  # Set the reactive var where the different variables will be stored
  sav_out <- reactiveValues()
  sav_out$load <- FALSE
  sav_out$nre_done <- FALSE

  ########################  Create a new Project ###############################

  # set the project name
  p_name_sav <- renderText({input$p_name_sav})
  sav_out$p_name_sav <- p_name_sav

  observe({change_box("save_dir_sav_box", !(is.null(input$save_dir_sav)))})

  # set the directory where the project will be stored
  shinyDirChoose(input,
                 'save_dir_sav',
                 roots = volumes,
                 filetypes = c('', 'csv'))
  save_dir_sav <- reactive(input$save_dir_sav)
  path_save_dir_sav <- reactive({
    home <- normalizePath("~")
    file.path(volumes[save_dir_sav()$root],
              paste(unlist(save_dir_sav()$path[-1]),
              collapse = .Platform$file.sep))
  })
  output$save_dir_sav_out <- renderText({basename(path_save_dir_sav())})

 ####################  Set Pre and Post-instatllation Data #####################
  # Pre
  observe({change_box("pre_dir_sav_box", !(is.null(input$pre_dir_sav)))})

  observe({
    if (input$next_init_1!=0 & input$type_init == 2 & input$new_init == 1){
      # set the directory from where the pre-installation will be read
      shinyDirChoose(input,
                     'pre_dir_sav',
                     roots = volumes,
                     filetypes = c('', 'csv'))
       home <- normalizePath("~")
       # get the pre data directory paths
       sav_out$pre_dir_sav <- file.path(volumes[input$pre_dir_sav$root],
                                        paste(unlist(input$pre_dir_sav$path[-1]),
                                        collapse = .Platform$file.sep))
       # get the pre data files paths
       sav_out$pre_path_sav <- list.files(sav_out$pre_dir_sav,
                                          "*\\.csv",
                                          full.names = T,
                                          include.dirs =F)
       # get the pre data files names
       sav_out$pre_names_sav <- list.files(sav_out$pre_dir_sav,
                                           "*\\.csv",
                                           full.names = F,
                                           include.dirs =F)
       #Render the pre path
       output$pre_dir_sav_out <- renderText({basename(sav_out$pre_dir_sav)})
    }
  })

  observe({change_box("post_dir_sav_box", !(is.null(input$post_dir_sav)))})

  # Post
  observe({
    if (input$next_init_1!=0 & input$type_init == 2 & input$new_init == 1){
      # set the directory from where the post-installation will be read
      shinyDirChoose(input,
                     'post_dir_sav',
                     roots = volumes,
                     filetypes = c('', 'csv'))
       home <- normalizePath("~")
       # get the post data directory paths
       sav_out$post_dir_sav <- file.path(volumes[input$post_dir_sav$root],
                                        paste(unlist(input$post_dir_sav$path[-1]),
                                        collapse = .Platform$file.sep))
       # get the post data files paths
       sav_out$post_path_sav <- list.files(sav_out$post_dir_sav,
                                           "*\\.csv",
                                           full.names = T,
                                           include.dirs =F)
       # get the post data files names
       sav_out$post_names_sav <- list.files(sav_out$post_dir_sav,
                                            "*\\.csv",
                                            full.names = F,
                                            include.dirs =F)
       #Render the post path
       output$post_dir_sav_out <- renderText({basename(sav_out$post_dir_sav)})
    }
  })

  output$fields_sav_new <- reactive(!(is.null(input$save_dir_sav)) &&
                                    !(is.null(input$pre_dir_sav)) &&
                                    !(is.null(input$post_dir_sav)))
  outputOptions(output, "fields_sav_new", suspendWhenHidden = FALSE)


  # Extract the summary of the pre data files
  observeEvent(input$next_init_sav,{
    if (length(sav_out$pre_names_sav)!=0 &
       length(sav_out$post_names_sav)!=0 &
       length(sav_out$pre_names_sav)== length(sav_out$post_names_sav)){
      sav_out$files_names <- sav_out$pre_names_sav
      sav_out <- data_load(sav_out$pre_path_sav,
                           sav_out$files_names,
                           sav_out, Post =F, clean = T)
      sav_out <- data_load(sav_out$post_path_sav,
                           sav_out$files_names,
                           sav_out, Post = T, clean = T)
      sav_out$Data_pre_summary <- sav_out$Data_pre_summary_0[,c(1:6,9)]
      sav_out$Data_post_summary <- sav_out$Data_post_summary_0[,c(1:6,9)]
      # sav_out$zmin <- min(sav_out$Data_pre_summary_0[,8],
      #                     sav_out$Data_pre_summary_0[,8], na.rm = T)
      # sav_out$zmax <- max(sav_out$Data_post_summary_0[,7],
      #                     sav_out$Data_post_summary_0[,7], na.rm = T)
      output$intEndBox_sav <- renderInfoBox({
        infoBox(h4("The project setup is completed"),
                "Proceed to the savings analysis",
                icon = icon("thumbs-o-up"),
                fill = T, color = "navy")
      })
    }
    else if (length(sav_out$pre_names_sav)==0 & length(sav_out$post_names_sav)==0){
      output$intEndBox_sav <- renderInfoBox({
        infoBox(h4("No csv files found"),
                "Please select a valid directory for Pre and Post data",
                icon = icon("times-circle-o"),
                fill = T,
                color = "red")
      })
    }
    else if (length(sav_out$pre_names_sav)!=0 & length(sav_out$post_names_sav)==0){
      output$intEndBox_sav <- renderInfoBox({
        infoBox(h4("No csv files found"),
                "Please select a valid directory for Post data",
                icon = icon("times-circle-o"),
                fill = T,
                color = "red")
      })

    }
    else if (length(sav_out$pre_names_sav)==0 & length(sav_out$post_names_sav)!=0){
      output$intEndBox_sav <- renderInfoBox({
        infoBox(h4("No csv files found"),
                "Please select a valid directory for Pre data",
                icon = icon("times-circle-o"),
                fill = T,
                color = "red")
      })
    }
    else if (length(sav_out$pre_names_sav)!=0 &
            length(sav_out$post_names_sav)!=0 &
            length(sav_out$pre_names_sav)!= length(sav_out$post_names_sav)){
      miss <- union(setdiff(sav_out$pre_names_sav, sav_out$post_names_sav),
                    setdiff(sav_out$post_names_sav, sav_out$pre_names_sav))
      # keep only the pre and post for which we have data for both
      '%nin%' <- Negate('%in%')
      sav_out$pre_names_sav<-sav_out$pre_names_sav[sav_out$pre_names_sav %nin% miss]
      sav_out$post_names_sav<-sav_out$post_names_sav[sav_out$post_names_sav %nin% miss]
      for (m in miss){
        sav_out$pre_path_sav <- sav_out$pre_path_sav[grep(m,
                                                          sav_out$pre_path_sav,
                                                          invert = T)]
        sav_out$post_path_sav <- sav_out$post_path_sav[grep(m,
                                                            sav_out$post_path_sav,
                                                            invert = T)]
      }
      if (length(miss)==1){
       miss
      }
      else(miss <- paste0(miss,collapse = " ; "))
      sav_out$files_names <- sav_out$pre_names_sav
      sav_out <- data_load(sav_out$pre_path_sav,
                           sav_out$files_names,
                           sav_out, Post =F, clean = T)
      sav_out <- data_load(sav_out$post_path_sav,
                           sav_out$files_names,
                           sav_out, Post = T, clean = T)
      sav_out$Data_pre_summary <- sav_out$Data_pre_summary_0[,c(1:6,9)]
      sav_out$Data_post_summary <- sav_out$Data_post_summary_0[,c(1:6,9)]
      # sav_out$zmin <- min(sav_out$Data_pre_summary_0[,8],
      #                     sav_out$Data_pre_summary_0[,8], na.rm = T)
      # sav_out$zmax <- max(sav_out$Data_post_summary_0[,7],
      #                     sav_out$Data_post_summary_0[,7], na.rm = T)
      output$intEndBox_sav <- renderInfoBox({
        infoBox(h4("The initialization of the project is partially completed"),
                paste("Proceed to the savings analysis. However, note that missing
                files have been detected for: ", miss, sep = ""),
                icon = icon("exclamation-triangle"),
                fill = T,
                color = "orange")
      })
    }
  })

  #Render the Pre summary tab
  output$pre_summary_tab_sav <- renderDataTable({
   summary_tab <- sav_out$Data_pre_summary
   return(summary_tab)
  }, options = list(pageLength = 5))

  #Download the table
  output$pre_summary_tab_sav_dl <- downloadHandler(
    filename = function () {
      paste0(input$p_name_sav, "_pre_summary", ".csv")
    },
    content = function(file) {
      write.csv(sav_out$Data_pre_summary, file, row.names = FALSE)
    }
  )

  #Render the post summary tab
  output$post_summary_tab_sav <- renderDataTable({
   summary_tab <- sav_out$Data_post_summary
   return(summary_tab)
  }, options = list(pageLength = 5))

  #Download the table
  output$post_summary_tab_sav_dl <- downloadHandler(
    filename = function () {
      paste0(input$p_name_sav, "_post_summary", ".csv")
    },
    content = function(file) {
      write.csv(sav_out$Data_post_summary, file, row.names = FALSE)
    }
  )

  #######################  Pre and post Data Visualization #####################

  # update the pre-installation files names for the visualization
  observe({
    updateSelectInput(session,
                      "Data_vis_sav",
                      choices = sav_out$files_names)
  })


  observeEvent(input$plot_go_sav,{

    name_i <- input$Data_vis_sav
    Data_pre_summary_0_i <- dplyr::filter(sav_out$Data_pre_summary_0,
                                          Name == name_i)
    Data_post_summary_0_i <- dplyr::filter(sav_out$Data_post_summary_0,
                                           Name == name_i)

    # Pre data
    pre_Data_list <- sav_out$Data_pre
    pre_Data <- pre_Data_list[[name_i]]

    # Post data
    post_Data_list <- sav_out$Data_post
    post_Data <- post_Data_list[[name_i]]

    # Data variables

    variables_i <- c(c("Temp"),
                     names(pre_Data)[names(pre_Data) %nin%
                                     c("time","eload","Temp")])
    output$radio_plot_pre_sav <- renderUI({
      radioButtons(inputId = 'var_pre_sav',
                   label = 'Select the Input Variable to Plot',
                   choices = variables_i,
                   selected = "Temp",
                   inline = T)
    })

    output$radio_plot_post_sav <- renderUI({
      radioButtons(inputId = 'var_post_sav',
                   label = 'Select the Input Variable to Plot',
                   choices = variables_i,
                   selected = "Temp",
                   inline = T)
    })
    # Axis range
    range_i <- axis_range(pre_Data,post_Data)

    # heatmap for the selected building
    output$heatmap_pre_sav <- plotly::renderPlotly({
      p <- eload_heatmap(pre_Data,
                         zauto = F,
                         zmin = min(Data_pre_summary_0_i[,8],
                                    Data_post_summary_0_i[,8], na.rm = T),
                         zmax = max(Data_pre_summary_0_i[,7],
                                    Data_post_summary_0_i[,7], na.rm = T))
      return(p)
    })
    output$heatmap_post_sav <- plotly::renderPlotly({
      p <- eload_heatmap(post_Data,
                         zauto = F,
                         zmin = min(Data_pre_summary_0_i[,8],
                                    Data_post_summary_0_i[,8], na.rm = T),
                         zmax = max(Data_pre_summary_0_i[,7],
                                    Data_post_summary_0_i[,7], na.rm = T))
      return(p)
    })

    # plot time serie for the selected building
    output$ts_plot_pre_sav <- dygraphs::renderDygraph({
      p <- act_plot(pre_Data,
                    low_range_0 = range_i$low_range,
                    high_range_0 = range_i$high_range,
                    low_range_T_0 = range_i$low_range_T,
                    high_range_T_0 = range_i$high_range_T)
      return(p)
    })
    output$ts_plot_post_sav <- dygraphs::renderDygraph({
      p <- act_plot(post_Data,
                    low_range_0 = range_i$low_range,
                    high_range_0 = range_i$high_range,
                    low_range_T_0 = range_i$low_range_T,
                    high_range_T_0 = range_i$high_range_T)
      return(p)
    })

    # plot eload vs Temp for the selected building
    output$scatter_plot_pre_sav <- renderPlot({
      p <- eload_vs_input_plot(pre_Data,input$var_pre_sav)
      return(p)
    })
    output$scatter_plot_post_sav <- renderPlot({
      p <- eload_vs_input_plot(post_Data,input$var_post_sav)
      return(p)
    })
  })


  ###########################  Train Baseline Models (sav) #####################

  # generate UI corresponding to the baseline Models
  output$Model_Desc_sav <- renderText({
   switch(input$Model_sav,
    "TOWT" = paste("TOWT is a piecewise linear model and where the predicted
    energy consumption is a combination of two terms that relate the energy
    consumption to the time of the week and the piecewise-continuous effect
    of the temperature. Each time of the week has a different predicted energy
    consumption, and the temperature effect is estimated separately
    for periods of the day with high and low energy consumption in order to
    capture the pattern for occupied and unoccupied building periods."),
    "GBM" = paste("The GBM baseline model is based on the Gradient Boosting
    Machine (GBM) algorithm that is an ensemble trees based machine learning
    method. The GBM generate a model of the energy consumption using time and
    temperature as independent vaiables. However, he practical advantage of using
    the GBM model, in comparison to TOWT model is that it is capable of handle
    additional independent variables, such as holidays indicator, humidity,
    or solar radiation. GBM model has several hyper-parameters that needs to be
    tuned in order to produce an accurate model. These parameters are tuned
    automaticaly using a search grid and a k-folds cross validation procedure.
    It is possible to change the definition of the search grid using the
    Hyper-parameters Setup tab, however for a user that is not familiar with these
    hyper-paprameters, we advise using the default values.")
   )
  })

  output$Model_Name_sav <- renderText({
      switch(input$Model_sav,
        "TOWT" = paste("Time-of-Week-and-Temperature model"),
        "GBM" = paste("Gradient Boosting Machine model")
      )
   })

  # Train the baseline model and format the results
  observeEvent(input$Train_go_sav,{
    switch(input$Model_sav,
      "TOWT" = pam_list <- list(timescaleDays = input$timescaleDays_sav),
      "GBM" = pam_list <- list(k_folds = input$kfolds_sav,
                               ncores = input$ncores_sav,
                               iter = input$gbm_iter_sav,
                               depth = input$depth_sav,
                               lr = input$lr_sav)
    )
    if (input$Model_sav == "GBM"){
      switch(input$d_off_GBM_sav,
        "no_sav" = days_off_path <- NULL,
        "def_d_off_sav" = days_off_path <- system.file("extdata",
                                                      "USA_Fed_Holidays.csv",
                                                       package = "RMV2.0"),
        "yes_sav" = {path_obj <- input$d_off_path_sav
                    days_off_path <- path_obj$datapath}
      )
    }
    res_base <- suppressWarnings(train_model(sav_out, screen = F,
                                             Model = input$Model_sav,
                                             pam_list = pam_list,
                                             days_off_path = days_off_path))
    if (length(res_base$failures)==0){
      sav_out$files_names_mod <- sav_out$files_names
      models_list <- res_base$res_list
      results_summary <- train_model_summary(models_list,
                                             sav_out$files_names)
      sav_out$model_obj_list <- list(models_list = models_list,
                                     results_summary = results_summary)
      output$trainEndBox_sav <- renderInfoBox({
        infoBox(h4("The Baseline models training is completed"),
                "No failures reported",
                icon = icon("thumbs-o-up"),
                fill = T,
                color = "navy")
      })
    }
    else{
      failures <- sav_out$Data_pre_summary[res_base$failures,1]
      sav_out$files_names_mod <- sav_out$files_names[sav_out$files_names %nin% failures]
      models_list <- res_base$res_list
      results_summary <- train_model_summary(models_list,
                                             sav_out$files_names_mod)
      sav_out$model_obj_list <- list(models_list = models_list,
                                     results_summary = results_summary)
      output$trainEndBox_sav <- renderInfoBox({
        infoBox(h4("Baseline modeling failed for:"),
                paste0(failures, collapse = "; "),
                icon = icon("times-circle-o"),
                fill = T,
                color = "red")
      })
    }
    sav_out$Model <- input$Model_sav

  })



  ###########################  Visualize Baseline Results ######################

  # Render the model summary tab
  output$model_metrics_tab_sav <- renderDataTable({
    results_obj <- sav_out$model_obj_list
    summary_tab <- results_obj$results_summary
    return(summary_tab)
  }, options = list(pageLength = 5))

  #Download the table
  output$model_metrics_tab_sav_dl <- downloadHandler(
    filename = function () {
      paste0(input$p_name_sav, "_model_metrics", ".csv")
    },
    content = function(file) {
      write.csv(sav_out$model_obj_list$results_summary,
        file, row.names = FALSE)
    }
  )


  #Perform the Screening
  observeEvent(input$screen_go_sav,{
    results_obj <- sav_out$model_obj_list
    summary_tab <- results_obj$results_summary
    sav_out$screen_summary_list <- screen_summary(summary_tab,
                                                  input$R2_tresh_sav,
                                                  input$CVRMSE_tresh_sav,
                                                  input$NMBE_tresh_sav)
    output$pie_plot_screen_sav <- plotly::renderPlotly({
      p <- screen_pie_plot(sav_out$screen_summary_list)
      return(p)
    })
  })


  # update the pre-installation files names for the visualization
  observe({
    updateSelectInput(session, "Res_vis_sav",choices = sav_out$files_names_mod)
  })

  # plot time serie for the selected building
  observeEvent(input$base_plot_go_sav,{
    name_i <- input$Res_vis_sav

    # Pre data
    pre_Data_list <- sav_out$Data_pre
    pre_Data <- pre_Data_list[[name_i]]

    # Post data
    post_Data_list <- sav_out$Data_post
    post_Data <- post_Data_list[[name_i]]

    # Axis range
    range_i <- axis_range(pre_Data,post_Data)

    # Data variables
    if (sav_out$Model == "GBM"){
      variables_i <- c(c("Temp"),
                       names(pre_Data)[names(pre_Data) %nin%
                                       c("time","eload","Temp")])
    }
    if (sav_out$Model == "TOWT"){
      variables_i <- "Temp"
    }
    output$radio_plot_err_sav <- renderUI({
      radioButtons(inputId = 'var_err_sav',
                   label = 'Select the Input Variable to Plot',
                   choices = variables_i,
                   selected = "Temp",
                   inline = T)
    })


    # model results plot
    idx_i <-  which(input$Res_vis_sav == sav_out$files_names_mod)
    model_obj_list <- sav_out$model_obj_list
    model_list <- model_obj_list$models_list
    model_obj <- model_list[[idx_i]]

    output$pre_ts_plot_mod_sav <- dygraphs::renderDygraph({
      p <- pre_plot(model_obj,
                    low_range_0 = range_i$low_range,
                    high_range_0 = range_i$high_range,
                    low_range_T_0 = range_i$low_range_T,
                    high_range_T_0 = range_i$high_range_T)
      return(p)
    })

    output$post_ts_plot_mod_sav <- dygraphs::renderDygraph({
      p <- post_plot(model_obj,
                    low_range_0 = range_i$low_range,
                    high_range_0 = range_i$high_range,
                    low_range_T_0 = range_i$low_range_T,
                    high_range_T_0 = range_i$high_range_T)
      return(p)
    })

    # plot actual vs fit for the selected building
    output$scatter_plot_act_fit_sav <- renderPlot({
      p <- act_vs_fit_plot(model_obj)
      return(p)
    })

    # plot actual vs fit for the selected building
    output$scatter_plot_err_input_sav <- renderPlot({
      p <- errors_vs_input_plot(model_obj, input$var_err_sav)
      return(p)
    })

    # Residual autocorrelation plot for the selected building
    output$acf_plot_sav <- renderPlot({
      p <- acf_plot(model_obj, lag_max = input$lag_max_sav)
      return(p)
    })
  })


  ###########################  Savings Analysis ###############################

  ##  -----  Deprecated  -----
  ## exclude the uncertainty calculation
  # observeEvent(input$sav_est_go,{
  #  sav_out$sav_est_tab <- savings_summary(sav_out, input$inCL)
  #  sav_out$CL <- input$inCL
  # })
  observeEvent(input$sav_est_go,{
   sav_out$sav_est_tab <- savings_summary(sav_out)
  })

  output$sav_est_tab <- renderDataTable({
    sav_tab <- sav_out$sav_est_tab
    return(sav_tab)
  }, options = list(pageLength = 5))

  #Download the table
  output$sav_est_tab_dl <- downloadHandler(
    filename = function () {
      paste0(input$p_name_sav, "_savings_analysis", ".csv")
    },
    content = function(file) {
      write.csv(sav_out$sav_est_tab, file, row.names = FALSE)
    }
  )


  bar_plot_sav_error <- eventReactive(input$sav_est_go,{
   p <- savings_results_plot(sav_out$sav_est_tab)
   return(p)
  })

  output$bar_plot_sav_error <- plotly::renderPlotly({
   p <- bar_plot_sav_error()
   return(p)
  })

  ###########################  Savings & CUSUM ########################

  # update the pre-installation files names for the visualization
  observe({
    updateSelectInput(session,
                      "Res_vis_sav_2",
                      choices = sav_out$files_names_mod)
  })

  ##  -----  Deprecated  -----
  # # plot time serie for the selected building
  # model_obj_to_plot_sav_2 <- eventReactive(input$plot_gran_go_sav,{
  #   idx_i <-  which(input$Res_vis_sav_2 == sav_out$files_names_mod)
  #   model_obj_list <- sav_out$model_obj_list
  #   model_obj_list <- model_obj_list$models_list
  #   model_obj <- model_obj_list[[idx_i]]
  #   return(model_obj)
  # })
  #
  # output$ts_post_plot_sav <- dygraphs::renderDygraph({
  #  post_plot(model_obj_to_plot_sav_2(), plot_group="savings_1")
  # })
  observeEvent(input$plot_gran_go_sav,{
    idx_i <-  which(input$Res_vis_sav_2 == sav_out$files_names_mod)
    model_obj_list <- sav_out$model_obj_list
    model_obj_list <- model_obj_list$models_list
    model_obj <- model_obj_list[[idx_i]]

    # plot savings time serie for the selected building
    output$ts_savings_plot_sav <- dygraphs::renderDygraph({
                                    savings_plot_type(model_obj,
                                                      input$Plot_gran_sav)})

    # plot CUSUM for the selected building
    output$ts_cusum_plot_sav <- dygraphs::renderDygraph({
                                  cusum_plot_type(model_obj,
                                                  input$Plot_cusum_gran_sav)})

    # Savings heatmap for the selected building
    output$heatmap_savings_sav <- plotly::renderPlotly({
      p <- savings_heatmap(model_obj,
                         zauto = T)
      return(p)
    })
  })
  ##  -----  Deprecated  -----
  # # plot time serie for the selected building
  # plot_obj_savings_sav_2 <- eventReactive(input$plot_gran_go_sav,{
  #  idx_i <-  which(input$Res_vis_sav_2 == sav_out$files_names_mod)
  #  model_obj_list <- sav_out$model_obj_list
  #  model_obj_list <- model_obj_list$models_list
  #  model_obj <- model_obj_list[[idx_i]]
  #  switch(input$Plot_gran_sav,
  #    "Original Granularity" = output$ts_savings_plot_sav <- dygraphs::renderDygraph({save_plot(model_obj, plot_group="savings_1")}),
  #    "Daily" = output$ts_savings_plot_sav <- dygraphs::renderDygraph({daily_save_barplot(model_obj, plot_group="savings_1")}),
  #    "Weekly" = output$ts_savings_plot_sav <- dygraphs::renderDygraph({weekly_save_barplot(model_obj, plot_group="savings_1")}),
  #    "Monthly" = output$ts_savings_plot_sav <- dygraphs::renderDygraph({monthly_save_barplot(model_obj, plot_group="savings_1")})
  #  )
  # })
  #
  # output$ts_savings_plot_sav <- dygraphs::renderDygraph({
  #  plot_obj_savings_sav_2()
  # })
  #
  # plot_obj_cusum_sav_2 <- eventReactive(input$plot_gran_go_sav,{
  #  idx_i <-  which(input$Res_vis_sav_2 == sav_out$files_names_mod)
  #  model_obj_list <- sav_out$model_obj_list
  #  model_obj_list <- model_obj_list$models_list
  #  model_obj <- model_obj_list[[idx_i]]
  #  switch(input$Plot_cusum_gran_sav,
  #    "Original Granularity" = cusum_plot(model_obj, plot_group="savings_1"),
  #    "Daily" = daily_cusum_barplot(model_obj, plot_group="savings_1"),
  #    "Weekly" = weekly_cusum_barplot(model_obj, plot_group="savings_1"),
  #    "Monthly" = monthly_cusum_barplot(model_obj, plot_group="savings_1")
  #  )
  # })
  #
  # output$ts_cusum_plot_sav <- dygraphs::renderDygraph({
  #  plot_obj_cusum_sav_2()
  # })
  #
  # # Savings heatmap for the selected building
  # output$heatmap_savings_sav <- plotly::renderPlotly({
  #   p <- savings_heatmap(model_obj_to_plot_sav_2(),
  #                      zauto = T)
  #   return(p)
  # })

  #####################  Non routine events identification #####################

  # Train the baseline model and format the results
  observeEvent(input$nre_go_sav,{
    res_cbt <- suppressWarnings(nre_eval(sav_out))
    sav_out$nre_done <- TRUE
    if (dim(res_cbt$sav_est_tab_2)[1]!=0){
      sav_est_tab_2 <- res_cbt$sav_est_tab_2
      sav_out$files_names_nre <- sav_est_tab_2$Name
      sav_out$nre_obj_list <- res_cbt
      num_nre <- dim(res_cbt$sav_est_tab_2)[1]
      output$nreEndBox_sav <- renderInfoBox({
        infoBox(h4("Number of Buildings with NRE"),
                icon = icon("warning"),
                paste0(num_nre, collapse = "; "),
                fill = T,
                color = "orange")
      })
      output$nre_test_tab_sav <- renderDataTable({
        sav_tab_2 <- res_cbt$sav_est_tab_2
        sav_tab_2 <- subset(sav_tab_2, select=-NRE)
        return(sav_tab_2)
      }, options = list(pageLength = 5))
    }
    else{
      output$nreEndBox_sav <- renderInfoBox({
        infoBox(h4("No NRE detected"),
                icon = icon("thumbs-o-up"),
                fill = T,
                color = "aqua")
      })
    }

  })

  #Download the table
  output$nre_test_tab_sav_dl <- downloadHandler(
    filename = function () {
      paste0(input$p_name_sav, "_non_routine", ".csv")
    },
    content = function(file) {
      write.csv(subset(sav_out$nre_obj_list$sav_est_tab_2,
                        select=-NRE),
                        file, row.names = FALSE)
    }
  )

  # update the nre files names for the visualization
  observe({
    updateSelectInput(session, "nre_vis_sav",choices = sav_out$files_names_nre)
  })

  observeEvent(input$nre_plot_go_sav,{
    name_i <- input$nre_vis_sav

    # nre results
    #idx_i <-  which(input$nre_vis_sav == sav_out$files_names_nre)
    nre_obj_list <- sav_out$nre_obj_list
    cbt_obj_list <- nre_obj_list$cbt_obj_list
    cbt_obj_list <- cbt_obj_list[[name_i]]

    # model results plot
    idx_j <-  which(input$nre_vis_sav == sav_out$files_names_mod)
    model_obj_list <- sav_out$model_obj_list
    model_list <- model_obj_list$models_list
    model_obj <- model_list[[idx_j]]

    output$nre_ts_plot_sav <- dygraphs::renderDygraph({
      p <- cpt_save_plot(model_obj,cbt_obj_list)
      return(p)
    })

  })

 #####################  Non routine events identification (load)################
  observeEvent(input$nre_go_sav_lo,{
    if(sav_out$nre_done){
      res_cbt <- sav_out$nre_obj_list
    }
    else{
      res_cbt <- suppressWarnings(nre_eval(sav_out))
    }
    if (dim(res_cbt$sav_est_tab_2)[1]!=0){
      sav_est_tab_2 <- res_cbt$sav_est_tab_2
      sav_out$files_names_nre <- sav_est_tab_2$Name
      sav_out$nre_obj_list <- res_cbt
      num_nre <- dim(res_cbt$sav_est_tab_2)[1]
      output$nreEndBox_sav_lo <- renderInfoBox({
        infoBox(h4("Number of Buildings with Potential NRE"),
                icon = icon("warning"),
                paste0(num_nre, collapse = "; "),
                fill = T,
                color = "orange")
      })
      output$nre_test_tab_sav_lo <- renderDataTable({
        sav_tab_2 <- res_cbt$sav_est_tab_2
        sav_tab_2 <- subset(sav_tab_2, select=-NRE)
        return(sav_tab_2)
      }, options = list(pageLength = 5))
    }
    else{
      output$nreEndBox_sav_lo <- renderInfoBox({
        infoBox(h4("No Potential NRE detected"),
                icon = icon("thumbs-o-up"),
                fill = T,
                color = "aqua")
      })
    }

  })

  #Download the table
  output$nre_test_tab_sav_lo_dl <- downloadHandler(
    filename = function () {
      paste0(input$p_name_sav, "_savings_analysis", ".csv")
    },
    content = function(file) {
      write.csv(subset(sav_out$nre_obj_list$sav_est_tab_2,
                        select=-NRE),
        file, row.names = FALSE)
    }
  )

  # update the nre files names for the visualization
  observe({
    updateSelectInput(session, "nre_vis_sav_lo",choices = sav_out$files_names_nre)
  })

  observeEvent(input$nre_plot_go_sav_lo,{
    name_i <- input$nre_vis_sav_lo

    # nre results
    #idx_i <-  which(input$nre_vis_sav == sav_out$files_names_nre)
    nre_obj_list <- sav_out$nre_obj_list
    cbt_obj_list <- nre_obj_list$cbt_obj_list
    cbt_obj_list <- cbt_obj_list[[name_i]]

    # model results plot
    idx_j <-  which(input$nre_vis_sav_lo == sav_out$files_names_mod)
    model_obj_list <- sav_out$model_obj_list
    model_list <- model_obj_list$models_list
    model_obj <- model_list[[idx_j]]

    output$nre_ts_plot_sav_lo <- dygraphs::renderDygraph({
      p <- cpt_save_plot(model_obj,cbt_obj_list)
      return(p)
    })

  })

  ############################  Results summary ################################

  # update the pre-data files names for the visualization
   observe({
     updateSelectInput(session, "Res_summ_sav",choices = sav_out$files_names_mod)
   })

  observeEvent(input$summary_go_sav,{
    if (input$type_summ == 2){
      sav_tab <- sav_out$sav_est_tab
      idx_i <- which(input$Res_summ_sav == sav_tab$Name)
      output$R2BoxSav <- renderValueBox({
        valueBox(
          paste0("R2"),
          h4(strong(paste0(sav_tab$R2[idx_i], "%"))),
          icon = icon("area-chart"),
          color = "aqua"
        )
      })
      output$CVRMSEBoxSav <- renderValueBox({
        valueBox(
          paste0("CV(RMSE)"),
          h4(strong(paste0(sav_tab$CVRMSE[idx_i], "%"))),
          icon = icon("area-chart"),
          color = "aqua"
        )
      })
      output$NMBEBoxSav <- renderValueBox({
        valueBox(
          paste0("NMBE"),
          h4(strong(paste0(sav_tab$NMBE[idx_i], "%"))),
          icon = icon("area-chart"),
          color = "aqua"
        )
      })
      output$pre_ts_plot_mod_sav_2 <- dygraphs::renderDygraph({
        model_obj_list <- sav_out$model_obj_list
        model_list <- model_obj_list$models_list
        model_obj <- model_list[[idx_i]]
        p <- pre_plot(model_obj)
        return(p)
      })
      output$post_ts_plot_mod_sav_2 <- dygraphs::renderDygraph({
        model_obj_list <- sav_out$model_obj_list
        model_list <- model_obj_list$models_list
        model_obj <- model_list[[idx_i]]
        p <- post_plot(model_obj)
        return(p)
      })
      output$SavingsBoxSav <- renderValueBox({
        valueBox(
          paste0("Savings"),
          h4(strong(paste0(sav_tab$Savings[idx_i], " kWh"))),
          icon = icon("leaf"),
          color = "teal"
        )
      })
      output$FsBoxSav <- renderValueBox({
        valueBox(
          paste0("Fractional Savings (FS)"),
          h4(strong(paste0(sav_tab$FS[idx_i], " %"))),
          icon = icon("leaf"),
          color = "teal"
        )
      })
      output$FsBoxSav <- renderValueBox({
        valueBox(
          paste0("Fractional Savings (FS)"),
          h4(strong(paste0(sav_tab$FS[idx_i], " %"))),
          icon = icon("leaf"),
          color = "teal"
        )
      })
      if(input$nre_go_sav || input$nre_go_sav_lo){
        res_cbt <- sav_out$nre_obj_list
        if (dim(res_cbt$sav_est_tab_2)[1]!=0){
          sav_est_tab_2 <- res_cbt$sav_est_tab_2
          idx_i_2 <- which(input$Res_summ_sav == sav_est_tab_2$Name)
          if (length(idx_i_2)!=0){
            output$NreBoxSav <- renderInfoBox({
              valueBox(
                paste0("Number of detected change points"),
                h4(strong(paste0(sav_est_tab_2$cpts[idx_i_2]))),
                icon = icon("warning"),
                color = "orange"
             )
           })
          }
          else{
            output$NreBoxSav <- renderInfoBox({
              valueBox(
                paste0("No Potential NRE detected"),
                h4("_"),
                icon = icon("thumbs-o-up"),
                color = "aqua"
              )
            })
          }
        }
      }



      ##  -----  Deprecated  -----
      ## exclude the uncertainty calculation
      # output$SavRangeBoxSav <- renderValueBox({
      #   valueBox(
      #     paste0("Savings Range"),
      #     h4(strong(paste0(sav_tab$Savings_Range[idx_i], " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("leaf"),
      #     color = "teal"
      #   )
      # })
      # output$FsRangeBoxSav <- renderValueBox({
      #   valueBox(
      #     paste0("FS Range (in %)"),
      #     h4(strong(paste0(sav_tab$FS_Range[idx_i], " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("leaf"),
      #     color = "teal"
      #   )
      # })
      # output$FsuBoxSav <- renderValueBox({
      #   valueBox(
      #     paste0("FSU"),
      #     h4(strong(paste0(sav_tab$FSU[idx_i], "%", " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("bar-chart-o"),
      #     color = "aqua"
      #   )
      # })
    }
    else if (input$type_summ == 1 & input$level_summ == 1){
      port_sav_tab <- portfolio_savings(sav_out)
      output$bar_plot_sav_error_2 <- plotly::renderPlotly({
       p <- savings_results_plot(sav_out$sav_est_tab)
       return(p)
      })
      output$PortSavingsBoxSav <- renderValueBox({
        valueBox(
          paste0("Savings"),
          h4(strong(paste0(port_sav_tab$Savings_portfolio, " kWh"))),
          icon = icon("leaf"),
          color = "teal"
        )
      })
      output$PortFsBoxSav <- renderValueBox({
        valueBox(
          paste0("FS (in %)"),
          h4(strong(paste0(port_sav_tab$FS_portfolio, " %"))),
          icon = icon("leaf"),
          color = "teal"
        )
      })
      ##  -----  Deprecated  -----
      ## exclude the uncertainty calculation
      # output$PortFsuBoxSav <- renderValueBox({
      #   valueBox(
      #     paste0("FSU"),
      #     h4(strong(paste0(port_sav_tab$FSU_portfolio, "%", " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("bar-chart-o"),
      #     color = "aqua"
      #   )
      # })
      # output$PortSavRangeBoxSav <- renderValueBox({
      #   valueBox(
      #     paste0("Savings Range"),
      #     h4(strong(paste0(port_sav_tab$Savings_Range, " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("leaf"),
      #     color = "teal"
      #   )
      # })
      # output$PortFsRangeBoxSav <- renderValueBox({
      #   valueBox(
      #     paste0("FS Range (in %)"),
      #     h4(strong(paste0(port_sav_tab$FS_Range, " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("leaf"),
      #     color = "teal"
      #   )
      # })
    }
    else if (input$type_summ == 1 & input$level_summ == 2){
      port_sav_tab <- portfolio_savings(sav_out, screened =T)
      output$bar_plot_sav_error_3 <- plotly::renderPlotly({
       p <- savings_results_plot(sav_out$sav_est_tab)
       return(p)
      })
      output$PortSavingsBoxSav_2 <- renderValueBox({
        valueBox(
          paste0("Savings"),
          h4(strong(paste0(port_sav_tab$Savings_portfolio, " kWh"))),
          icon = icon("leaf"),
          color = "teal"
        )
      })
      output$PortFsBoxSav_2 <- renderValueBox({
        valueBox(
          paste0("FS (in %)"),
          h4(strong(paste0(port_sav_tab$FS_portfolio, " %"))),
          icon = icon("leaf"),
          color = "teal"
        )
      })
      ##  -----  Deprecated  -----
      ## exclude the uncertainty calculation
      # output$PortFsuBoxSav_2 <- renderValueBox({
      #   valueBox(
      #     paste0("FSU"),
      #     h4(strong(paste0(port_sav_tab$FSU_portfolio, "%", " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("bar-chart-o"),
      #     color = "aqua"
      #   )
      # })
      # output$PortSavRangeBoxSav_2 <- renderValueBox({
      #   valueBox(
      #     paste0("Savings Range"),
      #     h4(strong(paste0(port_sav_tab$Savings_Range, " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("leaf"),
      #     color = "teal"
      #   )
      # })
      # output$PortFsRangeBoxSav_2 <- renderValueBox({
      #   valueBox(
      #     paste0("FS Range (in %)"),
      #     h4(strong(paste0(port_sav_tab$FS_Range, " (@ ", sav_out$CL, "%", " CL)"))),
      #     icon = icon("leaf"),
      #     color = "teal"
      #   )
      # })
    }

  })
  ########################  Save the new Project ###############################

  observeEvent(input$save_sav,{
   save_session(path_save_dir_sav(),input$p_name_sav,sav_out)
  })

  ########################  Save the predictions ###############################

  observeEvent(input$save_pred_sav,{
   save_predictions(sav_out$model_obj_list,path_save_dir_sav())
  })

  #=============================================================================
  #                Savings Analysis Server Functions loaded project            #
  #=============================================================================


  ###########################  Load a Project ##################################

  observe({change_box("load_sav_box", !(is.null(input$load_sav)))})

  # Extract the project file path
  shinyFileChoose(input,
                  'load_sav',
                  roots = volumes,
                  filetypes = c('', 'rds'))

   observeEvent(input$load_sav, {
     load_sav <- parseFilePaths(roots=volumes, input$load_sav)
     sav_out$load_sav <- as.character(load_sav$datapath)
     output$load_sav_out <- renderText({basename(sav_out$load_sav)})
   })

   output$fields_sav_load <- reactive(!(is.null(input$load_sav)))
   outputOptions(output, "fields_sav_load", suspendWhenHidden = FALSE)

   # Finalize the Project loading
   observeEvent(input$next_init_sav_lo,{
     load_res <- load_session(sav_out$load_sav)

     #Populate screen_out variable
     sav_out$load <- TRUE
     sav_out$p_name_sav <- load_res$p_name_sav
     sav_out$files_names <- load_res$files_names
     sav_out$Data_pre <- load_res$Data_pre
     sav_out$Data_pre_summary <- load_res$Data_pre_summary
     sav_out$Data_pre_summary_0 <- load_res$Data_pre_summary_0
     sav_out$Data_post <- load_res$Data_post
     sav_out$Data_post_summary <- load_res$Data_post_summary
     sav_out$Data_post_summary_0 <- load_res$Data_post_summary_0
     sav_out$Model <- load_res$Model
     sav_out$model_obj_list <- load_res$model_obj_list
     sav_out$files_names_mod <- load_res$files_names_mod
     sav_out$sav_est_tab <- load_res$sav_est_tab
     ##  -----  Deprecated  -----
     ## exclude the uncertainty calculation
     #sav_out$CL <- load_res$CL
     sav_out$screen_summary_list <- load_res$screen_summary_list
     sav_out$files_names_nre <- load_res$files_names_nre
     sav_out$nre_obj_list <- load_res$nre_obj_list
     sav_out$nre_done <- load_res$nre_done

     output$intEndBox_sav_lo <- renderInfoBox({
       infoBox(h4("The project is loaded"),
               "Proceed to the savings analysis",
               icon = icon("thumbs-o-up"),
               fill = T, color = "navy")
     })
   })

 })#end of shinyServer
