#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Packages ####
## referenced global.R

# Server ####
# Define server logic
shinyServer(function(input, output, session) {
  
  # Modal ----
  ## Not linked to anything so occurs at start up
  modal_startup_title <- "baytrendsmap"
  modal_startup_txt <- paste0("This app provides access to maps depicting " 
    , "short- and long-term changes/trends in nutrients, dissolved oxygen (DO), "
    , "Secchi depth (a measure of clarity), and chlorophyll-a. The View Tidal Trends "
    , "tab includes the results for more than 130 stations located throughout the "
    , "mainstem of the Chesapeake Bay as well as the tidal portions of numerous "
    , "tributaries on the western and eastern shores since the mid-1980s.   

  The Create Custom Map tab provides options to create trend maps on the data "
    , "provided or allow users to upload a personal baytrends (R package designed to "
    , "fit GAMs for the tidal Chesapeake Bay water quality data) output file.   

  Click HELP in the main menu for information on how to use this app.")
  # Same text as HTML so can format
  modal_startup_html <- p(hr()
      , "This app provides access to maps depicting"
      , "short- and long-term changes/trends in nutrients, dissolved oxygen (DO), "
      , "Secchi depth (a measure of clarity), and chlorophyll-a."
      , br()
      , br()
      , "The "
      , tags$strong("View Tidal Trends ")
      , "tab includes the results for more than 130 stations located throughout the "
      , "mainstem of the Chesapeake Bay as well as the tidal portions of numerous "
      , "tributaries on the western and eastern shores since the mid-1980s."
      , br()
      , br()
      , "The "
      , tags$strong("Create Custom Map ")
      , "tab provides options to create trend maps on the data "
      , "provided or allow users to upload a personal baytrends (R package designed to "
      , "fit GAMs for the tidal Chesapeake Bay water quality data) output file."
      , br()
      , br()
      , "Click "
      , tags$strong("HELP ")
      , "in the main menu for information on how to use this app."
      )
    
  # modal_startup <- modalDialog(title = modal_startup_title
  #                              , modal_startup_txt
  #                              , easyClose = TRUE)
  #showModal(modal_startup)
  shinyalert(title = modal_startup_title
             , text = modal_startup_html
             , type = ""
             , closeOnEsc = TRUE
             , closeOnClickOutside = TRUE
             , imageUrl = "circle-info.svg"
             , html = TRUE)
  
  
  # Data ####

  ## df_import ####
  
  # Save last loaded file type
  click_filetype <- reactiveValues(data = NULL)
  
  output$txt_click_filetype <- renderText({
    str_click <- paste0("Loaded file type = ", click_filetype$data)
    return(str_click)
  })## txt_click_filetype ~ END
  
  observeEvent(input$but_radio_load, {
      click_filetype$data <- "final"
      click_filtdata$data <- NULL
  })## observerEvent ~ but_radio_load
  
  observeEvent(input$fn_input, {
    click_filetype$data <- "user"
    click_filtdata$data <- NULL
  })## observerEvent ~ fn_input
  
  file_watch <- reactive({
    # trigger for df_import()
    paste(input$fn_input, input$but_radio_load)
  })## file_watch ~ EN
  
  df_import <- eventReactive(file_watch(), {
    # use a multi-item reactive so keep on a single line

    inFile       <- input$fn_input
    inFile_radio <- input$radio_input
    
    # Need is.null as first rather than last.
    # It will always trigger on loading of the app
    
    if(is.null(click_filetype$data)){
      return(NULL)
    } else if (click_filetype$data == "user"){
      # Define file
      fn_inFile <- inFile$datapath
      # Read user imported file
      df_input <- read.csv(fn_inFile
                           , header = TRUE
                           , sep = ","
                           , quote = "\""
                           , stringsAsFactors = FALSE)
      # validate required columns
      col_req <- c("station"
                   , "layer"
                   , "latitude"
                   , "longitude"
                   , "cbSeg92"
                   , "state"
                   , "stationGrpName"
                   , "parmName"
                   , "gamName"
                   , "periodName"
                   , "seasonName"
                   , "gamDiff.bl.mn.obs"
                   , "gamDiff.cr.mn.obs"
                   , "gamDiff.abs.chg.obs"
                   , "gamDiff.pct.chg"
                   , "gamDiff.chg.pval")
      # Check
      col_req_match <- col_req %in% colnames(df_input)
      col_missing <- col_req[!col_req_match]
      #
      validate(need(sum(col_req_match) == length(col_req)
                    , paste0("ERROR\nRequired columns missing from the data:\n"
                             , paste("* ", col_missing, collapse = "\n"))))
    } else if (click_filetype$data == "final") {
      fn_inFile <- pick_files_names[match(inFile_radio, pick_files_radio)]
      df_input <- read.csv(fn_inFile
                          , header = TRUE
                          , sep = ","
                          , quote = "\""
                          , stringsAsFactors = FALSE)
      # saved files are already validated so no QC on column names
    }##IF~is.null~END
    #
    msg <- paste0("file loaded, advanced; ", inFile_radio)
    message(msg)
    #
    return(df_input)
    #
    #
  })##df_import~END

  
  ## df_import_cols_widen ####
  df_import_cols_widen <- eventReactive(file_watch(), {
    # uses a multi-item reactive so keep on a single line

    col_widen <-  c("station"
                    , "layer"
                    , "latitude"
                    , "longitude"
                    , "cbSeg92"
                    #, "state"
                    #, "stationGrpName"
                    , "parmName"
                    , "gamName"
                    , "periodName"
                    , "seasonName"
                    #, "gamDiff.bl.mn.obs"
                    #, "gamDiff.cr.mn.obs"
                    #, "gamDiff.abs.chg.obs"
                    #, "gamDiff.pct.chg"
                    #, "gamDiff.chg.pval"
                    , "mapLayer")
    
    
    df_i <- df_import()
    
    # Need is.null as first rather than last.
    # It will always trigger on loading of the app
    
    if(is.null(click_filetype$data)){
      return(NULL)
    } else {
      # Same for both file types
      #
      # get col nums of matching names
      col_num_widen <- match(col_widen, names(df_i))
      # remove NA
      col_nums <- col_num_widen[!is.na(col_num_widen)]
    }##IF~is.null~END
    #
    return(col_nums)
    #
  })##df_import_cols_widen ~ END
  
  ## df_import_DT ####
  cols_widen <- c(1,3)
  
  output$df_import_DT <- DT::renderDT({
    # # input$df_import will be NULL initially. After the user selects
    # # and uploads a file, it will be a data frame with 'name',
    # # 'size', 'type', and 'datapath' columns. The 'datapath'
    # # column will contain the local filenames where the data can
    # # be found.
    # 
    # inFile <- input$fn_input
    # 
    # if (is.null(inFile)){
    #   return(NULL)
    # }##IF~is.null~END
    # 
    # #message(getwd())
    # 
    # # # Add "Results" folder if missing
    # # boo_Results <- dir.exists(file.path(".", "data"))
    # # if(boo_Results==FALSE){
    # #   dir.create(file.path(".", "data"))
    # # }
    # # 
    # # # Remove all files in "data" folder
    # # fn_results <- list.files(file.path(".", "data"), full.names=TRUE)
    # # file.remove(fn_results)
    # 
    # # Read user imported file
    # df_input <- read.csv(inFile$datapath, header = TRUE,
    #                      sep = ",", quote = "\"", stringsAsFactors = FALSE)
    # 
    # # # Write to "data" folder - Import as TSV
    # # fn_input <- file.path(".", "data", "data_import.tsv")
    # # write.table(df_input, fn_input, row.names=FALSE, col.names=TRUE, sep="\t")
    # # 
    # # # Copy to "data" folder - Import "as is"
    # # file.copy(input$fn_input$datapath, file.path(".", "data", input$fn_input$name))
    # # 
    # # # read.table(file = inFile$datapath, header = input$header,
    # # #          sep = input$sep, quote = input$quote)
    # return(df_input)
    
    # inFile <- input$fn_input
    # 
    # if (is.null(inFile)){
    #   return(NULL)
    # }##IF~is.null~END
    
    df_z <- df_import()
    
    # Remove 2 columns that have so much info that blows up size of rows.
    str_names <- names(df_z)
    str_names_drop <- c("sa.sig.inc", "sa.sig.dec")
    str_names_keep <- str_names[!(str_names %in% str_names_drop)]
    
    return(df_z[, str_names_keep])
    
  }##expression~END
  , filter="top"
  , caption = "Table 1. Imported data."
  #, rownames = FALSE
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                 , autoWidth = TRUE
                 , columnDefs = list(list(width = col_width_manual
                                          , targets = df_import_cols_widen())))
  )##output$df_import_DT~END
  
  
  # Save whether "apply filters" button has been clicked
  click_filtdata <- reactiveValues(data = NULL)
  
  observeEvent(input$but_filt_apply, {
    click_filtdata$data <- TRUE
  })## observerEvent ~ but_filt_apply
  
  # FILTER ----
  ## df_filt ####
  df_filt <- eventReactive (input$but_filt_apply, {
    # if filters not null then apply to df_import
    # it is possible to select no data
    
    # temp data frame
    df_y <- df_import()
    
    #
    str_col_2 <- "state"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~state~END
    #
    str_col_2 <- "cbSeg92"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~cbSeg92~END
    #
    str_col_2 <- "stationGrpName"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~stationGrpName~END
    #
    str_col_2 <- "station"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~station~END
    #
    str_col_2 <- "layer"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~layer~END
    #
    str_col_2 <- "parmName"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~parmName~END
    #
    str_col_2 <- "gamName"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~gamName~END
    #
    str_col_2 <- "periodName"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~periodName~END
    #
    str_col_2 <- "seasonName"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~seasonName~END
    #
    str_col_2 <- "mapLayer"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2)))
    if(!is.null(str_SI_value) & click_filetype$data == "final"){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~mapLayer~END
    #
    #
    return(df_y)
    #
  })##df_filt~END
  
  ## df_filt_DT ####
  output$df_filt_DT <- DT::renderDT({
    
    df_f <- df_filt()
    
    # Remove 2 columns that have so much info that blows up size of rows.
    str_names <- names(df_f)
    str_names_drop <- c("sa.sig.inc", "sa.sig.dec")
    str_names_keep <- str_names[!(str_names %in% str_names_drop)]
    
    return(df_f[, str_names_keep])
    
  }##expression~END
  , filter="top"
  , caption = "Table 3. Filtered data."
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                 , autoWidth = TRUE
                 , columnDefs = list(list(width = col_width_manual
                                          , targets = df_filt_cols_widen())))
  )##df_filt_DT~END
  
  ## df_filt_dups ####
  df_filt_dups <- eventReactive (filt_watch4(), {
    # data
    
    if(is.null(click_filetype$data)){
      return(NULL)
    } else if(is.null(click_filtdata$data)){
      df_tmp <- df_import()
    } else {
      df_tmp <- df_filt()
    }## IF ~ filetype and filter click ~ END
    
    # If no data selected
    if(is.null(click_filetype$data)){
      n_dups <- "NA"
    } else if (click_filetype$data == "user") {
      # Check and report back number and locations of duplicates
      tib_n_dups <- dplyr::summarize(dplyr::group_by(df_tmp, station)
                                     , n_Parameter = dplyr::n_distinct(parmName, na.rm=TRUE)
                                     , n_GAM = dplyr::n_distinct(gamName, na.rm=TRUE)
                                     , n_Layer = dplyr::n_distinct(layer, na.rm=TRUE)
                                     , n_Period = dplyr::n_distinct(periodName, na.rm=TRUE)
                                     , n_Season = dplyr::n_distinct(seasonName, na.rm=TRUE)
                                     , .groups = "drop_last")
    } else if (click_filetype$data == "final"){
      tib_n_dups <- dplyr::summarize(dplyr::group_by(df_tmp, station)
                                     , n_mapLayer = dplyr::n_distinct(mapLayer, na.rm=TRUE)
                                     , .groups = "drop_last")
    }## IF ~ click_filetype$data ~ END
    
    #
    return(as.data.frame(tib_n_dups))
    #
  })##df_filt_dups~END
  
  ## df_filt_cols_widen ####
  df_filt_cols_widen <- eventReactive(file_watch(), {
    # uses a multi-item reactive so keep on a single line
    
    col_widen <-  c("station"
                    , "layer"
                    , "latitude"
                    , "longitude"
                    , "cbSeg92"
                    #, "state"
                    #, "stationGrpName"
                    , "parmName"
                    , "gamName"
                    , "periodName"
                    , "seasonName"
                    #, "gamDiff.bl.mn.obs"
                    #, "gamDiff.cr.mn.obs"
                    #, "gamDiff.abs.chg.obs"
                    #, "gamDiff.pct.chg"
                    #, "gamDiff.chg.pval"
                    , "mapLayer")
    
    
    df_f <- df_filt()
    
    # Need is.null as first rather than last.
    # It will always trigger on loading of the app
    
    if(is.null(click_filetype$data)){
      return(NULL)
    } else {
      # Same for both file types
      #
      # get col nums of matching names
      col_num_widen <- match(col_widen, names(df_f))
      # remove NA
      col_nums <- col_num_widen[!is.na(col_num_widen)]
    }##IF~is.null~END
    #
    return(col_nums)
    #
  })##df_filt_cols_widen ~ END
  
  ## df_filt_dups_DT ####
  output$df_filt_dups_DT <- DT::renderDT({
      df_tmp <- df_filt_dups()
      return(df_tmp)
    }##expression~END
    , filter="top"
    , caption = "Table 2. Station summary."
    , rownames = FALSE
    , options=list(scrollX=TRUE
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000) )
  )##df_filt_dups_DT~END
  
  
  ## df_filt_dups_num ####
  # Number of sites with more than 1 entry per field
  output$filt_dups_num <- renderText({
    #
    #input$but_filt_apply
    #input$fn_input
    # data
    if(is.null(click_filtdata$data)){
      df_tmp <- df_import()
    } else {
      df_tmp <- df_filt()
    }##IF~END
    #
    
    # If no data selected
    if(is.null(click_filetype$data)){
       n_dups <- "NA"
    } else if (click_filetype$data == "user") {
      # Check and report back number and locations of duplicates
      tib_n_dups <- dplyr::summarize(dplyr::group_by(df_tmp, station)
                                  , n_Parameter = dplyr::n_distinct(parmName, na.rm=TRUE)
                                  , n_GAM = dplyr::n_distinct(gamName, na.rm=TRUE)
                                  , n_Layer = dplyr::n_distinct(layer, na.rm=TRUE)
                                  , n_Period = dplyr::n_distinct(periodName, na.rm=TRUE)
                                  , n_Season = dplyr::n_distinct(seasonName, na.rm=TRUE)
                                  , .groups = "drop_last")
      n_dups <- sum(tib_n_dups[, 2:ncol(tib_n_dups)] != 1)
    } else if (click_filetype$data == "final"){
      tib_n_dups <- dplyr::summarize(dplyr::group_by(df_tmp, station)
                                  , n_mapLayer = dplyr::n_distinct(mapLayer, na.rm=TRUE)
                                  , .groups = "drop_last")
      n_dups <- sum(tib_n_dups[, 2:ncol(tib_n_dups)] != 1)
    }## IF ~ click_filetype$data ~ END
      
    # # DEBUG
    # tmp_stat <- c("CB1.1", "CB2.1", "CB3.3E", "CB3.3W", "CB4.1W")
    # tib_abc <- tibble(station = tmp_stat
    #                      , n_Parameter = 1
    #                      , n_GAM = 2
    #                      , n_Layer = 1
    #                      , n_Period = 1
    #                      , n_Season = 1)
      
    str_n_dups <- paste0("Number of stations with more than 1 record = ", n_dups)
   # str_n_dups <- n_dups
    #
    return(str_n_dups)
    #
  })##df_filt_dups_num~END
  
  # output$filt_dups_num_text <- renderText({
  #   str_n_dups_text <- paste0("Numbers of stations with more than one record = ", filt_dups_num())
  # })
  
  ## filter helper text ####
  output$txt_nrow_df_import <- renderText({
    ifelse(is.null(input$fn_input)
           , NULL
           , paste0("Records, df_import, n = ", nrow(df_filt_dups()), "."))
           #, paste0("Records, df_import, n = ", nrow(df_import()), "."))
  })
  # nrow_df_import <- eventReactive(input$but_filt_apply{
  #   inFile <- input$fn_input
  #   
  #   if (is.null(inFile)){
  #     return(NULL)
  #   }##IF~is.null~END
  #   
  #   nrow_df <- nrow(df_import())
  #   
  #   return(nrow_df)
  # })
  
  # UI ----
  ## UI, Filter, Data ####
  
  ## UI, Filter, Collapse ####
  output$filt_collapse <- renderUI({
    # filters change based on file format; final vs. user.
    # Default is "final" file.
    if(is.null(click_filetype$data)) {
      #return(NULL)
      p("No file loaded.  Return to step '1. Select Data'.")
    } else if(click_filetype$data == "user") {
      # "User" file filters
      bsCollapse(multiple = TRUE,
                 bsCollapsePanel("Filter by 'State'", style='info',
                                 fluidRow(column(1), column(10, radioButtons('sel_state', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                 uiOutput('filt_state')
                                 )##bsCollapsePanel~state~END
                 , bsCollapsePanel("Filter by 'CB Segment'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_cbSeg92', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_cbSeg92')
                                  )##bsCollapsePanel~cbSeg92~END
                 , bsCollapsePanel("Filter by 'Station Group'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_stationGrpName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_stationGrpName')
                     )##bsCollapsePanel~stationGrpName~END
                 , bsCollapsePanel("Filter by 'Station Identifier'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_station', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_station')
                     )##bsCollapsePanel~station~END
                 , bsCollapsePanel("Filter by 'Full Parameter Name'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_parmName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_parmName')
                     )##bsCollapsePanel~parmName~END
                 , bsCollapsePanel("Filter by 'GAM Formula Name'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_gamName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_gamName')
                      )##bsCollapsePanel~gamName~END
                 , bsCollapsePanel("Filter by 'Sample Layer'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_layer', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_layer')
                      )##bsCollapsePanel~layer~END
                 , bsCollapsePanel("Filter by 'Period Name'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_periodName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_periodName')
                     )##bsCollapsePanel~periodName~END
                 , bsCollapsePanel("Filter by 'Season Name'", style='info',
                                   fluidRow(column(1), column(10, radioButtons('sel_seasonName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_seasonName')
                     )##bsCollapsePanel~seasonName~END
                 #, open = "Filter by 'Season Name'" # to auto open panels
      )##bsCollapse~END
    } else if (click_filetype$data == "final"){
      # "final" file filters
      bsCollapse(multiple = TRUE,
         bsCollapsePanel("Filter by 'State'", style='info',
                         fluidRow(column(1), column(10, radioButtons('sel_state', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                         uiOutput('filt_state')
             )##bsCollapsePanel~state~END
         , bsCollapsePanel("Filter by 'CB Segment'", style='info',
                           fluidRow(column(1), column(10, radioButtons('sel_cbSeg92', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                           uiOutput('filt_cbSeg92')
             )##bsCollapsePanel~cbSeg92~END
         , bsCollapsePanel("Filter by 'Station Group'", style='info',
                           fluidRow(column(1), column(10, radioButtons('sel_stationGrpName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                           uiOutput('filt_stationGrpName')
             )##bsCollapsePanel~stationGrpName~END
         , bsCollapsePanel("Filter by 'Station Identifier'", style='info',
                           fluidRow(column(1), column(10, radioButtons('sel_station', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                           uiOutput('filt_station')
             )##bsCollapsePanel~station~END
         , bsCollapsePanel("Filter by 'Map Layer'", style='info',
                           #fluidRow(column(1), column(10, radioButtons('sel_mapLayer', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                    uiOutput('filt_mapLayer')
              )##bsCollapsePanel~station~END
         , open = "Filter by 'Map Layer'" # to auto open panels
      )##bsCollapse~END
      #radioButtons("radio_mapLayer", "Select Map Layer", choices = pick_mapLayer)
      
    }## IF ~ file type ~ END

    
  })## filt_collapse
  
  output$filt_state <- renderUI({
    str_col <- "state"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_state~END
  #
  output$filt_cbSeg92 <- renderUI({
    str_col <- "cbSeg92"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI,  h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_cbSeg92~END
  #
  output$filt_stationGrpName <- renderUI({
    str_col <- "stationGrpName"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(input$sel_stationGrpName == 1){
                       unique(df_x[, str_col])
                     } else {NULL})

    )##fluidRow~END
  })##filt_stationGrpName~END
  #
  output$filt_station <- renderUI({
    str_col <- "station"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_station~END
  #
  output$filt_layer <- renderUI({
    str_col <- "layer"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_layer~END
  
  #
  output$filt_parmName <- renderUI({
    str_col <- "parmName"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})

    )##fluidRow~END
  })##filt_parmName~END
  #
  output$filt_gamName <- renderUI({
    str_col <- "gamName"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_gamName~END
  #
  output$filt_periodName <- renderUI({
    str_col <- "periodName"
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(input$sel_periodName == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_periodName~END
  #
  output$filt_seasonName <- renderUI({
    str_col <- "seasonName"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = sort(unique(df_x[, str_col])),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_seasonNameEND
  
  output$filt_mapLayer <- renderUI({
    if(click_filetype$data == "final"){
      str_col <- "mapLayer"
      str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
      str_SI <- paste0("SI_", str_col)
      df_x <- df_import()
      fluidRow(
        selectInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                    list("Parameter | Layer | Season" = sort(unique(df_x[, str_col]))),
                       multiple = FALSE
        )## selectInput ~ mapLayer
      )##fluidRow~END
    } else {
      return(NULL)
    }## IF ~ click_filetype$data == "final" ~ END
  })##filt_mapLayer~END
  
    ## UI, Map Options ####
  output$opt_var <- renderUI({
    str_col <- "variable"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_gamDiff_Desc,
                     multiple = FALSE,
                     selected = pick_gamDiff_Desc[1])

    )##fluidRow~END
  })##opt_var~END
  #
  output$opt_classInt <- renderUI({
    str_col <- "classInt"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_classInt,
                     multiple = FALSE,
                     selected = pick_classInt[3])
      
    )##fluidRow~END
  })##classInt~END
  #
  output$opt_pal <- renderUI({
    str_col <- "pal"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_pal_names,
                     multiple = FALSE,
                     selected = pick_pal_names[1])
      
    )##fluidRow~END
  })##opt_pal~END
  
  output$opt_pal_change <- renderUI({
    str_col <- "pal_change"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_pal_change,
                     multiple = FALSE,
                     selected = pick_pal_change[1])
      
    )##fluidRow~END
  })##opt_pal_change~END
  #
  output$opt_ext <- renderUI({
    str_col <- "ext"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_ext,
                     multiple = FALSE,
                     selected = pick_ext[3])
      
    )##fluidRow~END
  })##opt_ext~END
  #
  output$opt_riverNames <- renderUI({
    str_col <- "riverNames"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Add ", str_col, ":")),
                     choices = c("Yes", "No"),
                     multiple = FALSE,
                     selected = "Yes")
      
    )##fluidRow~END
  })##riverNames~END
  #
  
  # TREND Map
  output$opt_ext_t <- renderUI({
    str_col <- "ext"
    str_SI <- paste0("SI_", str_col, "_t")
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_ext,
                     multiple = FALSE,
                     selected = pick_ext[3])
      
    )##fluidRow~END
  })##opt_ext_t~END
  #
  output$opt_riverNames_t <- renderUI({
    str_col <- "riverNames"
    str_SI <- paste0("SI_", str_col, "_t")
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Add ", str_col, ":")),
                     choices = c("Yes", "No"),
                     multiple = FALSE,
                     selected = "Yes")
      
    )##fluidRow~END
  })##riverNames_t~END
  #
  output$opt_upisgood <- renderUI({
    str_col <- "upisgood"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Increasing trend is 'good'?")),
                     choices = c("TRUE", "FALSE"),
                     multiple = FALSE,
                     selected = "TRUE")
      
    )##fluidRow~END
  })##opt_upisgood~END
  #
  output$opt_zoomregion_t <- renderUI({
    str_col <- "zoomregion"
    str_SI <- paste0("SI_", str_col, "_t")
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_zoomregion,
                     multiple = FALSE,
                     selected = pick_ext[1])
      
    )##fluidRow~END
  })##opt_zoomregion_t~END
  #
  output$opt_zoomregion_r <- renderUI({
    str_col <- "zoomregion"
    str_SI <- paste0("SI_", str_col, "_r")
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_zoomregion,
                     multiple = FALSE,
                     selected = pick_ext[1])
      
    )##fluidRow~END
  })##opt_zoomregion_r~END
  
 
  
  # MAP, Range ####
  ## MAP, Range, user ----
  map_range <- eventReactive (input$but_map_range, {

    # start with base map
    m_r <- map_base
    
    # data for plot
    df_mr <- df_filt()
    mr_cI_type  <- ifelse(is.null(input$SI_classInt)
                          , "pretty"
                          , input$SI_classInt)
    mr_pal <- ifelse(is.null(input$SI_pal)
                     , "PuOr"
                     , pick_pal[match(input$SI_pal, pick_pal_names)])
    mr_var_name <- ifelse(is.null(input$SI_variable)
                          , "Baseline mean"
                          , input$SI_variable)
    mr_var <- pick_gamDiff[match(mr_var_name, pick_gamDiff_Desc)]
    # mr_var <- input$SI_variable
    # mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    brks_user <- eval(parse(text = paste0("c(", input$breaks, ")"))) 
    #evals to NULL if left blank
    
    # breaks vs numclasses
    ## input is "" for blank
    if(is.null(brks_user)==TRUE){
      # no breaks, use slider for num classes
      mr_numclass <- input$numclass
      # derive breaks from user n and style
      mr_cI_val <- classInt::classIntervals(df_mr[, mr_var]
                                            , mr_numclass
                                            , mr_cI_type)
      # Redo num classes as "pretty" picks its own number of breaks
      mr_numclass <- ifelse(mr_cI_type=="pretty"
                            , length(mr_cI_val$brks) - 1
                            , mr_numclass)
      #mr_numclass <- ifelse(mr_cI_type=="pretty"
      # , length(mr_cI_val$brks)
      # , mr_numclass)
      # breaks
      mr_brks <- mr_cI_val$brks
    } else {
      # user breaks, use instead of slider for numclasses
      mr_numclass <- length(brks_user) - 1
      # breaks
      mr_brks <- brks_user
    }##IF~brks_user~END
    
    mr_pal_col <- RColorBrewer::brewer.pal(n=mr_numclass, name=mr_pal)
    
    # River Names
    boo_riverNames <- ifelse(is.null(input$SI_riverNames)
                             , "Yes"
                             , input$SI_riverNames)
   if(boo_riverNames == "Yes"){
      m_r <- m_r + 
        annotate(geom = "text"
                 , x = as.numeric(lab_Sus[2])
                 , y=as.numeric(lab_Sus[3])
                 , label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2])
                 , y=as.numeric(lab_Pat[3])
                 , label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2])
                 , y=as.numeric(lab_Cho[3])
                 , label=lab_Cho[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2])
                 , y=as.numeric(lab_Pot[3])
                 , label=lab_Pot[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2])
                 , y=as.numeric(lab_Rap[3])
                 , label=lab_Rap[1], hjust=1) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2])
                 , y=as.numeric(lab_Yor[3])
                 , label=lab_Yor[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2])
                 , y=as.numeric(lab_Jam[3])
                 , label=lab_Jam[1], hjust=0)
    }##IF~riverNames~END
    
    # Title
    mr_title <- input$map_range_title
    if(!is.null(mr_title)){
      m_r <- m_r +
       labs(title=paste(mr_title, collapse="; "))
       # labs(title=paste(mr_pal_col, collapse="; "))
    }##IF~riverNames~END
    
    # Points
    # fortify
    fort_df_mr <- ggplot2::fortify(df_mr)
    # Add to data frame

    ## Break, Color
    fort_df_mr$map_brk_col <- cut(fort_df_mr[, mr_var]
                                  , breaks = mr_brks
                                  #, labels = brewer.pal(max(3, mr_numclass)
                                  #, mr_pal)[1:mr_numclass]
                                  , labels = mr_pal_col
                                  , include.lowest = TRUE
                                  )
    # Minimum of 3 different levels or get warning
    ## Break, Text
    fort_df_mr$map_brk_num <- cut(fort_df_mr[, mr_var]
                                  , breaks = mr_brks
                                  , include.lowest = TRUE
                                  )
    
    # Points, Add
    m_r <- m_r + geom_point(data=fort_df_mr
                                    , aes_string(x =" longitude"
                                                 , y = "latitude"
                                               #  , fill = "map_brk_num")
                                                , fill = "map_brk_col")
                                    , size = 4
                                    , pch = 21
                                    , color = "black"
                                    , na.rm = TRUE)
 # browser()  
    # Points, Fill
#    m_r <- m_r + scale_fill_brewer(palette = mr_pal
#                                   , name = mr_var_name
#                                   , labels = levels(fort_df_mr$map_brk_num)
#                                    )
    # m_r <- m_r + scale_fill_discrete(name = mr_var_name
    #                     #, labels = paste(c(">", rep("< "
    # , length(mr_cI_val$brks)-1)), round(mr_cI_val$brks, 2))) +
    #                     , labels = levels(fort_df_mr$map_brk_num)
    #                     )
    
    # 2021-08-06, Issue #51, 
    # Custom breaks - legend has only values for where have data 
    # Should have all breaks
    
    m_r <- m_r + scale_fill_brewer(palette = mr_pal
                                   , name = mr_var_name
                                   , labels = levels(fort_df_mr$map_brk_num)
                                   , drop = FALSE
    )
    
    
    
    # Legend 
    m_r <- m_r + theme(legend.position = "bottom"
                       , legend.box = "horizontal"
                       , legend.title = element_text(face = "bold"))

    
   # Zoom
   zoom_buffer <- input$map_range_val_zoom
   ## Zoom, points
   x_min_pts <- min(fort_df_mr[, "longitude"])
   x_max_pts <- max(fort_df_mr[, "longitude"])
   y_min_pts <- min(fort_df_mr[, "latitude"])
   y_max_pts <- max(fort_df_mr[, "latitude"])
   # Bounding Box
   #                EXT_MIN_X, EXT_MIN_Y, EXT_MAX_X, EXT_MAX_Y
   bbox_points <- c(x_min_pts, y_min_pts, x_max_pts, y_max_pts)

    zoomregion <- input$SI_zoomregion_r

    if(is.null(zoomregion)==TRUE){
     # do nothing
    } else if (zoomregion == "none") {
      # more nothing
    } else {
      # convert to values
      zoomregion_bbox <- eval(parse(text=pick_zoomregion_bbox[match(zoomregion
                                                           , pick_zoomregion)]))
      #zoomregion_bbox <- bbox_Cho
      # Zoom, Limits
      x_min <- zoomregion_bbox[1] - zoom_buffer
      x_max <- zoomregion_bbox[3] + zoom_buffer
      y_min <- zoomregion_bbox[2] - zoom_buffer / (map_coord_ratio * 5)
      y_max <- zoomregion_bbox[4] + zoom_buffer / (map_coord_ratio * 5)
      # replot map with zoom region and buffer
      m_r <- m_r + ggplot2::coord_fixed(ratio = map_coord_ratio
                                        , xlim = c(x_min, x_max)
                                        , ylim = c(y_min, y_max))
   }##IF~opt_zoomregion_t~END
    
    
    # # save map
    mr_ext <- ifelse(is.null(input$SI_ext), "png", input$SI_ext) #"png"
    #date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- file.path("map", paste0("map_range.", mr_ext))
    ggplot2::ggsave(fn_out
                    , plot = m_r
                    , device = mr_ext
                    , height = plot_h
                    , width = plot_w
                    , units = plot_units
                    , scale = plot_scale
                    , bg = "white")
    
    # enable save button
    shinyjs::enable("but_map_range_save")
    #
    return(m_r)
    #return(ggplotly(m_r))
    #
  })##map_range~END
  
  ## Map, Range, default ----
  map_range_filt_default <- eventReactive (input$but_filt_apply, {
    # Add all defaults
    #
    m_r <- map_base
    #
    # data for plot
    df_mr <- df_filt()
    mr_cI_type  <- "pretty"
    mr_pal <- "PuOr"
    mr_var_name <- "Baseline mean"
    mr_var <- pick_gamDiff[match(mr_var_name, pick_gamDiff_Desc)]
    # mr_var <- input$SI_variable
    # mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    brks_user <- NULL
    #evals to NULL if left blank
    
    # breaks vs numclasses
      # no breaks, use slider for num classes
      mr_numclass <- input$numclass
      # derive breaks from user n and style
      mr_cI_val <- classInt::classIntervals(df_mr[, mr_var]
                                            , mr_numclass
                                            , mr_cI_type)
      # Redo num classes as "pretty" picks its own number of breaks
      mr_numclass <- ifelse(mr_cI_type=="pretty"
                            , length(mr_cI_val$brks) - 1
                            , mr_numclass)
      #mr_numclass <- ifelse(mr_cI_type=="pretty"
      # , length(mr_cI_val$brks)
      # , mr_numclass)
      # breaks
      mr_brks <- mr_cI_val$brks
    
    mr_pal_col <- RColorBrewer::brewer.pal(n=mr_numclass, name=mr_pal)
    
    # River Names
    boo_riverNames <- "Yes"
    
    if(boo_riverNames == "Yes"){
      m_r <- m_r + 
        annotate(geom = "text"
                 , x = as.numeric(lab_Sus[2])
                 , y=as.numeric(lab_Sus[3])
                 , label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2])
                 , y=as.numeric(lab_Pat[3])
                 , label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2])
                 , y=as.numeric(lab_Cho[3])
                 , label=lab_Cho[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2])
                 , y=as.numeric(lab_Pot[3])
                 , label=lab_Pot[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2])
                 , y=as.numeric(lab_Rap[3])
                 , label=lab_Rap[1], hjust=1) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2])
                 , y=as.numeric(lab_Yor[3])
                 , label=lab_Yor[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2])
                 , y=as.numeric(lab_Jam[3])
                 , label=lab_Jam[1], hjust=0)
    }##IF~riverNames~END
    
    # Title

    sep1 <- ": "
    sep2 <- "\n" #"; "
    #
    mr_title_parmName   <- sort(unique(df_mr[, "parmName"]))
    mr_title_gamName    <- sort(unique(df_mr[, "gamName"]))
    mr_title_periodName <- sort(unique(df_mr[, "periodName"]))
    #
   
      # "User" file
      mr_title_layer      <- sort(unique(df_mr[, "layer"]))
      mr_title_seasonName <- sort(unique(df_mr[, "seasonName"]))
      str_title <- paste(paste(mr_title_parmName, collapse = ", ")
                         , paste("GAM", paste(mr_title_gamName
                                              , collapse = ", ")
                                 , sep = sep1)
                         , paste("Layer", paste(mr_title_layer
                                                , collapse = ", ")
                                 , sep = sep1)
                         , paste("Period", paste(mr_title_periodName
                                                 , collapse = ", ")
                                 , sep = sep1)
                         , paste("Season", paste(mr_title_seasonName
                                                 , collapse = ", ")
                                 , sep = sep1)
                         , sep = sep2)

    
    
    mr_title <- str_title
    
    if(!is.null(mr_title)){
      m_r <- m_r +
        labs(title=paste(mr_title, collapse="; "))
      # labs(title=paste(mr_pal_col, collapse="; "))
    }##IF~riverNames~END
    
    
    # Points
    # fortify
    fort_df_mr <- ggplot2::fortify(df_mr)
    # Add to data frame
    
    ## Break, Color
    fort_df_mr$map_brk_col <- cut(fort_df_mr[, mr_var]
                                  , breaks = mr_brks
                                  #, labels = brewer.pal(max(3, mr_numclass)
                                  #, mr_pal)[1:mr_numclass]
                                  , labels = mr_pal_col
                                  , include.lowest = TRUE
    )
    # Minimum of 3 different levels or get warning
    ## Break, Text
    fort_df_mr$map_brk_num <- cut(fort_df_mr[, mr_var]
                                  , breaks = mr_brks
                                  , include.lowest = TRUE
    )
    
    # Points, Add
    m_r <- m_r + geom_point(data=fort_df_mr
                            , aes_string(x =" longitude"
                                         , y = "latitude"
                                         #  , fill = "map_brk_num")
                                         , fill = "map_brk_col")
                            , size = 4
                            , pch = 21
                            , color = "black"
                            , na.rm = TRUE)
    
    
    
    m_r <- m_r + scale_fill_brewer(palette = mr_pal
                                   , name = mr_var_name
                                   , labels = levels(fort_df_mr$map_brk_num)
                                   , drop = FALSE
    )
    
    
    
    # Legend 
    m_r <- m_r + theme(legend.position = "bottom"
                       , legend.box = "horizontal"
                       , legend.title = element_text(face = "bold"))
    
    
    
    # # save map
    mr_ext <- "png"
    #date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- file.path("map", paste0("map_range.", mr_ext))
    ggplot2::ggsave(fn_out
                    , plot = m_r
                    , device = mr_ext
                    , height = plot_h
                    , width = plot_w
                    , units = plot_units
                    , scale = plot_scale
                    , bg = "white")
    #
    return(m_r)
    #
  })## map_range_filt_default
  
  output$map_r_render_example <- renderPlot({
    print(m_r_d) 
  })##map_r~END
  
  output$map_r_render <- renderPlot({
  #output$map_r_render <- renderPlotly({
    # default map to show
    if(input$but_map_range == 0){
      #if(input$but_filt_apply == 0){
        m_r_2 <- map_base
      # } else {
      #   m_r_2 <- map_range_filt_default # different map
      # }## IF ~ input$but_filt_apply
    } else {
      m_r_2 <- map_range()  
    }
    print(m_r_2) 
    #ggplotly(m_r_2)
  })##map_r~END
  
  ## but_mr_title ####
  observeEvent(input$but_mr_title, {
    sep1 <- ": "
    sep2 <- "\n" #"; "
    #
    df_filt_mr <- df_filt()
    #
    mr_title_parmName   <- sort(unique(df_filt_mr[, "parmName"]))
    mr_title_gamName    <- sort(unique(df_filt_mr[, "gamName"]))
    mr_title_periodName <- sort(unique(df_filt_mr[, "periodName"]))
    #
    if(!is.null(input$fn_input)){
      # "User" file
      mr_title_layer      <- sort(unique(df_filt_mr[, "layer"]))
      mr_title_seasonName <- sort(unique(df_filt_mr[, "seasonName"]))
      str_title <- paste(paste(mr_title_parmName, collapse = ", ")
                         , paste("GAM", paste(mr_title_gamName
                                              , collapse = ", ")
                                 , sep = sep1)
                         , paste("Layer", paste(mr_title_layer
                                                , collapse = ", ")
                                 , sep = sep1)
                         , paste("Period", paste(mr_title_periodName
                                                 , collapse = ", ")
                                 , sep = sep1)
                         , paste("Season", paste(mr_title_seasonName
                                                 , collapse = ", ")
                                 , sep = sep1)
                         , sep = sep2)
      
    } else {
      # "final" file
      #str_mapLayer     <- unlist(strsplit(input$SI_mapLayer, "[|]"))
      mr_title_mapLayer <- unlist(strsplit(df_filt_mr[, "mapLayer"], "[|]"))
      str_title <- paste(paste(mr_title_parmName, collapse = ", ")
                         , paste("GAM", paste(mr_title_gamName
                                              , collapse = ", ")
                                 , sep = sep1)
                         , paste("Layer", mr_title_mapLayer[2], sep = sep1)
                         , paste("Period", paste(mr_title_periodName
                                                 , collapse = ", ")
                                 , sep = sep1)
                         , paste("Season", mr_title_mapLayer[3], sep = sep1)
                         , sep = sep2)
    }## IF ~ is.null(input$fn_input) ~ END
    #
    updateTextAreaInput(session, "map_range_title", value = str_title)
    # max is 89 characters, if need to wrap dynamically
    #https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-set-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
  })##observerEvent~input$but_mr_title~END
  
    
  ## but_map_range_save ####
  output$but_map_range_save <- downloadHandler(
    filename = function() {
      mr_ext <- input$SI_ext
      fn_out <- file.path("map", paste0("map_range.", mr_ext))
      if(file.exists(fn_out)==TRUE) {
        mr_ext <- input$SI_ext
        date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("map_range_", date_time, ".", mr_ext)
      } else {
          "Error_UpdateMapBeforeSave.pdf"
      }
      # #paste0(input$fn_input, input$SI_ext)
    }, ##filename~END
    content = function(fn) {
       mr_ext <- input$SI_ext
       fn_out <- file.path("map", paste0("map_range.", mr_ext))
       #print(map_range())
     # ggplot2::ggsave(file, plot = ggplot2::last_plot(), device = ext, height = 9, width = 9/1.5, units = "in" )
      # #file.copy("map_range.pdf", fn, overwrite=TRUE)
       if(file.exists(fn_out)==TRUE){
         file.copy(fn_out, fn, overwrite = TRUE)
       } else {
         fn_out_error <- file.path("map", "Error_UpdateMapBeforeSave.pdf")
         file.copy(fn_out_error, fn, overwrite = TRUE)
       }
    }##content~END
  )##map_r_save~END
  # Need error handling in case change EXT but didn't "update" the map.
  # expected name doesn't match the saved file name.
  # file.exists(fn_out)
  
  
  # MAP, Trend ####
  map_trend <- eventReactive (input$but_map_trend, {
    
    # validate p-value, poss > sig
    validate(need(input$map_trend_pval_poss > input$map_trend_pval_sig
                  , paste0("ERROR\nThe 'possible' p-value ("
                           , input$map_trend_pval_poss
                           , ") should be greater than the 'significant' p-value ("
                           , input$map_trend_pval_sig
                           , ").")))

    # start with base map
    m_t <- map_base
    mt_pal <- ifelse(is.null(input$SI_pal_change)
                     , "Orange_Green"
                     , input$SI_pal_change)
    
    if(mt_pal == "Orange_Green"){
      pal_mt <- pal_change_OrGn
    } else if (mt_pal == "Red_Blue") {
      pal_mt <- pal_change_RdBu
    } else if (mt_pal == "Purple_Green") {
      pal_mt <- pal_change_PRGn
    } ## IF ~ mt_pal ~ END
      
    # data for plot
    df_mt <- df_filt()
    # mr_cI_type  <- input$SI_classInt
    # mr_pal <- input$SI_pal
    # mr_var <- input$SI_variable
    # mr_numclass <- input$numclass
    # mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    # mr_pal_col <- RColorBrewer::brewer.pal(n=mr_numclass, name=mr_pal)
    # 
    # mr_cI_val <- classInt::classIntervals(df_mr[, mr_var], mr_numclass, mr_cI_type)
    
    # River Names
    boo_riverNames_t <- ifelse(is.null(input$SI_riverNames_t)
                               , "Yes"
                               , input$SI_riverNames_t)
    if(boo_riverNames_t == "Yes"){
      m_t <- m_t + 
        annotate(geom = "text", x = as.numeric(lab_Sus[2])
                 , y=as.numeric(lab_Sus[3])
                 , label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2])
                 , y=as.numeric(lab_Pat[3])
                 , label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2])
                 , y=as.numeric(lab_Cho[3])
                 , label=lab_Cho[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2])
                 , y=as.numeric(lab_Pot[3])
                 , label=lab_Pot[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2])
                 , y=as.numeric(lab_Rap[3])
                 , label=lab_Rap[1], hjust=1) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2])
                 , y=as.numeric(lab_Yor[3])
                 , label=lab_Yor[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2])
                 , y=as.numeric(lab_Jam[3])
                 , label=lab_Jam[1], hjust=0)
    }##IF~riverNames~END

    # Title
    mt_title <- input$map_trend_title
    if(!is.null(mt_title)==TRUE){
      m_t <- m_t + labs(title=paste(mt_title, collapse="; "))
      # labs(title=paste(mr_pal_col, collapse="; "))
    }##IF~riverNames~END
    
    # Points ##
    
    boo_upisgood   <- ifelse(is.null(input$SI_upisgood)
                             , TRUE
                             , input$SI_upisgood) # TRUE
    chg_pval_poss <- input$map_trend_pval_poss # 0.25
    chg_pval_sig <- input$map_trend_pval_sig # 0.05
    #
    #if (boo_upisgood == TRUE){
      df_mt[df_mt[, "gamDiff.chg.pval"] > chg_pval_poss, "ChangeClass"] <- "NS"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
              df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posIncr"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
              df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posDecr"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
              df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigIncr"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
              df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigDecr"
   # } else {
   # df_mt[df_mt[, "gamDiff.chg.pval"] > chg_pval_poss, "ChangeClass"] <- "NS"
   # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
   #         df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posIncr"
   # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
   #         df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posDecr"
   # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
   #         df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigIncr"
   # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
   #         df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigDecr"
   # }##boo_upisgood~END
      
    # Caption - p-value
    m_t <- m_t + ggplot2::labs(caption = paste0("p-value thresholds (possible
                                                        , significant) = "
                                                , chg_pval_poss
                                                , ", "
                                                , chg_pval_sig))
      
    
    # fortify
    fort_df_mt <- ggplot2::fortify(df_mt)
    #
    #
    trend_ChangeClass <- c("sigDecr"
                           , "sigIncr"
                           , "posDecr"
                           , "posIncr"
                           , "NS")
    trend_leg_label   <- c("Significant Decrease"
                           , "Significant Increase"
                           , "Possible Decrease"
                           , "Possible Increase"
                           , "Unlikely")
    # trend_leg_color   <- c("orange", "green", "orange", "green", "dark gray")
    # trend_leg_shape   <- c("25", "24", "21", "21", "23")
    trend_leg_size    <- c("4", "4", "3", "3", "2")
    
    # Default (up is good = TRUE)
    trend_leg_color   <- c(pal_mt[1]  # "orange"
                           , pal_mt[2]# "green"
                           , pal_mt[1]# "orange"
                           , pal_mt[2]# "green"
                           , "dark gray")
    trend_leg_shape   <- c("25", "24", "21", "21", "23")
    manval_color <- c("sigDecr" = pal_mt[1]   # "orange"
                      , "sigIncr" = pal_mt[2] # "green"
                      , "posDecr" = pal_mt[1] # "orange"
                      , "posIncr" = pal_mt[2] # "green"
                      , "NS" = "dark gray")
    manval_shape <- c("sigDecr" = 25
                      , "sigIncr" = 24
                      , "posDecr" = 21
                      , "posIncr" = 21
                      , "NS" = 23)
    manval_size  <- c("sigDecr" = 4
                      , "sigIncr" = 4
                      , "posDecr" = 3
                      , "posIncr" = 3
                      , "NS" = 2)
    
    if(boo_upisgood == FALSE){
      # Direction Arrows the same
      # Just the colors change
      trend_leg_color   <- c(pal_mt[2] # "green"
                             , pal_mt[1] # "orange"
                             , pal_mt[2] # "green"
                             , pal_mt[1] # "orange"
                             , "dark gray")
      manval_color <- c("sigDecr" = pal_mt[2] # "green"
                        , "sigIncr" = pal_mt[1] # "orange"
                        , "posDecr" = pal_mt[2] # "green"
                        , "posIncr" = pal_mt[1] # "orange"
                        , "NS" = "dark gray")
      
    }##IF~boo_upisgood~END
    
    
    # Add to data frame
    fort_df_mt[, "ChangeClass"] <- factor(fort_df_mt[, "ChangeClass"]
                                          , trend_ChangeClass)
    # 
    fort_df_mt$ChangeClass_color <- trend_leg_color[match(fort_df_mt$ChangeClass
                                                          , trend_ChangeClass)]
    fort_df_mt$ChangeClass_shape <- trend_leg_shape[match(fort_df_mt$ChangeClass
                                                          , trend_ChangeClass)]
    fort_df_mt$ChangeClass_size  <- trend_leg_size[match(fort_df_mt$ChangeClass
                                                         , trend_ChangeClass)]
    
    m_t <- m_t + geom_point(data=fort_df_mt
                                   , aes_string(x = "longitude"
                                                , y = "latitude"
                                                #, group ="ChangeClass"
                                                #, color="ChangeClass"
                                                , shape = "ChangeClass"
                                                , size = "ChangeClass"
                                                , fill = "ChangeClass"
                                                )
                                     , color = "black"
                                   # , color = fort_df_mt$ChangeClass_color 
                                   # , shape = as.numeric(fort_df_mt$ChangeClass_shape)
                                   # , size = as.numeric(fort_df_mt$ChangeClass_size)
                                   # , fill = fort_df_mt$ChangeClass_color
                                   #, na.rm=TRUE
                                   ) +
      # scale_color_manual(name = "Type of change", labels = trend_leg_label
      #                    , values = manval_color, drop = FALSE ) +
      scale_shape_manual(name = "Type of change"
                         , labels = trend_leg_label
                         , values = manval_shape
                         , drop = FALSE ) + 
      scale_fill_manual( name = "Type of change"
                         , labels = trend_leg_label
                         , values = manval_color
                         , drop = FALSE ) + 
      scale_size_manual( name = "Type of change"
                         , labels = trend_leg_label
                         , values = manval_size
                         ,  drop = FALSE ) +
      theme(legend.position = c(1, 0.12)
            , legend.justification = c(1, 0)
            , legend.title = element_text(face = "bold"))
      # theme(legend.position = "bottom"
      #       , legend.box = "horizontal"
      #       , legend.title=element_text(face="bold"))
    # could use position as coordinates but uses 0:1 not coordinates of the plot.
    
    # drop = FALSE keeps all factor levels
    
    # Zoom
    zoom_buffer <- input$map_trend_val_zoom
    ## Zoom, points
    x_min_pts <- min(fort_df_mt[, "longitude"])
    x_max_pts <- max(fort_df_mt[, "longitude"])
    y_min_pts <- min(fort_df_mt[, "latitude"])
    y_max_pts <- max(fort_df_mt[, "latitude"])
    # Bounding Box
    #                EXT_MIN_X, EXT_MIN_Y, EXT_MAX_X, EXT_MAX_Y
    bbox_points <- c(x_min_pts, y_min_pts, x_max_pts, y_max_pts)

    zoomregion <- input$SI_zoomregion_t

    if(is.null(zoomregion)==TRUE){
      # do nothing
    } else if(zoomregion == "none"){
      # more nothing
    } else {
      # convert to values
      zoomregion_bbox <- eval(parse(text=
                      pick_zoomregion_bbox[match(zoomregion, pick_zoomregion)]))
      #zoomregion_bbox <- bbox_Cho
      # Zoom, Limits
      x_min <- zoomregion_bbox[1] - zoom_buffer
      x_max <- zoomregion_bbox[3] + zoom_buffer
      y_min <- zoomregion_bbox[2] - zoom_buffer / (map_coord_ratio * 5)
      y_max <- zoomregion_bbox[4] + zoom_buffer / (map_coord_ratio * 5)
      # replot map with zoom region and buffer
      m_t <- m_t + ggplot2::coord_fixed(ratio = map_coord_ratio
                                        , xlim = c(x_min, x_max)
                                        , ylim = c(y_min, y_max))
    }##IF~opt_zoomregion_t~END
    
    # # save map
    mt_ext <- ifelse(is.null(input$SI_ext_t)
                     , "png"
                     , input$SI_ext_t) #"png"
    #date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- file.path("map", paste0("map_change.", mt_ext))
    ggplot2::ggsave(fn_out
                    , plot = m_t
                    , device = mt_ext
                    , height = plot_h
                    , width = plot_w
                    , units = plot_units
                    , scale = plot_scale
                    , bg = "white")
    # Save so download button just copies
    
    # enable save button
    shinyjs::enable("but_map_trend_save")
    
    #return(ggplotly(m_t))
    return(m_t)
    #
  })##map_trend~END
  
  output$map_t_render <- renderPlot({
  #output$map_t_render <- renderPlotly({
    # default map to show
    if(input$but_map_trend == 0){
      m_t_2 <- map_base
    } else {
      m_t_2 <- map_trend()  
    }
    print(m_t_2)
    #ggplotly(m_t_2)
  })##map_r~END
  
  ## but_mt_title ####
  observeEvent(input$but_mt_title, {
    sep1 <- ": "
    sep2 <- "\n" #"; "
    sep_clsp <- ", "
    #
    df_filt_mt <- df_filt()
    #
    mt_title_parmName   <- sort(unique(df_filt_mt[, "parmName"]))
    mt_title_gamName    <- sort(unique(df_filt_mt[, "gamName"]))
    mt_title_periodName <- sort(unique(df_filt_mt[, "periodName"]))
    #
    if(!is.null(input$fn_input)){
      # "User" file
      mt_title_layer      <- sort(unique(df_filt_mt[, "layer"]))
      mt_title_seasonName <- sort(unique(df_filt_mt[, "seasonName"]))
      str_title <- paste(paste(mt_title_parmName, collapse = ", ")
                         , paste("GAM", paste(mt_title_gamName
                                              , collapse = sep_clsp)
                                 , sep = sep1)
                         , paste("Layer", paste(mt_title_layer
                                                , collapse = sep_clsp)
                                 , sep = sep1)
                         , paste("Period", paste(mt_title_periodName
                                                 , collapse = sep_clsp)
                                 , sep = sep1)
                         , paste("Season", paste(mt_title_seasonName
                                                 , collapse = sep_clsp)
                                 , sep = sep1)
                         , paste("p-value thresholds (possible, significant)"
                                 , paste(input$map_trend_pval_poss
                                         , input$map_trend_pval_sig
                                         , sep = sep_clsp)
                                 , sep = sep1)
                         , sep = sep2)
      
    } else {
      # "final" file
      mt_title_mapLayer <- unlist(strsplit(df_filt_mt[, "mapLayer"], "[|]"))
      str_title <- paste(paste(mt_title_parmName, collapse = ", ")
                         , paste("GAM", paste(mt_title_gamName
                                              , collapse = ", ")
                                 , sep = sep1)
                         , paste("Layer", mt_title_mapLayer[2]
                                 , sep = sep1)
                         , paste("Period", paste(mt_title_periodName
                                                 , collapse = ", ")
                                 , sep = sep1)
                         , paste("Season", mt_title_mapLayer[3]
                                 , sep = sep1)
                         , paste("p-value thresholds (possible, significant)"
                                 , paste(input$map_trend_pval_poss
                                         , input$map_trend_pval_sig
                                         , sep = sep_clsp)
                                 , sep = sep1)
                         , sep = sep2)
    }## IF ~ is.null(input$fn_input) ~ END
    #
    updateTextAreaInput(session, "map_trend_title", value = str_title)
    # max is 89 characters, if need to wrap dynamically
    #https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-set-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
  })##observerEvent~input$but_mr_title~END
  
  # # but_map_trend_save ####
  output$but_map_trend_save <- downloadHandler(
    filename = function() {
      mt_ext <- input$SI_ext_t
      date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("map_change_", date_time, ".", mt_ext)
      # #paste0(input$fn_input, input$SI_ext)
    }, ##filename~END
    content = function(fn) {
      mt_ext <- input$SI_ext_t
      fn_out <- file.path("map", paste0("map_change.", mt_ext))
      #print(map_range())
      # ggplot2::ggsave(file, plot = ggplot2::last_plot(), device = ext, height = 9, width = 9/1.5, units = "in" )
      # #file.copy("map_range.pdf", fn, overwrite=TRUE)
      file.copy(fn_out, fn, overwrite = TRUE)
    }##content~END
  )##map_t_save~END
  
  # Button, Filters ####
  filt_watch <- reactive({
    # Watch for filt_clear and df_filt
    paste(input$but_ClearFilters
          , input$fn_input
          , input$but_radio_load)
  })## file_watch ~ EN
  
  filt_watch4 <- reactive({
    # Watch for filt_clear and df_filt
    paste(input$but_ClearFilters
          , input$fn_input
          , input$but_radio_load
          , input$but_filt_apply)
  })## file_watch ~ EN
  
  
  observeEvent(filt_watch(), {
    # Clear User Selections for Query
    #clearFilterSelection(session)
    # Clear Filter Selections
    #clearFilterSelection <- function(mySession) {
      # reset all fields
    #
    df_x <- df_import()
    #
    #updateSelectizeInput(session, "SI_seasonName", choices=unique(df_x[, "seasonName"]), selected=character(1))
    # reset values to entire domain of values for each box.
      str_col <- "state"
    updateSelectizeInput(session, "SI_state"         
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "cbSeg92"
    updateSelectizeInput(session, "SI_cbSeg92"       
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "stationGrpName"
    updateSelectizeInput(session, "SI_stationGrpName"
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "station"
    updateSelectizeInput(session, "SI_station"       
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "parmName"
    updateSelectizeInput(session, "SI_parmName"      
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "gamName"
    updateSelectizeInput(session, "SI_gamName"       
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "layer"
    updateSelectizeInput(session, "SI_layer"         
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "periodName"
    updateSelectizeInput(session, "SI_periodName"    
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
      str_col <- "seasonName"
    updateSelectizeInput(session, "SI_seasonName"    
                         , choices=sort(unique(df_x[, str_col]))
                         , selected=sort(unique(df_x[, str_col])))
    # final file
    if(is.null(click_filetype$data)){
      # do nothing
    } else if (click_filetype$data == "final"){
        str_col <- "mapLayer"
      updateSelectInput(session, "SI_mapLayer"    
                        , choices=sort(unique(df_x[, str_col]))
                        , selected=sort(unique(df_x[, str_col])))
    }## IF ~ click_filetype$data ~ END
    #
    #}##FUNCTION~clearFilterSelection~END
  })## observeEvent ~ clear filters ~ END
  
  
  filt_watch_basic <- reactive({
    # Watch for filt_clear and df_filt
    # paste(input$but_ClearFilters
    #       , input$fn_input
    #       , input$but_radio_load)
    #input$but_radio_load_basic
    input$radio_input_basic
  })## file_watch ~ EN
  
  
  # Button, Filters, BASIC ----
  observeEvent(filt_watch_basic(), {
    # Clear User Selections for Query
    #clearFilterSelection(session)
    # Clear Filter Selections
    #clearFilterSelection <- function(mySession) {
    # reset all fields
    #
    df_x <- df_import_basic()
    #
    #updateSelectizeInput(session, "SI_seasonName", choices=unique(df_x[, "seasonName"]), selected=character(1))
    # reset values to entire domain of values for each box.
   
    # Remove saved map PNG
    fn_png <- list.files("map", "\\.png$")
    if(length(fn_png) >0 ) {
      file.remove(paste0("map/", fn_png))
    }##IF ~ length(fn_png)
    # Remove zip file
    fn_zip <- "map/baytrendsmap.zip"
    if(file.exists(fn_zip)) {
      file.remove(fn_zip)
    }## IF ~ fn_zip
    
    # hide download button
    ## new data so can't have updated the maps
    #shinyjs::disable("but_map_basic_save")

    # final file
   
      str_col <- "mapLayer"
      updateSelectInput(session, "SI_mapLayer_Basic"    
                        , choices=sort(unique(df_x[, str_col]))
                        , selected=sort(unique(df_x[, str_col])))

    #
    #}##FUNCTION~clearFilterSelection~END
  })## observeEvent ~ clear filters ~ END
  
  # Help ####
  output$help_html <- renderUI({
    fn_help_html <- file.path(".", "www", "ShinyHelp.html")
    fe_help_html <- file.exists(fn_help_html)
    if(fe_help_html==TRUE){
      return(includeHTML(fn_help_html))
    } else {
      return(NULL)
    }##IF~fe_help_html~END
  })##help_html~END
  
  # MAP, Range, Leaflet ----
  output$map_r_leaflet <- renderLeaflet({
    
    # data for plot
    df_mrl <- df_filt()
    
    #url_A <- "https://raw.githubusercontent.com/tetratech/baytrends_files/main/plots_NLT_FA_F_FP/"
    url_A <- paste0(url_remote_base)
    #url_B <- "_chla_S.png"
    url_B <- paste0("_"
                    , tolower("CHLA")
                    , "_"
                    , toupper(substr("Surface", 1,1))
                    , ".png")
    
    # # Split mapLayer
    # df_split <- as.data.frame(matrix(unlist(strsplit(df_mrl$mapLayer, "\\|"))
    #                                  , ncol = 3
    #                                  , byrow = TRUE))
    # names(df_split) <- c("mL_parmName", "mL_layer", "mL_seasonName")
    # df_mrl[, "mL_parmName"] <- df_split[, "mL_parmName"]
    # 
    # url_B <- paste0("_"
    #                 , tolower(df_mrl$mL_parmName)
    #                 , "_"
    #                 , toupper(substr(df_mrl$layer, 1, 1))
    #                 , ".png")
    
    # Different plot if not 'Full Period, Non Flow Adjusted' dataset
    inFile_radio <- input$radio_input
    plots_dir <- paste0(df_pick_files[df_pick_files[, "radio"] == inFile_radio
                                      , "dir_plot"], "/")
    plots_boo <- df_pick_files[df_pick_files[, "radio"] == inFile_radio
                               , "show_plots"]
    
    if(isTRUE(plots_boo)) {
      df_mrl$url <- paste0(url_A, plots_dir, df_mrl$station[1], url_B)
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> More info</a>')
    } else {
      df_mrl$url <- paste0(url_A, plots_dir, "_no_plot.png")
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> No plot</a>')
    }## IF ~ inFile_radio ~ END
    
    col_Stations <- "blue"
    col_Segs     <- "black" # "grey59"
    fill_Segs    <- "lightskyblue" 
    
    # Map
    leaflet(data = df_mrl) %>%
      # Groups, Base
      #addTiles(group="OSM (default)") %>%  #default tile too cluttered
      addProviderTiles("CartoDB.Positron", group="Positron") %>%
      addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
      # Groups, Overlay
      addPolygons(data = ogr_shp
                  , color = col_Segs 
                  , fill = fill_Segs
                  , group = "CB Outline") %>%
      addCircles(lng=~longitude
                 , lat=~latitude
                 , color=col_Stations
                 , popup=~paste0("Station: ", station, as.character("<br>")
                                 , "Latitude: ", latitude, as.character("<br>")
                                 , "Longitude: ", longitude, as.character("<br>")
                                 , "Segment: ", cbSeg92, as.character("<br>")
                                 , "Season: ", seasonName, as.character("<br>")
                                 , "Period: ", periodName, as.character("<br>")
                                 , "GAM: ", gamName, as.character("<br>")
                                 , "Parameter: ", parmName, as.character("<br>")
                                 , "Trend Chart: ", url_click
                                 )
                 , radius=30
                 , group = "Stations") %>%
      # Legend
      addLegend("bottomleft"
                , colors = c(col_Stations, col_Segs)
                , labels = c("Stations", "CB Outline")
                , values = NA) %>%
      # Layers
      # addLayersControl(baseGroups = c("OSM (default)"
      #                                 , "Positron"
      #                                 , "Toner Lite")
      addLayersControl(baseGroups = c("Positron"
                                      , "Toner Lite"
                                      , "Open Street Map")
                       , overlayGroups = c("Stations"
                                           , "CB Outline")) %>%
      # Mini map
      addMiniMap(toggleDisplay = TRUE) %>%
      # Hide Groups
      hideGroup("CB Outline")
    
    
  })## map_r_leaflet ~ END
  
  # MAP, Range, Leaflet, proxy ----
  # update map based on user selections
  # tied to Update button
  # https://rstudio.github.io/leaflet/shiny.html
  # need a reactive to trigger, use map update button
  observeEvent(input$but_map_range, {
    
    #~~~~
    # Repeat code from Map_Range (static)
    
    # data for plot
    df_mr <- df_filt()
    
    #~~~~~~~~
    mr_cI_type  <- ifelse(is.null(input$SI_classInt)
                          , "pretty"
                          , input$SI_classInt)
    mr_pal <- ifelse(is.null(input$SI_pal)
                     , "PuOr"
                     , pick_pal[match(input$SI_pal, pick_pal_names)])
    mr_var_name <- ifelse(is.null(input$SI_variable)
                          , "Baseline mean"
                          , input$SI_variable)
    mr_var <- pick_gamDiff[match(mr_var_name, pick_gamDiff_Desc)]
    # mr_var <- input$SI_variable
    # mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    brks_user <- eval(parse(text = paste0("c(", input$breaks, ")"))) 
    #evals to NULL if left blank
    
    # breaks vs numclasses
    ## input is "" for blank
    if(is.null(brks_user)==TRUE){
      # no breaks, use slider for num classes
      mr_numclass <- input$numclass
      # derive breaks from user n and style
      mr_cI_val <- classInt::classIntervals(df_mr[, mr_var]
                                            , mr_numclass
                                            , mr_cI_type)
      # Redo num classes as "pretty" picks its own number of breaks
      mr_numclass <- ifelse(mr_cI_type=="pretty"
                            , length(mr_cI_val$brks) - 1
                            , mr_numclass)
      #mr_numclass <- ifelse(mr_cI_type=="pretty"
      # , length(mr_cI_val$brks)
      # , mr_numclass)
      # breaks
      mr_brks <- mr_cI_val$brks
    } else {
      # user breaks, use instead of slider for numclasses
      mr_numclass <- length(brks_user) - 1
      # breaks
      mr_brks <- brks_user
    }##IF~brks_user~END
    
    mr_pal_col <- RColorBrewer::brewer.pal(n = mr_numclass, name = mr_pal)
    
    ## Break, Color
    df_mr$map_brk_col <- cut(df_mr[, mr_var]
                                  , breaks = mr_brks
                                  #, labels = brewer.pal(max(3, mr_numclass)
                                  #, mr_pal)[1:mr_numclass]
                                  , labels = mr_pal_col
                                  , include.lowest = TRUE
    )
    # Minimum of 3 different levels or get warning
    ## Break, Text
    df_mr$map_brk_num <- cut(df_mr[, mr_var]
                                  , breaks = mr_brks
                                  , include.lowest = TRUE
    )
    
    #~~~~
    
    # data for plot
    df_mrl <- df_mr # df_filt()
  
    #url_A <- "https://raw.githubusercontent.com/tetratech/baytrends_files/main/plots_NLT_FA_F_FP/"
    url_A <- paste0(url_remote_base)
    # #url_B <- "_chla_S.png"
    # url_B <- paste0("_"
    #                 , tolower("CHLA")
    #                 , "_"
    #                 , toupper(substr("Surface", 1, 1))
    #                 , ".png")
    
    # Split mapLayer
    df_split <- as.data.frame(matrix(unlist(strsplit(df_mrl$mapLayer, "\\|"))
                                     , ncol = 3
                                     , byrow = TRUE))
    names(df_split) <- c("mL_parmName", "mL_layer", "mL_seasonName")
    df_mrl[, "mL_parmName"] <- df_split[, "mL_parmName"]

    url_B <- paste0("_"
                    , tolower(df_mrl$mL_parmName[1])
                    , "_"
                    , toupper(substr(df_mrl$layer[1], 1, 1))
                    , ".png")
    
    # Different plot if not 'Full Period, Non Flow Adjusted' dataset
    inFile_radio <- input$radio_input
    plots_dir <- paste0(df_pick_files[df_pick_files[, "radio"] == inFile_radio
                               , "dir_plot"], "/")
    plots_boo <- df_pick_files[df_pick_files[, "radio"] == inFile_radio
                               , "show_plots"]

    if(isTRUE(plots_boo)) {
      df_mrl$url <- paste0(url_A, plots_dir, df_mrl$station, url_B)
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> More info</a>')
    } else {
      df_mrl$url <- paste0(url_A, plots_dir, "_no_plot.png")
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> No plot</a>')
    }## IF ~ inFile_radio ~ END
    
    col_Stations <- "purple"
    col_Segs     <- "black" # "grey59"
    
    map_brk_num_leg <- levels(df_mrl$map_brk_num)
    mr_pal_col_leg <- mr_pal_col
    
    # New map
    leafletProxy("map_r_leaflet", data = df_mrl) %>%
      # Remove stations
      clearGroup("Stations") %>%
      # Remove Legend
      clearControls() %>%
      # Circles, NEW
      addCircleMarkers(lng=~longitude
                       , lat=~latitude
                       , color=~map_brk_col
                       , fill=~map_brk_col
                       , group = "Stations"
                       , stroke = TRUE
                       , fillOpacity = 0.75
                       , popup=~paste0("Station: ", station, as.character("<br>")
                                       , "Latitude: ", latitude, as.character("<br>")
                                       , "Longitude: ", longitude, as.character("<br>")
                                       , "Segment: ", cbSeg92, as.character("<br>")
                                       , "Season: ", seasonName, as.character("<br>")
                                       , "Period: ", periodName, as.character("<br>")
                                       , "GAM:", gamName, as.character("<br>")
                                       , "Parameter: ", parmName, as.character("<br>")
                                       , "Variable: ", mr_var_name, as.character("<br>")
                                       , "Trend Chart: ", url_click)
                       ) %>%
      addLegend("bottomleft"
                , colors = mr_pal_col
                , labels = map_brk_num_leg
                , values = NA)
    
    
    
    
      # Legend, NEW
      # addLegend("bottomleft"
      #           , colors = c(col_Stations, col_Segs)
      #           , labels = c("Stations", "CB Outline")
      #           , values = NA)
    
  }
  )## observeEvent
  
  # points, custom icons
  # https://stackoverflow.com/questions/41372139/using-diamond-triangle-and-star-shapes-in-r-leaflet
  
# BASIC ----
# Basic map interface
  
  ## File Watch ----
  file_watch_basic <- reactive({
    # trigger for df_import()
    #paste(input$fn_input, input$but_radio_load)
    # input$but_radio_load_basic
    input$radio_input_basic
  })## file_watch ~ EN
  
  # file_watch_basic_maps <- reactive({
  #   # trigger for df_import()
  #   #paste(input$fn_input, input$but_radio_load)
  #   # input$but_radio_load_basic
  #   input$radio_input_basic
  #   input$SI_mapLayer_basic
  #   input$SI_pal_basic
  #   input$SI_upisgood_basic
  # })## file_watch ~ EN
  
  file_watch_basic_map_r <- reactive({
    # trigger for df_import()
    #paste(input$fn_input, input$but_radio_load)
    # input$but_radio_load_basic
    input$radio_input_basic
    input$SI_mapLayer_basic
    input$SI_pal_range_basic
  })## file_watch ~ EN
  
  file_watch_basic_map_t <- reactive({
    # trigger for df_import()
    #paste(input$fn_input, input$but_radio_load)
    # input$but_radio_load_basic
    input$radio_input_basic
    input$SI_mapLayer_basic
    input$SI_pal_trend_basic
    input$SI_upisgood_basic
  })## file_watch ~ EN
  
  ## UI, Filter, Collapse ----
  output$filt_mapLayer_basic <- renderUI({
    str_col <- "mapLayer"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col, "_basic")))
    str_SI <- paste0("SI_", str_col, "_basic")
    df_x <- df_import_basic()
    fluidRow(
      selectInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                  list("Parameter | Layer | Season" = sort(unique(df_x[, str_col]))),
                  multiple = FALSE
      )## selectInput ~ mapLayer
    )##fluidRow~END
  })##filt_mapLayer_Basic
  
  output$filt_collapse_mapLayer_basic <- renderUI({
    # filters change based on file format; final vs. user.
    # Default is "final" file.
    # if(input$but_radio_load_basic == 0) {
    #   #return(NULL)
    #   p("No data loaded, make a selection under '1. Choose Data'.")
    # } else {
      # "final" file filters
      bsCollapse(multiple = TRUE
                 , bsCollapsePanel("Filter by 'Map Layer'", style='info',
                                   #fluidRow(column(1), column(10, radioButtons('sel_mapLayer', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                   uiOutput('filt_mapLayer_basic')
                 )##bsCollapsePanel~station~END
                 , open = "Filter by 'Map Layer'" # to auto open panels
      )##bsCollapse
    # }## IF ~ file type ~ END
  })## filt_collapse_basic
  
  ## 

  
  output$opt_pal_range_basic <- renderUI({
    str_col <- "pal"
    str_SI <- paste0("SI_", str_col, "_range_basic")
    fluidRow(
      selectizeInput(str_SI, h4("  Select palette:"),
                     choices = pick_pal_names,
                     multiple = FALSE,
                     selected = pick_pal_names[1])
      
    )##fluidRow~END
  })##opt_pal_basic_range
  
  # output$filt_collapse_pal_range_basic <- renderUI({
  #   bsCollapse(multiple = TRUE
  #              , bsCollapsePanel("  Select palette:"
  #                                , style = 'info'
  #                                , uiOutput('filt_pal_range_basic')
  #                                )
  #              , open = "  Select palette:") # force panel open
  # })## filt_collapse_pal_range_basic
  
  output$opt_pal_trend_basic <- renderUI({
    str_col <- "pal"
    str_SI <- paste0("SI_", str_col, "_trend_basic")
    fluidRow(
      selectizeInput(str_SI, h4("  Select palette:"),
                     choices = pick_pal_change,
                     multiple = FALSE,
                     selected = pick_pal_change[1])
      
    )##fluidRow~END
  })##opt_pal_basic_trend
  
  output$opt_upisgood_basic <- renderUI({
    str_col <- "upisgood"
    str_SI <- paste0("SI_", str_col, "_basic")
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Increasing trend is 'good'?")),
                     choices = c("TRUE", "FALSE"),
                     multiple = FALSE,
                     selected = "TRUE")
      
    )##fluidRow~END
  })##opt_upisgood_basic
  
  ## Data ----
  ### Data, df_import_basic----
  df_import_basic <- eventReactive(file_watch_basic(), {
    # use a multi-item reactive so keep on a single line
    
    inFile_radio <- input$radio_input_basic
    
    # Need is.null as first rather than last.
    # It will always trigger on loading of the app
    
    # if(input$but_radio_load_basic == 0){
    #   return(NULL)
    # } else {
      fn_inFile <- pick_files_names[match(inFile_radio, pick_files_radio)]
      df_input <- read.csv(fn_inFile
                           , header = TRUE
                           , sep = ","
                           , quote = "\""
                           , stringsAsFactors = FALSE)
      # saved files are already validated so no QC on column names
    # }##IF~is.null~END
    #
    msg <- paste0("file loaded, basic; ", inFile_radio)
    message(msg)
    #
    return(df_input)
    #
    #
  })##df_import_basic
  
  ### Data, df_filt_basic ----
  #df_filt_basic <- eventReactive (input$but_map_basic, {
  df_filt_basic <- eventReactive (input$SI_mapLayer_basic, {
    # if filters not null then apply to df_import
    # it is possible to select no data

    # temp data frame
    df_y <- df_import_basic()
    #
    str_col_2 <- "mapLayer"
    str_SI_value <- eval(parse(text = paste0("input$SI_", str_col_2, "_basic")))
    if(!is.null(str_SI_value)){
      df_y <- df_y[df_y[, str_col_2] %in% str_SI_value, ]
    }##IF~mapLayer~END
    
    msg <- paste0("filter data, basic; ", str_col_2, " = ", str_SI_value)
    message(msg)
    #
    return(df_y)
    #
  })##df_filt~END
  

  ## MAP, Basic ####
  
  ### MAP, Basic, Range, Leaflet, render ----
  output$map_r_leaflet_basic <- renderLeaflet({
   
    # data for plot
    df_mrl <- df_import_basic()
    # default data - load default data
    
    
    #url_A <- "https://raw.githubusercontent.com/tetratech/baytrends_files/main/plots_NLT_FA_F_FP/"
    url_A <- paste0(url_remote_base)
    #url_B <- "_chla_S.png"
    url_B <- paste0("_"
                    , tolower("CHLA")
                    , "_"
                    , toupper(substr("Surface", 1,1))
                    , ".png")
    
    # # Split mapLayer
    # df_split <- as.data.frame(matrix(unlist(strsplit(df_mrl$mapLayer, "\\|"))
    #                                  , ncol = 3
    #                                  , byrow = TRUE))
    # names(df_split) <- c("mL_parmName", "mL_layer", "mL_seasonName")
    # df_mrl[, "mL_parmName"] <- df_split[, "mL_parmName"]
    # 
    # url_B <- paste0("_"
    #                 , tolower(df_mrl$mL_parmName)
    #                 , "_"
    #                 , toupper(substr(df_mrl$layer, 1, 1))
    #                 , ".png")
       
    # Different plot if not 'Full Period, Non Flow Adjusted' dataset
    inFile_radio <- input$radio_input_basic
    plots_dir <- paste0(df_pick_files[df_pick_files[, "radio"] == inFile_radio
                                      , "dir_plot"], "/")
    plots_boo <- df_pick_files[df_pick_files[, "radio"] == inFile_radio
                               , "show_plots"]
    
    if(isTRUE(plots_boo)) {
      df_mrl$url <- paste0(url_A, plots_dir, df_mrl$station[1], url_B)
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> More info</a>')
    } else {
      df_mrl$url <- paste0(url_A, plots_dir, "_no_plot.png")
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> No plot</a>')
    }## IF ~ inFile_radio ~ END
    
    col_Stations <- "blue"
    col_Segs     <- "black" # "grey59"
    fill_Segs    <- "lightskyblue" 
    
    # Map
    leaflet(data = df_mrl) %>%
      # Groups, Base
      #addTiles(group="OSM (default)") %>%  #default tile too cluttered
      addProviderTiles("CartoDB.Positron", group="Positron") %>%
      addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
      # Groups, Overlay
      addPolygons(data = ogr_shp
                  , color = col_Segs 
                  , fill = fill_Segs
                  , group = "CB Outline") %>%
      addCircles(lng=~longitude
                 , lat=~latitude
                 , color=col_Stations
                 , popup=~paste0("Station: ", station, as.character("<br>")
                                 , "Latitude: ", latitude, as.character("<br>")
                                 , "Longitude: ", longitude, as.character("<br>")
                                 , "Segment: ", cbSeg92, as.character("<br>")
                                 , "Season: ", seasonName, as.character("<br>")
                                 , "Period: ", periodName, as.character("<br>")
                                 , "GAM:", gamName, as.character("<br>")
                                 , "Parameter: ", parmName, as.character("<br>")
                                 , "Trend Chart: ", url_click
                 )
                 , radius=30
                 , group = "Stations") %>%
      # Legend
      addLegend("bottomleft"
                , colors = c(col_Stations, col_Segs)
                , labels = c("Stations", "CB Outline")
                , values = NA
                , title = "Stations") %>%
      # Layers
      # addLayersControl(baseGroups = c("OSM (default)"
      #                                 , "Positron"
      #                                 , "Toner Lite")
      addLayersControl(baseGroups = c("Positron"
                                      , "Toner Lite"
                                      , "Open Street Map")
                       , overlayGroups = c("Stations"
                                           , "CB Outline")) %>%
      # Mini map
      addMiniMap(toggleDisplay = TRUE) %>%
      # Hide Groups
      hideGroup("CB Outline")
    
    
  })## map_r_leaflet_basic
  
  ### MAP, Basic, Range, Static ----
  map_range_basic <- eventReactive (file_watch_basic_map_r(), {

    # start with base map
    m_r <- map_base
    
    # data for plot
    df_mr <- df_filt_basic()
    mr_cI_type  <- "pretty"
    mr_pal <- ifelse(is.null(input$SI_pal_range_basic)
                     , "PuOr"
                     , pick_pal[match(input$SI_pal_range_basic, pick_pal_names)])
    mr_var_name <- "Current mean"
    mr_var <- pick_gamDiff[match(mr_var_name, pick_gamDiff_Desc)]
    # mr_var <- input$SI_variable
    # mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    brks_user <- NULL 
    #evals to NULL if left blank
    
    # breaks vs numclasses
    ## input is "" for blank
    # no breaks, use slider for num classes
    mr_numclass <- 5
    # derive breaks from user n and style
    mr_cI_val <- classInt::classIntervals(df_mr[, mr_var]
                                          , mr_numclass
                                          , mr_cI_type)
    # Redo num classes as "pretty" picks its own number of breaks
    mr_numclass <- ifelse(mr_cI_type=="pretty"
                          , length(mr_cI_val$brks) - 1
                          , mr_numclass)
    #mr_numclass <- ifelse(mr_cI_type=="pretty"
    # , length(mr_cI_val$brks)
    # , mr_numclass)
    # breaks
    mr_brks <- mr_cI_val$brks
    
    mr_pal_col <- RColorBrewer::brewer.pal(n=mr_numclass, name=mr_pal)
    
    # River Names
    boo_riverNames <- "Yes"
    
    if(boo_riverNames == "Yes"){
      m_r <- m_r + 
        annotate(geom = "text"
                 , x = as.numeric(lab_Sus[2])
                 , y=as.numeric(lab_Sus[3])
                 , label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2])
                 , y=as.numeric(lab_Pat[3])
                 , label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2])
                 , y=as.numeric(lab_Cho[3])
                 , label=lab_Cho[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2])
                 , y=as.numeric(lab_Pot[3])
                 , label=lab_Pot[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2])
                 , y=as.numeric(lab_Rap[3])
                 , label=lab_Rap[1], hjust=1) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2])
                 , y=as.numeric(lab_Yor[3])
                 , label=lab_Yor[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2])
                 , y=as.numeric(lab_Jam[3])
                 , label=lab_Jam[1], hjust=0)
    }##IF~riverNames~END
    
    # Title
    # mr_title <- input$map_range_title
    # if(!is.null(mr_title)){
    #   m_r <- m_r +
    #     labs(title=paste(mr_title, collapse="; "))
    #   # labs(title=paste(mr_pal_col, collapse="; "))
    # }##IF~riverNames~END
    
    
    # Title
    
    sep1 <- ": "
    sep2 <- "\n" #"; "
    #
    mr_title_parmName   <- sort(unique(df_mr[, "parmName"]))
    mr_title_gamName    <- sort(unique(df_mr[, "gamName"]))
    mr_title_periodName <- sort(unique(df_mr[, "periodName"]))
    #
    
    # "User" file
    mr_title_layer      <- sort(unique(df_mr[, "layer"]))
    mr_title_seasonName <- sort(unique(df_mr[, "seasonName"]))
    str_title <- paste(paste(mr_title_parmName, collapse = ", ")
                       , paste("GAM", paste(mr_title_gamName
                                            , collapse = ", ")
                               , sep = sep1)
                       , paste("Layer", paste(mr_title_layer
                                              , collapse = ", ")
                               , sep = sep1)
                       , paste("Period", paste(mr_title_periodName
                                               , collapse = ", ")
                               , sep = sep1)
                       , paste("Season", paste(mr_title_seasonName
                                               , collapse = ", ")
                               , sep = sep1)
                       , sep = sep2)
    
    
    
    mr_title <- str_title
    
    if(!is.null(mr_title)){
      m_r <- m_r +
        labs(title=paste(mr_title, collapse="; "))
      # labs(title=paste(mr_pal_col, collapse="; "))
    }##IF~riverNames~END
    
    
    
    # Points
    # fortify
    fort_df_mr <- ggplot2::fortify(df_mr)
    # Add to data frame
    
    ## Break, Color
    fort_df_mr$map_brk_col <- cut(fort_df_mr[, mr_var]
                                  , breaks = mr_brks
                                  #, labels = brewer.pal(max(3, mr_numclass)
                                  #, mr_pal)[1:mr_numclass]
                                  , labels = mr_pal_col
                                  , include.lowest = TRUE
    )
    # Minimum of 3 different levels or get warning
    ## Break, Text
    fort_df_mr$map_brk_num <- cut(fort_df_mr[, mr_var]
                                  , breaks = mr_brks
                                  , include.lowest = TRUE
    )
    
    # Points, Add
    m_r <- m_r + geom_point(data=fort_df_mr
                            , aes_string(x =" longitude"
                                         , y = "latitude"
                                         #  , fill = "map_brk_num")
                                         , fill = "map_brk_col")
                            , size = 4
                            , pch = 21
                            , color = "black"
                            , na.rm = TRUE)
    # Points, Fill
    #    m_r <- m_r + scale_fill_brewer(palette = mr_pal
    #                                   , name = mr_var_name
    #                                   , labels = levels(fort_df_mr$map_brk_num)
    #                                    )
    # m_r <- m_r + scale_fill_discrete(name = mr_var_name
    #                     #, labels = paste(c(">", rep("< "
    # , length(mr_cI_val$brks)-1)), round(mr_cI_val$brks, 2))) +
    #                     , labels = levels(fort_df_mr$map_brk_num)
    #                     )
    
    # 2021-08-06, Issue #51, 
    # Custom breaks - legend has only values for where have data 
    # Should have all breaks
    
    m_r <- m_r + scale_fill_brewer(palette = mr_pal
                                   , name = mr_var_name
                                   , labels = levels(fort_df_mr$map_brk_num)
                                   , drop = FALSE
    )
    
    
    
    # Legend 
    m_r <- m_r + theme(legend.position = "bottom"
                       , legend.box = "horizontal"
                       , legend.title = element_text(face = "bold"))
    
    
    # Zoom
    ## removed
    
    
    # # save map
    mr_ext <- "png" #ifelse(is.null(input$SI_ext), "png", input$SI_ext) #"png"
    #date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- file.path("map", paste0("map_range.", mr_ext))
    ggplot2::ggsave(fn_out
                    , plot = m_r
                    , device = mr_ext
                    , height = plot_h
                    , width = plot_w
                    , units = plot_units
                    , scale = plot_scale
                    , bg = "white")
    
    #
    return(m_r)
    #return(ggplotly(m_r))
    #
  })##map_range_basic
  
  ### MAP, Basic, Trend ----
  map_trend_basic <- eventReactive (file_watch_basic_map_t(), {
   
    # # validate p-value, poss > sig
    # validate(need(input$map_trend_pval_poss > input$map_trend_pval_sig
    #               , paste0("ERROR\nThe 'possible' p-value ("
    #                        , input$map_trend_pval_poss
    #                        , ") should be greater than the 'significant' p-value ("
    #                        , input$map_trend_pval_sig
    #                        , ").")))
  
    # start with base map
    m_t <- map_base
    mt_pal <- ifelse(is.null(input$SI_pal_trend_basic)
                     , "Red_Blue"
                     , input$SI_pal_trend_basic)
    
    if(mt_pal == "Orange_Green"){
      pal_mt <- pal_change_OrGn
    } else if (mt_pal == "Red_Blue") {
      pal_mt <- pal_change_RdBu
    } else if (mt_pal == "Purple_Green") {
      pal_mt <- pal_change_PuGn
    } ## IF ~ mt_pal ~ END
    
    # pal_mt <- pal_change_RdBu
    
    # data for plot
    df_mt <- df_filt_basic()
    # mr_cI_type  <- input$SI_classInt
    # mr_pal <- input$SI_pal
    # mr_var <- input$SI_variable
    # mr_numclass <- input$numclass
    # mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    # mr_pal_col <- RColorBrewer::brewer.pal(n=mr_numclass, name=mr_pal)
    # 
    # mr_cI_val <- classInt::classIntervals(df_mr[, mr_var], mr_numclass, mr_cI_type)
    
    # River Names
    boo_riverNames_t <- "Yes"
    if(boo_riverNames_t == "Yes"){
      m_t <- m_t + 
        annotate(geom = "text", x = as.numeric(lab_Sus[2])
                 , y=as.numeric(lab_Sus[3])
                 , label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2])
                 , y=as.numeric(lab_Pat[3])
                 , label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2])
                 , y=as.numeric(lab_Cho[3])
                 , label=lab_Cho[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2])
                 , y=as.numeric(lab_Pot[3])
                 , label=lab_Pot[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2])
                 , y=as.numeric(lab_Rap[3])
                 , label=lab_Rap[1], hjust=1) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2])
                 , y=as.numeric(lab_Yor[3])
                 , label=lab_Yor[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2])
                 , y=as.numeric(lab_Jam[3])
                 , label=lab_Jam[1], hjust=0)
    }##IF~riverNames~END
    
    # Title
    # mt_title <- input$map_trend_title
    # if(!is.null(mt_title)==TRUE){
    #   m_t <- m_t + labs(title=paste(mt_title, collapse="; "))
    #   # labs(title=paste(mr_pal_col, collapse="; "))
    # }##IF~riverNames~END
   
    sep1 <- ": "
    sep2 <- "\n" #"; "
    sep_clsp <- ", "
    #
    #
    mt_title_parmName   <- sort(unique(df_mt[, "parmName"]))
    mt_title_gamName    <- sort(unique(df_mt[, "gamName"]))
    mt_title_periodName <- sort(unique(df_mt[, "periodName"]))
    #
      # "final" file
      mt_title_mapLayer <- unlist(strsplit(df_mt[, "mapLayer"], "[|]"))
      str_title <- paste(paste(mt_title_parmName, collapse = ", ")
                         , paste("GAM", paste(mt_title_gamName
                                              , collapse = ", ")
                                 , sep = sep1)
                         , paste("Layer", mt_title_mapLayer[2]
                                 , sep = sep1)
                         , paste("Period", paste(mt_title_periodName
                                                 , collapse = ", ")
                                 , sep = sep1)
                         , paste("Season", mt_title_mapLayer[3]
                                 , sep = sep1)
                         , sep = sep2)

      mt_title <- str_title
    
      if(!is.null(mt_title)==TRUE){
        m_t <- m_t + labs(title=paste(mt_title, collapse="; "))
        # labs(title=paste(mr_pal_col, collapse="; "))
      }##IF~riverNames~END
    
    
    # Points ##
    
    boo_upisgood   <- ifelse(is.null(input$SI_upisgood_basic)
                             , TRUE
                             , input$SI_upisgood_basic) # TRUE
    chg_pval_poss <- 0.25 #input$map_trend_pval_poss # 0.25
    chg_pval_sig <- 0.05 #input$map_trend_pval_sig # 0.05
    #
    #if (boo_upisgood == TRUE){
    df_mt[df_mt[, "gamDiff.chg.pval"] > chg_pval_poss, "ChangeClass"] <- "NS"
    df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
            df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posIncr"
    df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
            df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posDecr"
    df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
            df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigIncr"
    df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
            df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigDecr"
    # } else {
    # df_mt[df_mt[, "gamDiff.chg.pval"] > chg_pval_poss, "ChangeClass"] <- "NS"
    # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
    #         df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posIncr"
    # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & 
    #         df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posDecr"
    # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
    #         df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigIncr"
    # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & 
    #         df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigDecr"
    # }##boo_upisgood~END
    
    # Caption - p-value
    m_t <- m_t + ggplot2::labs(caption = paste0("p-value thresholds (possible
                                                        , significant) = "
                                                , chg_pval_poss
                                                , ", "
                                                , chg_pval_sig))
    
    
    # fortify
    fort_df_mt <- ggplot2::fortify(df_mt)
    #
    #
    trend_ChangeClass <- c("sigDecr"
                           , "sigIncr"
                           , "posDecr"
                           , "posIncr"
                           , "NS")
    trend_leg_label   <- c("Significant Decrease"
                           , "Significant Increase"
                           , "Possible Decrease"
                           , "Possible Increase"
                           , "Unlikely")
    # trend_leg_color   <- c("orange", "green", "orange", "green", "dark gray")
    # trend_leg_shape   <- c("25", "24", "21", "21", "23")
    trend_leg_size    <- c("4", "4", "3", "3", "2")
    
    # Default (up is good = TRUE)
    trend_leg_color   <- c(pal_mt[1]  # "orange"
                           , pal_mt[2]# "green"
                           , pal_mt[1]# "orange"
                           , pal_mt[2]# "green"
                           , "dark gray")
    trend_leg_shape   <- c("25", "24", "21", "21", "23")
    manval_color <- c("sigDecr" = pal_mt[1]   # "orange"
                      , "sigIncr" = pal_mt[2] # "green"
                      , "posDecr" = pal_mt[1] # "orange"
                      , "posIncr" = pal_mt[2] # "green"
                      , "NS" = "dark gray")
    manval_shape <- c("sigDecr" = 25
                      , "sigIncr" = 24
                      , "posDecr" = 21
                      , "posIncr" = 21
                      , "NS" = 23)
    manval_size  <- c("sigDecr" = 4
                      , "sigIncr" = 4
                      , "posDecr" = 3
                      , "posIncr" = 3
                      , "NS" = 2)
    
    if(boo_upisgood == FALSE){
      # Direction Arrows the same
      # Just the colors change
      trend_leg_color   <- c(pal_mt[2] # "green"
                             , pal_mt[1] # "orange"
                             , pal_mt[2] # "green"
                             , pal_mt[1] # "orange"
                             , "dark gray")
      manval_color <- c("sigDecr" = pal_mt[2] # "green"
                        , "sigIncr" = pal_mt[1] # "orange"
                        , "posDecr" = pal_mt[2] # "green"
                        , "posIncr" = pal_mt[1] # "orange"
                        , "NS" = "dark gray")
      
    }##IF~boo_upisgood~END
    
    
    # Add to data frame
    fort_df_mt[, "ChangeClass"] <- factor(fort_df_mt[, "ChangeClass"]
                                          , trend_ChangeClass)
    # 
    fort_df_mt$ChangeClass_color <- trend_leg_color[match(fort_df_mt$ChangeClass
                                                          , trend_ChangeClass)]
    fort_df_mt$ChangeClass_shape <- trend_leg_shape[match(fort_df_mt$ChangeClass
                                                          , trend_ChangeClass)]
    fort_df_mt$ChangeClass_size  <- trend_leg_size[match(fort_df_mt$ChangeClass
                                                         , trend_ChangeClass)]
    
    m_t <- m_t + geom_point(data=fort_df_mt
                            , aes_string(x = "longitude"
                                         , y = "latitude"
                                         #, group ="ChangeClass"
                                         #, color="ChangeClass"
                                         , shape = "ChangeClass"
                                         , size = "ChangeClass"
                                         , fill = "ChangeClass"
                            )
                            , color = "black"
                            # , color = fort_df_mt$ChangeClass_color 
                            # , shape = as.numeric(fort_df_mt$ChangeClass_shape)
                            # , size = as.numeric(fort_df_mt$ChangeClass_size)
                            # , fill = fort_df_mt$ChangeClass_color
                            #, na.rm=TRUE
    ) +
      # scale_color_manual(name = "Type of change", labels = trend_leg_label
      #                    , values = manval_color, drop = FALSE ) +
      scale_shape_manual(name = "Type of change"
                         , labels = trend_leg_label
                         , values = manval_shape
                         , drop = FALSE ) + 
      scale_fill_manual( name = "Type of change"
                         , labels = trend_leg_label
                         , values = manval_color
                         , drop = FALSE ) + 
      scale_size_manual( name = "Type of change"
                         , labels = trend_leg_label
                         , values = manval_size
                         ,  drop = FALSE ) +
      theme(legend.position = c(1, 0.12)
            , legend.justification = c(1, 0)
            , legend.title = element_text(face = "bold"))
    # theme(legend.position = "bottom"
    #       , legend.box = "horizontal"
    #       , legend.title=element_text(face="bold"))
    # could use position as coordinates but uses 0:1 not coordinates of the plot.
    
    # drop = FALSE keeps all factor levels
    
    # Zoom
   # Remove for basic
    
    # # save map
    mt_ext <-  "png"
    #date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- file.path("map", paste0("map_change.", mt_ext))
    ggplot2::ggsave(fn_out
                    , plot = m_t
                    , device = mt_ext
                    , height = plot_h
                    , width = plot_w
                    , units = plot_units
                    , scale = plot_scale
                    , bg = "white")
    # Save so download button just copies
    #
    #return(ggplotly(m_t))
    return(m_t)
    #
  })##map_trend_basic
  
  
  
  ## MAP, Basic, render----
  
  ### MAP, Basic, Range, Leaflet, proxy ----
  # update map based on user selections
  # tied to Update button
  # https://rstudio.github.io/leaflet/shiny.html
  # need a reactive to trigger, use map update button
  observeEvent(file_watch_basic_map_r(), {
    
    #~~~~
    # Repeat code from Map_Range (static)
    
    # data for plot
    df_mr <- df_filt_basic()
    
    #~~~~~~~~
    mr_cI_type  <- "pretty"
    mr_pal <- ifelse(is.null(input$SI_pal_range_basic)
                     , "PuOr"
                     , pick_pal[match(input$SI_pal_range_basic, pick_pal_names)])
    mr_var_name <- "Current mean"
    mr_var <- pick_gamDiff[match(mr_var_name, pick_gamDiff_Desc)]
    # mr_var <- input$SI_variable
    # mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    brks_user <- NULL
    #evals to NULL if left blank
    
    # breaks vs numclasses
    ## input is "" for blank
    # no breaks, use slider for num classes
    mr_numclass <- 5
    # derive breaks from user n and style
    mr_cI_val <- classInt::classIntervals(df_mr[, mr_var]
                                          , mr_numclass
                                          , mr_cI_type)
    # Redo num classes as "pretty" picks its own number of breaks
    mr_numclass <- ifelse(mr_cI_type=="pretty"
                          , length(mr_cI_val$brks) - 1
                          , mr_numclass)
    #mr_numclass <- ifelse(mr_cI_type=="pretty"
    # , length(mr_cI_val$brks)
    # , mr_numclass)
    # breaks
    mr_brks <- mr_cI_val$brks
    
    mr_pal_col <- RColorBrewer::brewer.pal(n=mr_numclass, name=mr_pal)
    
    ## Break, Color
    df_mr$map_brk_col <- cut(df_mr[, mr_var]
                             , breaks = mr_brks
                             #, labels = brewer.pal(max(3, mr_numclass)
                             #, mr_pal)[1:mr_numclass]
                             , labels = mr_pal_col
                             , include.lowest = TRUE
    )
    # Minimum of 3 different levels or get warning
    ## Break, Text
    df_mr$map_brk_num <- cut(df_mr[, mr_var]
                             , breaks = mr_brks
                             , include.lowest = TRUE
    )
    
    #~~~~
    
    # data for plot
    df_mrl <- df_mr # df_filt()
    
    #url_A <- "https://raw.githubusercontent.com/tetratech/baytrends_files/main/plots_NLT_FA_F_FP/"
    url_A <- paste0(url_remote_base)
    # #url_B <- "_chla_S.png"
    # url_B <- paste0("_"
    #                 , tolower("CHLA")
    #                 , "_"
    #                 , toupper(substr("Surface", 1, 1))
    #                 , ".png")
    
    # Split mapLayer
    df_split <- as.data.frame(matrix(unlist(strsplit(df_mrl$mapLayer, "\\|"))
                                     , ncol = 3
                                     , byrow = TRUE))
    names(df_split) <- c("mL_parmName", "mL_layer", "mL_seasonName")
    df_mrl[, "mL_parmName"] <- df_split[, "mL_parmName"]
    
    url_B <- paste0("_"
                    , tolower(df_mrl$mL_parmName[1])
                    , "_"
                    , toupper(substr(df_mrl$layer[1], 1, 1))
                    , ".png")
    
    # Different plot if not 'Full Period, Non Flow Adjusted' dataset
    inFile_radio <- input$radio_input_basic
    plots_dir <- paste0(df_pick_files[df_pick_files[, "radio"] == inFile_radio
                                      , "dir_plot"], "/")
    plots_boo <- df_pick_files[df_pick_files[, "radio"] == inFile_radio
                               , "show_plots"]
 
    if(isTRUE(plots_boo)) {
      df_mrl$url <- paste0(url_A, plots_dir, df_mrl$station, url_B)
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> More info</a>')
    } else {
      df_mrl$url <- paste0(url_A, plots_dir, "_no_plot.png")
      df_mrl$url_click <- paste0('<a href="'
                                 , df_mrl[, "url"]
                                 , '", target=\"blank\"> No plot</a>')
    }## IF ~ inFile_radio ~ END
    
    col_Stations <- "purple"
    col_Segs     <- "black" # "grey59"
    
    map_brk_num_leg <- levels(df_mrl$map_brk_num)
    mr_pal_col_leg <- mr_pal_col
    
    # New map
    leafletProxy("map_r_leaflet_basic", data = df_mrl) %>%
      # Remove stations
      clearGroup("Stations") %>%
      # Remove Legend
      clearControls() %>%
      # Circles, NEW
      addCircleMarkers(lng=~longitude
                       , lat=~latitude
                       , color=~map_brk_col
                       , fill=~map_brk_col
                       , group = "Stations"
                       , stroke = TRUE
                       , fillOpacity = 0.75
                       , popup=~paste0("Station: ", station, as.character("<br>")
                                       , "Latitude: ", latitude, as.character("<br>")
                                       , "Longitude: ", longitude, as.character("<br>")
                                       , "Segment: ", cbSeg92, as.character("<br>")
                                       , "Season: ", seasonName, as.character("<br>")
                                       , "Period: ", periodName, as.character("<br>")
                                       , "GAM: ", gamName, as.character("<br>")
                                       , "Parameter: ", parmName, as.character("<br>")
                                       , "Variable: ", mr_var_name, as.character("<br>")
                                       , "Trend Chart: ", url_click)
      ) %>%
      addLegend("bottomleft"
                , colors = mr_pal_col
                , labels = map_brk_num_leg
                , values = NA
                , title = mr_var_name)
    
    
    
    
    # Legend, NEW
    # addLegend("bottomleft"
    #           , colors = c(col_Stations, col_Segs)
    #           , labels = c("Stations", "CB Outline")
    #           , values = NA)
    
  }
  )## observeEvent
 
  
  ### MAP, Basic, render, Range, Static----
  output$map_r_render_basic <- renderPlot({
    #output$map_r_render <- renderPlotly({
    # default map to show
    # if(input$but_map_basic == 0){
    #   #if(input$but_filt_apply == 0){
    #   m_r_2 <- map_base
    #   # } else {
    #   #   m_r_2 <- map_range_filt_default # different map
    #   # }## IF ~ input$but_filt_apply
    # } else {
    #   m_r_2 <- map_range_basic()  
    # }
    m_r_2 <- map_range_basic()
    print(m_r_2) 
    #ggplotly(m_r_2)
  })##map_r~END
  
  ### MAP, Basic, render, Trend ----  
  output$map_t_render_basic <- renderPlot({
    #output$map_t_render <- renderPlotly({
    # default map to show
    # if(input$but_map_basic == 0){
    #   m_t_2 <- map_base
    # } else {
    #   m_t_2 <- map_trend_basic()  
    # }
    
    # ggplot() + 
    #   annotate("text", x = 10, y = 10, size = 6, label = "LOADING...") + 
    #   theme_void()
    # mt_ext <-  "png"
    # fn_out <- file.path("map", paste0("map_change.", mt_ext))
    # validate(need(file.exists(fn_out), "Please wait for map to render and load."))
    # validate stops and doesn't refresh
    m_t_2 <- map_trend_basic() 
    print(m_t_2)
    #ggplotly(m_t_2)
  })##map_r~END
  
  ## Button ----
  
  # observeEvent(input$but_map_basic, {
  #   #
  #   # Enable save button
  #   shinyjs::enable("but_map_basic_save")
  #   #
  #   # Remove saved map PNG
  #   fn_png <- list.files("map", "\\.png$")
  #   if(length(fn_png) >0 ) {
  #     file.remove(paste0("map/", fn_png))
  #   }##IF ~ length(fn_png)
  #   # Remove zip file
  #   fn_zip <- "map/baytrendsmap.zip"
  #   if(file.exists(fn_zip)) {
  #     file.remove(fn_zip)
  #   }## IF ~ fn_zip
  # })## observerEvent ~ but_map_basic
  # 
  # ### Button, map, Basic, save ####
  # # create zip file
  # # Can save leaflet as HTML if make a reactive and use saveWidget
  # # https://stackoverflow.com/questions/52210682/download-leaflet-map-from-a-shiny-app-hosted-on-shiny-io
  # output$but_map_basic_save <- downloadHandler(
  #   filename = function() {
  #     # Remove old files
  #     # Remove zip file
  #     fn_zip <- "map/baytrendsmap.zip"
  #     if(file.exists(fn_zip)) {
  #       file.remove(fn_zip)
  #     }## IF ~ fn_zip
  #     
  #  #    mr_ext <- "png" #input$SI_ext
  #  #    fn_out <- file.path("map", paste0("map_range.", mr_ext))
  #  #    
  #  #    
  #  # #   saveWidget(map_r_leaflet_basic, "map_leaflet.html")
  #  #    
  #  #    
  #  #    
  #  #    if(file.exists(fn_out)==TRUE) {
  #       mr_ext <- "zip" #input$SI_ext
  #       date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
  #       paste0("baytrendsmap_", date_time, ".", mr_ext)
  #  #    } else {
  #  #      "Error_UpdateMapBeforeSave.pdf"
  #  #    }
  #  #    # #paste0(input$fn_input, input$SI_ext)
  #    } ##filename~END
  #   , content = function(fn) {
  # 
  #      # zip files
  #      fn_maps <- list.files(path = "map", pattern = "\\.png$")
  #      zip(zipfile = "map/baytrendsmap.zip", files = file.path("map", fn_maps))
  #       # ensure exists
  #      mr_ext <- "zip"
  #      fn_out <- paste0("map/baytrendsmap.", mr_ext)
  #  #    #print(map_range())
  #  #    # ggplot2::ggsave(file, plot = ggplot2::last_plot(), device = ext, height = 9, width = 9/1.5, units = "in" )
  #  #    # #file.copy("map_range.pdf", fn, overwrite=TRUE)
  #     if(file.exists(fn_out) == TRUE){
  #       file.copy(fn_out, fn, overwrite = TRUE)
  #     } else {
  #       fn_out_error <- file.path("map", "Error.zip")
  #       file.copy(fn_out_error, fn, overwrite = TRUE)
  #     }
  #    }##content~END
  # )##downloadHander
  # Need error handling in case change EXT but didn't "update" the map.
  # expected name doesn't match the saved file name.
  # file.exists(fn_out)
  
  output$but_map_range_basic_save <- downloadHandler(
    filename = function() {
      map_ext <- "png"
      date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("baytrendsmap_range_", date_time, ".", map_ext)
    } ##filename~END
    , content = function(fn) {
      map_ext <- "png"
      fn_maps <- list.files(path = "map"
                            , pattern = "^map_range\\.png$"
                            , full.names = TRUE)
      fn_out <- file.path("map", paste0("map_range", map_ext))
      if(is.null(fn_maps) == FALSE){
        file.copy(fn_maps, fn, overwrite = TRUE)
      } else {
        fn_out_error <- file.path("www", "Error_SomethingWrong.png")
        file.copy(fn_out_error, fn, overwrite = TRUE)
      }
    }##content~END
  )##downloadHander
  
  output$but_map_trend_basic_save <- downloadHandler(
    filename = function() {
      map_ext <- "png"
      date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("baytrendsmap_change_", date_time, ".", map_ext)
    } ##filename~END
    , content = function(fn) {
      map_ext <- "png"
      fn_maps <- list.files(path = "map"
                            , pattern = "^map_change\\.png$"
                            , full.names = TRUE)
      fn_out <- paste0("map/baytrendsmap.", map_ext)
      if(is.null(fn_maps) == FALSE){
        file.copy(fn_maps, fn, overwrite = TRUE)
      } else {
        fn_out_error <- file.path("www", "Error_SomethingWrong.png")
        file.copy(fn_out_error, fn, overwrite = TRUE)
      }
    }##content~END
  )##downloadHander
  
  
})##shinyServer~END
