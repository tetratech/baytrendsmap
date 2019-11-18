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
  
  # Data ####
  ## df_import ####
  df_import <- eventReactive(input$fn_input, {
    #
    inFile <- input$fn_input
    
    if (is.null(inFile)){
      return(NULL)
    }##IF~is.null~END
    #
    # Read user imported file
    df_input <- read.csv(inFile$datapath, header = TRUE,
                         sep = ",", quote = "\"", stringsAsFactors = FALSE)
    #
    return(df_input)
    #
  })##df_import~END
  
  ## df_import_DT ####
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
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000) )
  )##output$df_import_DT~END
  
  
  # df_filt ####
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
    #
    return(df_y)
    #
  })##df_filt~END
  
  # df_filt_DT ####
  output$df_filt_DT <- DT::renderDT({
    
    df_f <- df_filt()
    
    # Remove 2 columns that have so much info that blows up size of rows.
    str_names <- names(df_f)
    str_names_drop <- c("sa.sig.inc", "sa.sig.dec")
    str_names_keep <- str_names[!(str_names %in% str_names_drop)]
    
    return(df_f[, str_names_keep])
    
  }##expression~END
  , filter="top"
  , caption = "Table 2. Filtered map data."
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000) )
  )##df_filt_DT~END
  
  # df_filt_dups ####
  df_filt_dups <- eventReactive ({
    input$but_filt_apply
    input$fn_input
    }, {
    # data
    if(input$but_filt_apply == 0){
      df_tmp <- df_import()
    } else {
      df_tmp <- df_filt()
    }
    
    #
    # Check and report back number and locations of duplicates
    tib_abc <- summarize(group_by(df_tmp, station)
                         , n_Parameter = n_distinct(parmName, na.rm=TRUE)
                         , n_GAM = n_distinct(gamName, na.rm=TRUE)
                         , n_Layer = n_distinct(layer, na.rm=TRUE)
                         , n_Period = n_distinct(periodName, na.rm=TRUE)
                         , n_Season = n_distinct(seasonName, na.rm=TRUE)
                         )
    #
    return(as.data.frame(tib_abc))
    #
  })##df_filt_dups~END
  
  # df_filt_dups_DT ####
  output$df_filt_dups_DT <- DT::renderDT({
    df_tmp <- df_filt_dups()
    return(df_tmp)
  }##expression~END
  , filter="top"
  , caption = "Table 3. Filtered map data duplicates by column."
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000) )
  )##df_filt_dups_DT~END
  
  
  # df_filt_dups_num ####
  # Number of sites with more than 1 entry per field
  output$filt_dups_num <- renderText({
    #
    input$but_filt_apply
    input$fn_input
    # data
    if(input$but_filt_apply == 0){
      df_tmp <- df_import()
    } else {
      df_tmp <- df_filt()
    }##IF~END
    #
    # Check and report back number and locations of duplicates
    tib_abc <- dplyr::summarize(dplyr::group_by(df_tmp, station)
                         , n_Parameter = dplyr::n_distinct(parmName, na.rm=TRUE)
                         , n_GAM = dplyr::n_distinct(gamName, na.rm=TRUE)
                         , n_Layer = dplyr::n_distinct(layer, na.rm=TRUE)
                         , n_Period = dplyr::n_distinct(periodName, na.rm=TRUE)
                         , n_Season = dplyr::n_distinct(seasonName, na.rm=TRUE)
                          )
    #
    # # DEBUG
    # tmp_stat <- c("CB1.1", "CB2.1", "CB3.3E", "CB3.3W", "CB4.1W")
    # tib_abc <- tibble(station = tmp_stat
    #                      , n_Parameter = 1
    #                      , n_GAM = 2
    #                      , n_Layer = 1
    #                      , n_Period = 1
    #                      , n_Season = 1)

    n_dups <- sum(tib_abc[, 2:ncol(tib_abc)] != 1)
    #
    str_n_dups <- paste0(n_dups, " = Number of entries with more than 1 record.")
    #
    return(str_n_dups)
    #
  })##df_filt_dups_num~END
  
  # # helper ####
  output$txt_nrow_df_import <- renderText({
    ifelse(is.null(input$fn_input), NULL, paste0("Records, df_import, n = ", nrow(df_import()), "."))
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
  
  # UI - Filter Data ####
  output$filt_state <- renderUI({
    str_col <- "state"
    str_sel <- eval(parse(text = paste0("input$sel_", str_col)))
    str_SI <- paste0("SI_", str_col)
    df_x <- df_import()
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
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
                     choices = unique(df_x[, str_col]),
                     multiple = TRUE,
                     selected = if(str_sel == 1){
                       unique(df_x[, str_col])
                     } else {NULL})
      
    )##fluidRow~END
  })##filt_seasonNameEND
  
  # UI - Map Options ####
  output$opt_var <- renderUI({
    str_col <- "variable"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_gamDiff,
                     multiple = FALSE,
                     selected = pick_gamDiff[1])

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
                     choices = pick_pal,
                     multiple = FALSE,
                     selected = pick_pal[1])
      
    )##fluidRow~END
  })##opt_pal~END
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
      selectizeInput(str_SI, h4(paste0("  Up is good?")),
                     choices = c("TRUE", "FALSE"),
                     multiple = FALSE,
                     selected = "TRUE")
      
    )##fluidRow~END
  })##riverNames_t~END
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
  })##opt_ext_t~END
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
  })##opt_ext_t~END
  
 
  
  # Map Range ####
  map_range <- eventReactive (input$but_map_range, {
    # start with base map
    m_r <- map_base
    
    # data for plot
    df_mr <- df_filt()
    mr_cI_type  <- input$SI_classInt
    mr_pal <- input$SI_pal
    mr_var <- input$SI_variable
    mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
    brks_user <- eval(parse(text = paste0("c(", input$breaks, ")"))) #evals to NULL if left blank
    
    # breaks vs numclasses
    ## input is "" for blank
    if(is.null(brks_user)==TRUE){
      # no breaks, use slider for num classes
      mr_numclass <- input$numclass
      # derive breaks from user n and style
      mr_cI_val <- classInt::classIntervals(df_mr[, mr_var], mr_numclass, mr_cI_type)
      # Redo num classes as "pretty" picks its own number of breaks
      mr_numclass <- ifelse(mr_cI_type=="pretty", length(mr_cI_val$brks) - 1, mr_numclass)
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
    boo_riverNames <- input$SI_riverNames
   if(boo_riverNames == "Yes"){
      m_r <- m_r + 
        annotate(geom = "text", x = as.numeric(lab_Sus[2]), y=as.numeric(lab_Sus[3]), label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2]), y=as.numeric(lab_Pat[3]), label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2]), y=as.numeric(lab_Cho[3]), label=lab_Cho[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2]), y=as.numeric(lab_Pot[3]), label=lab_Pot[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2]), y=as.numeric(lab_Rap[3]), label=lab_Rap[1], hjust=1) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2]), y=as.numeric(lab_Yor[3]), label=lab_Yor[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2]), y=as.numeric(lab_Jam[3]), label=lab_Jam[1], hjust=0)
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
                                  , labels = brewer.pal(max(3, mr_numclass), mr_pal)[1:mr_numclass]
                                  )
    # Minimum of 3 different levels or get warning
    ## Break, Text
    fort_df_mr$map_brk_num <- cut(fort_df_mr[, mr_var]
                                  , breaks = mr_brks
                                  )
    
    # Add points to map
    m_r <- m_r + geom_point(data=fort_df_mr
                                    , aes_string(x =" longitude"
                                                 , y = "latitude"
                                                 , fill = "map_brk_col")
                                    , size = 4
                                    , pch = 21
                                    , color = "black"
                                    , na.rm = TRUE)
    
    # Legend
    #Modify Legend 
    m_r <- m_r + scale_fill_discrete(name = mr_var_name
                        #, labels = paste(c(">", rep("< ", length(mr_cI_val$brks)-1)), round(mr_cI_val$brks, 2))) +
                        , labels = levels(fort_df_mr$map_brk_num)) +
      theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face = "bold"))
    
    
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
      zoomregion_bbox <- eval(parse(text=pick_zoomregion_bbox[match(zoomregion, pick_zoomregion)]))
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
    mr_ext <- input$SI_ext #"pdf"
    #date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- file.path("map", paste0("map_range.", mr_ext))
    ggplot2::ggsave(fn_out, plot = m_r, device = mr_ext, height = 9, width = 9/1.5, units = "in" )
      
    #
    return(m_r)
    #return(ggplotly(m_r))
    #
  })##map_range~END
  
  
  output$map_r_render <- renderPlot({
  #output$map_r_render <- renderPlotly({
    # default map to show
    if(input$but_map_range == 0){
      m_r_2 <- map_base
    } else {
      m_r_2 <- map_range()  
    }
    print(m_r_2) 
    #ggplotly(m_r_2)
  })##map_r~END
  
  # but_mr_title ####
  observeEvent(input$but_mr_title, {
    sep1 <- ": "
    sep2 <- "\n" #"; "
    str_title <- paste(input$SI_parmName
                      , paste("GAM", input$SI_gamName, sep = sep1)
                      , paste("Layer", input$SI_layer, sep = sep1)
                      , paste("Period", input$SI_periodName, sep = sep1)
                      , paste("Season", input$SI_seasonName, sep = sep1)
                      , sep = sep2)
    updateTextAreaInput(session, "map_range_title", value = str_title)
    # max is 89 characters, if need to wrap dynamically
    #https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-set-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
  })##observerEvent~input$but_mr_title~END
  
    
  # but_map_range_save ####
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
  
  
  # Map Trend ####
  map_trend <- eventReactive (input$but_map_trend, {
    # start with base map
    m_t <- map_base
    
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
    boo_riverNames_t <- input$SI_riverNames_t
    if(boo_riverNames_t == "Yes"){
      m_t <- m_t + 
        annotate(geom = "text", x = as.numeric(lab_Sus[2]), y=as.numeric(lab_Sus[3]), label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2]), y=as.numeric(lab_Pat[3]), label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2]), y=as.numeric(lab_Cho[3]), label=lab_Cho[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2]), y=as.numeric(lab_Pot[3]), label=lab_Pot[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2]), y=as.numeric(lab_Rap[3]), label=lab_Rap[1], hjust=1) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2]), y=as.numeric(lab_Yor[3]), label=lab_Yor[1], hjust=0) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2]), y=as.numeric(lab_Jam[3]), label=lab_Jam[1], hjust=0)
    }##IF~riverNames~END
    
    # Title
    mt_title <- input$map_trend_title
    if(!is.null(mt_title)==TRUE){
      m_t <- m_t + labs(title=paste(mt_title, collapse="; "))
      # labs(title=paste(mr_pal_col, collapse="; "))
    }##IF~riverNames~END
    
    # Points ##
    
    boo_upisgood   <- input$SI_upisgood # TRUE
    chg_pval_poss <- input$map_trend_val_poss # 0.25
    chg_pval_sig <- input$map_trend_val_sig # 0.05
    #
    #if (boo_upisgood == TRUE){
      df_mt[df_mt[, "gamDiff.chg.pval"] > chg_pval_poss, "ChangeClass"] <- "NS"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posIncr"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posDecr"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigIncr"
      df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigDecr"
    #} else {
      # df_mt[df_mt[, "gamDiff.chg.pval"] > chg_pval_poss, "ChangeClass"] <- "NS"
      # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posIncr"
      # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_poss & df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posDecr"
      # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & df_mt[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigIncr"
      # df_mt[df_mt[, "gamDiff.chg.pval"] < chg_pval_sig  & df_mt[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigDecr"
   # }##boo_upisgood~END
    
    # fortify
    fort_df_mt <- ggplot2::fortify(df_mt)
    #
    #
    trend_ChangeClass <- c("sigDecr", "sigIncr", "posDecr", "posIncr", "NS")
    trend_leg_label   <- c("Significant Decrease", "Significant Increase", "Possible Decrease", "Possible Increase", "Unlikely")
    # trend_leg_color   <- c("orange", "green", "orange", "green", "dark gray")
    # trend_leg_shape   <- c("25", "24", "21", "21", "23")
    trend_leg_size    <- c("4", "4", "3", "3", "2")
    
    # Default (up is good = TRUE)
    trend_leg_color   <- c("orange", "green", "orange", "green", "dark gray")
    trend_leg_shape   <- c("25", "24", "21", "21", "23")
    manval_color <- c("sigDecr" = "orange", "sigIncr" = "green", "posDecr" = "orange", "posIncr" = "green", "NS" = "dark gray")
    manval_shape <- c("sigDecr" = 25, "sigIncr" = 24, "posDecr" = 21, "posIncr" = 21, "NS" = 23)
    manval_size  <- c("sigDecr" = 4, "sigIncr" = 4, "posDecr" = 3, "posIncr" = 3, "NS" = 2)
    
    if(boo_upisgood == FALSE){
      # Direction Arrows the same
      # Just the colors change
      trend_leg_color   <- c("green", "orange", "green", "orange", "dark gray")
      manval_color <- c("sigDecr" = "green", "sigIncr" = "orange", "posDecr" = "green", "posIncr" = "orange", "NS" = "dark gray")
      
    }##IF~boo_upisgood~END
    
    
    # Add to data frame
    fort_df_mt[, "ChangeClass"] <- factor(fort_df_mt[, "ChangeClass"], trend_ChangeClass)
    # 
    fort_df_mt$ChangeClass_color <- trend_leg_color[match(fort_df_mt$ChangeClass, trend_ChangeClass)]
    fort_df_mt$ChangeClass_shape <- trend_leg_shape[match(fort_df_mt$ChangeClass, trend_ChangeClass)]
    fort_df_mt$ChangeClass_size  <- trend_leg_size[match(fort_df_mt$ChangeClass, trend_ChangeClass)]
    
    m_t <- m_t + geom_point(data=fort_df_mt
                                   , aes_string(x="longitude", y="latitude"#, group ="ChangeClass"
                                                #, color="ChangeClass"
                                                , shape="ChangeClass"
                                                , size="ChangeClass"
                                                , fill="ChangeClass"
                                                )
                                     , color = "black"
                                   # , color = fort_df_mt$ChangeClass_color 
                                   # , shape = as.numeric(fort_df_mt$ChangeClass_shape)
                                   # , size = as.numeric(fort_df_mt$ChangeClass_size)
                                   # , fill = fort_df_mt$ChangeClass_color
                                   #, na.rm=TRUE
                                   ) +
      #scale_color_manual(name = "Type of trend", labels = trend_leg_label, values = manval_color, drop = FALSE ) + 
      scale_shape_manual(name = "Type of trend", labels = trend_leg_label, values = manval_shape, drop = FALSE ) + 
      scale_fill_manual( name = "Type of trend", labels = trend_leg_label, values = manval_color, drop = FALSE ) + 
      scale_size_manual( name = "Type of trend", labels = trend_leg_label, values = manval_size,  drop = FALSE ) +
      theme(legend.position = c(1, 0.12), legend.justification = c(1, 0), legend.title = element_text(face = "bold"))
      #theme(legend.position = "bottom", legend.box = "horizontal", legend.title=element_text(face="bold"))
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
      zoomregion_bbox <- eval(parse(text=pick_zoomregion_bbox[match(zoomregion, pick_zoomregion)]))
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
    mt_ext <- input$SI_ext_t #"pdf"
    #date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- file.path("map", paste0("map_trend.", mt_ext))
    ggplot2::ggsave(fn_out, plot = m_t, device = mt_ext, height = 9, width = 9/1.5, units = "in" )
    # Save so download button just copies
    #
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
  
  # but_mt_title ####
  observeEvent(input$but_mt_title, {
    sep1 <- ": "
    sep2 <- "\n" #"; "
    str_title <- paste(input$SI_parmName
                       , paste("GAM", input$SI_gamName, sep = sep1)
                       , paste("Layer", input$SI_layer, sep = sep1)
                       , paste("Period", input$SI_periodName, sep = sep1)
                       , paste("Season", input$SI_seasonName, sep = sep1)
                       , sep = sep2)
    updateTextAreaInput(session, "map_trend_title", value = str_title)
    # max is 89 characters, if need to wrap dynamically
    #https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-set-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
  })##observerEvent~input$but_mr_title~END
  
  # # but_map_trend_save ####
  output$but_map_trend_save <- downloadHandler(
    filename = function() {
      mt_ext <- input$SI_ext_t
      date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("map_trend_", date_time, ".", mt_ext)
      # #paste0(input$fn_input, input$SI_ext)
    }, ##filename~END
    content = function(fn) {
      mt_ext <- input$SI_ext_t
      fn_out <- file.path("map", paste0("map_trend.", mt_ext))
      #print(map_range())
      # ggplot2::ggsave(file, plot = ggplot2::last_plot(), device = ext, height = 9, width = 9/1.5, units = "in" )
      # #file.copy("map_range.pdf", fn, overwrite=TRUE)
      file.copy(fn_out, fn, overwrite = TRUE)
    }##content~END
  )##map_t_save~END
  
  # Button, Clear Filters ####
  observeEvent(input$but_ClearFilters, {
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
    updateSelectizeInput(session, "SI_state"         , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "cbSeg92"
    updateSelectizeInput(session, "SI_cbSeg92"       , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "stationGrpName"
    updateSelectizeInput(session, "SI_stationGrpName", choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "station"
    updateSelectizeInput(session, "SI_station"       , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "parmName"
    updateSelectizeInput(session, "SI_parmName"      , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "gamName"
    updateSelectizeInput(session, "SI_gamName"       , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "layer"
    updateSelectizeInput(session, "SI_layer"         , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "periodName"
    updateSelectizeInput(session, "SI_periodName"    , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))
      str_col <- "seasonName"
    updateSelectizeInput(session, "SI_seasonName"    , choices=unique(df_x[, str_col]), selected=unique(df_x[, str_col]))

    #}##FUNCTION~clearFilterSelection~END
  })
  
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
  
})##shinyServer~END
