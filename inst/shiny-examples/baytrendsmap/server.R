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
shinyServer(function(input, output) {
  
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
                     , n_layer = n_distinct(layer, na.rm=TRUE)
                     , n_gamName = n_distinct(gamName, na.rm=TRUE)
                     , n_periodName = n_distinct(periodName, na.rm=TRUE)
                     , n_seasonName = n_distinct(seasonName, na.rm=TRUE)
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
  output$opt_col <- renderUI({
    str_col <- "color"
    str_SI <- paste0("SI_", str_col)
    fluidRow(
      selectizeInput(str_SI, h4(paste0("  Select ", str_col, ":")),
                     choices = pick_color,
                     multiple = FALSE,
                     selected = pick_color[1])
      
    )##fluidRow~END
  })##opt_col~END
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
  
  
  # Map Range ####
  map_range <- eventReactive (input$but_map_range, {
    # start with base map
    m_r <- map_base
    
    # data for plot
    #df_mr <- df_filt()
    # str_var <- input$SI_variable
    # str_color <- input$SI_color
    # str_classInt <- input$SI_clasInt
    
    
    
    # River Names
    boo_riverNames <- input$SI_riverNames
   if(boo_riverNames == "Yes"){
      m_r <- m_r + 
        annotate(geom = "text", x = as.numeric(lab_Sus[2]), y=as.numeric(lab_Sus[3]), label=lab_Sus[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pat[2]), y=as.numeric(lab_Pat[3]), label=lab_Pat[1]) +
        annotate(geom = "text", x = as.numeric(lab_Cho[2]), y=as.numeric(lab_Cho[3]), label=lab_Cho[1]) +
        annotate(geom = "text", x = as.numeric(lab_Pot[2]), y=as.numeric(lab_Pot[3]), label=lab_Pot[1]) +
        annotate(geom = "text", x = as.numeric(lab_Rap[2]), y=as.numeric(lab_Rap[3]), label=lab_Rap[1]) +
        annotate(geom = "text", x = as.numeric(lab_Yor[2]), y=as.numeric(lab_Yor[3]), label=lab_Yor[1]) +
        annotate(geom = "text", x = as.numeric(lab_Jam[2]), y=as.numeric(lab_Jam[3]), label=lab_Jam[1])
    }##IF~riverNames~END
    
    # Title
    if(!is.null(input$SI_layer)){
      m_r <- m_r + 
        labs(title=paste(input$SI_layer, collapse=";"))
    }##IF~riverNames~END
    
    # save map
    ext <- input$SI_ext #"pdf"
    date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn_out <- paste0("map_range.", ext)
    # ggplot2::ggsave(fn_out, plot = m_r, device = ext, height = 9, width = 9/1.5, units = "in" )
    ggplot2::ggsave(fn_out, plot = m_r, device = ext, height = 9, width = 9/1.5, units = "in" )
      
    #
    return(m_r)
    #
  })##map_range~END
  
  
  
  output$map_r_render <- renderPlot({
    # default map to show
    if(input$but_map_range == 0){
      m_r_2 <- map_base
    } else {
      m_r_2 <- map_range()  
    }
    print(m_r_2)                   
  })##map_r~END
  
  
  # df_filt_dups ####
  output$but_map_range_save <- downloadHandler(
    filename = function() {
      ext <- input$SI_ext
      date_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("map_range_", date_time, ".", ext)
      # #paste0(input$fn_input, input$SI_ext)
    }, ##filename~END
    content = function(fn) {
       ext <- input$SI_ext
       fn_out <- paste0("map_range.", ext)
       #print(map_range())
     # ggplot2::ggsave(file, plot = ggplot2::last_plot(), device = ext, height = 9, width = 9/1.5, units = "in" )
      # #file.copy("map_range.pdf", fn, overwrite=TRUE)
        file.copy(fn_out, fn, overwrite = TRUE)
    }##content~END
  )##map_r_save~END
  
  
  # Map Trend ####
  
  
})##shinyServer~END
