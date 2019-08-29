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
    #
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
  df_filt_dups <- eventReactive (input$but_filt_apply, {
    # data
    df_tmp <- df_filt()
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
  
  # Filter UI's ####
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
  #
  # output$opt_var <- renderUI({
  #   df_xx <- df_filt()
  #   str_col <- "seasonName"
  #   str_SI <- paste0("SI_", str_col)
  #   fluidRow(
  #     selectizeInput(str_col, h4(paste0("  Select ", str_col, ":")),
  #                    choices = unique(df_x[, str_col]),
  #                    multiple = TRUE,
  #                    selected = if(str_sel == 1){
  #                      unique(df_x[, str_col])
  #                    } else {NULL})
  #   
  #   )##fluidRow~END
  # })opt_var~END
  
  
})##shinyServer~END
