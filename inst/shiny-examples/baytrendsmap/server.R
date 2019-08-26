#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  # df_import ####
  output$df_import_DT <- renderDT({
    # input$df_import will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$fn_input
    
    if (is.null(inFile)){
      return(NULL)
    }##IF~is.null~END
    
    #message(getwd())
    
    # Add "Results" folder if missing
    boo_Results <- dir.exists(file.path(".", "data"))
    if(boo_Results==FALSE){
      dir.create(file.path(".", "data"))
    }
    
    # Remove all files in "data" folder
    fn_results <- list.files(file.path(".", "data"), full.names=TRUE)
    file.remove(fn_results)
    
    # Read user imported file
    df_input <- read.csv(inFile$datapath, header = TRUE,
                         sep = ",", quote = "\"", stringsAsFactors = FALSE)
    
    # Write to "data" folder - Import as TSV
    fn_input <- file.path(".", "data", "data_import.tsv")
    write.table(df_input, fn_input, row.names=FALSE, col.names=TRUE, sep="\t")
    
    # Copy to "data" folder - Import "as is"
    file.copy(input$fn_input$datapath, file.path(".", "data", input$fn_input$name))
    
    # read.table(file = inFile$datapath, header = input$header,
    #          sep = input$sep, quote = input$quote)
    
    return(df_input)
    
  }##expression~END
  , filter="top", options=list(scrollX=TRUE)
  )##output$df_import_DT~END
  
})
