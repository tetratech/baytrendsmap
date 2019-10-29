function(){
  tabPanel("HELP"
           , fluidPage(
             fluidRow(h2("baytrends output map tool R package Help", style  = "text-align:center"))##fluidRow~END
            , htmlOutput("help_html")
                 
           
       )##fluidPage~END   
           
  )##tabPanel~END
}##FUNCTION~END
