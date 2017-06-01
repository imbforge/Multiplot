# This app is supposed to plot multiple data sets based on the selection of another plot
library(shiny)
library(shinyjs)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Multiplotter"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        tabsetPanel( type="tabs",
#==============================================================================
                     tabPanel( "Data",
                         # get the data file
                         fileInput('file_input', 'Choose Input File',
                                   accept=c('text/txt', 'text/tsv')
                         )
                     ), # end of get data
#==============================================================================
                     tabPanel( "Plot",
                        # get the parameters to plot
                        uiOutput("column_name_x_axis_selector"),
                        uiOutput("column_name_y_axis_selector"),
                        
                        uiOutput("column_name_x_axis_target"),
                        uiOutput("column_name_y_axis_target")
                        
                    
                     ) # end of plotting parameters
            
        )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("selectorPlot", brush = "plot_brush"),
       
       plotOutput("targetPlot")
    )
  )
))
