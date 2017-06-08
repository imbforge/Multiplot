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
                         ),
                         
                         # experiment IDs (from column 1-3) are transformed to meaningful names given in this file
                         fileInput('file_translation', 'Choose Translation File')
                     ), # end of get data
#==============================================================================
                     tabPanel( "Plot",
                        # get the parameters to plot
                        uiOutput("generate_column_name_x_axis_selector"),
                        uiOutput("generate_column_name_y_axis_selector"),
                        checkboxInput("colorPlot", 
                                      "Use third column to colour selector", 
                                      value = FALSE),
                        uiOutput("generate_column_name_z_axis_selector"),
                        
                        checkboxInput("controlExperimentCheck",
                                      "Use only one experiment as control for gating",
                                      value = FALSE),
                        uiOutput("generate_column_control_experiment"),
                        # uiOutput("generate_slider_z_axis_selector"),
                        
                        # here there should be a horizontal line
                        
                        uiOutput("generate_column_name_x_axis_target"),
                        uiOutput("generate_column_name_y_axis_target"),
                        checkboxInput("colorTargetPlot", 
                                      "Use third column to colour target", 
                                      value = FALSE),
                        uiOutput("generate_column_name_z_axis_target"),
                        
                        checkboxInput("facetTargetPlot",
                                      "Split plot by experiment",
                                      value = FALSE)
                    
                     ) # end of plotting parameters
            
        )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("selectorPlot", brush = "plot_brush"),
       
       plotOutput("targetPlot")
       # plotOutput("targetPlot", height = "1000px")
       # plotOutput("targetPlot")
    )
  )
))
