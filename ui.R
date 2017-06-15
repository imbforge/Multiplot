# This app is supposed to plot multiple data sets based on the selection of another plot

if(!require("colourpicker")) {install.packages("colourpicker")} # for colourInput
if(!require("ggplot2")) {install.packages("ggplot2")}
if(!require("scales")) {install.packages("scales")} # for oob=squish
if(!require("shiny")) {install.packages("shiny")} 


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
                         tabPanel( "Gate",
                                   # get the parameters to plot
                                   # uiOutput("generate_column_name_x_axis_selector"),
                                   selectInput("column_select_x_axis",
                                               label = "Select a column to plot on x axis of selector",
                                               choices = NULL,
                                               selected = NULL),
                                   
                                   selectInput("column_select_y_axis",
                                               label = "Select a column to plot on y axis of selector",
                                               choices = NULL,
                                               selected = NULL),
                                   # uiOutput("generate_column_name_y_axis_selector"),
                                   checkboxInput("colorPlot", 
                                                 "Use third column to colour selector", 
                                                 value = FALSE),
                                   
                                   selectInput("column_select_z_axis",
                                               label = "Select a column to plot on z axis of selector",
                                               choices = NULL,
                                               selected = NULL),
                                   # uiOutput("generate_column_name_z_axis_selector"),
                                   
                                   numericInput("colour_select_min", 
                                                "Colour scale min value",
                                                value = 0),
                                   numericInput("colour_select_max", 
                                                "Colour scale max value",
                                                value = 100),
                                   colourInput("colour_select_min_colour",
                                               "Choose a colour for gradient min",
                                               value = "#FF0000",
                                               palette = "limited"),
                                   colourInput("colour_select_max_colour",
                                               "Choose a colour for gradient max",
                                               value = "#0000FF",
                                               palette = "limited"),
                                   checkboxInput("controlSelectLogScaleCheck",
                                                 "Log-scale colour scaling",
                                                 value = FALSE),
                                   
                                   checkboxInput("controlExperimentCheck",
                                                 "Use only one experiment as control for gating",
                                                 value = FALSE),
                                   # uiOutput("generate_column_control_experiment"),
                                   selectInput("control_experiment",
                                               label = "Select an experiment as control",
                                               choices = NULL,
                                               selected = NULL),
                                   # download the plot
                                   downloadButton('downloadSelectPlot', 'Download Select Plot')
                         ),
                         tabPanel( "Target",
                                   
                                   
                                   
                                   #===========================================
                                   # here there should be a horizontal line
                                   
                                   # uiOutput("generate_column_name_x_axis_target"),
                                   # uiOutput("generate_column_name_y_axis_target"),
                                   selectInput("column_target_x_axis",
                                               label = "Select a column to plot on x axis of target",
                                               choices = NULL,
                                               selected = NULL),
                                   selectInput("column_target_y_axis",
                                               label = "Select a column to plot on y axis of target",
                                               choices = NULL,
                                               selected = NULL),
                                   checkboxInput("colorTargetPlot", 
                                                 "Use third column to colour target", 
                                                 value = FALSE),
                                   # uiOutput("generate_column_name_z_axis_target"),
                                   selectInput("column_target_z_axis",
                                               label = "Select a column to plot on z axis of target",
                                               choices = NULL,
                                               selected = NULL),
                                   
                                   numericInput("colour_target_min", 
                                                "Colour scale min value",
                                                value = 0),
                                   numericInput("colour_target_max", 
                                                "Colour scale max value",
                                                value = 100),
                                   colourInput("colour_target_min_colour",
                                               "Choose a colour for gradient min",
                                               value = "#FF0000",
                                               palette = "limited"),
                                   colourInput("colour_target_max_colour",
                                               "Choose a colour for gradient max",
                                               value = "#0000FF",
                                               palette = "limited"),
                                   checkboxInput("controlTargetLogScaleCheck",
                                                 "Log-scale colour scaling",
                                                 value = FALSE),
                                   
                                   checkboxInput("facetTargetPlot",
                                                 "Split plot by experiment",
                                                 value = FALSE),
                                   
                                   #=================================================
                                   # here there should be another horizontal line
                                   downloadButton('downloadTargetPlot', 'Download Target Plot')
                                   
                         ) # end of plotting parameters
                         
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("selectorPlot", brush = "plot_brush"),
            
            uiOutput("generate_targetPlotArea")
        )
    )
))
