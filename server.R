#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    ##################
    # Data functions #
    ##################
    
    # load data here
    raw.data <- reactive({
        
        if (is.null(input$file_input)) {return(NULL)}
        
        else if (input$file_input$type == 'application/zip') {
            
            # produce a temporary folder for unzipping
            target_dir <- paste0( dirname(input$file_input$datapath), '1')
            fused_file <- paste0( target_dir, '/fused_file.tsv' )
            
            # catch all file names and finally unzip the data
            file_list <- unzip(input$file_input$datapath, list=T, overwrite=F)
            system( paste0("unzip ", input$file_input$datapath, ' -d ', target_dir))
            
            # system call to run python script
            # output needs to be written to temporary directory
            system( paste0("python unite_data_v3.py --data ", target_dir, " --out ", fused_file))
            
            # read python table output to R data table
            # this table already contains an "experiment" column
            tmp.data <- read.table(file=fused_file, header=T, sep='\t', stringsAsFactors=FALSE)
            
            # replace letters or signs that could be understood as mathematical symbols in later eval() commands
            tmp.data$experiment <- gsub("[-*/+ ]", "_", tmp.data$experiment)
            
            # remove unzipped folder?
            system( paste0('rm -r ', target_dir) )
            
            return(tmp.data)
        }
        else {
            
            tmp.data <- read.table(file=input$file_input$datapath, header=T, sep='\t', stringsAsFactors=FALSE)
            
            # produce a column containing the experiment name 
            if(! 'experiment' %in% colnames(tmp.data)) {
                tmp.data$experiment <- paste(tmp.data$Row, tmp.data$Column, tmp.data$Timepoint,sep='_')
            }
            
            # replace letters or signs that could be understood as mathematical symbols in later eval() commands
            tmp.data$experiment <- gsub("[-*/+ ]", "_", tmp.data$experiment)
            
            tmp.data$experiment <- as.factor(tmp.data$experiment)
            
            return(tmp.data)
        }
    })
    
    
    ##################
    # User Interface #
    ##################
    
    # select, which column to plot (by name)
    output$column_name_x_axis_selector <- renderUI({
        
        selectInput("column_select_x_axis", 
                    label="Select a column to plot on x axis of selector",  
                    choices=names(raw.data()),
                    selected=names(raw.data())[length(raw.data())]
        )
    })
    
    output$column_name_y_axis_selector <- renderUI({
        
        selectInput("column_select_y_axis", 
                    label="Select a column to plot on y axis of selector",  
                    choices=names(raw.data()),
                    selected=names(raw.data())[length(raw.data())]
        )
    })
    
    output$column_name_x_axis_target <- renderUI({
        
        selectInput("column_target_x_axis", 
                    label="Select a column to plot on x axis of target",  
                    choices=names(raw.data()),
                    selected=names(raw.data())[length(raw.data())]
        )
    })
    
    output$column_name_y_axis_target <- renderUI({
        
        selectInput("column_target_y_axis", 
                    label="Select a column to plot on y axis of target",  
                    choices=names(raw.data()),
                    selected=names(raw.data())[length(raw.data())]
        )
    })
    
    ##################
    # Plot functions #
    ##################
    
    # function to create an empty plot with a text complaining about what is not good.
    empty_plot <- function(anders) {
        ggplot(data=data.frame(x=1)) + 
            geom_text(aes_q(10,20, label=anders)) + 
            labs(x="", y="") + 
            scale_x_continuous(breaks = 1, labels = "") + 
            scale_y_continuous(breaks = 1, labels = "")
    }
    
    # this plot will serve as the selector for the target plot
    output$selectorPlot <- renderPlot({
        
        # get the plotting data (already pre-filtered by input$ parameters)
        plot.data <- raw.data()
        plot.data <- plot.data[,c(input$column_select_x_axis, input$column_select_y_axis, input$column_target_x_axis, input$column_target_y_axis)]
        
        if (is.null(plot.data) | is.null(input$column_select_x_axis) | is.null(input$column_select_y_axis)) { 
            return( empty_plot("not enough data...") )
        }
        
        ggplot(data = plot.data,
               aes_string(input$column_select_x_axis, input$column_select_y_axis)) +
            geom_point()
        
    })
    
    # this plot will show only those values selected in selectorPlot
    output$targetPlot <- renderPlot({
        
        target.data <- brushedPoints(raw.data(), input$plot_brush)
        
        if (is.null(target.data)) { 
            return( empty_plot("not enough data...") )
        }
        
        ggplot(data = target.data,
               aes_string(input$column_target_x_axis, input$column_target_y_axis)) +
            geom_point()
    })
})