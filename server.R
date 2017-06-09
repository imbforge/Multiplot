#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# enable file uploads up to 500MB
options(shiny.maxRequestSize=500*1024^2) 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
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
    
    # load translation table
    translation.data <- reactive({
        if (is.null(input$file_translation)) {return(NULL)}
        read.table(file=input$file_translation$datapath, header=T, sep='\t', stringsAsFactors=FALSE)
    })
    
    # name experiments in data table
    all.data <- reactive({
        # check the prerequisites
        if (is.null(input$file_input)) { return(NULL) }
        if (is.null(input$file_translation)) { return(raw.data()) }
        
        # name the experiments
        t.data <- translation.data()
        colnames(t.data) <- c("experiment","temp.experiment")
        
        # fuse the two tables, move added column to old "experiment" column and delete the added temp column
        noname.data <- raw.data()
        tmp.data <- merge( noname.data, t.data, by="experiment", all.x=TRUE )
        
        order.levels <- c( unique(t.data$temp.experiment), unique(tmp.data$experiment[is.na(tmp.data$temp.experiment)]) ) # use the same order for plotting that is found in the translation table and add all elements not found in that table at the end
        
        tmp.data$temp.experiment[is.na(tmp.data$temp.experiment)] <- tmp.data$experiment[is.na(tmp.data$temp.experiment)] # fix names of temp.experiment names that were generated as NA while merging
        tmp.data$experiment <- factor(tmp.data$temp.experiment, levels=order.levels) # overwrite old experiment IDs
        tmp.data$temp.experiment <- NULL # clean up
        named.data <- tmp.data # re-create plot.data
        rm(tmp.data) # clean more
        return(named.data)
    })
    
    
    
    ##################
    # User Interface #
    ##################
    
    # generate a drop down list from input data column names
    column2plot <- function(id, label, indata) {
        renderUI({
            
            selectInput(id, 
                        label=label,  
                        choices=names(indata),
                        selected=names(indata)[length(indata)]
            )
        })
    }
    
    # select, which column to plot (by name)
    observe({
        
        available_colnames <- NULL
        last_colname <- NULL
        
        if (!is.null(all.data())) {
            available_colnames <- colnames(all.data())
            last_colname <- available_colnames[1]
        }
        
        updateSelectInput(session,
                          "column_select_x_axis",
                          label = "Select a column to plot on x axis of selector",
                          choices = available_colnames,
                          selected = last_colname)
        
        updateSelectInput(session,
                          "column_select_y_axis",
                          label = "Select a column to plot on y axis of selector",
                          choices = available_colnames,
                          selected = last_colname)
        
        updateSelectInput(session,
                          "column_select_z_axis",
                          label = "Select a column to plot on z axis of selector",
                          choices = available_colnames,
                          selected = last_colname)
        
        updateSelectInput(session,
                          "column_target_x_axis",
                          label = "Select a column to plot on x axis of target",
                          choices = available_colnames,
                          selected = last_colname)
        
        updateSelectInput(session,
                          "column_target_y_axis",
                          label = "Select a column to plot on y axis of target",
                          choices = available_colnames,
                          selected = last_colname)
        
        updateSelectInput(session,
                          "column_target_z_axis",
                          label = "Select a column to plot on z axis of target",
                          choices = available_colnames,
                          selected = last_colname)
    })
    
    # select, which experiment should be used as "control" data set in select plot 
    # to guide selection in target plot
    observe({
        updateSelectInput(session, "control_experiment",
                    label = "Select an experiment as control",
                    choices = all.data()$experiment,
                    selected = all.data()$experiment[1])
    })
    
    
    # input controls for max and min values for colour scaling
    observe({
        if (is.null(all.data())) {return(NULL)}
        
        if (is.null(input$column_select_z_axis)) {
            # min_value = NULL
            return()
        }
        else {
            min_value = min(as.numeric(as.character(all.data()[,input$column_select_z_axis])), na.rm = TRUE)
        }
        
        updateNumericInput(session, "colour_select_min", 
                           label = "Colour scale min value", 
                           value = min_value
        ) # the minimum value of Colour legend
    })
    
    observe({
        if (is.null(all.data())) {return(NULL)}
        
        if (is.null(input$column_select_z_axis)) {
            # max_value = NULL
            return()
        }
        else {
            max_value = max(as.numeric(as.character(all.data()[,input$column_select_z_axis])), na.rm = TRUE)
        }
        
        updateNumericInput(session, "colour_select_max", 
                           label = "Colour scale max value", 
                           value = max_value
        ) # the maximum value of Colour legend
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
        
        # get an empty plot, if no data are available
        if (is.null(all.data()) | is.null(input$column_select_x_axis) | is.null(input$column_select_y_axis)) { 
            return( empty_plot("not enough data...") )
        }
        
        # get the plotting data (already pre-filtered by input$ parameters)
        plot.data <- all.data()
        plot.data <- plot.data[,c("experiment",
                                  input$column_select_x_axis, 
                                  input$column_select_y_axis, 
                                  input$column_select_z_axis, 
                                  input$column_target_x_axis, 
                                  input$column_target_y_axis,
                                  input$column_target_z_axis)]
        
        # only one experiment is to be used, filter now...
        if (input$controlExperimentCheck) {
            plot.data <- plot.data[plot.data$experiment == input$control_experiment, ]
        }
        
        
        
        colour_log <- NULL
        
        if (input$controlLogScaleCheck) {
            colour_log <- scale_colour_continuous(limits=c(input$colour_select_min, input$colour_select_max), trans="log10")
        } else {
            colour_log <- scale_colour_continuous(limits=c(input$colour_select_min, input$colour_select_max))
            }
        
        # plot either with colouring of points or not - depending on selected z axis
        if (!input$colorPlot){
            ggplot(data = plot.data,
                   aes_string(input$column_select_x_axis, input$column_select_y_axis)) +
                geom_point()
        } else {
            ggplot(data = plot.data,
                   aes_string(input$column_select_x_axis, input$column_select_y_axis, color=input$column_select_z_axis)) +
                geom_point() +
                theme(legend.position = "bottom") +
                colour_log
        }
        
    })
    
    # this plot will show only those values selected in selectorPlot
    # create the plot dynamically will make sure the plot is always shown in a decent size
    output$generate_targetPlot <- renderUI({
        
        # set a default height
        plotHeight <- paste0(400, "px")
        
        # get the amount of experiments and determine the height by that
        target.data <- brushedPoints(all.data(), input$plot_brush)
        countExperiments <- length(unique(target.data$experiment))
        
        if (countExperiments > 2 & input$facetTargetPlot == TRUE){
            plotHeight <- paste0(400 * countExperiments, "px")
        }
        
        plotOutput("targetPlot", height = plotHeight)
    })
    
    output$targetPlot <- renderPlot({
        
        if (is.null(all.data())) {
            return( empty_plot("not enough data...") )
        }
        
        target.data <- brushedPoints(all.data(), input$plot_brush)
        
        # empty plot, if no data is selected
        if (is.null(target.data) | is.null(input$column_target_x_axis) | is.null(input$column_target_y_axis)) { 
            return( empty_plot("not enough data...") )
        }
        
        
        facetting <- NULL
        
        # check, if the target plot should be plotted by experiment
        if (input$facetTargetPlot) {
            facetting <- facet_wrap( ~ experiment, ncol = 1)
        }
        
        # plot selected data either with coloured points (by z axis) or not
        if (!input$colorTargetPlot) {
            ggplot(data = target.data,
                   aes_string(input$column_target_x_axis, input$column_target_y_axis)) +
                geom_point() + 
                facetting
        } else {
            ggplot(data = target.data,
                   aes_string(input$column_target_x_axis, input$column_target_y_axis, color=input$column_target_z_axis)) +
                geom_point() +
                facetting
        }
        
    }) # end renderPlot
})
