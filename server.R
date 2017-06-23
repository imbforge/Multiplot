
if(require(shiny)){
    library("shiny")
} else {
    install.packages("shiny")
    library("shiny")
}


# enable file uploads up to 500MB
options(shiny.maxRequestSize=500*1024^2) 
options(stringsAsFactors = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    global_select_plot <- NULL
   
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
        tmp.data$experiment <- tmp.data$temp.experiment[order(tmp.data$temp.experiment)] # overwrite old experiment IDs
        tmp.data$temp.experiment <- NULL # clean up
        named.data <- tmp.data # re-create plot.data
        rm(tmp.data) # clean more
        return(named.data)
    })
    
    
    
    ##################
    # User Interface #
    ##################
    
    # generate a drop down list from input data column names
    # select, which column to plot (by name)
    observe({
        
        available_colnames <- NULL
        first_colname <- NULL
        
        if (!is.null(all.data())) {
            available_colnames <- colnames(all.data())
            first_colname <- available_colnames[1]
        }
        
        updateSelectInput(session,
                          "column_select_x_axis",
                          label = "Select a column to plot on x axis of selector",
                          choices = available_colnames,
                          selected = first_colname)
        
        updateSelectInput(session,
                          "column_select_y_axis",
                          label = "Select a column to plot on y axis of selector",
                          choices = available_colnames,
                          selected = first_colname)
        
        updateSelectInput(session,
                          "column_select_z_axis",
                          label = "Select a column to plot on z axis of selector",
                          choices = available_colnames,
                          selected = first_colname)
        
        updateSelectInput(session,
                          "column_target_x_axis",
                          label = "Select a column to plot on x axis of target",
                          choices = available_colnames,
                          selected = first_colname)
        
        updateSelectInput(session,
                          "column_target_y_axis",
                          label = "Select a column to plot on y axis of target",
                          choices = available_colnames,
                          selected = first_colname)
        
        updateSelectInput(session,
                          "column_target_z_axis",
                          label = "Select a column to plot on z axis of target",
                          choices = available_colnames,
                          selected = first_colname)
    })
    
    # select, which experiment should be used as "control" data set in select plot 
    # to guide selection in target plot
    observe({
        updateSelectInput(session, "control_experiment",
                    label = "Select an experiment as control",
                    choices = as.character(all.data()$experiment),
                    selected = as.character(all.data()$experiment)[1])
    })
    
    
    # input controls for max and min values for colour scaling
    observe({
        if (is.null(all.data())) {return(NULL)}
        
        if (is.null(input$column_select_z_axis ) | input$column_select_z_axis == "" ) {
            # min_value = NULL
            # don't do anything
        } else {
            # check, if all elements in vector are numeric - replace with NA
            tmp <- all.data()[,input$column_select_z_axis]
            tmp[!sapply(tmp, is.numeric, USE.NAMES = F)] <- NA
            
            if (all(is.na(tmp))) {
                min_value <- 0
                max_value <- 1
            } else {
                min_value = min(tmp, na.rm = TRUE)
                max_value = max(tmp, na.rm = TRUE)
            }
            
            
            updateNumericInput(session, "colour_select_min", 
                               label = "Colour scale min value", 
                               value = min_value
            ) # the minimum value of Colour legend
            
            updateNumericInput(session, "colour_select_max", 
                               label = "Colour scale max value", 
                               value = max_value
            ) # the maximum value of Colour legend
        } # end if
        
        if (is.null(input$column_target_z_axis ) | input$column_target_z_axis == "" ) {
            # min_value = NULL
            # don't do anything
        } else {
            # check, if all elements in vector are numeric - replace with NA
            tmp <- all.data()[,input$column_target_z_axis]
            tmp[!sapply(tmp, is.numeric, USE.NAMES = FALSE)] <- NA
            
            if (all(is.na(tmp))) {
                min_value <- 0
                max_value <- 1
            } else {
                min_value = min(tmp, na.rm = TRUE)
                max_value = max(tmp, na.rm = TRUE)
            }
            
            
            updateNumericInput(session, "colour_target_min", 
                               label = "Colour scale min value", 
                               value = min_value
            ) # the minimum value of Colour legend
            
            updateNumericInput(session, "colour_target_max", 
                               label = "Colour scale max value", 
                               value = max_value
            ) # the maximum value of Colour legend
        } # end if
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
        f.selectorPlot()
    })
    
    # wrap the plot in an extra function to make it available as download
    f.selectorPlot <- function(){
        # get an empty plot, if no data are available
        if (is.null(all.data()) | 
            input$column_select_x_axis == "" | 
            input$column_select_y_axis == "" |
            input$column_select_z_axis == "" | 
            input$column_target_x_axis == "" | 
            input$column_target_y_axis == "" |
            input$column_target_z_axis == "" ) { 
            
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
        
        if (input$controlSelectLogScaleCheck) {
            colour_log <- scale_colour_continuous(low = input$colour_select_min_colour, high = input$colour_select_max_colour,
                                                  limits=c(input$colour_select_min, input$colour_select_max), oob = squish, trans="log10") #, oob=squish
        } else {
            colour_log <- scale_colour_continuous(low = input$colour_select_min_colour, high = input$colour_select_max_colour,
                                                  limits=c(input$colour_select_min, input$colour_select_max), oob = squish) #, oob=squish(?????)
            }
        
        # plot either with colouring of points or not - depending on selected z axis
        if (!input$colorPlot){
            p.select <- ggplot(data = plot.data,
                   aes_string(input$column_select_x_axis, input$column_select_y_axis)) +
                geom_point()
        } else {
            p.select <- ggplot(data = plot.data,
                   aes_string(input$column_select_x_axis, input$column_select_y_axis, color=input$column_select_z_axis)) +
                geom_point() +
                theme(legend.position = "bottom") +
                colour_log
        }
        
        return(p.select)
        
    } # end f.selectorPlot
    
    # this plot will show only those values selected in selectorPlot
    # create the plot dynamically will make sure the plot is always shown in a decent size
    output$generate_targetPlotArea <- renderUI({
        
        # set a default height
        plotHeight <- paste0(400, "px")
        
        # get the amount of experiments and determine the height by that
        target.data <- brushedPoints(df = all.data(), 
                                     brush = input$plot_brush)
        
        
        countExperiments <- length(unique(target.data$experiment))
        
        if (countExperiments > 2 & input$facetTargetPlot == TRUE){
            plotHeight <- paste0(400 * countExperiments, "px")
        }
        
        plotOutput("targetPlot", height = plotHeight)
    })
    
    output$targetPlot <- renderPlot({
        f.targetPlot()
    })
    
    f.targetPlot <- function(){
        
        if (is.null(all.data())) {
            return( empty_plot("not enough data...") )
        }
        
        # print("debug")
        # print(input$plot_brush)
        
        target.data <- brushedPoints(df = all.data(), 
                                     brush = input$plot_brush)
        
        # empty plot, if no data is selected
        if (is.null(target.data) | 
            input$column_target_x_axis == "" | 
            input$column_target_y_axis == "" |
            input$column_target_z_axis == "" ) { 
            return( empty_plot("not enough data...") )
        }
        
        
        facetting <- NULL
        
        # check, if the target plot should be plotted by experiment
        if (input$facetTargetPlot) {
            facetting <- facet_wrap( ~ experiment, ncol = 1)
        }
        
        colour_log <- NULL
        
        if (input$controlTargetLogScaleCheck) {
            colour_log <- scale_colour_continuous(low = input$colour_target_min_colour, high = input$colour_target_max_colour,
                                                  limits=c(input$colour_target_min, input$colour_target_max), oob = squish, trans="log10") #, oob=squish
        } else {
            colour_log <- scale_colour_continuous(low = input$colour_target_min_colour, high = input$colour_target_max_colour,
                                                  limits=c(input$colour_target_min, input$colour_target_max), oob = squish) #, oob=squish(?????)
        }
        
        # plot selected data either with coloured points (by z axis) or not
        # make sure to have either log scaled colour scale from above or linear scaling
        if (!input$colorTargetPlot) {
            p.target <- ggplot(data = target.data,
                   aes_string(input$column_target_x_axis, input$column_target_y_axis)) +
                geom_point() + 
                facetting
        } else {
            p.target <- ggplot(data = target.data,
                   aes_string(input$column_target_x_axis, input$column_target_y_axis, color=input$column_target_z_axis)) +
                geom_point() +
                colour_log +
                facetting +
                theme(legend.position = "bottom")
        }
        
        return(p.target)
        
    } # end targetPlot
    
    # magic behind the download plot button
    output$downloadSelectPlot <- downloadHandler(
        filename = "select_plot.pdf",
        content = function(file) {
            # write pdf of ggplot
            ggsave(filename=file, plot = f.selectorPlot(), device = 'pdf', width=200, height=150, unit="mm")
        }
    )
    
    output$downloadTargetPlot <- downloadHandler(
        
        filename = "target_plot.pdf",
        content = function(file) {
            # get the amount of experiments and determine the height by that
            target.data <- brushedPoints(df = all.data(), 
                                         brush = input$plot_brush)
            
            
            countExperiments <- length(unique(target.data$experiment))
            
            if (countExperiments > 2 & input$facetTargetPlot == TRUE){
                plotHeight <- 75 * countExperiments
            } else {
                plotHeight <- 75
            }
            
            # write pdf of ggplot
            ggsave(filename=file, plot = f.targetPlot(), device = 'pdf', width=100, height=plotHeight, unit="mm", limitsize = FALSE)
        }
    )
})
