library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(rio)
library(viridis)

colors <- viridis(5)
options(scipen = 999)

counter <- 1

shinyServer(
  
  function(input, session, output){
    
    data <- reactiveValues(d = NULL)
    
    observeEvent(input$file, { #if time series is being uploaded
      
      if (input$data_type == "raw breathing [specific]"){ #this is for lab internal respiration files, which require specific formatting if raw
        
        data$d <- readLines(input$file$datapath) %>%
          as.data.frame() %>%
          separate(col = ".", into = c("sensors", "message1", "message2", "message3") ,sep = "\\$") %>% #separate single column into sensor info and msgs (sometimes multiple msgs are in one line)
          separate(col = "sensors", into = c("counter", "sensor"), sep ="(?<=[[:digit:]])\t") %>%  #separate the sensors
          mutate(sensor = as.numeric(sensor))
        
      } else if (input$data_type == "time series [generic]"){ #this is for generic data with consistent, predictable formatting, that can be read in with rio
        data$d <- import(input$file$datapath) %>%
          as.data.frame()
      }
      
      data$d$no_groups <- 1 #this will help with data that is grouped (such as blocks in an experiment)
      
      if (input$data_type == "raw breathing [specific]"){
        
        #updateSelectInput(session, "ts_col", choices = c("sensor", colnames(data$d %>% select(-sensor)))) #default = sensor, here we update columns with column names
        updateCheckboxGroupInput(session, "ts_col", choices = c("sensor", colnames(data$d %>% select(-sensor)))) #default = sensor, here we update columns with column names
        
      } else if (input$data_type == "time series [generic]"){
        
        updateCheckboxGroupInput(session, "ts_col", choices = colnames(data$d)) #here we update columns with column names
        
      }
      
      updateSelectInput(session, "group_col", choices = c("no_groups", colnames(data$d %>% select(-no_groups)))) #this is for groups that may be present in the data set, i. e., experimental blocks. This will plot those lines separately. By default, no groups will be used (hence the code that puts no_groups first)
      
      data$d <- data$d %>% 
        mutate(dontPlot = 0, #this gets updated later and marks the rows not to be plotted (the ones cut out)
               idx = 1:n(), #this is for the x axis
               cut_marker = NA, #at the position the second cut happens, this will be numbered and ...
               filled_groups = NA) #this variable filled from top to bottom --> this is so that the lines before and after cut are not connected
      
      data$d$cut_marker[1] <- 1
      
      
    })
    
    #this will contain clicks. By default, 0, 0
    click <- reactiveValues(tb = data.frame(x = c(0,0), 
                                            y = c(0,0)))
    
    #this is the data frame that is displayed in the table tab, containing the indices of the removed rows
    removed_rows <- reactiveValues(rem = data.frame(from = double(),
                                                    to = double()))
    
    observeEvent(input$indices, { #if index from previous work is uploaded
      
      removed_rows$rem <- import(input$indices$datapath)
      
    }
    )
    
    
    observeEvent(input$reset, { #if reset button pressed
      if (is.null(data$d)){return(NULL)}
      click$tb <- data.frame(x = c(0,0), y = c(0,0)) #reset to 0
    })
    
    observeEvent(input$reset_all, { #if reset button pressed
      if (is.null(data$d)){return(NULL)}
      click$tb <- data.frame(x = c(0,0), y = c(0,0)) #reset to 0
      removed_rows$rem <- data.frame(from = double(),
                                     to = double())
      data$d$no_groups <- 1 
      data$d <- data$d %>% 
        mutate(dontPlot = 0, 
               idx = 1:n(), 
               cut_marker = NA,
               filled_groups = NA) 
      data$d$cut_marker[1] <- 1
    })
    
    observeEvent(input$remove, { #if remove button pressed
      if (is.null(data$d)){return(NULL)}
      if (nrow(click$tb) == 2 & click$tb$x[1] != click$tb$x[2] & click$tb$x[1] != 0 & click$tb$x[2] != 0){ #we only perform an action if we have two rows in the column, the x values from and to are not the same and the values are not 0
        data$d[(click$tb$x[1]:click$tb$x[2]),]$dontPlot <- counter #here we mark the area not to plot
        counter <- counter + 1 
        x <- click$tb$x #makes it easier to index
        x <- x[order(x)] #we order, because user may mark the end of area first
        
        removed_rows$rem <- removed_rows$rem %>% #adding the removal info to the data frame for plotting and export
          add_row(from = x[1], 
                  to = x[2])
        
        data$d$cut_marker[x[1]] = max(data$d$cut_marker, na.rm = TRUE) + 1 #we mark the end of the removed area so that it becomes another group for plotting later when filled_groups is filled again (see the prepping steps just before plotting)
        click$tb <- data.frame(x = c(0, 0), y = c(0, 0)) #after action complete, reset click data frame
      }
      
      
      
    })
    
    observeEvent(input$undo, { #if we want to undo the last removal action
      if (is.null(data$d)){return(NULL)}
      if (nrow(removed_rows$rem) >= 1){
        data$d[removed_rows$rem[nrow(removed_rows$rem),]$from : removed_rows$rem[nrow(removed_rows$rem),]$to,]$dontPlot <- 0 #include the recently removed area again in plot
        data$d[removed_rows$rem[nrow(removed_rows$rem),]$from,]$cut_marker <- NA #reset the cut_marker to NA, so that the groups are going back to previous stage
        
        removed_rows$rem <- removed_rows$rem[-nrow(removed_rows$rem),] #remove last row of the output table
      }
      
    })
    
    observeEvent(input$plot_click, { #if click
      if (is.null(data$d)){return(NULL)}
      
      x <- round(input$plot_click$x)
      y <- round(input$plot_click$y)
      click$tb <- #this pipe stolen from https://stackoverflow.com/questions/75953789/shiny-get-x-y-axis-locations-from-a-plot-from-multiple-clicks
        isolate(click$tb) %>% 
        add_row(
          x = x,
          y = y
        )
      
      if (nrow(removed_rows$rem) >= 1 & any(x > removed_rows$rem$from & x < removed_rows$rem$to)){
        
        removed_rows$rem <- removed_rows$rem[-which(x > removed_rows$rem$from & x < removed_rows$rem$to),]
        click$tb <- data.frame(x = c(0,0), y = c(0,0))
        updateNumericInput(session, "from", value = click$tb$x[1])
        updateNumericInput(session, "to", value = click$tb$x[2])
        
      } else {
        
        if (nrow(click$tb) == 3){ #at the third click and when no removal happened, just reset it, keep the last click and be ready for next click
          click$tb <- click$tb[3,]
        }
        
        if (nrow(click$tb) == 1){
          updateNumericInput(session, "from", value = click$tb$x[1]) #update the input with the x values (this is sort of a feedback loop. For the actual plot, the numeric fields are used, and these numbers can come from clicks or manual input)
        }
        
        if (nrow(click$tb) == 2){
          updateNumericInput(session, "to", value = click$tb$x[2])
        }
      }
      
      
    })
    
    observeEvent(input$from, {
      if (is.null(data$d)){return(NULL)}
      
      if (nrow(click$tb) %in% c(1, 2)){ #this is sort of a feedback loop. For the actual plot, the numeric fields are used, and these numbers can come from clicks or manual input
        click$tb$x[1] <- input$from
      }
      
    })
    
    observeEvent(input$to, {
      if (is.null(data$d)){return(NULL)}
      
      if (nrow(click$tb) == 2){ #this is sort of a feedback loop. For the actual plot, the numeric fields are used, and these numbers can come from clicks or manual input
        click$tb$x[2] <- input$to
      }
      
    })
    
    
    output$output_plot <- renderPlot({
      if (is.null(data$d)|is.null(input$ts_col)){return(NULL)}
      
      #the following lines of code transfer the group start markers over to filled_groups which is then filled downwards. This is a way to implement groups that are not dependent on the order that the areas are marked
      # We don't do this anymore, we just change the background and not remove data from the plot, hence this is not necessary anymore
      # data$d <- data$d %>%
      #   mutate(filled_groups = cut_marker) %>%
      #   fill(filled_groups)

      data$d$group <- interaction(data$d[,"filled_groups"], data$d[,input$group_col]) #this integrates the user input about dataset groups with the groups resulting from removing data

      gg <- ggplot(data = data$d, aes_string(x = "idx")) #build basic df

      if (nrow(removed_rows$rem) >= 1){
        gg <- gg +
          annotate("rect", xmin = removed_rows$rem$from, xmax = removed_rows$rem$to, ymin = -Inf, ymax = Inf, fill = "lightgrey") #this shows all removed data in grey
      }

      if (nrow(click$tb) == 2 & (click$tb$x[1] != 0 | click$tb$x[2] != 0)) {

        gg <- gg +
          annotate("rect", xmin = click$tb$x[1], xmax = click$tb$x[2], ymin = -Inf, ymax = Inf, fill="red") #currently active area in red
      }

      col_vals <- seq(1, length(colors), length.out = length(input$ts_col)) #equally spaced columns

      for(i in 1:length(input$ts_col)){

        gg <- gg +
          #geom_line(aes_string(y = input$ts_col[i], group = "group"), color = colors[col_vals[i]]) #not sure i like those colors
          geom_line(aes_string(y = input$ts_col[i], group = "group"))

      }

      if (click$tb$x[1] != 0 | click$tb$x[2] != 0) { #ignore default; this marks the start and end of area

        gg <- gg +
          geom_vline(data = click$tb, aes(xintercept = x), color = "red")

      }

      gg <- gg +
        labs(x = "",
             y = "") +
        theme_classic()

      gg
      
      
    }, 
    
    #here we define the x zoom
    reactive( #make it so that it's always within allowed range of [1, 50000]
      if (input$width <= 100 & input$width > 0){
        width = input$width/100*50000
      } else if (input$width > 100) {
        width = 100/100*50000
      } else if (input$width < 1) {
        width = 1/100*50000
      }),
    
    reactive( #same with height
      if (input$height < 100 & input$height > 0){
        height = input$height/100*50000
      } else if (input$height > 100) {
        height = 100/100*50000
      } else if (input$height < 1) {
        height = 1/100*50000
      })
    )
    
    #this is what shows up at the top right, gives the # currently selected values
    output$Nvalues <- renderText({
      if (is.null(data$d)){return(NULL)}
      if (nrow(click$tb) == 2){
        val <- abs(click$tb$x[1] - click$tb$x[2])
      } else {
        val <- 0
      }
      paste0(val, " values selected.")
    }
    )
    
    #counter for the number of areas removed
    output$Nremoved <- renderText({
      if (is.null(data$d)){return(NULL)}
      paste0(nrow(removed_rows$rem), " artifacts removed.")
    })
    
    #generate the table
    output$table <- renderDataTable({
      if (is.null(data$d)){return(NULL)}
      dt_out <- removed_rows$rem[nrow(removed_rows$rem):1,]
      dt_out$N <- dt_out$to - dt_out$from
      dt_out
    }
    )
    
    
    #demo data download
    output$downloadDemo <- downloadHandler(
      filename = function() {
        paste0("Demo.txt")
      },
      content = function(file) {
        writeLines(readLines("resp20_20220126.17.49.41.txt"), file)
      }
    )
    
    #index table download
    output$downloadRemovedRows <- downloadHandler(
      filename = function() {
        paste0("removed_indices_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".txt")
      },
      content = function(file) { 
        if (is.null(data$d)){ #if no data is uploaded by user yet, download an example file
          writeLines(readLines("resp20_20220126.17.49.41.txt"), file)
        } else { #else write actual removed areas and sort by start of area.
          removed_rows$rem %>%
            arrange(from) %>%
            write.table(file, row.names = FALSE)
        }
      }
    )
    
    #modified data download
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("data_mod_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".txt")
      },
      content = function(file) { #I'd like to have two download buttons, but have yet to figure this out
        
        if (nrow(removed_rows$rem) >= 1){
          rows <- vector()
          for(i in 1:nrow(removed_rows$rem)){
            rows <- append(rows, removed_rows$rem$from[i]:removed_rows$rem$to[i])
          }
          download <- data$d %>%
            filter(!row_number() %in% rows)
        } else {
          download <- data$d
        }
        
        download %>%
          
          select(-no_groups,
                 -dontPlot, 
                 -idx, 
                 -cut_marker,
                 -filled_groups,
                 -group) %>%
          write.table(file, row.names = FALSE)
        
      }
    )
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
  }
)

