library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(rio)
options(scipen = 999)

# data <- readLines("resp20_20220126.17.49.41.txt")
# #data <- readLines("resp32_20220215.14.20.46.txt")
# 
# data <- data %>%
#   as.data.frame() %>%
#   separate(col = ".", into = c("sensors", "message1", "message2", "message3") ,sep = "\\$") %>% #separate single column into sensor info and msgs (sometimes multiple msgs are in one line)
#   separate(col = "sensors", into = c("counter", "sensor"), sep ="(?<=[[:digit:]])\t") %>% #separate the sensors
#   mutate(sensor = as.numeric(sensor),
#          dontPlot = 0,
#          idx = seq_along(sensor),
#          cut_marker = NA,
#          filled_groups = NA)
# 
# data$cut_marker[1] <- 1
# 
# data <- list()

counter <- 1

shinyServer(
  function(input, session, output){
    
    # d <- reactive({
    #   # Make sure requirements are met
    #   req(input$file)
    #   
    #   readLines(input$file$datapath) %>%
    #     as.data.frame() %>%
    #     separate(col = ".", into = c("sensors", "message1", "message2", "message3") ,sep = "\\$") %>% #separate single column into sensor info and msgs (sometimes multiple msgs are in one line)
    #     separate(col = "sensors", into = c("counter", "sensor"), sep ="(?<=[[:digit:]])\t") %>%  #separate the sensors
    #     mutate(sensor = as.numeric(sensor),
    #                     dontPlot = 0,
    #                     idx = seq_along(sensor),
    #                     cut_marker = NA,
    #                     filled_groups = NA)
    # 
    # })
    
    data <- reactiveValues(d = NULL)
    
    observeEvent(input$file, {
      
      data$d <- readLines(input$file$datapath) %>%
        as.data.frame() %>%
        separate(col = ".", into = c("sensors", "message1", "message2", "message3") ,sep = "\\$") %>% #separate single column into sensor info and msgs (sometimes multiple msgs are in one line)
        separate(col = "sensors", into = c("counter", "sensor"), sep ="(?<=[[:digit:]])\t") %>%  #separate the sensors
        mutate(sensor = as.numeric(sensor),
               dontPlot = 0,
               idx = seq_along(sensor),
               cut_marker = NA,
               filled_groups = NA)
      
      data$d$cut_marker[1] <- 1
    })
    
    click <- reactiveValues(tb = data.frame(x = c(0,0), 
                                            y = c(0,0)))
    
    removed_rows <- reactiveValues(rem = data.frame(from = double(),
                                                    to = double()))
    
    
    observeEvent(input$reset, {
      if (is.null(data$d)){return(NULL)}
      click$tb <- data.frame(x = c(0,0), y = c(0,0))
      
    })
    
    observeEvent(input$remove, {
      if (is.null(data$d)){return(NULL)}
      if (nrow(click$tb) == 2){
        data$d[(click$tb$x[1]:click$tb$x[2]),]$dontPlot <- counter
        counter <- counter + 1
        x <- click$tb$x
        x <- x[order(x)]
        
        removed_rows$rem <- removed_rows$rem %>%
          add_row(from = x[1],
                  to = x[2])
        
        data$d$cut_marker[x[1]] = max(data$d$cut_marker, na.rm = TRUE) + 1
        click$tb <- data.frame(x = c(0, 0), y = c(0, 0))
      }
      
      
      
    })
    
    observeEvent(input$back, {
      if (is.null(data$d)){return(NULL)}
      if (nrow(removed_rows$rem) >= 1){
        data$d[removed_rows$rem[nrow(removed_rows$rem),]$from : removed_rows$rem[nrow(removed_rows$rem),]$to,]$dontPlot <- 0
        data$d[removed_rows$rem[nrow(removed_rows$rem),]$from,]$cut_marker <- NA
        
        removed_rows$rem <- removed_rows$rem[-nrow(removed_rows$rem),]
      }
      
    })
    
    # observeEvent(input$save, {
    #   if (is.null(data$d)){return(NULL)}
    #   #write.table(removed_rows$rem, "data.txt", sep = "\t", row.names = FALSE)
    #   write.table(data$d, "data.txt", sep = "\t", row.names = FALSE)
    # })
    
    observeEvent(input$plot_click, {
      if (is.null(data$d)){return(NULL)}
      click$tb <- 
        isolate(click$tb) %>% 
        add_row(
          x = round(input$plot_click$x),
          y = round(input$plot_click$y)
        )
      
      if (nrow(click$tb) == 3){
        click$tb <- click$tb[3,]
      }
      
      if (nrow(click$tb) == 1){
        updateNumericInput(session, "from", value = click$tb$x[1])
      }
      
      if (nrow(click$tb) == 2){
        updateNumericInput(session, "to", value = click$tb$x[2])
      }
    })
    
    observeEvent(input$from, {
      if (is.null(data$d)){return(NULL)}
      
      if (nrow(click$tb) %in% c(1, 2)){
        click$tb$x[1] <- input$from
      }
      
    })
    
    observeEvent(input$to, {
      if (is.null(data$d)){return(NULL)}
      
      if (nrow(click$tb) == 2){
        click$tb$x[2] <- input$to
      }
      
    })
    
    
    output$output_plot <- renderPlot({
      if (is.null(data$d)){return(NULL)}
      data$d <- data$d %>% 
        mutate(filled_groups = cut_marker) %>%
        fill(filled_groups) 
      
      gg <- ggplot(data = data$d[data$d$dontPlot == 0,], aes(x = idx, y = sensor))
      
      if (nrow(removed_rows$rem) >= 1){
        gg <- gg +
          annotate("rect", xmin = removed_rows$rem$from, xmax = removed_rows$rem$to, ymin = -Inf, ymax = Inf, fill = "lightgrey")
      }
      
      if (nrow(click$tb) == 2 & (click$tb$x[1] != 0 | click$tb$x[2] != 0)) {
        
        #gg <- gg + geom_rect(data = NULL, aes(xmin = click$tb$x[1], xmax = click$tb$x[2], ymin = -Inf, ymax = Inf), fill = "red", color = "red")
        gg <- gg + 
          annotate("rect", xmin = click$tb$x[1], xmax = click$tb$x[2], ymin = -Inf, ymax = Inf, fill="red")
      }
      
      gg <- gg + 
        geom_line(aes(group = factor(filled_groups)))
      
      if (click$tb$x[1] != 0 | click$tb$x[2] != 0) {
        
        gg <- gg +
          geom_vline(data = click$tb, aes(xintercept = x), color = "red")
        
      }
      
      gg <- gg +
        labs(x = "",
             y = "") +
        theme_classic()
      
      gg
      
    }, 
    
    reactive(
      if (input$width < 100 & input$width > 0){
        width = input$width/100*50000
      } else if (input$width > 100) {
        width = 100/100*50000
      } else if (input$width < 1) {
        width = 1/100*50000
      }),
    
    reactive(
      if (input$height < 100 & input$height > 0){
        height = input$height/100*50000
      } else if (input$height > 100) {
        height = 100/100*50000
      } else if (input$height < 1) {
        height = 1/100*50000
      })
    )
    
    
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
    
    output$Nremoved <- renderText({
      if (is.null(data$d)){return(NULL)}
      paste0(nrow(removed_rows$rem), " artifacts removed.")
    })
    
    output$table <- renderDataTable({
      if (is.null(data$d)){return(NULL)}
      dt_out <- removed_rows$rem[nrow(removed_rows$rem):1,]
      dt_out$N <- dt_out$to - dt_out$from
      dt_out
    }
    )
    
    output$download <- downloadHandler(
      filename = function() {
        paste0("removed_indices.txt")
      },
      content = function(file) {
        if (is.null(data$d)){
          writeLines(readLines("resp20_20220126.17.49.41.txt"), file)
        } else {
          write.table(removed_rows$rem, file, row.names = FALSE)
        }
      }
    )
    
  }
)

