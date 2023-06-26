#TODO:
# - option to download modified data frame
# - accept more file types and be able to select column(s) to plot

library(shinyWidgets)
library(DT)

fluidPage(
  
  titlePanel("Remove timestamps!"),
  
  fluidRow(
    column(5,
           fileInput("file", "Upload raw respiration file...")), 
    column(2, offset = 5,
           textOutput("Nremoved"),
           textOutput("Nvalues"),
           div(style = "margin-top:10px"),
           downloadButton("download", "Download..."))),
  
  fluidRow(
    column(11, offset = 1, 
           tabsetPanel(type = "tabs",
                       tabPanel("Plot", 
                                fluidRow(
                                  column(1,
                                         noUiSliderInput(
                                           inputId = "height",
                                           label = "Y Zoom",
                                           min = 0, max = 5,
                                           value = 1,
                                           orientation = "vertical",
                                           height = "350px",
                                           direction = "rtl",
                                         )),
                                  column(9, 
                                         div(style='width:100%; overflow-x: scroll; height:400px; overflow-y: scroll', 
                                             plotOutput("output_plot", 
                                                        click = "plot_click")),
                                         fluidRow(
                                           column(10, offset = 1,
                                                  noUiSliderInput(
                                                    inputId = "width",
                                                    label = "X Zoom",
                                                    min = 0, max = 100,
                                                    value = 30,
                                                    width = "100%",
                                                    format = wNumbFormat(decimals = 0)
                                                  )
                                           )
                                         )
                                  ),
                                  column(1,
                                         numericInput("from", "from", value = 0),
                                         numericInput("to", "to", value = 0),
                                         actionButton("reset", "Reset", class = "btn-default"),
                                         div(style = "margin-top:10px"),
                                         actionButton("back", "Undo", class = "btn-default"),
                                         div(style = "margin-top:10px"),
                                         actionButton("remove", "Remove", class = "btn-danger")
                                  ),
                                ),
                                
                                
                       ),
                       
                       tabPanel("Table", 
                                column(3,
                                       DTOutput("table")
                                )
                       ),
                       tabPanel("Help",
                                br(),
                                p(
                                  "This App reads in your file and helps you 
                                  look through the time series and remove noisy data. 
                                  To do that, simply upload your data and then scroll
                                  through the plot. When you find an area you would like
                                  to remove, click the plot at the start of that 
                                  and a second time at the end of the area. Alternatively,
                                  you can enter index values into the 'from' and 'to' fields.
                                  The selection will now turn red. To 'remove' this part of
                                  the data, click 'Remove'. Keep in mind that the data
                                  actually remains unchanged, but you can save the areas you selected
                                  by clicking 'Save'."),
                                p(
                                  strong("File to upload:"),
                                  "Currently, this app is tailored to a very specific file format. In future,
                                  this app will generalize to a variety of inputs. A demo file can be downloaded
                                  by clicking the download button before anything is uploaded (you may need to
                                  restart the app now). This demo file can then be uploaded and should work."
                                ),
                                p(
                                  strong("Remove:"), 
                                  "Once you hit 'Remove', the start and end point 
                                  of the area will be recorded as index values (see the table tab)."), 
                                p(
                                  strong("Download:"),
                                  "Once you hit 'Download', the table will be saved as a tab delimited text file.
                                  Data will not be removed from the actual file. You will need to do that in 
                                  a separate step. You may want to name the file like the input file, with some
                                  extension."))
           )
    )
  )
)



