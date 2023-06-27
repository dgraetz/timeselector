#TODO:
# - option to download modified data frame

library(shinyWidgets)
library(DT)

fluidPage(
  
  titlePanel("Remove timestamps"),
  
  fluidRow(
    column(10,
           p("Go through the data and click to the left and right of an area you would 
      like to remove. Click remove to record the index values of that area. Download
      a table defining the removed areas for further data processing or modified data.
      Check the help tab for further details.")
    ),
    column(2,
           downloadButton("downloadDemo", "Download Demo Data [raw]..."))
  ),
  
  fluidRow(
    column(2, offset = 1,
           selectInput("data_type", "Select Data type", choices = c("raw breathing [specific]", "time series [generic]"))
    ),
    column(2,
           fileInput("file", "Upload time series...")
    ),
    column(2, 
           selectInput("ts_col", "Select column", choices = c("Please wait..."))
    ),
    column(2,
           selectInput("group_col", "Select grouping variable", choices = c("Please wait..."))
    ),
    column(2, offset = 1,
           textOutput("Nremoved"),
           textOutput("Nvalues"),
           div(style = "margin-top:10px")
    )
  ),
  
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
                                         )
                                  ),
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
                                         actionButton("reset_all", "Reset all", class = "btn-default"),
                                         div(style = "margin-top:10px"),
                                         actionButton("undo", "Undo", class = "btn-default"),
                                         div(style = "margin-top:10px"),
                                         actionButton("remove", "Remove", class = "btn-danger"),
                                         div(style = "margin-top:40px"),
                                         downloadButton("downloadRemovedRows", "Download Indices..."),
                                         downloadButton("downloadData", "Download modified Data...")
                                  ),
                                ),
                                
                                
                       ),
                       
                       tabPanel("Table", 
                                column(3,
                                       DTOutput("table")
                                )
                       ),
                       tabPanel("Help",
                                column(7,
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
                                  the data, click 'Remove'. Keep in mind that the data itself
                                  will actually remain unchanged - you can save the areas you selected
                                  by clicking 'Download indices' or 'Download modified Data'.
                                  No x axis will be used from the data for plotting, just index values.
                                  Please take this into account if you previously removed rows from your data 
                                  or if spacing between data points varies and is important.
                                  This app can get slow if your data is large, because of plotting speed.
                                  I found that installing the 'AGG' plotting device makes this app considerably
                                  faster, see https://stackoverflow.com/questions/63795842/slow-graph-rendering-with-ggplot2-rstudio-gpu-issue"),
                                  p(
                                    strong("Download Demo Data:"),
                                    "This will provide you with a raw respiration data file that was
                                    collected in our lab. The formatting is pretty specific, but you can
                                    upload it with the Data Type = raw and column = 'sensor' setting to try this app. Don't
                                    worry about the formatting of your files, though, the generic 'Data Type' 
                                    setting should be able to handle most file formats." 
                                  ),
                                  p(
                                    strong("Data Type:"),
                                    "For users who work with consistently formatted data files, use the generic
                                  time series mode. Your data will be read in with the rio::import() function.
                                  This packages typically handles tab delimited files, csv files, Rdata and RDS
                                  files pretty well. You can upload a simple vector as well as long as it is defined
                                  in an RDS or Rdata file or in new rows in a csv or txt file."
                                  ),
                                  p(
                                    strong("Select column:"),
                                    "Select the column that contains the time series (in other words, the y value of
                                  the data). No x axis will be used from the data, just index values will be used.
                                  Please take this into account if you previously removed rows from your data."
                                  ),
                                  p(
                                    strong("Select grouping variable"),
                                    "Choose a column here if you have defined groups of data. These will not be displayed
                                  with different colors, but as separate lines. This is the variable that goes into the 
                                  ggplot group aesthetics argument."
                                  ),
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
                                    strong("Reset:"),
                                    "This will reset the current selection to 0."
                                  ),
                                  p(
                                    strong("Reset all:"),
                                    "This will reset all selections made so far."
                                  ),
                                  p(
                                    strong("Download Indices:"),
                                    "Once you hit this button, the table from the table tab will be saved as 
                                    a tab delimited text file. Data will not be removed from the actual file.
                                    You will need to do that in a separate step. You may want to name the file 
                                    like the input file (with some change in the name to prevent overwriting."),
                                  p(
                                    strong("Download mdoified Data:"),
                                    "The rows of the data you indicated in this app will be removed and provided for
                                    download. The formatting of your file may differ from your original input. It will be
                                    saved with 'write.table().'"
                                  )
                                )
                       )
           )
    )
  )
)
