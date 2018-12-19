library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinycssloaders)
library(ggmap)
library(ggplot2)
library(plotly)
library(bsplus)
library("htmltools")

options(spinner.color.background="#FF0000")

sidebar <- shinydashboard::dashboardSidebar(
width = 180,
hr(),
shinydashboard::sidebarMenu(id="tabs",
shinydashboard::menuItem("Application", tabName="mapPlot"),
shinydashboard::menuItem("Instructions", tabName = "about", selected=TRUE)
)
)

body <- shinydashboard::dashboardBody(
shinydashboard::tabItems(
shinydashboard::tabItem(tabName = "mapPlot",

tags$style(HTML("
.box.box-solid.box-primary>.box-header {
color:#fff;
background:#222d32
}
.box.box-solid.box-primary{
border-bottom-color:#D3D3D3;
border-left-color:#D3D3D3;
border-right-color:#D3D3D3;
border-top-color:#D3D3D3;
background:#D3D3D3
}
")),

fluidRow(
column(width = 4, 

shinydashboard::box(width = NULL, status = "primary", title = "Plot metrics", solidHeader = TRUE,
fileInput('datafile', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')) %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip(title = 'Select a file from your local computer. See the "Information" tab on the left for example files.', placement = "left")),

use_bs_tooltip(),

numericInput("zoom", "Zoom", 13, min = 3, max = 21, step = 1) %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip(title = "Set the zoom parameter to an integer between 3 and 21 inclusive. Small values zoom out, large values zoom in. Note: If you zoom in too much, you might lose the overlaid points.", placement = "left")),

numericInput("alpha", "Alpha", 1, min = 0, max = 1, step = 0.1) %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = "Set the alpha parameter to values between 0 and 1 inclusive. The alpha parameter designates the transparency of overlaid points. With an alpha value of 1, the points are entirely opaque; with an alpha value of 0, the points are invisible (you will only see the line connecting now-invisible points).", placement = "left")),

selectInput("mapType", "Map type", choices = c("satellite", "terrain", "hybrid")) %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = "Type of background map.", placement = "left")),

selectInput("line", "Line?", choices = c("No", "Yes")) %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = "If yes, a line will be drawn connecting the overlaid points in the order they appear in the read-in document. If no, the overlaid points will not be connected by a line.", placement = "left")),

conditionalPanel(
condition = "input.line == 'Yes'",
textInput("lineColor", "Line color", value = "black")  %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = "Choose the line color.", placement = "left"))
))),

column(width = 8,
shinydashboard::box(width = NULL, shinycssloaders::withSpinner(plotlyOutput("plotlyMap"), color = "#990000"), collapsible = FALSE, background = "black", title = "MDRS map with overlaid coordinates", status = "primary", solidHeader = TRUE)))),

shinydashboard::tabItem(tabName = "about",
shiny::fluidRow("Run by the Mars Society, the", a("Mars Desert Research Station", href="http://mdrs.marssociety.org/about-the-mdrs/"), 'is a space analogue habitat in the deserts of Utah. Each year, crews of approximately six members spend two weeks at the facility simulating a Mars mission. Part of this simulation involves extravehicular activities (EVAs), in which crew members wear spacesuits that provide "life support", "depressurize" in the airlock, and exit the habitat to investigate the outdoors.', style='padding-right: 30px;', style='padding-left: 30px;', style='padding-bottom:15px;', style='padding-top:10px;'),

shiny::fluidRow('The current application can be accessed from the tab on the left called "Application". It is intended for crew members to overlay information obtained during their EVAs onto maps. To use this application, you simply need to create a .CSV file that contains latitude and longitude measurements from your EVA and upload it into the application. The application will then plot these latitude and longitude measurements onto a map surrounding the habitat.', style='padding-right: 30px;', style='padding-left: 30px;', style='padding-bottom:15px;'),

shiny::fluidRow("Specifically, the CSV file must contain at least two columns with headings called 'Latitude' and 'Longitude'. An example of the first six lines of this format is shown in File 1. Reading this .CSV file into the application will produce a map with these measurements superimposed as shown in Figure 1.", style='padding-right: 30px;', style='padding-left: 30px;'),

br(),
br(),
div(p('File 1'), style="text-align: center;"),
div(img(src='Figure1.png', style="width: 100%"), style="text-align: center;"),
br(),
br(),

div(p('Figure 1'), style="text-align: center;"),
div(img(src='Figure2.png', style="width: 60%"), style="text-align: center;"),
br(),
br(),

shiny::fluidRow("You can also add one more quantitative variable (to be mapped to size) and/or one more qualitative variable (to be mapped to color). An example of this format is shown in File 2. These are simulated measurements where crewmembers recorded", a("radio signal scores", href="https://en.wikipedia.org/wiki/QSA_and_QRK_radio_signal_reports/"), "at six locations around the habitat. Here, they will link the QSA values (radio strength) as colors and the QRK values (radio intelligibility) as sizes. The result of uploading this file is shown in Figure 2.", style='padding-right: 30px;', style='padding-left: 30px;'),

br(),
br(),
div(p('File 2'), style="text-align: center;"),
div(img(src='Figure3.png', style="width: 100%"), style="text-align: center;"),
br(),
br(),

div(p('Figure 2'), style="text-align: center;"),
div(img(src='Figure4.png', style="width: 60%"), style="text-align: center;"),
br(),
br(),

shiny::fluidRow("Another example of this format is shown in File 3 (where only the quantitative field is given for point sizes) and the application will keep all points a default color, resulting in the map shown in Figure 3.", style='padding-right: 30px;', style='padding-left: 30px;'),

br(),
br(),
div(p('File 3'), style="text-align: center;"),
div(img(src='Figure5.png', style="width: 100%"), style="text-align: center;"),
br(),
br(),

div(p('Figure 3'), style="text-align: center;"),
div(img(src='Figure6.png', style="width: 60%"), style="text-align: center;"),
br(),
br(),

shiny::fluidRow("And one last example of this format is shown in File 4 (where only the qualitative field is given for point colors) and the application will keep all points a default size, resulting in the map shown in Figure 4.", style='padding-right: 30px;', style='padding-left: 30px;'),

br(),
br(),
div(p('File 4'), style="text-align: center;"),
div(img(src='Figure7.png', style="width: 100%"), style="text-align: center;"),
br(),
br(),

div(p('Figure 4'), style="text-align: center;"),
div(img(src='Figure8.png', style="width: 60%"), style="text-align: center;"),
br(),
br(),

shiny::fluidRow("You can download any of the above four example .CSV files at ", a("radio signal scores", href="https://github.com/lrutter/MDRSMaps/tree/master/files"), '. Or, create your own CSV file from your EVA and upload it to the application by clicking on the "Application" tab to the left!', style='padding-right: 30px;', style='padding-left: 30px;'),

br(),
br(),

shiny::fluidRow(strong("Note 1:"), "Please report errors", a("here", href="https://github.com/lrutter/MDRSMaps/issues"), ".", style='padding-right: 30px;', style='padding-left: 30px;'),

shiny::fluidRow(strong("Note 2:"), "For color definitions, you can use hexadecimal colors or color names from the list below:", style='padding-right: 30px;', style='padding-left: 30px;', style='padding-bottom: 15px;'),

div(img(src='Figure9.png', style="width: 100%"), style="text-align: center;")

)))

ui <- shinydashboard::dashboardPage(skin = "red",
shinydashboard::dashboardHeader(title = "MDRS Map", titleWidth = 180),
sidebar,
body
)


server=function(input, output, session){

#This function is repsonsible for loading in the selected file
filedata <- reactive({
infile <- input$datafile
if (is.null(infile)) {
# User has not uploaded a file yet
return(NULL)
}
read.csv(infile$datapath)
})

p <- eventReactive({c(filedata(), input$zoom, input$alpha, input$line, input$lineColor, input$mapType)}, {

df <-filedata()
if (is.null(df)) return(NULL)

df <- as.data.frame(df)
df <- df[,1:ncol(df)]

register_google(key = "AIzaSyCcJu4DttxEDccaixZomOCXcUhptYHX2n4")
# Center on MDRS
bbox <- ggmap::make_bbox(lon=Longitude, lat=Latitude, data=df, f = 0.05)
mapLoc <- get_map(location = bbox, zoom = input$zoom, maptype = input$mapType)

if (ncol(df) == 2){
df$size = 1
df$color = as.factor("blue")

p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_point(aes(x = Longitude, y = Latitude, size = size, color = color), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()

if (input$line == "Yes"){p <- p + geom_line(aes(x = Longitude, y = Latitude), data=df, color=input$lineColor)}
}

else if (ncol(df) == 3){
classCol = class(df[,3])
# If only color is defined
if (classCol %in% c("factor", "character")){
df$size = 1
p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_point(aes_string(x = colnames(df)[2], y = colnames(df)[1], size = colnames(df)[4], color = colnames(df)[3]), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()
if (input$line == "Yes"){p <- p + geom_line(aes(x = Longitude, y = Latitude), data=df, color=input$lineColor)}
}
# If only size is defined
else if (classCol %in% c("integer", "numeric")){
df$color = "blue"
p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_point(aes_string(x = colnames(df)[2], y = colnames(df)[1], size = colnames(df)[3], color = colnames(df)[4]), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()
if (input$line == "Yes"){p <- p + geom_line(aes(x = Longitude, y = Latitude), data=df, color=input$lineColor)}
}    
}

else if (ncol(df) == 4){
colNms = sapply(df[,3:4], class)
colCol = which(colNms %in% c("factor", "character"))+2
sizeCol = which(colNms %in% c("integer", "numeric"))+2

p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_point(aes_string(x = colnames(df)[2], y = colnames(df)[1], size = colnames(df)[sizeCol], color = colnames(df)[colCol]), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()
if (input$line == "Yes"){p <- p + geom_line(aes(x = Longitude, y = Latitude), data=df, color=input$lineColor)}
}

return(p)
})

output$plotlyMap <- renderPlotly({
gp <- ggplotly(p())

df <-filedata()
if (is.null(df)) return(NULL)

if (ncol(df) == 2){
    for (i in 1:length(gp$x$data[[3]]$text)){
    inputString <- gp$x$data[[3]]$text[i]
    rem1 = unlist(strsplit(inputString, "<br />size:"))[1]
    rem2 = unlist(strsplit(rem1, "<br />color:"))[1]
    gp$x$data[[3]]$text[i] <- rem2
    }
}

else if (ncol(df) == 3){
    for (i in 1:length(gp$x$data)){
        for (j in 1:length(gp$x$data[[i]]$text)){
            inputString <- gp$x$data[[i]]$text[j]
            if(!is.null(inputString)){
                rem1 = unlist(strsplit(inputString, "<br />lon:"))[1]
                rem2 = unlist(strsplit(rem1, "<br />lat:"))[1]
                gp$x$data[[i]]$text[j] <- rem2
            }
        }
    }
}

else if (ncol(df) == 4){
    for (i in 1:length(gp$x$data)){
        for (j in 1:length(gp$x$data[[i]]$text)){
            inputString <- gp$x$data[[i]]$text[j]
            if(!is.null(inputString)){
                rem1 = unlist(strsplit(inputString, "<br />lon:"))[1]
                rem2 = unlist(strsplit(rem1, "<br />lat:"))[1]
                gp$x$data[[i]]$text[j] <- rem2
            }
        }
    }
}

return(gp)
})

}

shiny::shinyApp(ui = ui, server = server)