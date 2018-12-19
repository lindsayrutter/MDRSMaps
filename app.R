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
fileInput('datafile', 'Choose CSV File',
accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),

"Test words here",

use_bs_tooltip(),

numericInput("zoom", "Zoom", 13, min = 3, max = 21, step = 1) %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = "Set the zoom parameter to an integer between 3 and 21 inclusive. Small values zoom out, large values zoom in. Note: If you zoom in too much, you might lose the overlaid points.", placement = "left")),

numericInput("alpha", "Alpha", 1, min = 0, max = 1, step = 0.1) %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = "Set the alpha parameter to values between 0 and 1 inclusive. The alpha parameter designates the transparency of overlaid points. With an alpha value of 1, the points are entirely opaque; with an alpha value of 0, the points are invisible (you will only see the line connecting now-invisible points).", placement = "left")),

radioButtons("line", "Line?", choices = c("No", "Yes")),

conditionalPanel(
    condition = "input.line == 'Yes'",
    textInput("lineColor", "Line color", value = "black")  %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = "A line will be drawn connecting the overlaid points in the order they appear in the read-in document. Choose the color for this line.", placement = "left"))
))),

column(width = 8,
shinydashboard::box(width = NULL, shinycssloaders::withSpinner(plotlyOutput("plotlyMap"), color = "#990000"), collapsible = FALSE, background = "black", title = "MDRS map with overlaid coordinates", status = "primary", solidHeader = TRUE)))),

shinydashboard::tabItem(tabName = "about",
shiny::fluidRow("Run by the Mars Society, the Mars Desert Research Station is a space analogue habitat in the deserts of Utah. Each year, crews of about seven members spend two weeks at the facility simulating a Mars mission. Part of this simulation invovles extravehicular activities (EVAs), where crew members done spacesuits and investigate the surroundings of the habitat.", style='padding:10px;'),
shiny::fluidRow("This application is intended for crew members to overlay information obtained during their EVAs onto maps. To use this application, you simply need to have GPS coordinates saved from your EVAs. You can then simply select a .CSV file from your local computer that contains latitude and longitude measurements. The application will then plot these latitude and longitude measurements onto a map surrounding the habitat.", style='padding:10px;'),

shiny::fluidRow("Specifically, the CSV file must contain at least three columns with headings called 'ID', 'Latitude', and 'Longitude'. An example of this format is shown in Figure 1. The ID column indicates the order in which you recorded latitude and longitude measurements during your EVA. Reading this .CSV file into the application will produce a map with these six measurements superimposed as shown in Figure 2.", style='padding:10px;'),

#Need internet connection

br(),
br(),
div(p('Figure 1'), style="text-align: center;"),
div(img(src='Figure1.png', style="width: 100%"), style="text-align: center;"),
br(),
br(),

div(p('Figure 2'), style="text-align: center;"),
div(img(src='Figure2.png', style="width: 70%"), style="text-align: center;"),
br(),
br()

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

p <- eventReactive({c(filedata(), input$zoom, input$alpha, input$lineColor)}, {

    df <-filedata()
    if (is.null(df)) return(NULL)
    
    df <- as.data.frame(df)
    df <- df[,1:ncol(df)]
    
    register_google(key = "AIzaSyCcJu4DttxEDccaixZomOCXcUhptYHX2n4")
    # Center on MDRS
    bbox <- ggmap::make_bbox(lon=Longitude, lat=Latitude, data=df, f = 0.05)
    mapLoc <- get_map(location = bbox, zoom = input$zoom, maptype = "satellite")
    
    if (ncol(df) == 2){
        df$size = 2
        df$color = as.factor("blue")
        
        p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_line(aes(x = Longitude, y = Latitude), data=df, color=input$lineColor) + geom_point(aes(x = Longitude, y = Latitude, size = size, color = color), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()
    }
    
    else if (ncol(df) == 3){
        classCol = class(df[,3])
        # If only color is defined
        if (classCol %in% c("factor", "character")){
            df$size = 2
            p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_line(aes(x = Longitude, y = Latitude), data=df, color=input$lineColor) + geom_point(aes_string(x = colnames(df)[2], y = colnames(df)[1], size = colnames(df)[4], color = colnames(df)[3]), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()
        }
        # If only size is defined
        else if (classCol %in% c("integer", "numeric")){
            df$color = "blue"
            p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_line(aes(x = Longitude, y = Latitude), data=df, color=input$lineColor) + geom_point(aes_string(x = colnames(df)[2], y = colnames(df)[1], size = colnames(df)[3], color = colnames(df)[4]), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()
        }    
    }
    
    else if (ncol(df) == 4){
        colNms = sapply(df[,3:4], class)
        colCol = which(colNms %in% c("factor", "character"))+2
        sizeCol = which(colNms %in% c("integer", "numeric"))+2
        
        p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_line(aes(x = Longitude, y = Latitude), data=df, color = input$lineColor) + geom_point(aes_string(x = colnames(df)[2], y = colnames(df)[1], size = colnames(df)[sizeCol], color = colnames(df)[colCol]), data = df, alpha = input$alpha) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()
    }

    return(p)
})

output$plotlyMap <- renderPlotly({
    gp <- ggplotly(p())
    return(gp)
})

}

shiny::shinyApp(ui = ui, server = server)
