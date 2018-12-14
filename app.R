library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinycssloaders)
library(ggmap)
library(ggplot2)
library(plotly)

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
accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))),

column(width = 4,
shinydashboard::box(width = NULL, status = "primary", title = "Latitude", solidHeader = TRUE,
uiOutput("outMinLat"), uiOutput("outMedLat"), uiOutput("outMaxLat"))),

column(width = 4,
shinydashboard::box(width = NULL, status = "primary", title = "Latitude", solidHeader = TRUE,
uiOutput("outMinLon"), uiOutput("outMedLon"), uiOutput("outMaxLon")))),

fluidRow(
column(width = 12,
shinydashboard::box(width = NULL, shinycssloaders::withSpinner(plotlyOutput("plotlyMap"), color = "#990000"), collapsible = FALSE, background = "black", title = "MDRS map with overlaid coordinates", status = "primary", solidHeader = TRUE)))),

shinydashboard::tabItem(tabName = "about",
shiny::fluidRow("Run by the Mars Society, the Mars Desert Research Station is a space analogue habitat in the deserts of Utah. Each year, crews of about seven members spend two weeks at the facility simulating a Mars mission. Part of this simulation invovles extravehicular activities (EVAs), where crew members done spacesuits and investigate the surroundings of the habitat.", style='padding:10px;'),
shiny::fluidRow("This application is intended for crew members to overlay information obtained during their EVAs onto maps. To use this application, you simply need to have GPS coordinates saved from your EVAs. You can then simply select a .CSV file from your local computer that contains latitude and longitude measurements. The application will then plot these latitude and longitude measurements onto a map surrounding the habitat.", style='padding:10px;'),

shiny::fluidRow("Specifically, the CSV file must contain at least three columns with headings called 'ID', 'Latitude', and 'Longitude'. An example of this format is shown in Figure 1. The ID column indicates the order in which you recorded latitude and longitude measurements during your EVA. Reading this .CSV file into the application will produce a map with these six measurements superimposed as shown in Figure 2.", style='padding:10px;'),

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

filedata <- eventReactive(input$datafile, {
    infile <- input$datafile
    validate(need(!is.null(infile), "Select a file."))
    read.csv(infile$datapath)
}, ignoreNULL = FALSE)

df <- eventReactive(filedata(), {
    validate(need(!is.null(df), "Select a file."))
    df <-filedata()
    df <- as.data.frame(df)
    df <- df[,1:ncol(df)]
    df <- setNames(df, c("ID", "Latitude", "Longitude"))
    df
}, ignoreNULL = FALSE)


output$outMinLat <- renderUI({
    minLat = min(df()$Latitude)
    numericInput("minLat", "Update Minimum", value = minLat)
})

output$outMedLat <- renderUI({
    medLat = mean(df()$Latitude)
    numericInput("medLat", "Update Center", value = medLat)
})

output$outMaxLat <- renderUI({
    maxLat = max(df()$Latitude)
    numericInput("maxLat", "Update Maximum", value = maxLat)
})

output$outMinLon <- renderUI({
    minLon = min(df()$Longitude)
    numericInput("minLon", "Update Minimum", value = minLon)
})

output$outMedLon <- renderUI({
    medLon = mean(df()$Longitude)
    numericInput("medLon", "Update Center", value = medLon)
})

output$outMaxLon <- renderUI({
    maxLon = max(df()$Longitude)
    numericInput("maxLon", "Update Maximum", value = maxLon)
})



output$plotlyMap <- renderPlotly({

register_google(key = "AIzaSyCcJu4DttxEDccaixZomOCXcUhptYHX2n4")
# Center on MDRS
island = get_map(location = c(lon = -110.7919, lat = 38.4065), zoom = 13, maptype = "satellite")

p <- ggmap(island, extent = "panel", legend = "bottomright") +
    geom_point(aes(x = Longitude, y = Latitude), data = df(), size = 4, color = "#ff0000") + xlab("Longitude") + ylab("Latitude")
#+ scale_x_continuous(limits = c(minLon, maxLon), expand = c(0, 0)) +
#scale_y_continuous(limits = c(minLat, maxLat), expand = c(0, 0))
gp <- ggplotly(p)

return(gp)

})}

shiny::shinyApp(ui = ui, server = server)