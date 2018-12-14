library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinycssloaders)



options(spinner.color.background="#F5F5F5")


ui=pageWithSidebar(
headerPanel('Simple matrixInput'),
sidebarPanel(
fileInput('datafile', 'Choose CSV File',
accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),

mainPanel(plotOutput(outputId = 'plotMap')))

server=function(input, output){

#This function is repsonsible for loading in the selected file
filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
    }
    read.csv(infile$datapath)
})
    

output$plotMap <- renderPlot({

df <-filedata()
if (is.null(df)) return(NULL)

df <- as.data.frame(df)
df <- df[,1:ncol(df)]
df <- setNames(df, c("ID", "Latitude", "Longitude"))

register_google(key = "AIzaSyCcJu4DttxEDccaixZomOCXcUhptYHX2n4")
# Center on MDRS
island = get_map(location = c(lon = -110.7919, lat = 38.4065), zoom = 13, maptype = "satellite")

p <- ggmap(island, extent = "panel", legend = "bottomright") +
    geom_point(aes(x = Longitude, y = Latitude), data = df, size = 4, color = "#ff0000")
#+ scale_x_continuous(limits = c(minLon, maxLon), expand = c(0, 0)) +
#scale_y_continuous(limits = c(minLat, maxLat), expand = c(0, 0))

return(p)

})}

shinyApp(ui, server)