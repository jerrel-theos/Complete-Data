library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(plotly)
library(rworldmap)
library(RMariaDB)
library(DBI) 
library(markdown)


db <- dbConnect(MariaDB(),
                     user='any',
                     password='any',
                     dbname='any',
                     host='www.db4free.net')

complete <- dbGetQuery(db, 'SELECT* from complete')

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    title = "Complete Data",
                    # header
                    dashboardHeader(
                        title = "Complete",
                        tags$li(class = "dropdown",
                                style = "margin-top: 7px; margin-right: 5px;",
                                tags$img(src = "Matana.png", height = 45, length = 45)
                    )),
                    
                    # sidebar
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Table", tabName = "table", icon = icon("table")),
                            menuItem("Summary",tabName = "summary", icon = icon("clipboard-list")),
                            menuItem("Data & Code Download", icon = icon("download", lib='glyphicon'), href = "https://github.com/jerrel-theos/Complete-Data")
                        )
                    ),
                    
                    # body
                    dashboardBody(
                        
                        tabItems(
                            tabItem(tabName = "dashboard",
                                    
                                    frow1 <- fixedRow(
                                        valueBoxOutput("value1", width = 4),
                                        valueBoxOutput("value2", width = 4),
                                        valueBoxOutput("value3", width = 4),
                                    ),
                                    
                                    frow2 <- fluidRow(
                                                box(title = "Histogram",
                                                    status = "primary",
                                                    solidHeader = F,
                                                    plotlyOutput("histogram")),
                                                box(title = "Bar Plot",
                                                    status = "danger",
                                                    solidHeader = F,
                                                    plotlyOutput("bar_plot"))
                                            ),
                                    
                                    frow3 <-fluidRow(
                                                box(title = "Box Plot",
                                                    width = 12,
                                                    status = "success",
                                                    solidHeader = F,
                                                    plotlyOutput("box_plot")))
                                        ),
                                        tabItem(tabName = "table",
                                                fluidRow(
                                                    DT::dataTableOutput("data_table"))
                                        ),
                                        tabItem(tabName = "summary",
                                                fluidRow(
                                                    verbatimTextOutput("summary")
                                        )
                                    )
                            )
                        )
                        
                        
                    )


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    output$value1 <- renderValueBox({
        customer <- filter(complete, Type == "Customer")
        number_of_customer <- round(count(customer), 2)
        valueBox(
            number_of_customer,
            "Number of Customer",
            icon = icon("user-friends"),
            color = "red"
        )
    })
    
    output$value2 <- renderValueBox({
        max_quantity <- round(max(complete$Quantity), 2)
        valueBox(
            max_quantity,
            "Most ordered",
            icon = icon("cubes"),
            color = "blue"
        )
    })
    
    output$value3 <- renderValueBox({
        avg_quantity <- round(mean(complete$Quantity), 2)
        valueBox(
            avg_quantity,
            "Average of Quantity",
            icon = icon("balance-scale"),
            color = "green"
        )
    })
    
    output$histogram <- renderPlotly({
        ggplot(complete, aes(x=complete$Quantity)) +
            stat_bin(breaks=seq(0,150,6), fill="maroon", color="pink", alpha=0.9) +
            ggtitle("Density of Quantity Orders by Customers") +
            xlab("Quantity") +
            ylab("Density")
    })
    
    output$bar_plot <- renderPlotly({
        ggplot(complete, aes(x=complete$Country, y=complete$Quantity) ) +
            geom_bar(stat="identity", fill="orange") +
            ggtitle("Sales in Each Country") +
            coord_flip() +
            xlab("Country") +
            ylab("Quantity") +
            theme(
                panel.grid.minor.y = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.position="none")  
    })
    
    output$box_plot <- renderPlotly({
        Country <- complete$Country
        ggplot(complete, aes(x=complete$ProductName, y=complete$Quantity, fill=ProductName)) +
            geom_boxplot(color="darkgrey", alpha=0.8) +
            ggtitle("Number of Product Orders by Customers") +
            theme(plot.title = element_text(size=12)) +
            xlab('Product Name') +
            ylab('Quantity')    
    })
    
    output$data_table  <- DT::renderDataTable({DT::datatable(complete)
    })
    
    output$summary  <- renderPrint({summary(complete [,c(-1:-4,-7:-10,-14:-15)])
    })
    
})

# Run the application 
shinyApp(ui = ui, server = server)
