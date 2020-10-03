#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(highcharter)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Dashboard FIFA 2019"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName="overview", icon = icon("dashboard")),
            menuItem("Club", tabName="club", icon = icon("futbol")),
            menuItem("Table Data", tabName="table", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName="overview", 
                fluidRow(
                    # A static infoBox
                    infoBox("Most Countries of Origin", "England", icon = icon("flag")),
                    infoBox("Most Expensive Club", "Real Madrid", icon = icon("futbol")),
                    infoBox("Most Position", "ST", icon = icon("user-plus"))
                ),
                fluidRow(
                  box(title = "Mean Value by Nationality", solidHeader = T, plotlyOutput("mean_nationality")),
                  box(title = "Total Value by Nationality", solidHeader = T, plotlyOutput("total_nationality"))
                ),
                fluidRow(
                  box(title = "Scatter Plot Wage and Overall", solidHeader = T, plotlyOutput("wage_overall")),
                  box(title = "Scatter Plot Age and Value", solidHeader = T, plotlyOutput("age_value"))
                )
            ),
            tabItem(
                tabName= "club",
                fluidRow(
                    box(solidHeader = T, 
                        selectInput("select_club", label="Select Club", choices = data_fifa$Club, selected = "Arsenal"),
                        sliderInput("count", label= "Number Object in Grapich", value = 5, min = 3, max = 15)),
                    box(title = "Club Info", solidHeader = T,
                        uiOutput("info_club"))
                ),
                fluidRow(
                    box(solidHeader = T,
                        highchartOutput("overall_club")),
                    box(solidHeader = T, 
                        highchartOutput("wage_club"))
                )
            ),
            tabItem(
                tabName= "table",
                h1("Table of Data"),
                dataTableOutput("table_data")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data_fifa <- read_csv("data/data_fifa_clean.csv")
    
    data_fifa$ID <- as.character(data_fifa$ID)
    data_fifa$`International Reputation` <- as.factor(data_fifa$`International Reputation`)
    data_fifa$`Skill Moves` <- as.factor(data_fifa$`Skill Moves`)
    data_fifa$Joined <- as.factor(data_fifa$Joined)
    
    data_fifa <- data_fifa %>% 
        replace_na(list(Club = 'No_club', `International Reputation` = "1",
                        `Skill Moves` = "2", Value = mean(data_fifa$Value, na.rm=T), 
                        Weight = mean(data_fifa$Weight, na.rm = T), `Preferred Foot` = 'Right'))
    
    # club data
    
    output$mean_nationality <- renderPlotly({
        mean_value_by_nationality <- data_fifa %>% 
            group_by(Nationality) %>% 
            summarise("Mean.Value.by.Country" = mean(Value, na.rm = T)) %>%
            arrange(desc(Mean.Value.by.Country)) 
        
        g <- ggplot(mean_value_by_nationality[1:10,], 
                    aes(x = reorder(Nationality, -Mean.Value.by.Country),
                        y = Mean.Value.by.Country, nationality = Nationality, mean = Mean.Value.by.Country)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            xlab("Nationality") + ylab("Mean Value")
        
        ggplotly(g,tooltip=c("nationality", "mean"))
        
    })
    
    output$total_nationality <- renderPlotly({
        total_value_by_nationality <- data_fifa %>% 
            group_by(Nationality) %>% 
            summarise("Total Value by Country" = sum(Value)) %>%
            arrange(desc(`Total Value by Country`))
        
        g <- ggplot(total_value_by_nationality[1:10,], 
                    aes(x = reorder(Nationality, -`Total Value by Country`),
                        y = `Total Value by Country`, nationality = Nationality, Total = `Total Value by Country`)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            xlab("Nationality") + ylab("Total Value")
        
        ggplotly(g,tooltip=c("nationality", "Total"))
        
    })
    
    output$age_value <- renderPlotly({
        ggplotly(ggplot(data_fifa, aes(x=Age, y=Value)) + 
                     geom_point(color = "steelblue"))
    })
    
    output$wage_overall <- renderPlotly({
        ggplotly(ggplot(data_fifa, aes(x=Wage, y=Overall)) + 
                     geom_point(color = "steelblue"))
        
    })
    
    output$info_club <- renderUI({
        club_name <- input$select_club
        club_data <- data_fifa %>% filter(Club == club_name)
        club_overall <- mean(club_data$Overall)
        club_overall <- round(club_overall, 2)
        club_value <- sum(club_data$Value)
        club_players <- dim(club_data)
        club_players <- club_players[1]
        club_wage <- mean(club_data$Wage)
        club_wage <- round(club_wage,2)
        club_age <- median(club_data$Age)
        #####
        str_name <- paste0("Club Name:", club_name)
        str_overall <- paste0(" Overall: ", club_overall)
        str_value <- paste("Club Value:", club_value, "€")
        str_players <- paste("Number of Players:", club_players)
        str_mage <- paste("Mean Wage:", club_wage, "€")
        str_age <- paste("Median Age:", club_age)
        HTML(paste(str_name, str_overall, str_value, str_players, str_mage, str_age, sep = "<br/>"))
    })
   
    output$overall_club <- renderHighchart({
        club_data <- data_fifa %>% filter(Club == input$select_club)
        overall <- club_data[, c("Name", "Overall")] %>% arrange(desc(Overall))
        overall[1:input$count,] %>% 
            hchart(type = "bar", hcaes(x = Name, y = Overall), name ="Overall") %>%
            hc_title(text = paste0("Top ", input$count, " Overall in ", input$select_club)) %>%
            hc_subtitle(text = "Source: kaggle.com")
    })
    
    output$wage_club <- renderHighchart({
        club_data <- data_fifa %>% filter(Club == input$select_club)
        wage <- club_data[, c("Name", "Wage")] %>% arrange(desc(Wage))
        wage[1:input$count,] %>% 
            hchart(type = "bar", hcaes(x = Name, y = Wage), name ="Wage") %>%
            hc_title(text = paste0("Top ",input$count, " Expensive Wage in ", input$club_name)) %>%
            hc_subtitle(text = "Source: kaggle.com")
    })
    
    output$table_data<- DT::renderDataTable({
        datatable(data_fifa, filter = 'top', options = list(
            pageLength = 10, autoWidth = F))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
