#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
remotes::install_github("deepanshu88/shinyDarkmode")
library(shiny)
library(shinydashboard)
library(shinyDarkmode)
library(shinyjs)
library(shinythemes)   
library(ggplot2)
library(dplyr)
library(gridExtra)
library(shinythemes)
library(shinyBS)

df <- read.csv("D:/Semester7/Information Visualization/project/file.csv")
# Drop unnecessary columns 
df <- subset(df, select = -c(X,CustomerID,Transaction_ID,Product_SKU,
                             Product_Description,Date,Coupon_Code))
# Change data type of Transaction_Date column
df$Transaction_Date <- as.Date(df$Transaction_Date, format = "%Y-%m-%d")
# Nulls
df <- na.omit(df)
# Duplicates
df <- df[!duplicated(df),]
# Encoding
column_types <- sapply(df, class)
print(column_types)

# Statistics
print(summary(df))

# Create new attribute for seasons
df <- df %>%
  mutate(Season = case_when(
    Month %in% c(12,1,2) ~ "Winter",
    Month %in% c(3,4,5) ~ "Spring",
    Month %in% c(6,7,8) ~ "Summer",
    Month %in% c(9,10,11) ~ "Fall"
  ))

# Create new attribute for total_sales
df <- df %>% mutate(Total_sales = df$Avg_Price + df$Delivery_Charges + df$GST)

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  use_darkmode(),
  
  
  bsTooltip("selecting_chart", "Choose Between Bar Or Pie Charts", placement = "left", trigger = "hover",
            options = NULL),
  
  bsTooltip("show_per", "Show Percentage on Pie Chart Note: Choose Only When CheckBox", placement = "bottom", trigger = "hover",
            options = NULL),
  bsTooltip("show_count", "Show count on Pie Chart Note: Choose Only When CheckBox", placement = "bottom", trigger = "hover",
            options = NULL),

  dashboardPage(
    dashboardHeader(title = "Online Shopping Data Analysis"),
    
    dashboardSidebar(
      useShinyjs(),
      selectInput(
        inputId = "mode",
        label = "choose the mode:",
        choices = c("light", "dark"),
        selected = "light"),
      checkboxInput("togglemode", "Dark Mode", value = FALSE)
      
    ),
    
    dashboardBody(
      fluidRow(
        box(class = "dark-fluidrow",width = 16, title = "Discovering Categorical Attributes",collapsible = TRUE,
            sidebarLayout(
              sidebarPanel(
                id="sidebar",
                selectInput("selecting_cat",
                            "Choose An Attribute To Visualize",
                            choices = c('Gender','Location','Product_Category','Coupon_Status')),
                radioButtons("selecting_chart",
                             "Choose Chart Type", 
                             choices = c("Pie Chart", "Bar Chart"), 
                             selected = "Pie Chart"),
                checkboxInput("show_per", "Show Percentage", value = FALSE),
                checkboxInput("show_count", "Show Count", value = FALSE)
              ),
              
              mainPanel(
                plotOutput("pieChart")
              )
            )
        )
      ),
      
      fluidRow(
        box(class = "dark-fluidrow",width = 16,title = "Study Disribution Using Histograms",collapsible = TRUE,
            sidebarLayout(
              sidebarPanel(
                id="sidebar", 
                selectInput(inputId = "Bins1",
                            label = "Select Number of bins For Tenure_Months Attribute",
                            choices = c(10,20,30,40,50),
                            selected = 20),
                selectInput(inputId = "Bins2",
                            label = "Select Number of bins For Quantity Attribute",
                            choices = c(5,12,15,20,25),
                            selected = 20)
              ),
              
              mainPanel(
                plotOutput("Histogram")
              )
            )
        )
      ),
      
      fluidRow(
        box(class = "dark-fluidrow",width = 16,title = "Study Disribution Using Density Plot",collapsible = TRUE,
            sidebarLayout(
              sidebarPanel(
                tags$div(title="Slide to adjust avreage Price",
                         sliderInput("price_BandWidth",
                                     "Bandwidth adjustment for Avg_price attribute",
                                     min = 1,
                                     max = 5,
                                     value = 1,
                                     step = 1)),
                
                tags$div(title="Slide to adjust delivery charges",
                         sliderInput("delivery_BandWidth",
                                     "Bandwidth adjustment for delivery_charges attribute",
                                     min = 50,
                                     max = 100,
                                     value = 60,
                                     step = 10))
              ),
              
              mainPanel(
                plotOutput("Density")
              )
            )
        )
      ),
      
      fluidRow(
        box(class = "dark-fluidrow",width = 16,title = "Sales Analysis",collapsible = TRUE,
            sidebarLayout(
              sidebarPanel(
                id="sidebar", 
                radioButtons("selecting_time_period",
                             "Choose Type Of Analysis", 
                             choices = c("Daily", "Monthly", "Seasonally (cumulative)"), 
                             selected = "Daily")
              ),
              
              mainPanel(
                plotOutput("Bar")
              )
            )
        )
      ),
      
      fluidRow(
        box(class = "dark-fluidrow",width = 16,title = "Sales Distribution Filterd By Location",collapsible = TRUE,
            sidebarLayout(
              sidebarPanel(
                id="sidebar",
                radioButtons("selecting_location",
                             "Choose Location To Filter Sales", 
                             choices = c("Chicago", "California", "New York", "New Jersey", "Washington DC"), 
                             selected = "Chicago")
              ),
              
              mainPanel(
                plotOutput("Sales_vs_Location")
              )
            )
        )
      ),
      
      fluidRow(
        box(class = "dark-fluidrow",width = 16,title = "Online Spend VS. Offline Spend",collapsible = TRUE,
            titlePanel(""),
            mainPanel(
              plotOutput("compare")
            )
        )
      ),
      
      fluidRow(
        box(class = "dark-fluidrow",width = 16,title = "Which Categories Are Prefered By Males And Females",collapsible = TRUE,
            sidebarLayout(
              sidebarPanel(
                id="sidebar",
                radioButtons("gender", "Filter Categories By Gender:",
                             choices = c("Both", "Male", "Female"),
                             selected = "Both")
              ),
              
              mainPanel(
                plotOutput("cat_vs_gender")
              )
            )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  darkmode_toggle(inputid = 'togglemode')
  
  addTooltip(session, id = "someInput", title = "This is an input.",
             placement = "left", trigger = "hover")
    output$pieChart <- renderPlot({
        X <- input$selecting_cat
        to_vis <- df %>% group_by_at(vars(X)) %>% summarise(count=n())
        if(input$selecting_chart == "Pie Chart"){
          Plot <- ggplot(to_vis, aes(x = "", y = count, fill = !!sym(X))) +
            geom_bar(stat = "identity") +
            coord_polar(theta = "y") +
            labs(title = "Pie Chart for Categories",
                 fill = names(X))
          if(input$show_per){
            updateCheckboxInput(session, "show_count", value = FALSE)
            Plot <- Plot + geom_text(aes(label = paste(round((count/45746)*100),"%")),
                                     position = position_stack(vjust = 0.5))
          }
          else if(input$show_count){
            updateCheckboxInput(session, "show_per", value = FALSE)
            Plot <- Plot + geom_text(aes(label = count),
                                     position = position_stack(vjust = 0.5))
          }
          print(Plot)
        } 
        else{
          ggplot(to_vis, aes(x = reorder(!!sym(X), -count), y = count, fill = !!sym(X))) +
            geom_bar(stat = "identity") +
            labs(title = paste("Bar Chart for Categories:"),
                 x = "Categories",
                 y = "Count")
        }
    })
    output$Histogram <- renderPlot({
      plot1 <- ggplot(df, aes(x = df$Tenure_Months)) +
        geom_histogram(bins = as.numeric(input$Bins1), fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram for Tenure_Months Distribution", x = "Tenure_Months", y = "Frequency")
      plot2 <- ggplot(df, aes(x = df$Quantity)) +
        geom_histogram(bins = as.numeric(input$Bins2), fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram for Quantity Distribution", x = "Quantity", y = "Frequency")
      grid.arrange(plot1, plot2, ncol = 2)
      

    })
    output$Density <- renderPlot({
      plot1 <- ggplot(df, aes(x = df$Avg_Price)) +
        geom_density(adjust = input$price_BandWidth, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Density Plot of Average Price", x = "Average Price", y = "Density")
      plot2 <- ggplot(df, aes(x = df$Delivery_Charges)) +
        geom_density(adjust = input$delivery_BandWidth, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Density Plot of Delivery Charges", x = "Delivery Charges", y = "Density")
      grid.arrange(plot1, plot2, ncol = 2)
    })
    output$Bar <- renderPlot({
      if(input$selecting_time_period == "Daily"){
        ggplot(df, aes(x = Transaction_Date, y = Total_sales, fill = Transaction_Date)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Analysing Daily Sales"),
               x = "Date",
               y = "Sales")
      }
      else if(input$selecting_time_period == "Monthly"){
        ggplot(df, aes(x = Month, y = Total_sales, fill = Month)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Analysing Monthly Sales"),
               x = "Month",
               y = "Sales")
      }
      else{
        ggplot(df, aes(x = Season, y = Total_sales, fill = Season)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Analysing Seasonally Sales"),
               x = "Season",
               y = "Sales")
      }
    })
    output$Sales_vs_Location <- renderPlot({
      selected_location <- input$selecting_location 
      filter_data <- subset(df, Location == selected_location)
      ggplot(filter_data, aes(x = Total_sales, fill = Location)) +
        geom_density(alpha = 0.7) +
        labs(title = paste("Sales Density Plot for Location", selected_location),
             x = "Sales", y = "Density")
    })
    output$compare <- renderPlot({
      p1 <- ggplot(df, aes(x = df$Online_Spend)) +
        geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram for Online_spend Distribution", x = "Online_spend", y = "Frequency")
      
      p2 <- ggplot(df, aes(x = df$Offline_Spend)) +
        geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram for Offline_Spend Distribution", x = "Offline_Spend", y = "Frequency")
      grid.arrange(p1, p2, ncol = 2)
    })
    output$cat_vs_gender <- renderPlot({
      gender_filter <- switch(input$gender,
                              "Both" = TRUE,
                              "Male" = df$Gender == "M",
                              "Female" = df$Gender == "F")
      
      filtered_data <- df[gender_filter, ]
      
      ggplot(filtered_data, aes(x = Product_Category, fill = Product_Category)) +
        geom_bar() +
        labs(title = paste("Product Category Count Plot for", input$gender),
             x = "Product Category", y = "Count")+
        coord_cartesian(ylim = c(0, 45746/2))
    })
    observe({
      addTooltip(
        session = session,
        id = "Histogram",
        title = "Tenure_Months graph represent Number of months the customer has been associated with the platform, 
                  Quantity Graph represent Quantity of the product purchased in the transaction; 
                  Observation: most users have more than 500 month association with the platform, one porduct is purshaced in bulk",
        placement = "bottom",
        trigger = "hover"
      )
    })

    observe({
      addTooltip(
        session = session,
        id = "Density",
        title = "Average price plot shows the avearage price of a product;
                 Delivery Charges plot shows the charges associated with the delivery of the product;
                 Observation: Avg price plot shows skeweness to the right which means that the mean is greater than the median,
                 The Delivery charge plot show that most of the charges are between 0 & 50",
        placement = "bottom",
        trigger = "hover"
      )
    })
    observe({
      addTooltip(
        session = session,
        id = "Bar",
        title = "Visualizations to show the amount of Sales; 
                 Observation: Sales seems to increase gradually especially on months that have celebration like december, 
                 Fall & winter have the highest sales",
        placement = "bottom",
        trigger = "hover"
      )
    })
    observe({
      addTooltip(
        session = session,
        id = "compare",
        title = "2 Graphs Comparing the amount spend by offline & online spenders;
                 Observation: Offline Spenders Seem to spend more money per purchase than Online Spenders",
        placement = "bottom",
        trigger = "hover"
      )
    })
    observe({
      addTooltip(
        session = session,
        id = "cat_vs_gender",
        title = "Graph to show products in each category.
                Observation:Both female and male users prefer Apparel & Nest-USA categories, Female Users have higher products overall",
        placement = "bottom",
        trigger = "hover"
      )
    })
    observe({
      if (input$mode=="dark") {
        shinyjs::runjs('
      document.body.style.backgroundColor = "#343a40";
      document.body.style.color = "#ffffff";
      $(".skin-blue").css("background-color", "#343a40"); 
      $(".navbar").css("background-color", "#343a40");  // Change navbar color if needed
      $(".dark-fluidrow").css("background-color", "#343a40");  // Change fluidRow background color
      $(".dark-fluidrow .box-header").css("background-color", "#343a40");  // Change box header background color
      $(".dark-fluidrow .box-header .box-title").css("color", "#ffffff");  // Change label text color
      $(".dark-fluidrow #sidebar").css("background-color", "#343a40");  // Change sidebar background color

    ')
        
      } else {
        shinyjs::runjs('
      document.body.style.backgroundColor = "#ffffff";
      document.body.style.color = "#000";
      $(".skin-blue").css("background-color", "#fff"); 
      $(".navbar").css("background-color", "#fff");  // Change navbar color if needed
      $(".dark-fluidrow").css("background-color", "#fff");  // Change fluidRow background color
      $(".dark-fluidrow .box-header").css("background-color", "#fff");  // Change box header background color
      $(".dark-fluidrow .box-header .box-title").css("color", "#000");  // Change label text color
      $(".dark-fluidrow #sidebar").css("background-color", "#D3D3D3");  // Change sidebar background color

    ')
        
      }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)