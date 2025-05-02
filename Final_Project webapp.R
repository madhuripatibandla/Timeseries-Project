library(shiny)
library(shinydashboard) 
library(ggplot2)
library(fpp3)
library(readxl)
library(dplyr)

# Load and clean data at startup
df1 <- read_excel("D:/online_retail_II.xlsx", sheet = 1)
df2 <- read_excel("D:/online_retail_II.xlsx", sheet = 2)

  retail_df <- bind_rows(df1, df2) %>%
  # rename the column to CustomerID
  rename(CustomerID = `Customer ID`) %>%  
  mutate(
    CustomerID = if_else(is.na(CustomerID), "Unknown", as.character(CustomerID)),
    Total_Price = Quantity * Price
  ) %>%
  distinct()

df_UK <- retail_df %>%
  filter(Country == "United Kingdom") %>%
  mutate(YearMonth = yearmonth(InvoiceDate)) %>%
  group_by(YearMonth) %>%
  summarise(Total_Sales = sum(Total_Price, na.rm = TRUE)) %>%
  as_tsibble(index = YearMonth)

# Define UI as a shinydashboard page

ui <- dashboardPage(
  dashboardHeader(title = "UK Retail Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Trend", tabName = "sales", icon = icon("area-chart")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line"),
               selectInput("year", "Select Year:", c(2009, 2010, 2011)),
               selectInput("month", "Select Month:", month.name),
               selectInput("model_type", "Model:", c("ETS", "ARIMA", "NNAR")),
               selectInput("description", "Product Description:",
                           choices = NULL,  
                           selected = NULL),
               actionButton("go", "Run Forecast"),
               actionButton("reset", "Clear")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sales",
              box(title = "Monthly Sales", width = 12, plotOutput("salesPlot"))),
      tabItem(tabName = "forecast",
              box(title = "Forecast Output", width = 12, plotOutput("forecastPlot")))
    )
  )
)

  
# Define Server logic
server <- function(input, output) {
  # Fit ARIMA once
  arima_model <- df_UK %>% model(ARIMA(Total_Sales))
  
  # Render the historical sales plot
  output$salesPlot <- renderPlot({
    autoplot(df_UK) +
      labs(x = "Year‑Month", y = "Total Sales",
           title = "UK Monthly Sales (2009–2011)") +
      theme_minimal()
  })
  
  # Render the forecast plot
  output$forecastPlot <- renderPlot({
    fc <- forecast(arima_model, h = 12)
    autoplot(fc, df_UK) +
      labs(x = "Year‑Month", y = "Total Sales",
           title = "ARIMA Forecast (Next 12 Months)") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)