#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(RODBC)
library(dplyr)
library(lubridate)
library(data.table)

myconnection <- odbc::dbConnect(odbc::odbc(),
                                driver = "SnowflakeDSIIDriver",
                                server = "pearsonproduct.us-east-1.snowflakecomputing.com",
                                uid = Sys.getenv("uid"),
                                pwd = Sys.getenv("pwd")
)

agg_data <- odbc::dbGetQuery(conn = myconnection, statement = 
                           "select * from CPDA_INTERNAL.C_SANDBOX.PULSE_REPLACE_AGGS")

summary_data <- data.table(agg_data)

ui <- page_sidebar(
  title = "Pulse Replacement",
  sidebar = sidebar(
    title = "Data controls",
    selectInput("sel_prod", "Select Product", choices = list("Overall","MLM","eText","Channels"), selected = "Overall"),
    selectInput("sel_dg", "Select Date Grain", choices = list("Year","Semester","Month","Week","Day" ), selected = "Month"),
    uiOutput("date_ui")
  ),
  "Summary Metrics",
  layout_columns(
    value_box(title = "Registered Users", value = textOutput("userCountText")),
    value_box(title = "Session Count", value = textOutput("sess_cnt")),
    value_box(title = "Session per User", value = textOutput("sess_per_user")),
    value_box(title = "Avg Session Time", value = textOutput("avg_sess_dur"))
  ),
  "Date Picker Results",
  layout_columns(
    value_box(title = "Registered Users", value = textOutput("date_picker_user")),
    value_box(title = "Session Count", value = textOutput("date_picker_sess"))
  ),
  card(
    card_header("User Count Graph"),
    layout_sidebar(
      sidebar = sidebar("Choose Date Range",
                        dateInput("start_date", "Start Date", value = Sys.Date() - 30),
                        dateInput("end_date", "End Date", value = Sys.Date()),
                        ),
      plotOutput("g")
    )
  )
)


server <- function(input, output) {

  filtered_dates <- reactive({summary_data %>%
      filter(BIZ_LABEL == input$sel_prod & DATEGRAIN == input$sel_dg) %>%
      pull(DATE_ENDED) %>%
      as.character()})
  
  output$date_ui <- renderUI({
    req(filtered_dates())
    selectInput("filt_date_choice","Select Date", choices = filtered_dates(), selected = "2022-01-01")
    })
  
  op_data <- reactive({
      req(input$sel_prod, input$sel_dg, input$filt_date_choice)
      
      selected_date <- as.Date(input$filt_date_choice, format="%Y-%m-%d")
    
      cleaned_data <- summary_data %>%
      filter(BIZ_LABEL == input$sel_prod & DATEGRAIN == input$sel_dg & DATE_ENDED == input$filt_date_choice) %>%
      mutate(sess_per_user = SESS_CNT/USER_CNT, avg_sess_time = TOTAL_SESS_TIME/SESS_CNT) 
    
      list(user_count = cleaned_data$USER_CNT,
           sess_count = cleaned_data$SESS_CNT,
           avg_sess_per_user = cleaned_data$sess_per_user,
           avg_sess_time = cleaned_data$avg_sess_time)
  })
  
  
  output$userCountText <- renderText({
    metrics_data <- op_data()
      format(metrics_data$user_count, big.mark = ",")
  })
  
  output$sess_cnt <- renderText({    
    metrics_data <- op_data()
    format(metrics_data$sess_count, big.mark = ",")
  })
  
  output$sess_per_user <- renderText({
    metrics_data <- op_data()
    sprintf("%.2f", metrics_data$avg_sess_per_user)
  })
  
  output$avg_sess_dur <- renderText({
    metrics_data <- op_data()
    sprintf("%.2f", metrics_data$avg_sess_time)
  })
  
  output$g <- renderPlot({
    summary_data %>%
      filter(BIZ_LABEL == input$sel_prod & DATEGRAIN == input$sel_dg) %>%
      select(DATE_ENDED, USER_CNT) -> gdata
    
    ggplot(gdata) +
      geom_area(aes(x = DATE_ENDED, y = USER_CNT),  alpha = .8) +
      annotate("rect", xmin = input$start_date, xmax = input$end_date, ymin = 0, ymax = Inf, alpha = .3, fill = "red") +
      theme_minimal()
  })
  
  observe({
    req(input$start_date, input$end_date)
    
    query <- sprintf("
    select count(distinct person_id) as unique_user_cnt,
    count(distinct session_pseudo_id) as unique_sess_cnt
    from CPDA_INTERNAL.C_SANDBOX.PULSE_REPLACE_BASE
    where SESSION_START_DATE between '%s' and '%s'
    ",input$start_date, input$end_date)
    
    result <- odbc::dbGetQuery(conn = myconnection, statement = query)
    
    output$date_picker_user <- renderText({format(result[[1]], big.mark = ",")})
    output$date_picker_sess <- renderText({format(result[[2]], big.mark = ",")})
  })
  
}

shinyApp(ui, server)
 