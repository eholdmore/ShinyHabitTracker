#### Habit Tracking App ####

library(shiny)
library(dplyr)
library(ggplot2)
library(googledrive)
library(googlesheets4)
library(gargle)
library(lubridate)
library(pander)
panderOptions("digits", 2)
library(anytime)
library(here)

# Google sheets authentification -----------------------------------------------

ui <- navbarPage(
  "Application",
  tabPanel("General",
           sidebarLayout(
             sidebarPanel(
               uiOutput("date"),
               uiOutput("exercise"),
               uiOutput("teeth_am"),
               uiOutput("teeth_pm"),
               uiOutput("read"),
               actionButton('submit','Submit'),
               uiOutput("submit_notice")
               
             ),
             mainPanel(
               h1("Habit Summary"),
               p("Here's how your habits have stacked up over time."),
               br(),
               tabsetPanel(type = "pills",
                           tabPanel("Exercise",
                                    h2("Movement & Exercise"),
                                    h3("Week"),
                                    br(),
                                    tableOutput("week_mvmt_tbl"),
                                    #plotOutput("week_mvmt_plot"),
                                    h3("Month"),
                                    br(),
                                    tableOutput("mon_mvmt_tbl"),
                                    #plotOutput("mon_mvmt_plot"),
                                    h3("Year"),
                                    br(),
                                    tableOutput("yr_mvmt_tbl")),
                                    #plotOutput("yr_mvmt_plot")),
                           tabPanel("Reading",
                                    h2("Read Every Day"),
                                    h3("Week"),
                                    br(),
                                    tableOutput("week_read_tbl"),
                                    #plotOutput("week_read_plot"),
                                    h3("Month"),
                                    br(),
                                    tableOutput("mon_read_tbl"),
                                    #plotOutput("mon_read_plot"),
                                    h3("Year"),
                                    br(),
                                    tableOutput("yr_read_tbl")),
                                    #plotOutput("yr_read_plot")),
                           tabPanel("Teeth",
                                    h2("Brush & Floss Your Teeth"),
                                    h3("Week"),
                                    br(),
                                    tableOutput("week_teeth_tbl"),
                                    #plotOutput("week_teeth_plot"),
                                    br(),
                                    h3("Month"),
                                    br(),
                                    tableOutput("mon_teeth_tbl"),
                                    #plotOutput("mon_teeth_plot"),
                                    br(),
                                    h3("Year"),
                                    br(),
                                    tableOutput("yr_teeth_tbl"))),
                                    #plotOutput("yr_teeth_plot"))),
             )
           )
  ))

server <- function(input, output,session) {
  
  output$date<-renderUI({
    dateInput("date", NULL)
  })
  
  output$exercise<-renderUI({
    numericInput("exercise", "How many minutes did you exercise?",value=0,min=0,max=200)
  })
  
  output$teeth_am<-renderUI({
    radioButtons("teeth_am","Did you brush your teeth this morning?", c("Yes","No"))
  })
  
  output$teeth_pm<-renderUI({
    radioButtons("teeth_pm","Did you brush your teeth tonight?", c("Yes","No"))
  })
  
  output$read<-renderUI({
    radioButtons("read","Did you read today?", c("Yes","No"))
  })
  
  Values <- reactive({
    as.data.frame(c(as.data.frame(input$date),
    as.data.frame(weekdays(input$date)),
    as.data.frame(input$teeth_am),
    as.data.frame(input$teeth_pm),
    as.data.frame(input$read),
    as.data.frame(input$exercise)))
  })
  
  observeEvent(input$submit, {
    
    # Access Google Sheet to collect data
    gs4_auth(path = "examplespreadsheetconnector-5e4cb75973ee.json",
             subject = "google-sheets-app-example@examplespreadsheetconnector.iam.gserviceaccount.com")
    data_sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1Kmcwul1zKqBewlrP9CGdRyQHcM7slxzfUMZcPzR3aRk/edit?usp=sharing")
    
    # Append new inputs to Google Sheet
    sheet_append(data_sheet, data = Values(), sheet = "2024")

    output$submit_notice<-renderUI({
      renderText("Habits submitted - great job!")
    })
  })
  
  # Clean data for easier manipulation
  # TO DO: Remove days with missing data
  gs4_auth(path = "examplespreadsheetconnector-5e4cb75973ee.json",
           subject = "google-sheets-app-example@examplespreadsheetconnector.iam.gserviceaccount.com")
  data_2024 <- range_read("https://docs.google.com/spreadsheets/d/1Kmcwul1zKqBewlrP9CGdRyQHcM7slxzfUMZcPzR3aRk/edit?usp=sharing")
  
  # Make a few reusable dataframes for different timeframes
  current_day <- Sys.Date()
  #current_day <- date(anydate("2024-03-27"))   #this is for testing
  
  df_ytd <- data_2024 %>% filter(Day < current_day)
  df_month <- data_2024 %>% filter(Day >= current_day %m-% months(1),
                                   Day <  current_day)
  df_week  <- data_2024 %>% filter(Day >= current_day %m-% weeks(1),
                                   Day <  current_day)
  
  exer_year_cnt <- df_ytd %>% count(Exercise = Exercise != 0) %>% rename(Days = n)
  exer_mnth_cnt <- df_month %>% count(Exercise = Exercise != 0) %>% rename(Days = n)
  exer_week_cnt <- df_week %>% count(Exercise = Exercise != 0) %>% rename(Days = n)
  
  ## Output Tables
  
  # We want to combine AM and PM brush teeth into one table
  #week
  tbl_week <- sapply(df_week,table)
  teetham_week_counts <- as.data.frame(tbl_week$Teeth_Morning)%>% rename(Response = Var1, Teeth_AM = Freq)
  check_levels <- levels(teetham_week_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    teetham_week_counts$Response <- as.character(teetham_week_counts$Response)
    teetham_week_counts <- rbind(c("No",0),teetham_week_counts)
  }
  teetham_week_counts$Response <- as.factor(teetham_week_counts$Response)
  check_levels <- levels(teetham_week_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    teetham_week_counts$Response <- as.character(teetham_week_counts$Response)
    teetham_week_counts <- rbind(teetham_week_counts,c("Yes",0))
  }
  
  teethpm_week_counts <- as.data.frame(tbl_week$Teeth_Evening)%>% rename(Response = Var1, Teeth_PM = Freq)
  check_levels <- levels(teethpm_week_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    teethpm_week_counts$Response <- as.character(teethpm_week_counts$Response)
    teethpm_week_counts <- rbind(c("No",0),teethpm_week_counts)
  }
  teethpm_week_counts$Response <- as.factor(teethpm_week_counts$Response)
  check_levels <- levels(teethpm_week_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    teethpm_week_counts$Response <- as.character(teethpm_week_counts$Response)
    teethpm_week_counts <- rbind(teethpm_week_counts,c("Yes",0))
  }
  
  teeth_week_counts <- full_join(teetham_week_counts,teethpm_week_counts,by="Response")
  colnames(teeth_week_counts) <- c("","Brush Morning", "Brush Evening")
  output$week_teeth_tbl <- renderTable(teeth_week_counts)
  
  #month
  tbl_month <- sapply(df_month,table)
  teetham_month_counts <- as.data.frame(tbl_month$Teeth_Morning)%>% rename(Response = Var1, Teeth_AM = Freq)
  check_levels <- levels(teetham_month_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    teetham_month_counts$Response <- as.character(teetham_month_counts$Response)
    teetham_month_counts <- rbind(c("No",0),teetham_month_counts)
  }
  teetham_month_counts$Response <- as.factor(teetham_month_counts$Response)
  check_levels <- levels(teetham_month_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    teetham_month_counts$Response <- as.character(teetham_month_counts$Response)
    teetham_month_counts <- rbind(teetham_month_counts,c("Yes",0))
  }
  
  teethpm_month_counts <- as.data.frame(tbl_month$Teeth_Evening)%>% rename(Response = Var1, Teeth_PM = Freq)
  check_levels <- levels(teethpm_month_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    teethpm_month_counts$Response <- as.character(teethpm_month_counts$Response)
    teethpm_month_counts <- rbind(c("No",0),teethpm_month_counts)
  }
  teethpm_month_counts$Response <- as.factor(teethpm_month_counts$Response)
  check_levels <- levels(teethpm_month_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    teethpm_month_counts$Response <- as.character(teethpm_month_counts$Response)
    teethpm_month_counts <- rbind(teethpm_month_counts,c("Yes",0))
  }
  
  teeth_month_counts <- full_join(teetham_month_counts,teethpm_month_counts,by="Response")
  colnames(teeth_month_counts) <- c("","Brush Morning", "Brush Evening")
  output$mon_teeth_tbl <- renderTable(teeth_month_counts)
  
  #year
  tbl_year <- sapply(df_ytd,table)
  
  teetham_year_counts <- as.data.frame(tbl_year$Teeth_Morning)%>% rename(Response = Var1, Teeth_AM = Freq)
  check_levels <- levels(teetham_year_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    teetham_year_counts$Response <- as.character(teetham_year_counts$Response)
    teetham_year_counts <- rbind(c("No",0),teetham_year_counts)
  }
  teetham_year_counts$Response <- as.factor(teetham_year_counts$Response)
  check_levels <- levels(teetham_year_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    teetham_year_counts$Response <- as.character(teetham_year_counts$Response)
    teetham_year_counts <- rbind(teetham_year_counts,c("Yes",0))
  }
  
  teethpm_year_counts <- as.data.frame(tbl_year$Teeth_Evening)%>% rename(Response = Var1, Teeth_PM = Freq)
  check_levels <- levels(teethpm_year_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    teethpm_year_counts$Response <- as.character(teethpm_year_counts$Response)
    teethpm_year_counts <- rbind(c("No",0),teethpm_year_counts)
  }
  teethpm_year_counts$Response <- as.factor(teethpm_year_counts$Response)
  check_levels <- levels(teethpm_year_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    teethpm_year_counts$Response <- as.character(teethpm_year_counts$Response)
    teethpm_year_counts <- rbind(teethpm_year_counts,c("Yes",0))
  }
  
  teeth_year_counts <- full_join(teetham_year_counts,teethpm_year_counts,by="Response")
  colnames(teeth_year_counts) <- c("","Brush Morning", "Brush Evening")
  output$yr_teeth_tbl <- renderTable(teeth_year_counts)
  
  # Exercise Tables
  output$week_mvmt_tbl <- renderTable(exer_week_cnt)
  output$mon_mvmt_tbl <- renderTable(exer_mnth_cnt)
  output$yr_mvmt_tbl <- renderTable(exer_year_cnt)
  
  # Reading Tables
  #week
  tbl_week <- sapply(df_week,table)
  read_week_counts <- as.data.frame(tbl_week$Read)%>% rename(Response = Var1, Read = Freq)
  check_levels <- levels(read_week_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    read_week_counts$Response <- as.character(read_week_counts$Response)
    read_week_counts <- rbind(c("No",0),read_week_counts)
  }
  read_week_counts$Response <- as.factor(read_week_counts$Response)
  check_levels <- levels(read_week_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    read_week_counts$Response <- as.character(read_week_counts$Response)
    read_week_counts <- rbind(read_week_counts,c("Yes",0))
  }
  
  colnames(read_week_counts) <- c("","Read")
  output$week_read_tbl <- renderTable(read_week_counts)
  
  #month
  tbl_month <- sapply(df_month,table)
  read_month_counts <- as.data.frame(tbl_month$Read)%>% rename(Response = Var1, Read = Freq)
  check_levels <- levels(read_month_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    read_month_counts$Response <- as.character(read_month_counts$Response)
    read_month_counts <- rbind(c("No",0),read_month_counts)
  }
  read_month_counts$Response <- as.factor(read_month_counts$Response)
  check_levels <- levels(read_month_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    read_month_counts$Response <- as.character(read_month_counts$Response)
    read_month_counts <- rbind(read_month_counts,c("Yes",0))
  }
  
  colnames(read_month_counts) <- c("","Read")
  output$mon_read_tbl <- renderTable(read_month_counts)
  
  #year
  tbl_year <- sapply(df_ytd,table)
  read_year_counts <- as.data.frame(tbl_year$Read)%>% rename(Response = Var1, Read = Freq)
  check_levels <- levels(read_year_counts$Response)==c("No","Yes")
  if(check_levels[[1]]==FALSE){
    read_year_counts$Response <- as.character(read_year_counts$Response)
    read_year_counts <- rbind(c("No",0),read_year_counts)
  }
  read_year_counts$Response <- as.factor(read_year_counts$Response)
  check_levels <- levels(read_year_counts$Response)==c("No","Yes")
  if(check_levels[[2]]==FALSE){
    read_year_counts$Response <- as.character(read_year_counts$Response)
    read_year_counts <- rbind(read_year_counts,c("Yes",0))
  }
  
  colnames(read_year_counts) <- c("","Read")
  output$yr_read_tbl <- renderTable(read_year_counts)
  
}

shinyApp(ui,server)

# The code below can be used to deploy your app to rshiny.io
#library(rsconnect)
#deployApp(appDir = "ExampleHabitTracker/",appFiles=c("app.R","examplespreadsheetconnector-5e4cb75973ee.json"))
