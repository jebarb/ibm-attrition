## app.R ##
library(shiny)
library(shinydashboard)

# create UI object
ui <- dashboardPage(
  dashboardHeader(title = "Employee Analysis"),
  
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proposal", tabName = "proposal", icon = icon("list")),
      menuItem("Exploratory Analysis", tabName = "exploratory_analysis", icon = icon("line-chart")),
      menuItem("Exploratory Analysis Results", tabName = "exploratory_analysis_summary", icon = icon("compass")),
      menuItem("Final Analysis", tabName = "final_analysis", icon = icon("calculator")),
      menuItem("Interactive Model", tabName = "interactive", icon = icon("edit"))
    )
  ),
  
  # Body content
  dashboardBody(
    # Individual tab content
    tabItems(
      # Proposal tab content
      tabItem(tabName = "proposal",
              h2("IBM Employee Data Analysis"),
              p("We used the IBM HR Analytics Employee Attrition & Performance data from Kagle to explore the correlation between features and to find the common pattern in the dataset in order to uncover the factors that lead to employee attrition. By doing that, our goal is to build a predictive model that can predict employee attrition with high accuracy"),
              h3("Questions we would like to answer"),
              tags$ul(
                tags$li("Is there a gender gap between workers of different fields?"), 
                tags$li("Does Martial Status have a significant effect on wage?"), 
                tags$li("Does the number of years spent with the company affect the rate of pay?"), 
                tags$li("How does performance rating contribute to wage?"),
                tags$li("Does work life balance concur with years in position or years with company?")
              ),
              h3("Goals"),
              tags$ul(
                tags$li("Answer the above questions"),
                tags$li("Created shiny app to display our results, as well as constructed the dashboard for final project submission"),
                tags$li("Post source to github")
              )
      ),
      # Exploratory analysis tab content
      tabItem(tabName = "exploratory_analysis",
              includeMarkdown("exploratory_analysis.md")
      ),
      # Exploratory analysis summary tab content
      tabItem(tabName = "exploratory_analysis_summary",
              h2("Exploratory Analysis Results"),
              h3("Some results we have found"),
              tags$ul(
                tags$li("In IBM, average monthly salary of female(6686.566) is higher than that of male (6380.508), which is out of what we expected"),
                tags$li("Across all job roles, the different pay between women and men are marginal"),
                tags$li("On higher position such as manager or manufacturing director, number of female and male are pretty much equal. But positions of sales executive and research scientist have attracted more male employees than females"),
                tags$li("As for marital status, people married or divorced have higher income than people are single, which is interesting to see. It’s also reasonable because they may have longer working years and higher position level. In this case, we plan to explore if job position concur with marital status"),
                tags$li("The performance rating in the dataset is either 3 or 4 (good or excellent), maybe everyone at IBM is good at their work"),
                tags$li("There is no strong correlation between work life balance and years in company or years in position. Also monthly income has trivial influence on work life balance"),
                tags$li("There is positive linear relationship between years at company and years since last promotion, with a significant adjusted R^2 0.382")
              ),
              h3("Next steps"),
              tags$ul(
                tags$li("Continue running on feature selection to delete irrelevant ones"),
                tags$li("Continue on model selection and compare the accuracy between different models"),
                tags$li("Perform logistic regression on employee attrition"),
                tags$li("Once we finish all analysis, we will put everything in shiny app to form a complete project")
              )
      ),
      # Final analysis tab content
      tabItem(tabName = "final_analysis",
              includeMarkdown("final_analysis.md")
      ),
      # Interactive tab content
      tabItem(tabName = "interactive",
              
              
              h2("Probability of Attrition"),
              verbatimTextOutput("view"),
              
              # display 3 inputs side by side
              bootstrapPage(
                div(style="display:inline-block", selectInput("OverTimeYes", "Overtime", c("No" = 1, "Yes" = 0), width = 200)), # boolean
                div(style="display:inline-block", selectInput("StockOptionLevel", "Stock option level", c("0" = 0, "1" = 1, "2" = 2, "3" = 3), width = 200)), # 0-3
                div(style="display:inline-block", selectInput("EnvironmentSatisfaction", "Environment satisfaction", c("Low" = 0, "Medium" = 1, "High" = 2, "Very High" = 3), width = 200))
              ),
              
              br(),
              
              bootstrapPage(
                div(style="display:inline-block", selectInput("JobSatisfaction", "Job satisfaction", c("Low" = 0, "Medium" = 1, "High" = 2, "Very High" = 3), width = 200)), # 1-4
                div(style="display:inline-block", selectInput("JobLevel", "Job level", c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5), width = 200)), # 1-5              
                div(style="display:inline-block", selectInput("Education", "Education", c("Below College" = 1, "College" = 2, "Bachelors" = 3, "Masters" = 4, "Doctorate" = 5), width = 200)) # 1-5              
              ),
              
              br(),
              
              bootstrapPage(
                div(style="display:inline-block", selectInput("WorkLifeBalance", "Work/life balance", c("Bad" = 1, "Good" = 2, "Better" = 3, "Best" = 4), width = 200)), # 1-4    
                div(style="display:inline-block", selectInput("MaritalStatus", "Marital status ", c("Single" = "Single", "Married" = "Married", "Divorced" = "Divorced"), width = 200)), # boolean  
                div(style="display:inline-block", selectInput("JobInvolvement", "Job involvement", c("Low" = 1, "Medium" = 2, "High" = 3, "Very High" = 4), width = 200)) # 1-4          
              ),
              
              br(),
              
              bootstrapPage(
                div(style="display:inline-block", numericInput("MonthlyIncome", "Monthly income", 0, width = 200)),
                div(style="display:inline-block", numericInput("TotalWorkingYears", "Total working years", 0, width = 200)), # 1-4  
                div(style="display:inline-block", numericInput("Age", "Age", 0, width = 200))                    
              ),
              
              br(),
              
              bootstrapPage(
                div(style="display:inline-block", numericInput("YearsWithCurrManager", "Years with manager", 0, width = 200)),   
                div(style="display:inline-block", numericInput("YearsAtCompany", "Years at company", 0, width = 200)),          
                div(style="display:inline-block", numericInput("YearsInCurrentRole", "Years in current role", 0, width = 200))     
              ),
              
              br(),
              
              bootstrapPage(
                div(style="display:inline-block", numericInput("DailyRate", "Daily rate", 0, width = 200)),               
                div(style="display:inline-block", numericInput("EmployeeNumber", "Employee number", 0, width = 200)),          
                div(style="display:inline-block", numericInput("TrainingTimesLastYear", "Trainings last year", 0, width = 200))   
              ),
              
              br(),
              
              bootstrapPage(
                div(style="display:inline-block", numericInput("DistanceFromHome", "Distance from home", 0, width = 200)),        
                div(style="display:inline-block", numericInput("NumCompaniesWorked", "Companies worked at", 0, width = 200)),     
                div(style="display:inline-block", numericInput("HourlyRate", "Hourly rate", 0, width = 200))
              )
      )
    )
  )
)

server <- function(input, output) {

  # watch 
  MartialStatusSingle <- reactive({
    switch(input$MartialStatus,
           'Single' = 1,
           'Married' = 0,
           'Divorced' = 0)
  })
  
  MartialStatusMarried <- reactive({
    switch(input$MartialStatus,
           'Single' = 0,
           'Married' = 1,
           'Divorced' = 0)
  })
  
  construct_df <- function() {
    data.frame(
      "OverTimeYes" = switch(input$OverTimeYes,
                             "0" = 0,
                             "1" = 1),
      "StockOptionLevel" = switch(input$StockOptionLevel,
                                  "0" = 0,
                                  "1" = 1,
                                  "2" = 2,
                                  "3" = 3,
                                  "4" = 4,
                                  "5" = 5),
      "TotalWorkingYears" = input$TotalWorkingYears,
      "MonthlyIncome" = input$MonthlyIncome,
      "JobInvolvement" = switch(input$JobInvolvement,
                                "0" = 0,
                                "1" = 1,
                                "2" = 2,
                                "3" = 3,
                                "4" = 4,
                                "5" = 5),
      "Age" = input$Age,
      "EnvironmentSatisfaction" = switch(input$EnvironmentSatisfaction,
                                         "0" = 0,
                                         "1" = 1,
                                         "2" = 2,
                                         "3" = 3,
                                         "4" = 4,
                                         "5" = 5),
      "JobSatisfaction" = switch(input$JobSatisfaction,
                                 "0" = 0,
                                 "1" = 1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5" = 5),
      "YearsWithCurrManager" = input$YearsWithCurrManager,
      "YearsAtCompany" = input$YearsAtCompany,
      "JobLevel" = switch(input$JobLevel,
                          "0" = 0,
                          "1" = 1,
                          "2" = 2,
                          "3" = 3,
                          "4" = 4,
                          "5" = 5),
      "YearsInCurrentRole" = input$YearsInCurrentRole,
      "DailyRate" = input$DailyRate,
      "EmployeeNumber" = input$EmployeeNumber,
      "TrainingTimesLastYear" = input$TrainingTimesLastYear,
      "DistanceFromHome" = input$DistanceFromHome,
      "Education" = switch(input$Education,
                           "0" = 0,
                           "1" = 1,
                           "2" = 2,
                           "3" = 3,
                           "4" = 4,
                           "5" = 5),
      "NumCompaniesWorked" = input$NumCompaniesWorked,
      "HourlyRate" = input$HourlyRate,
      "WorkLifeBalance" = switch(input$WorkLifeBalance,
                                 "0" = 0,
                                 "1" = 1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5" = 5),
      "MaritalStatusMarried" = switch(input$MaritalStatus,
                                      'Single' = 0,
                                      'Married' = 1,
                                      'Divorced' = 0),
      "MaritalStatusSingle" = switch(input$MaritalStatus,
                                     'Single' = 1,
                                     'Married' = 0,
                                     'Divorced' = 0)
    )
  }
  
  # import model from final analysis
  rf_optimal <- readRDS("rf_optimal.rda")
  
  # create reactive object to store input data
  values <- reactiveValues(df_data = NULL)
  
  # watch each field and update data frame when changed
  observeEvent(input$OverTimeYes, {
    values$df_data <- construct_df()
  })
  observeEvent(input$StockOptionLevel, {
    values$df_data <- construct_df()
  })
  observeEvent(input$TotalWorkingYears, {
    values$df_data <- construct_df()
  })
  observeEvent(input$MonthlyIncome, {
    values$df_data <- construct_df()
  })
  observeEvent(input$JobInvolvement, {
    values$df_data <- construct_df()
  })
  observeEvent(input$Age, {
    values$df_data <- construct_df()
  })
  observeEvent(input$EnvironmentSatisfaction, {
    values$df_data <- construct_df()
  })
  observeEvent(input$JobSatisfaction, {
    values$df_data <- construct_df()
  })
  observeEvent(input$YearsWithCurrManager, {
    values$df_data <- construct_df()
  })
  observeEvent(input$YearsAtCompany, {
    values$df_data <- construct_df()
  })
  observeEvent(input$JobLevel, {
    values$df_data <- construct_df()
  })
  observeEvent(input$YearsInCurrentRole, {
    values$df_data <- construct_df()
  })
  observeEvent(input$DailyRate, {
    values$df_data <- construct_df()
  })
  observeEvent(input$EmployeeNumber, {
    values$df_data <- construct_df()
  })
  observeEvent(input$TrainingTimesLastYear, {
    values$df_data <- construct_df()
  })
  observeEvent(input$DistanceFromHome, {
    values$df_data <- construct_df()
  })
  observeEvent(input$Education, {
    values$df_data <- construct_df()
  })
  observeEvent(input$NumCompaniesWorked, {
    values$df_data <- construct_df()
  })
  observeEvent(input$HourlyRate, {
    values$df_data <- construct_df()
  })
  observeEvent(input$WorkLifeBalance, {
    values$df_data <- construct_df()
  })
  observeEvent(input$MaritalStatus, {
    values$df_data <- construct_df()
  })
  
  # predict probability of attrition
  output$view <- reactive(predict(rf_optimal, values$df_data, type = "prob")[1,2])
  
}

shinyApp(ui, server)