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
      menuItem("Interactive Model", tabName = "interactive", icon = icon("edit")),
      menuItem("Final Results", tabName = "final_results", icon = icon("list"))
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
      # Exploratory code tab content
      tabItem(tabName = "exploratory_analysis_code",
              verbatimTextOutput("exploratory_code")
      ),
      # Final analysis tab content
      tabItem(tabName = "final_analysis",
              includeMarkdown("final_analysis.md")
      ),
      # Interactive tab content
      tabItem(tabName = "interactive",
              numericInput("tmp", "num", 0),
              numericInput("OverTimeYes", "OverTimeYes", 0),            
              numericInput("StockOptionLevel", "StockOptionLevel", 0),      
              numericInput("TotalWorkingYears", "TotalWorkingYears", 0),      
              numericInput("MonthlyIncome", "MonthlyIncome", 0),
              numericInput("JobInvolvement", "JobInvolvement", 0),          
              numericInput("Age", "Age", 0),                    
              numericInput("EnvironmentSatisfaction", "EnvironmentSatisfaction", 0), 
              numericInput("JobSatisfaction", "JobSatisfaction", 0),         
              numericInput("YearsWithCurrManager", "YearsWithCurrManager", 0),   
              numericInput("YearsAtCompany", "YearsAtCompany", 0),          
              numericInput("JobLevel", "JobLevel", 0),                
              numericInput("YearsInCurrentRole", "YearsInCurrentRole", 0),     
              numericInput("DailyRate", "DailyRate", 0),               
              numericInput("EmployeeNumber", "EmployeeNumber", 0),          
              numericInput("TrainingTimesLastYear", "TrainingTimesLastYear", 0),  
              numericInput("DistanceFromHome", "DistanceFromHome", 0),        
              numericInput("Education", "Education", 0),               
              numericInput("NumCompaniesWorked", "NumCompaniesWorked", 0),     
              numericInput("HourlyRate", "HourlyRate", 0),              
              numericInput("WorkLifeBalance", "WorkLifeBalance", 0),         
              numericInput("MaritalStatusMarried", "MaritalStatusMarried", 0),   
              numericInput("MaritalStatusSingle", "MaritalStatusSingle", 0),  
              verbatimTextOutput("view")
      )
    )
  )
)

server <- function(input, output) {
  # All code in external files for the sake of cleanliness
  
  construct_df <- function() {
    data.frame(
      "OverTimeYes" = input$OverTimeYes,
      "StockOptionLevel" = input$StockOptionLevel,
      "TotalWorkingYears" = input$TotalWorkingYears,
      "MonthlyIncome" = input$MonthlyIncome,
      "JobInvolvement" = input$JobInvolvement,
      "Age" = input$Age,
      "EnvironmentSatisfaction" = input$EnvironmentSatisfaction,
      "JobSatisfaction" = input$JobSatisfaction,
      "YearsWithCurrManager" = input$YearsWithCurrManager,
      "YearsAtCompany" = input$YearsAtCompany,
      "JobLevel" = input$JobLevel,
      "YearsInCurrentRole" = input$YearsInCurrentRole,
      "DailyRate" = input$DailyRate,
      "EmployeeNumber" = input$EmployeeNumber,
      "TrainingTimesLastYear" = input$TrainingTimesLastYear,
      "DistanceFromHome" = input$DistanceFromHome,
      "Education" = input$Education,
      "NumCompaniesWorked" = input$NumCompaniesWorked,
      "HourlyRate" = input$HourlyRate,
      "WorkLifeBalance" = input$WorkLifeBalance,
      "MaritalStatusMarried" = input$MaritalStatusMarried,
      "MaritalStatusSingle" = input$MaritalStatusSingle
    )
  }
  
  rf_optimal <- readRDS("rf_optimal.rda")
  values <- reactiveValues(df_data = NULL)
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
  observeEvent(input$MaritalStatusMarried, {
    values$df_data <- construct_df()
  })
  observeEvent(input$MaritalStatusSingle, {
    values$df_data <- construct_df()
  })
  output$view <- reactive(predict(rf_optimal, values$df_data, type = "prob")[1,2])
}

shinyApp(ui, server)