## app.R ##
library(shiny)
library(shinydashboard)

source("exploratory_analysis.R")
#source("final_analysis.R")

# create UI object
ui <- dashboardPage(
  dashboardHeader(title = "Employee Analysis"),
  
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proposal", tabName = "proposal", icon = icon("list")),
      menuItem("Exploratory Analysis", tabName = "exploratory_analysis", icon = icon("compass")),
      menuItem("Exploratory Analysis Results", tabName = "exploratory_analysis_summary", icon = icon("list")),
      menuItem("Exploratory Analysis Code", tabName = "exploratory_analysis_code", icon = icon("compass")),
      menuItem("Final Analysis", tabName = "final_analysis", icon = icon("compass")),
      menuItem("Interactive Model", tabName = "interactive", icon = icon("bar-chart"))
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
              h2("Exploratory Analysis"),
              h4("Graph of average monthly income across all roles faceted by gender"),
              plotOutput("exploratory_plot1", width = 600, height = 500),
              h4("Graph of percent of employee lost across job roles"),
              plotOutput("exploratory_plot2", width = 600, height = 500),
              h4("Average monthly income based on Marital status"),
              plotOutput("exploratory_plot3", width = 600, height = 500),
              h4("Average monthly income across different levels of Relationship satisfaction."),
              plotOutput("exploratory_plot4", width = 600, height = 500),
              h4("Average monthly income across different levels of Job Satisfaction."),
              plotOutput("exploratory_plot5", width = 600, height = 500),
              h4("There is no significant linear relationship between Work life balance and years since last promotion"),
              verbatimTextOutput("exploratory_plot6"),
              h4("There is no relationship between performance rating and monthly income"),
              plotOutput("exploratory_plot7", width = 600, height = 500),
              p('The figure below is also the visualization of correlations between 5 features. 
                The distribution of each variable is shown on the diagonal.
                On the bottom of the diagonal : the bivariate scatter plots with a fitted line are isplayed
                On the top of the diagonal : the value of the correlation plus the significance level as stars. 
                Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)'),
              tableOutput("exploratory_plot8"),
              h4(""),
              plotOutput("exploratory_plot9", width = 600, height = 500),
              h4("The figure below shows female has higher avarage salary than males in IBM."),
              plotOutput("exploratory_plot10", width = 600, height = 500),
              h4("The below plot shows gender distribution of gender across jobs"),
              plotOutput("exploratory_plot11", width = 600, height = 500),
              h4("The figure below illastrates the job distribution between male and female."),
              plotOutput("exploratory_plot12", width = 600, height = 500),
              h4("add info on plot 13"),
              plotOutput("exploratory_plot13", width = 600, height = 500),
              h4("add info on plot 14"),
              plotOutput("exploratory_plot14", width = 600, height = 500),
              h4("add info on plot 15"),
              plotOutput("exploratory_plot15", width = 600, height = 500),
              h4("add info on plot 16"),
              plotOutput("exploratory_plot16", width = 600, height = 500)
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
              h2("content")
      )
    )
  )
)

server <- function(input, output) {
  # All code in external files for the sake of cleanliness
  # Render all plots to be displayed in dashboard
  output$exploratory_plot1 <- renderPlot(exploratory_plot1)
  output$exploratory_plot2 <- renderPlot(exploratory_plot2)
  output$exploratory_plot3 <- renderPlot(exploratory_plot3)
  output$exploratory_plot4 <- renderPlot(exploratory_plot4)
  output$exploratory_plot5 <- renderPlot(exploratory_plot5)
  output$exploratory_plot6 <- renderPrint(exploratory_plot6)
  output$exploratory_plot7 <- renderPlot(boxplot(MonthlyIncome~WorkLifeBalance,data = employee,xlab="PerformanceRating", ylab="MonthlyIncome"))
  output$exploratory_plot8 <- renderTable(exploratory_plot8)
  output$exploratory_plot9 <- renderPlot(chart.Correlation(numeric, histogram=TRUE, pch=19))
  output$exploratory_plot10 <- renderPlot(exploratory_plot10)
  output$exploratory_plot11 <- renderPlot(exploratory_plot11)
  output$exploratory_plot12 <- renderPlot(exploratory_plot12)
  output$exploratory_plot13 <- renderPlot(corrplot(correlation_data,method="circle"))
  output$exploratory_plot14 <- renderPlot(exploratory_plot14)
  output$exploratory_plot15 <- renderPlot(exploratory_plot15)
  output$exploratory_plot16 <- renderPlot(exploratory_plot16)
  output$exploratory_code <- renderText(readChar("exploratory_analysis.R", file.info("exploratory_analysis.R")$size))
  
}

shinyApp(ui, server)