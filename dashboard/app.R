## app.R ##
library(shiny)
library(shinydashboard)
source("setup.R")

ui <- dashboardPage(
  dashboardHeader(title = "Employee Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proposal", tabName = "proposal", icon = icon("list")),
      menuItem("Exploratory Analysis", tabName = "exploratory", icon = icon("compass")),
      menuItem("Exploratory Results", tabName = "exploratory_summary", icon = icon("list")),
      menuItem("Interactive Analysis", tabName = "interactive", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
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
      tabItem(tabName = "exploratory",
              h2("Exploratory Analysis"),
              h4("Graph of average monthly income across all roles faceted by gender"),
              plotOutput("plot1", width = 600, height = 500),
              h4("Average monthly income based on Marital status"),
              plotOutput("plot3", width = 600, height = 500),
              br(),
              plotOutput("plot2", width = 600, height = 500)
              #h4(""),
              #plotOutput("plot4")
              #png("plot4.png")
      ),
      tabItem(tabName = "exploratory_summary",
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
      tabItem(tabName = "interactive",
              h2("Interactive content"))
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot(ggplot(gender_influence_monthlyincome, aes(x=Gender,y=average_monthly_income, color=Gender))+
                               geom_bar(stat = "identity",width=0.5,position=position_dodge())+
                               geom_text(aes(label=round_income), vjust=1.6, color="white",
                                         position = position_dodge(0.9), size=3.5)+
                               facet_wrap(~JobRole)+
                               theme_minimal()+
                               scale_fill_manual(values=c("red", "blue"))+
                               ggtitle("Monthly incomes across Job Roles")+
                               scale_y_continuous(name="Average Monthly Income"))
  output$plot2 <- renderPlot(ggplot(employee, aes(x = Gender, color = Gender)) +
                               geom_bar()+ 
                               facet_wrap(~JobRole)+
                               ggtitle("Job distribution among gender"))
  output$plot3 <- renderPlot(ggplot(testing, aes(x=testing$MaritalStatus,y=testing$mean))+
                               geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                               geom_text(aes(label=testing$mean), vjust=1.6, color="white",
                                         position = position_dodge(0.9), size=3.5)+
                               labs(x="Marital Status",y="Average Monthly Income")+
                               ggtitle("Put a ring on it"))
  output$plot4 <- renderPlot(boxplot(MonthlyIncome~WorkLifeBalance,data = employee,xlab="PerformanceRating", ylab="MonthlyIncome") + 
                               ggtitle("performance vs monthly income"))
}

shinyApp(ui, server)