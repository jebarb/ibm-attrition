## app.R ##
library(shiny)
library(shinydashboard)

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
              h4("Graph of percent of employee lost across job roles"),
              plotOutput("plot2", width = 600, height = 500),
              h4("Average monthly income based on Marital status"),
              plotOutput("plot3", width = 600, height = 500),
              h4("Average monthly income across different levels of Relationship satisfaction."),
              plotOutput("plot4", width = 600, height = 500),
              h4("Average monthly income across different levels of Job Satisfaction."),
              plotOutput("plot5", width = 600, height = 500),
              h4("There is no significant linear relationship between Work life balance and years since last promotion"),
              #plotOutput("plot6", width = 600, height = 500),
              plotOutput("plot7", width = 600, height = 500),
              h4("The figure below shows the correlation between 5 features and the significance level."),
              #plotOutput("plot8", width = 600, height = 500),
              #p('The figure below is also the visualization of correlations between 5 features. 
              #  The distribution of each variable is shown on the diagonal.
              #  On the bottom of the diagonal : the bivariate scatter plots with a fitted line are isplayed
              #  On the top of the diagonal : the value of the correlation plus the significance level as stars. 
              #  Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)'),
              plotOutput("plot9", width = 600, height = 500),
              h4("The figure below shows female has higher avarage salary than males in IBM."),
              plotOutput("plot10", width = 600, height = 500),
              plotOutput("plot11", width = 600, height = 500),
              h4("The figure below illastrates the job distribution between male and female."),
              plotOutput("plot12", width = 600, height = 500)
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
  
  library(tidyverse)
  library(stringr)
  library(PerformanceAnalytics)
  library(Hmisc)
  
  employee <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
  quit <- filter(employee, Attrition=="Yes")
  stayed <- filter(employee, Attrition=="No")
  
  # plot1
  gender_influence <- employee %>% group_by(Gender, JobRole) %>% summarise(average_monthly_income=mean(MonthlyIncome))
  gender_influence_monthlyincome <- gender_influence %>% mutate(round_income=round(average_monthly_income))
  
  # plot2
  Job_roles <- unique(quit$JobRole)
  roles_quit <- str_count(Job_roles)
  loss_by_job <- data_frame(Job=Job_roles, "Percentage loss"=(roles_quit/1470)*100)
  loss_by_job <- loss_by_job %>% mutate(round_loss=round(loss_by_job$`Percentage loss`,digits=2))
  loss_by_job <- arrange(loss_by_job, desc(round_loss))
  
  # plot3
  testing <- employee %>% group_by(MaritalStatus) %>% summarise(mean=round(mean(MonthlyIncome),digits=0))
  
  # plot4
  RelSat <-employee %>% group_by(RelationshipSatisfaction) %>% summarise(mean=mean(MonthlyIncome))
  
  # plot5
  JobSat <- employee %>% group_by( JobSatisfaction) %>% summarise(mean=mean(MonthlyIncome))
  Jobsatty <- c("Low", "Medium", "High", "Very High")
  
  # plot6
  lin_reg <- lm(WorkLifeBalance~ YearsSinceLastPromotion, employee)
  summary(lin_reg)
  
  # plot8
  numeric <- employee[, c(1,4,6,7,10)]
  #see correlation between features 
  res2 <- rcorr(as.matrix(numeric)) # data must be a matrix 
  
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  # plot10
  genderIncome <- employee %>% group_by(Gender) %>%
    summarise(meanSalary = mean(MonthlyIncome))
  
  # plot12
  genderJobIncome <- employee  %>% group_by(Gender, JobRole) %>%
    summarise(meanSalarybyJob = mean(MonthlyIncome))
  
  output$plot1 <- renderPlot(ggplot(gender_influence_monthlyincome, aes(x=Gender,y=average_monthly_income, color=Gender))+
                               geom_bar(stat = "identity",width=0.5,position=position_dodge())+
                               geom_text(aes(label=round_income), vjust=1.6, color="white",
                                         position = position_dodge(0.9), size=3.5)+
                               facet_wrap(~JobRole)+
                               theme_minimal()+
                               scale_fill_manual(values=c("red", "blue"))+
                               ggtitle("Monthly incomes across Job Roles")+
                               scale_y_continuous(name="Average Monthly Income"))
  
  output$plot2 <- renderPlot(ggplot(loss_by_job, aes(x=reorder(loss_by_job$Job,-loss_by_job$round_loss),y=loss_by_job$round_loss))+
                               geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                               geom_text(aes(label=loss_by_job$round_loss), vjust=1.6, color="white",
                                         position = position_dodge(0.9), size=3.5)+
                               theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                               labs(x="Job Roles",y="Percentage of Loss")+
                               ggtitle("Loss percentage across different Job Roles"))
  
  output$plot3 <- renderPlot(ggplot(testing, aes(x=testing$MaritalStatus,y=testing$mean))+
                               geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                               geom_text(aes(label=testing$mean), vjust=1.6, color="white",
                                         position = position_dodge(0.9), size=3.5)+
                               labs(x="Marital Status",y="Average Monthly Income")+
                               ggtitle("Put a ring on it"))
  
  output$plot4 <- renderPlot(ggplot(RelSat, aes(x=reorder(RelSat$RelationshipSatisfaction,-RelSat$mean),y=RelSat$mean))+
                               geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                               geom_text(aes(label=round(RelSat$mean)), vjust=1.6, color="white",
                                         position = position_dodge(0.9), size=3.5)+
                               labs(x="Relationship Satisfaction",y="Average Monthly Income")+
                               ggtitle("99 Problems"))
  
  output$plot5 <- renderPlot(ggplot(JobSat, aes(x=reorder(Jobsatty,-JobSat$mean),y=JobSat$mean))+
                               geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                               geom_text(aes(label=round(JobSat$mean)), vjust=1.6, color="white",
                                         position = position_dodge(0.9), size=3.5)+
                               labs(x="Job Satisfaction",y="Average Monthly Income")+
                               ggtitle("Mo Money Mo Problems"))
  
  output$plot6 <- renderPlot(summary(lin_reg))
  
  output$plot7 <- renderPlot(boxplot(MonthlyIncome~WorkLifeBalance,data = employee,xlab="PerformanceRating", ylab="MonthlyIncome"))
  
  output$plot8 <- renderPlot(flattenCorrMatrix(res2$r, res2$P))
  
  output$plot9 <- renderPlot(chart.Correlation(numeric, histogram=TRUE, pch=19))
  
  output$plot10 <- renderPlot(ggplot (genderIncome, aes (x = Gender, y = meanSalary)) + geom_bar(stat ="identity") + ggtitle("Mean monthly income between gender "))
  output$plot11 <- renderPlot(ggplot(employee, aes(x = Gender, color = Gender)) +
                               geom_bar()+ facet_wrap(~ JobRole)+ggtitle("Job distribution among gender")+ ggtitle("Job distribution across gender ")
  )
  output$plot12 <- renderPlot(ggplot(genderJobIncome, aes(x = JobRole,y = meanSalarybyJob, color = Gender)) +
                                geom_point() +ggtitle("Mean monthly income among diffrent jobs across gender "))
}

shinyApp(ui, server)