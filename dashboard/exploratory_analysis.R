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
plot1 <- renderPlot(ggplot(gender_influence_monthlyincome, aes(x=Gender,y=average_monthly_income, color=Gender))+
                      geom_bar(stat = "identity",width=0.5,position=position_dodge())+
                      geom_text(aes(label=round_income), vjust=1.6, color="white",
                                position = position_dodge(0.9), size=3.5)+
                      facet_wrap(~JobRole)+
                      theme_minimal()+
                      scale_fill_manual(values=c("red", "blue"))+
                      ggtitle("Monthly incomes across Job Roles")+
                      scale_y_continuous(name="Average Monthly Income"))

# plot2
Job_roles <- unique(quit$JobRole)
roles_quit <- str_count(Job_roles)
loss_by_job <- data_frame(Job=Job_roles, "Percentage loss"=(roles_quit/1470)*100)
loss_by_job <- loss_by_job %>% mutate(round_loss=round(loss_by_job$`Percentage loss`,digits=2))
loss_by_job <- arrange(loss_by_job, desc(round_loss))
plot2 <- renderPlot(ggplot(loss_by_job, aes(x=reorder(loss_by_job$Job,-loss_by_job$round_loss),y=loss_by_job$round_loss))+
                      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                      geom_text(aes(label=loss_by_job$round_loss), vjust=1.6, color="white",
                                position = position_dodge(0.9), size=3.5)+
                      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                      labs(x="Job Roles",y="Percentage of Loss")+
                      ggtitle("Loss percentage across different Job Roles"))

# plot3
testing <- employee %>% group_by(MaritalStatus) %>% summarise(mean=round(mean(MonthlyIncome),digits=0))
plot3 <- renderPlot(ggplot(testing, aes(x=testing$MaritalStatus,y=testing$mean))+
                      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                      geom_text(aes(label=testing$mean), vjust=1.6, color="white",
                                position = position_dodge(0.9), size=3.5)+
                      labs(x="Marital Status",y="Average Monthly Income")+
                      ggtitle("Put a ring on it"))

# plot4
RelSat <-employee %>% group_by(RelationshipSatisfaction) %>% summarise(mean=mean(MonthlyIncome))
plot4 <- renderPlot(ggplot(RelSat, aes(x=reorder(RelSat$RelationshipSatisfaction,-RelSat$mean),y=RelSat$mean))+
                      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                      geom_text(aes(label=round(RelSat$mean)), vjust=1.6, color="white",
                                position = position_dodge(0.9), size=3.5)+
                      labs(x="Relationship Satisfaction",y="Average Monthly Income")+
                      ggtitle("99 Problems"))

# plot5
JobSat <- employee %>% group_by( JobSatisfaction) %>% summarise(mean=mean(MonthlyIncome))
Jobsatty <- c("Low", "Medium", "High", "Very High")
plot5 <- renderPlot(ggplot(JobSat, aes(x=reorder(Jobsatty,-JobSat$mean),y=JobSat$mean))+
                      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
                      geom_text(aes(label=round(JobSat$mean)), vjust=1.6, color="white",
                                position = position_dodge(0.9), size=3.5)+
                      labs(x="Job Satisfaction",y="Average Monthly Income")+
                      ggtitle("Mo Money Mo Problems"))

# plot6
lin_reg <- lm(WorkLifeBalance~ YearsSinceLastPromotion, employee)
plot6 <- renderPlot(summary(lin_reg))

# plot7
plot7 <- renderPlot(boxplot(MonthlyIncome~WorkLifeBalance,data = employee,xlab="PerformanceRating", ylab="MonthlyIncome"))

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

plot8 <- renderPlot(flattenCorrMatrix(res2$r, res2$P))

# plot9
plot9 <- renderPlot(chart.Correlation(numeric, histogram=TRUE, pch=19))

# plot10
genderIncome <- employee %>% group_by(Gender) %>%
  summarise(meanSalary = mean(MonthlyIncome))
plot10 <- renderPlot(ggplot (genderIncome, aes (x = Gender, y = meanSalary)) + geom_bar(stat ="identity") + ggtitle("Mean monthly income between gender "))

# plot11
plot11 <- renderPlot(ggplot(employee, aes(x = Gender, color = Gender)) +
                       geom_bar()+ facet_wrap(~ JobRole)+ggtitle("Job distribution among gender")+ ggtitle("Job distribution across gender "))

# plot12
genderJobIncome <- employee  %>% group_by(Gender, JobRole) %>%
  summarise(meanSalarybyJob = mean(MonthlyIncome))
plot12 <- renderPlot(ggplot(genderJobIncome, aes(x = JobRole,y = meanSalarybyJob, color = Gender)) +
                       geom_point() +ggtitle("Mean monthly income among diffrent jobs across gender "))
