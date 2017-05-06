library(tidyverse)
library(stringr)
quit <- filter(employee, Attrition=="Yes")
stayed <- filter(employee, Attrition=="No")
nrow(quit)
#237
nrow(stayed)
#1233
retention_rate <- nrow(quit)/nrow(employee)
#16.12%
mean(quit$YearsWithCurrManager) #Average years with Manager of those who quit
#2.85 Years
mean(stayed$YearsWithCurrManager)#Average years with Manager of those who stay
#4.36 Years
gender_influence <- employee %>% group_by(Gender, JobRole) %>% summarise(average_monthly_income=mean(MonthlyIncome))
View(gender_influence)
gender_influence_monthlyincome <- gender_influence %>% mutate(round_income=round(average_monthly_income))
ggplot(gender_influence_monthlyincome, aes(x=Gender,y=average_monthly_income, color=Gender))+
    geom_bar(stat = "identity",width=0.5,position=position_dodge())+
    geom_text(aes(label=round_income), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5)+
    facet_wrap(~JobRole)+
    theme_minimal()+
    scale_fill_manual(values=c("red", "blue"))+
    ggtitle("Monthly incomes across Job Roles")+
    scale_y_continuous(name="Average Monthly Income")
female_loss <- str_count(quit$Gender, "Female") %>% sum()
male_loss <- str_count(quit$Gender, "Male") %>% sum()
male_total <- str_count(employee$Gender, "Male") %>% sum()
female_total <- str_count(employee$Gender, "Female") %>% sum()
loss_rate_male <- male_loss/nrow(employee)
loss_rate_female <- female_loss/nrow(employee)
loss_rate_female
loss_rate_male
female_total/nrow(employee)

depart_loss <- quit %>% group_by(Department,Gender)
View(depart_loss)
quit %>% ggplot()+
    geom_histogram(stat = count(quit$JobRole))
Job_roles <- unique(quit$JobRole)
roles_quit <- str_count(Job_roles)
View(Job_roles)
loss_by_job <- data_frame(Job=Job_roles, "Percentage loss"=(roles_quit/1470)*100)
View(loss_by_job)
loss_by_job <- loss_by_job %>% mutate(round_loss=round(loss_by_job$`Percentage loss`,digits=2))
loss_by_job <- arrange(loss_by_job, desc(round_loss))
ggplot(loss_by_job, aes(x=reorder(loss_by_job$Job, -loss_by_job$round_loss),y=loss_by_job$round_loss))+
    geom_bar(stat = "identity",width=0.5, position=position_dodge())+
    geom_text(aes(label=loss_by_job$round_loss), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    labs(x="Job Roles",y="Percentage of Loss")

mean(quit$Age)

mean(employee$NumCompaniesWorked)
typeof(quit$Age)
colnames(quit)
employee <- employee %>% mutate("Hours per day"=employee$DailyRate/employee$HourlyRate)
View(employee)
Job_roles_full <- unique(employee$JobRole)

