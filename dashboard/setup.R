library(tidyverse)
library(stringr)
employee <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
quit <- filter(employee, Attrition=="Yes")
stayed <- filter(employee, Attrition=="No")

# graph 1
gender_influence <- employee %>% group_by(Gender, JobRole) %>% summarise(average_monthly_income=mean(MonthlyIncome))
gender_influence_monthlyincome <- gender_influence %>% mutate(round_income=round(average_monthly_income))

# graph 2
testing <- employee %>% group_by(MaritalStatus) %>% summarise(mean=round(mean(MonthlyIncome),digits=0))

# graph 3