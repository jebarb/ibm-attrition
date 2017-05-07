data <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
ggplot(data, mapping = aes(x = YearsAtCompany, y = HourlyRate)) + 
  facet_wrap(~JobRole) +
  geom_jitter()