Abstract
--------

The purpose of this project is to analyze data provided from IBM’s Human
resource department in order to understand the common characteristics in
employee attrition, or possible employee discrimination. In order to
weight the importance of each variable presented, we filtered the data
based on variables that were integer type and performed a correlation
analysis. By doing so we were able to develop a basic understanding of
which variables to include in our predictive model. The predictive model
that we have developed has proven to predict employee attrition with an
84% accuracy rate. We also wanted to consider relationships in
non-integer type variables in hopes of observing possible discrimination
trends that can be supported by the dataset. We developed a number of
visuals in order to justify our conclusions. Lastly, we strived to
develop a user-friendly Shiny app for possible managers to use in order
to predict employee attrition. All code and details shall be presented
in the appropriate Rmd files as part of the final project.

Overview and Motivation:
------------------------

The issue of keeping one's employees happy and satisfied is a perennial
and age-old challenge. If an employee you have invested so much time and
money leaves for "greener pastures", then this would mean that you would
have to spend even more time and money to hire somebody else. Alongside
these reason maintaining happy employees have resulted in increase
profit for companies based on various studies done in industry. By
understanding the main reasons why employees leave and possible
discrimination, the increase probability your company will succeed.

Related Work
------------

In class we had discussed effective techniques of communication. As a
team we took that to heart and wanted to develop models that were not
only easy to understand but visually pleasing. We have looked into
various psychology journals, which study employee habits and utility in
order to develop a healthy reasoning of what possible variables are
important based on published studies.

Initial Questions
-----------------

-   Does Marital Status have a significant effect on wages?
-   How does performance rating contribute to monthly income?
-   How do different departments contribute to employee attrition?
-   How do different job roles contribute to employee attrition?
-   Can we give a general idea of working atmosphere based on department
    or job role attrition?
-   Can we predict employee attrition?
-   Does a higher education level contribute to lower attrition levels?
-   How can we tell if an employee is happy?
-   Does work life balance concur with years in position or years with
    company? How about with monthly income?

Some of these questions were answered by using visuals such as scatter
plots, bar graphs, and others. While with other questions such as
predicting employee attrition, we developed a healthy predictive model
based on the random forest classifier, which enabled an 84% accuracy
rate. Though some of the questions were straightforward and were
answered with yes or no, a few of them were impossible to determine. One
impossible question to answer was “How can we tell if an employee is
happy.” Essentially this question boils down to looking into background
studies, which observe employee utility from psychology journals. Yet
with what these studies propose we did not have the data for thus
forcing us to abandon a few questions. One of the main issues with the
data set was not the organization but the type of the variables and
transforming different types to allow for smooth analysis. For example;
on two of our team members laptop the “Age” variable operated as a
normal numeric yet with the last team mate the “Age” variable only
produced errors. Running into small technical issues which were resolved
using Google while also performing other yak shaving activities did slow
us down but in the end allowed for worthwhile analysis.

    employee <- read_csv("IBM-HR-Employee-Attrition.csv")

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   Attrition = col_character(),
    ##   BusinessTravel = col_character(),
    ##   Department = col_character(),
    ##   EducationField = col_character(),
    ##   Gender = col_character(),
    ##   JobRole = col_character(),
    ##   MaritalStatus = col_character(),
    ##   Over18 = col_character(),
    ##   OverTime = col_character()
    ## )

    ## See spec(...) for full column specifications.

    quit <- filter(employee, Attrition=="Yes") #Filter employee data based on those who have quit
    stayed <- filter(employee, Attrition=="No") #Filter employee data based on those who have stayed

    # plot1
    gender_influence <- employee %>% group_by(Gender, JobRole) %>%  summarise(average_monthly_income=mean(MonthlyIncome))
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

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    #Grouped employee data by "Gender" and "JobRole" in order to summarise average monthly income based on hose to factors. To make the plot look visually pleasing we rounded the Monthly income and placed it inside of the bar graph, we also highlighted which Gender is represented using blue for male and red for female. We removed the theme to increase signal to noise ratio. Finally we made sure to label the graph, x and y axis for clear interpretation.

Represents the average monthly incomes based on gender which we then
faceted against Job roles to observe the possible pay gap between
genders across various Job roles. From first looking at the graph you
may see slight gaps between the genders in certain roles such as
Research Director and Manager which one might conclude as insignificant.
But if we take the sum for the year of monthly income for Research
Director then males will be making a total of $199,896 versus females
making a total of $181,728. A $18,168 pay gap!

    # plot2
    Job_roles <- unique(quit$JobRole)
    roles_quit <- str_count(Job_roles)
    loss_by_job <- data_frame(Job=Job_roles, "Percentage loss"=(roles_quit/1470)*100)
    loss_by_job <- loss_by_job %>% mutate(round_loss=round(loss_by_job$`Percentage loss`,digits=2))
    loss_by_job <- arrange(loss_by_job, desc(round_loss))
    ggplot(loss_by_job, aes(x=reorder(loss_by_job$Job,-loss_by_job$round_loss),y=loss_by_job$round_loss))+
      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
      geom_text(aes(label=loss_by_job$round_loss), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(x="Job Roles",y="Percentage of Loss")+
      ggtitle("Loss percentage across different Job Roles")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    #Found all job roles that had quit and counted each position. Then divided the total count of each position by total observation. Arranged the loss by decreasing amount and graphed using bar plot using similar labels as exploratory plot 1.

We wanted to know based on Job roles where was the attrition coming
from? With the bar graph we found that the top three job roles with the
highest attrition by percentage were Healthcare Representative,
Manufacturing Director, and Laboratory Technician. So why does this
graph matter? We suggest that IBM looks into the underlying reasons why
these roles have the highest loss. From the definition we understand
that any loss will result in cost for the company. Are employees unhappy
in these roles, are they treated fairly, and are there opportunities for
growth?

    # plot3
    testing <- employee %>% group_by(MaritalStatus) %>% summarise(mean=round(mean(MonthlyIncome),digits=0))
    ggplot(testing, aes(x=testing$MaritalStatus,y=testing$mean))+
      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
      geom_text(aes(label=testing$mean), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      labs(x="Marital Status",y="Average Monthly Income")+
      ggtitle("Put a ring on it")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    #Group the original data set by Marital status and summarize mean monthly income. Then graphed the monthly income averages according to each category of marital status.

After grouping by relationship status, we averaged the monthly income in
order to observe any possible trends that occur based on marital status.
We concluded that employees who were single observed a deficit of $897
in average monthly income relative to the divorced and married
categories.

    # plot4
    RelSat <-employee %>% group_by(RelationshipSatisfaction) %>% summarise(mean=mean(MonthlyIncome))
    ggplot(RelSat, aes(x=reorder(RelSat$RelationshipSatisfaction,-RelSat$mean),y=RelSat$mean))+
      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
      geom_text(aes(label=round(RelSat$mean)), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      labs(x="Relationship Satisfaction",y="Average Monthly Income")+
      ggtitle("99 Problems")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    #Grouped the original data set based on Relationship Satisfaction and summarized average monthly income. Present the average monthly incomes based on Relationship Satisfaction using a bar graph and used appropriate labels.

Similar to the first plot we grouped our x-axis by “Relationship
Satisfaction” and observed average monthly income of the 4 groups. We
observed that as the level of satisfaction an employee perceives in
their relationship the higher average monthly income they earn. Some
psychology studies have concluded that a happy and successful
relationship outside of work results in being more productive and thus
earning more at work. Although we have the background theory, we do not
have further data to support these claims, but to simply make
observations and state our hypothesis.

    # plot5
    JobSat <- employee %>% group_by( JobSatisfaction) %>% summarise(mean=mean(MonthlyIncome))
    Jobsatty <- c("Low", "Medium", "High", "Very High")
    ggplot(JobSat, aes(x=reorder(Jobsatty,-JobSat$mean),y=JobSat$mean))+
      geom_bar(stat = "identity",width=0.5, position=position_dodge())+
      geom_text(aes(label=round(JobSat$mean)), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      labs(x="Job Satisfaction",y="Average Monthly Income")+
      ggtitle("Mo Money Mo Problems")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    #Grouped the original data set based on Job Satisfaction and summarized average monthly income. Present the average monthly incomes based on Job Satisfaction using a bar graph and used appropriate labels.

We observed the average monthly income based on Job satisfaction and
found an interesting trend. As the level of satisfaction for one’s job
decreases, the average monthly income that one earns increases. Our
reasoning begins with the idea that with increased pay comes increased
amounts of responsibility, which in turn causes more stress to perform
well and ensure that one’s team is performing well.

    # plot6
    lin_reg <- lm(WorkLifeBalance~ YearsSinceLastPromotion, employee)
    summary(lin_reg)

    ## 
    ## Call:
    ## lm(formula = WorkLifeBalance ~ YearsSinceLastPromotion, data = employee)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7844 -0.7569  0.2391  0.2431  1.2431 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.756936   0.022281 123.736   <2e-16 ***
    ## YearsSinceLastPromotion 0.001960   0.005722   0.343    0.732    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7067 on 1468 degrees of freedom
    ## Multiple R-squared:  7.995e-05,  Adjusted R-squared:  -0.0006012 
    ## F-statistic: 0.1174 on 1 and 1468 DF,  p-value: 0.732

    #explored linear regression of work life balance against years since last promotion and summarized.

This is a summary of a linear regression of Work life balance against
Years since last promotion in an attempt to distinguish the overall
atmosphere and attitude an employee who stays longer with the company
develops. The results were inconclusive.

    # plot7
    boxplot(MonthlyIncome~WorkLifeBalance,data = employee,xlab="Work Life Balance", ylab="Monthly Income")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    #Created a boxplot using monthly income as the y-axis and Work life balance as x-asxis. Then used appropriate labels 

We wondered whether Performance rating would have an effect on Monthly
income thus we used a box plot to compare the difference performance
ratings beside each other. The conclusion was that there was not enough
significance between the rating groups to emphasize any trend.

    # plot8
    numeric <- employee[, c(1,4,6,7,10)]
    #see correlation between certain features 
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

    flattenCorrMatrix(res2$r, res2$P)

    ##                 row           column          cor            p
    ## 1               Age        DailyRate  0.010660943 6.829722e-01
    ## 2               Age DistanceFromHome -0.001686120 9.484990e-01
    ## 3         DailyRate DistanceFromHome -0.004985337 8.485418e-01
    ## 4               Age        Education  0.208033726 8.881784e-16
    ## 5         DailyRate        Education -0.016806433 5.196625e-01
    ## 6  DistanceFromHome        Education  0.021041825 4.201513e-01
    ## 7               Age   EmployeeNumber -0.010145467 6.975261e-01
    ## 8         DailyRate   EmployeeNumber -0.050990433 5.062898e-02
    ## 9  DistanceFromHome   EmployeeNumber  0.032916408 2.071999e-01
    ## 10        Education   EmployeeNumber  0.042070094 1.068894e-01

    # plot9
    suppressWarnings(chart.Correlation(numeric, histogram=TRUE, pch=19))

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Performing a correlation against Age, daily rate, distance from home,
education, and employee number we did not find any significant
correlation. Thus later on we decided to perform a correlation on all
numeric variables against each other.

    # plot10
    genderIncome <- employee %>% group_by(Gender) %>%
      summarise(meanSalary = mean(MonthlyIncome))
    ggplot(genderIncome, aes (x = Gender, y = meanSalary)) +  ggtitle("Mean monthly income between gender ")+ geom_bar(stat = "identity",width=0.5, position=position_dodge())+
      geom_text(aes(label=round(meanSalary)), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    #Grouped the original data set by gender and summarized average monthly salary. This bar graph represents the average monthly salary general between genders. Used similar labels as exploratory plot 1.

As a general plot we wanted to understand, regardless of job roles,
education, or other factors how does the average monthly income differ
between males and females. The surprising conclusion from the graph was
that females overall made $306 more money than males. It would be then
be helpful to look at our first exploratory plot to understand where
this trend may be coming from.

    # plot11
    ggplot(employee, aes(x = Gender, color = Gender)) +
      geom_bar()+ facet_wrap(~ JobRole)+ggtitle("Job distribution among gender")+ ggtitle("Job distribution across gender ")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    #Decided to develop a visual of the amount of employees based on gender across all departments. Using geom_bar we were able to find interesting distributions.

Understanding how many females and males account for job roles is also
an important question to ask. We may move away from monthly salary and
ask based on the amount of each gender is there a tendency to hire or
attract a certain gender into certain positions. Questions to ask for
instance would be; in the Laboratory technician positions is the gap
there due to a shortage of qualified female laboratory technicians or
are the hiring managers discriminating against females. Now statistics
with inadequate data will not give you the immediate answer but it will
provide a compass of where to look.

    # plot12
    genderJobIncome <- employee  %>% group_by(Gender, JobRole) %>%
      summarise(meanSalarybyJob = mean(MonthlyIncome))
    ggplot(genderJobIncome, aes(x = JobRole,y = meanSalarybyJob, color = Gender)) +
      geom_point() +ggtitle("Mean monthly income among diffrent jobs across gender ")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-13-1.png)

    #By group gender and Job role we then summarized average monthly income and represented it in a scatter plot. Using appropriate labels as exploratory plot 1.

By grouping according to gender and Job role we then summarized the mean
monthly income in order to create a scatterplot that in theory would
emphasize the difference between genders across all job roles. Though a
scatterplot in this situation we found not to be the most influential of
graphs.

    # plot13
    employee$Attrition[employee$Attrition=="Yes"] <- 1
    employee$Attrition[employee$Attrition=="No"] <- 0
    integer_data <-employee[,c(1,2,4,6,7,10,11,13,14,15,17,19,20,21,24,25,26,28:35)]
    integer_data$Attrition <-as.numeric(integer_data$Attrition)
    correlation_data <- cor(integer_data)
    corrplot(correlation_data,method="circle")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    #First replacing the "Yes" and "No" reponses in the original data set under the attrition column to numeric type allowed us to use it with ease in certain facets wraps. By creating a new data set from only the integer based types of the original variables we were able to develop a correlation between all integer variables.

We could have coded a correlation for each variable we may have thought
was important individually but this would have taken too much labor.
Thus what we ended up doing was segmenting the original data set and by
taking only the integer based variables and performing a correlation
analysis. The deeper the blue circle, the higher the correlation between
the two variables is. This gave us a more specific set of variables to
test against one another. The variables we found to be over .70
correlated are:

-   Job Level VS Monthly income
-   Total working years VS Job level
-   Total working years VS Monthly income
-   Total working years VS Age
-   Performance salary hike VS Performance Rating
-   Years at Company VS Years with Current Manager
-   Years at Company VS Years in Current Role
-   Years in Current Role VS Years with Current Manager

<!-- -->

    # plot14
    quit <- filter(employee, Attrition==1)
    ggplot(data=quit)+
      geom_point(aes(y=quit$MonthlyIncome, x=quit$BusinessTravel,color=JobRole))+
      labs(y="Monthly Income", x="Travels")+
      ggtitle("Attritions trend Income VS Travel")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-15-1.png)

    #By segmenting the original data set into only those who have quit, we then used a scatter plot to examine job roles based on travel frequency.

By filtering the employees who have quit, we then graphed monthly
average income by amount traveled by employee and observed that the
largest group who quit were those who “Rarely traveled.” Now what does
this mean? Inconclusive.

    # plot15
    ggplot(data=employee)+
      geom_point(aes(y=employee$MonthlyIncome, x=employee$TotalWorkingYears,color=JobRole))+
      labs(y="Monthly Income", x="Working Years")+
      ggtitle("Observing trend of Income VS Working Years")

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-16-1.png)

    # Created a scatter plot that used Monthly income as the y-axis and x-axis as working years faceted against job roles.

We placed working years as the x-axis and monthly income as the y-axis
to develop a visual of where certain positions lie in terms of working
year experience. As expected the more working years an employee may have
the higher the monthly income. Also with increased working years results
in higher positions such as Manager or Director.

    # plot16
    ggplot(data=employee)+
      geom_point(aes(y=employee$PercentSalaryHike, x=employee$PerformanceRating))+
      labs(y="Percentage Hike", x="Performance Rating")+
      ggtitle("Attritions trend Percentage Hike VS Performance Rating")+
      facet_wrap(~Attrition)

![](exploratory_analysis_files/figure-markdown_strict/unnamed-chunk-17-1.png)

    # Created scatter plot with Percent salary hike as y-axis and Performance rating as x-axis.

Using the entire dataset we set Performance rating as the independent
variable and Performance salary hike as the dependent variable. What we
observed was that Performance rating only resulted in a rating of 3 or
4, which made an interesting find as a trend of the company but at the
same time did not provide much to conclude on.

Results
=======

Some results we have found
--------------------------

-   In IBM, average monthly salary of female(6686.566) is higher than
    that of male (6380.508), which is out of what we expected
-   Across all job roles, the different pay between women and men are
    marginal
-   On higher position such as manager or manufacturing director, number
    of female and male are pretty much equal. But positions of sales
    executive and research scientist have attracted more male employees
    than females
-   As for marital status, people married or divorced have higher income
    than people are single, which is interesting to see. It’s also
    reasonable because they may have longer working years and higher
    position level. In this case, we plan to explore if job position
    concur with marital status
-   The performance rating in the dataset is either 3 or 4 (good or
    excellent), maybe everyone at IBM is good at their work
-   There is no strong correlation between work life balance and years
    in company or years in position. Also monthly income has trivial
    influence on work life balance
-   There is positive linear relationship between years at company and
    years since last promotion, with a significant adjusted R^2 0.382

Next steps
----------

-   Continue running on feature selection to delete irrelevant ones
-   Continue on model selection and compare the accuracy between
    different models
-   Perform logistic regression on employee attrition
-   Once we finish all analysis, we will put everything in shiny app to
    form a complete project
