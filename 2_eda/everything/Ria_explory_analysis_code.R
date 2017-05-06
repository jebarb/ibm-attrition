library(tidyverse)
train <- read_csv('/Users/riapan/Desktop/final_project_390/HR-Employee-Attrition.csv')

#study relationships between worklifebalance VS years since last promotion 
lin_reg <- lm(WorkLifeBalance~ YearsSinceLastPromotion, train)
summary(lin_reg)
#seems no correlation 

lin_reg <- lm(YearsAtCompany~ YearsSinceLastPromotion, train)
summary(lin_reg)
#has some correlation. the corrilation is significant with p= , and the R^2 is 0.38 with p = ..

# how does performance rating affects monthly wages 
boxplot(MonthlyIncome~WorkLifeBalance,data = train,xlab="PerformanceRating", ylab="MonthlyIncome")
# no huge difference 
lin_reg <- lm(MonthlyIncome ~ WorkLifeBalance, train)
summary(lin_reg)
#not significant 

# then see the cor between years at company VS years since last promotion 
test <- train[, c('YearsAtCompany','YearsSinceLastPromotion')]
testCor <- cor(test)
testCor

#distribution of years at company VS worklifeBalance  , x needs to be categorical 
boxplot(YearsAtCompany~WorkLifeBalance,data = train,xlab="balance", ylab="years at company")

# correlations between different features -----------------------------------------------------------------------
numeric <- train[, c(1,4,6,7,10)]
numeric <- train[,c(1,4,6,7,10,11,13,14,15,17,19,20,21,24,25,26,28,29,30,31,32,33,34,35)]
res <- cor(numeric, method = c("pearson"))
round(res, 2)
# indicates the correlation coefficient to be computed. The default is pearson correlation coefficient
#which measures the linear dependence between two variables

# It returns both the correlation coefficients and the p-value of the correlation for all possible pairs 
#of columns in the data table.
library("Hmisc")
res2 <- rcorr(as.matrix(numeric)) # data must be a matrix 
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# form better matrix ------------------------------------------------------

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
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

# visualize ---------------------------------------------------------------
library(corrplot)
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank",  tl.col = "black", tl.srt = 45)
#Positive correlations are displayed in blue and negative correlations 
#in red color. Color intensity and the size of the circle are proportional to the
#correlation coefficients. In the right side of the correlogram, the legend color 
#shows the correlation coefficients and the corresponding colors.
#In the above plot, correlations with p-value > 0.05 are considered as insignificant. 
#In this case the correlation coefficient values are leaved blank or crosses are added.

library("PerformanceAnalytics")
chart.Correlation(numeric, histogram=TRUE, pch=19)

#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)

# Get some colors
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res2$r, col = col, symm = TRUE)
#x : the correlation matrix to be plotted
#col : color palettes
#symm : logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.


# will gender affects job role -------------------------------------------

#mean income across jobs between genders 
genderJobIncome <- train  %>% group_by(Gender, JobRole) %>%
    summarise(meanSalarybyJob = mean(MonthlyIncome))
ggplot(genderJobIncome, aes(x = JobRole,y = meanSalarybyJob, color = Gender)) +
    geom_point()

# mean salary across gender
genderIncome <- train  %>% group_by(Gender) %>%
    summarise(meanSalary = mean(MonthlyIncome))
ggplot (genderIncome, aes (x = Gender, y = meanSalary)) + geom_bar(stat ="identity")+
    ggtitle("Mean monthly income between gender ")

#job distribution between gender 
#graph 1 
ggplot(train, aes(x = Gender, y =JobRole, fill = JobRole)) +
    geom_bar(stat="identity")

#graph 2 
ggplot(employee, aes(x = Gender, color = Gender)) +
    geom_bar()+ facet_wrap(~ JobRole)+ggtitle("Job distribution among gender")+ 
    ggtitle("Job distribution across gender ")
