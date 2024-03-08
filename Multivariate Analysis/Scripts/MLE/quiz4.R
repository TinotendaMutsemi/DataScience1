
#read csv data
data <- read.csv("Air Pollution Data.csv")

#construct a qq plot for Radiation
qqnorm(data$Radiation)
qqline(data$Radiation)

#Calculate the correlation coefficient between the observed and theoretical quantiles, rq
rq <- cor(qnorm(ppoints(data$Radiation)), sort(data$Radiation))
rq

# 0.9599 0.9726 0.9771 for alpha = 0.001, 0.05, 0.1  respectively

#calaculate the mahalanobis() distance for N02 and O3
distances <- mahalanobis(data[,c("NO2","O3")], colMeans(data[,c("NO2","O3")]), cov(data[,c("NO2","O3")]))

#If these variables are bivariate normally distributed, we would expect 50% of the squared distances to be below
#the 0.5 quantile of the chi-squared distribution with 2 degrees of freedom.
qchisq(0.5, 2)

#distances below 0.5 quantile of the chi-squared distribution with 2 degrees of freedom
sum(distances < qchisq(0.5, 2))/length(distances)
