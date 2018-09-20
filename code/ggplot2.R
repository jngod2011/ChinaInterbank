data(ToothGrowth)
data(mtcars)

# Convert dose and cyl columns from numeric to factor variables
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
mtcars$cyl <- as.factor(mtcars$cyl)
head(ToothGrowth)
head(mtcars)

library(ggplot2)
# Box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()

ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot(fill='blue', color="red")
# scatter plot
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(color='darkblue')
