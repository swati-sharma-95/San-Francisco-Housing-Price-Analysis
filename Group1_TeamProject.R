df = read.csv('sf_clean.csv')
head(df)
summary(df$price)
dim(df)
View(df)

colnames(df)
lapply(df, class)
df$beds <- as.factor(df$beds)
df$bath <- as.factor(df$bath)
df$hood_district <- as.factor(df$hood_district)

#Univariate EDA
table(df$beds)    #more than 50% houses have either 1 or 2 beds
table(df$bath)    #more than 50% houses have only one bath
table(df$laundry) #most of the houses have in-unit laundry
table(df$pets)    #more than 50% houses don't allow any pets
table(df$housing_type)   
table(df$parking)    
table(df$hood_district)     #more than 50% houses are available in district 8-9
summary(df$price)
summary(df$sqft)

#Bivariate EDA
table(df$beds,df$bath)
table(df$pets, df$housing_type)
table(df$parking, df$hood_district)

#visualization
library('ggplot2')

##Price

ggplot(df, aes(x=beds, y=price))+geom_boxplot(fill = "cornflowerblue", alpha = .7) + ggtitle('House prices vs number of bedrooms')
ggplot(df, aes(x=bath, y=price))+geom_boxplot(fill = "cornflowerblue", alpha = .7) +ggtitle('House prices vs number of baths')
ggplot(df, aes(x=laundry, y=price))+geom_boxplot(fill = "cornflowerblue", alpha = .7) +ggtitle('House prices vs laundry units')
ggplot(df, aes(x=parking, y=price))+geom_boxplot(fill = "cornflowerblue", alpha = .7) +ggtitle('House prices vs parking availablity')
ggplot(df, aes(x=pets, y=price))+geom_boxplot(fill = "cornflowerblue", alpha = .7) +ggtitle('House prices vs pets allowed')
ggplot(df, aes(x=housing_type, y=price))+geom_boxplot(fill = "cornflowerblue", alpha = .7) +ggtitle('House prices vs housing type')
ggplot(df, aes(x=hood_district, y=price))+geom_boxplot(fill = "cornflowerblue", alpha = .7) +ggtitle('House prices vs district')

##SQ-foot

ggplot(df, aes(x=beds, y=sqft))+geom_boxplot()
ggplot(df, aes(x=bath, y=sqft))+geom_boxplot()
ggplot(df, aes(x=laundry, y=sqft))+geom_boxplot()
ggplot(df, aes(x=parking, y=sqft))+geom_boxplot()
ggplot(df, aes(x=pets, y=sqft))+geom_boxplot()
ggplot(df, aes(x=housing_type, y=sqft))+geom_boxplot()
ggplot(df, aes(x=hood_district, y=sqft))+geom_boxplot()

ggplot(df, aes(x=beds, fill=bath))+geom_bar(position = 'stack') + ggtitle('Count of houses with different number of beds and baths')
ggplot(df, aes(x=pets, fill=housing_type))+geom_bar(position = 'stack') + ggtitle('Count of houses with differnt housing type and allowance of pets')

ggplot(df, aes(x=beds, fill=laundry))+geom_bar(position = 'stack')
ggplot(df, aes(x=bath, fill=laundry))+geom_bar(position = 'stack')

##price-sqft
ggplot(df, aes(x=sqft, y=price))+geom_point()+
  geom_smooth(method = "lm") + ggtitle('Price of house vs SQFT')



#probablity dist

ggplot(df, aes(x=price))+geom_histogram(fill = "cornflowerblue", color = "white",bins = 50)
ggplot(df, aes(x=price))+geom_density(fill = "indianred3")

ggplot(df, aes(x=sqft))+geom_histogram(fill = "cornflowerblue", color = "white",bins = 50)
ggplot(df, aes(x=sqft))+geom_density(fill = "indianred3")



#hypothesis testing

## Tesing the hypothesis that the mean sqft of the houses in district 9 are equal to the mean sqft of houses in all district 

df2 <- subset(df, hood_district == 9)
true_mean = mean(df$price)
true_sqft = mean(df$sqft)

t.test(x=df2$sqft, mu= true_sqft, alternative = 'two.sided', conf.level = 0.95)
## Since the p value is 0.7897 which is a lot higher than 0.05, We can accept our null hypothesis



#probability dist

## Finding the probability of  of a house with sqft > 2500

mean(df$sqft)
sd(df$sqft)
summary(df$sqft)

pnorm(2500, mean = true_sqft, sd = sd(df$sqft), lower.tail = FALSE)
## The probability of a house with sqft more than 2500 is 0.0006652677, Which is very low

