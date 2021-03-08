#The Advertising data displays sales (in thousands of units) for a particular product as a 
#function of advertising budgets (in thousands of dollars) for TV, radio, and newspaper 
#media.

# Questions- 
#  1 - Is there a relationship between advertising budget and sales?
#  2 - How strong is the relationship between the advertising budget and sales?
#  3 - Which media contribute to sales?
#  4 - How can we predict future sales?

Adv_1 = read_xlsx("Advertising.xlsx")
Adv_2 = read_xlsx("Advertising.xlsx")
Adv_3 = read_xlsx("Advertising.xlsx")
Adv_4 = read_xlsx("Advertising.xlsx")

#1- To find the relationship between advertising budget for each source of advertising
#   and the overall sales, we need to perform correlation between each individually.
#   We can use Regression, and find the p-value, if the p-value is less than 0.05,
#   then there is a relationship between advertising and sales. 


#lm is the function used for regression in R
#lm(dependent~independent+independent+independent, data=name of data set)

regression = lm(sales ~ TV + radio + newspaper, data = Adv_1)
summary(regression)

#There is a relationship between advertising budget and sales, as the p-value is 
#significantly less than 0.05. 


#2- To find the strength of the relationship between advertising budget and sales we
#   need to perform correlation. 

head(Adv_2)
cor.test(Adv_2$TV, Adv_2$sales)
cor.test(Adv_2$radio, Adv_2$sales)
cor.test(Adv_2$newspaper, Adv_2$sales)

plot(Adv_2$TV, Adv_2$sales)
plot(Adv_2$radio, Adv_2$sales)
plot(Adv_2$newspaper, Adv_2$sales)


# We can say that there is a strong porisitve relationship between advertising budget 
# of TV and sales, a intermediate positive relationship between radio budget and sales
# and a weak positive relationship between newspaper budget and sales. 

#3- To find which media contributes to sales the most we need to find R-Sqaure, which
#   will tell us the percentage of variance in the sales explained by media. 

r_square <- lm(sales ~ TV + radio + newspaper, data = Adv_3)
r_square
summary(r_square)$r.squared

#We can say that almost 90% of the sales in explained by the different media's of 
#advertisement. 


#4- To predict future sales we need to find the error values. 

future_values <- lm(sales ~ TV + radio + newspaper, data = Adv_4)
future_values
summary(future_values)$coefficients[5:8]

#The predicted future sales for TV, radio, and newspaper are found. 
