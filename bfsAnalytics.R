library(readr)
BF = read_csv("./Data/BlackFriday.csv")
library(tidyverse)
library(knitr)
library(RColorBrewer)
options(scipen = 20)

variable_type <- matrix(c("User_ID", "Product_ID", "Gender", "Age", "Occupation", "City_Category", "Stay_In_Current_City_Years", "Marital_Status","Product_Category_1","Product_Category_2","Product_Category_3","Purchase","Nominal Discrete","Nominal Discrete","Nominal Discrete","Oridinal Discrete","Nominal Discrete","Nominal Discrete","Oridinal Discrete","Nominal Discrete","Nominal Discrete","Nominal Discrete","Nominal Discrete","Ratio Continuous"), ncol = 12, byrow = TRUE)
rownames(variable_type) <- c("variable","type")
print(variable_type)

#data cleaning
BF[is.na(BF)] = 0
BF <- BF %>% mutate_if(sapply(BF,is.character),as.factor)

BF <- BF %>% mutate_if(sapply(BF,is.integer),as.factor)

BF <- BF %>% mutate_if(sapply(BF,is.numeric),as.factor)  

BF$Purchase <- as.numeric(BF$Purchase)
str(BF)

#CHART 1

Age_purchase <- BF %>%group_by(Age) %>% 
  summarize(n = n_distinct(User_ID),Total_Purchase = sum(Purchase))%>%
  mutate(Avg_Purchase = Total_Purchase/n)

View(Age_purchase)

Age_purchase %>% ggplot(aes(x= Age, y = Avg_Purchase))+
  geom_bar( stat = "identity",alpha = 0.6 , width = 0.5, fill = "cornflowerblue") +
  labs(y= "Avg Purchase")+
  ggtitle("AGE VS PURCHASE") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(0,200000,400000,600000), labels = c("0","200k","400k","600k"))
#Conclusion  
#As the age grows, total purchase of customers first goes up and then decreases. Young people have the strongest purchasing power. Customers from 26 to 35 years old purchase the most, buying above 750 thousand dollars on average. 18 to 25 year-old customers and 36 to 45 year-old customers have similar purchasing power, their average purchases are about 650 thousand dollars. People in other age groups have relatively small purchasing power.

#CHART2

genderPurchase <- BF %>% group_by(User_ID,Gender) %>%
  summarise(n = n_distinct(User_ID), totalPurchase = sum(Purchase)) %>%
  mutate(avgPurchase = totalPurchase/n)

genderPurchase %>% ggplot(aes(x=Gender,y=avgPurchase, fill = Gender)) +
  labs(title = "Gender vs Purchase",y= "Average Purchases")+
  geom_boxplot()+ scale_y_log10()
#Conclusion  
#We can conclude from the graph that males have a wider distribution interval than female. This result might be influenced by the quantity of value in the data frame, but the main idea is that males are capable of providing a wider range of choices for stores. Therefore, we made preliminary conclusion that, on average, males purchase more than females during the Black Friday.

#CHART 3

maritalPurchase <- BF %>% group_by(City_Category,Marital_Status) %>%
  summarise(n=n_distinct(User_ID),totalPurchase = sum(Purchase)) %>%
  mutate(avgPurchase = totalPurchase/n)

maritalPurchase %>% ggplot(aes(x= City_Category,y = avgPurchase ,fill = Marital_Status)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  scale_y_continuous(breaks = seq(0,1000000,500000), labels = c("0","500k","1m")) +
  labs(title = "Marrige relation with city wise purchase", x = "Cities", y = "averagePurchase") 
  
#no distinct relationship

#CHART 4

occupationalPurchase <- BF %>% group_by(Occupation) %>%
  summarise(n=n_distinct(User_ID), totalPurchase = sum(Purchase)) %>%
  mutate(avgPurchase = totalPurchase/n)

occupationalPurchase %>% ggplot(aes(x=Occupation,y=avgPurchase, fill= Occupation)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,800000,200000), labels = c("0","200k","400k","600k","800k"))+
  labs(title = "Occupation and purchase", x="Occupation", y="Average Purchase")+
  coord_flip()


#CHART5

citygenPurchase <- BF %>% group_by(City_Category,Gender) %>%
  summarise(n = n_distinct(User_ID), totalPurchases = sum(Purchase)) %>%
  mutate(avgPurchases = totalPurchases/n)

citygenPurchase %>% ggplot(aes(x=City_Category,y=avgPurchases, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5) + 
  labs(title ="City wise gender purchases", x= "Cities", y="Average Purchases")+
  scale_y_continuous(breaks = seq(0,1500000,500000), labels = c("0","500k","1m","1.5m"))
 
#female purchases are relatively more than that of males

#CHART 6
Stay_Purchase = BF %>%
  group_by(Stay_In_Current_City_Years, City_Category) %>% 
  summarise(n = n_distinct(User_ID),
            Total_Purchase = sum(Purchase)) %>%
  mutate(Avg_Purchase = Total_Purchase/n)
ggplot(Stay_Purchase, aes(x = Stay_In_Current_City_Years, y = Avg_Purchase, fill = City_Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Relationship between Avg Purchase and Living Time", x = "Stay in Current City Years", y = "Avg Purchase", fill = "City Category") +
  scale_y_continuous(breaks = c(0,300000,600000,900000), labels = c("0", "300k", "600k", "900k")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = brewer.pal(n = 3, name = "Set2"))
#From this chart, we can conclude that people who stay in current cities for less than one year are more likely to spend more than people who stay for a long time. In addition, people in City A and City B have stronger purchasing power than those in City C.


#CHART 7
Category1_Purchase = BF %>%
  group_by(Product_Category_1) %>% 
  summarize(Times_Purchased = n(), Total_Purchase = sum(Purchase), Total_Customer = sum(n_distinct(User_ID)))
Category1_Purchase1 = data.frame(Category1_Purchase$Product_Category_1, Category1_Purchase$Times_Purchased, Category1_Purchase$Total_Purchase,Category1_Purchase$Total_Customer)
colnames(Category1_Purchase1) = c("Product Category 1", "Number of Products Purchased", "Total Amount Purchased ($)", "Total Number of Customers")
knitr::kable(Category1_Purchase1)





#PRELIMNARY STATISTICAL ANALYSIS

#STATISTICAL ANALYSIS 1

#IF MARITIAL STATUS IS A DETERMINANT OF PURCHASE AMOUNT ACROSS 3 CITIES

cityA0 <- BF %>%
  filter(City_Category == "A" & Marital_Status == 0) %>%
  group_by(User_ID)%>%
  summarise(totalPurchase = sum(Purchase))

cityA1 <- BF %>%
  filter(City_Category == "A" & Marital_Status == 1) %>%
  group_by(User_ID)%>%
  summarise(totalPurchase = sum(Purchase))

cityB0 <- BF %>%
  filter(City_Category == "B" & Marital_Status == 0) %>%
  group_by(User_ID)%>%
  summarise(totalPurchase = sum(Purchase))

cityB1 <- BF %>%
  filter(City_Category == "B" & Marital_Status == 1) %>%
  group_by(User_ID)%>%
  summarise(totalPurchase = sum(Purchase))

cityC0 <- BF %>%
  filter(City_Category == "C" & Marital_Status == 0) %>%
  group_by(User_ID)%>%
  summarise(totalPurchase = sum(Purchase))

cityC1 <- BF %>%
  filter(City_Category == "C" & Marital_Status == 1) %>%
  group_by(User_ID)%>%
  summarise(totalPurchase = sum(Purchase))
  
t.test(cityA0$totalPurchase, cityA1$totalPurchase)

t.test(cityB0$totalPurchase, cityB1$totalPurchase)

t.test(cityC0$totalPurchase, cityC1$totalPurchase)

# From the third plot we made earlier, we were unable to see a significant difference in average purchase between unmarried and married shoppers across three cities. That is why we did three t-tests here to see if a difference actually exists.  
# We chose to use t-test here because it can tell us if a difference between two population means exist. T-tests work between two continuous variables, purchases in this case.  
# We are trying to answer the question of whether a true difference in average purchase between married and unmarried shoppers exists across three cities.  
# We are unable to reject the null in all three t-tests, given p-values that large. Therefore, we conclude that there is a big chance that the true difference is actually 0 in all three cities. In other words, marriage does not really influence purchase on any significant scale.


#Statistical Analysis 2

#if AGE is Correlated with Purchase

age1 <- BF %>% 
  group_by(User_ID,Age) %>%
  summarise(totalPurchase = sum(Purchase))

age1$Age = as.numeric(age1$Age)
cor(age1$Age,age1$totalPurchase)

#RESULT -0.06819

#if OCCUPATION is correlated with Purchase

occ1 <- BF %>%
  group_by(User_ID,Occupation) %>%
  summarise(totalPurchase = sum(Purchase))

occ1$Occupation <- as.numeric(occ1$Occupation)
cor(occ1$Occupation,occ1$totalPurchase)

#0.00100 ,The correlation is too weak to be anything significant. Therefore, we conclude that there is no correlation between the two variables.

#STATISTICAL ANALYSIS 4

#GENDER RELATIONSHIP WITH PURCHASES

genderR <- BF %>%
  group_by(User_ID,Gender) %>%
  summarise(avgPurchase = mean(Purchase))

PG <- lm(avgPurchase ~ Gender, data = genderR)
summary(PG)

# 
# From the regression, we can see that on average, men would purchase 669.29 dollars more than women on Black Friday.  
# The null hypothesis is βi equals to 0. As the p-value is extremely small, we fail to reject the null hypothesis. Therefore gender does have an effect on average purchase.




# Conclusion
# 
# In conclusion, our analysis shows:  
#   1. Data visualizations are essential for firms to understand customers and their behaviors because plots and tables can directly display the relationship among factors at the first glance.  
# 2. The stores could pay more attention to target their customers in males at the age from 26 to 35 years old in the City A and City B. Furthermore, these stores could make more promotions and improvements on the Product 1 and 5. We made this prediction because this group of people took up the most significant part of the purchase amounts, and the two products are the two most popular products during the Black Friday. Also, to generate more profits, these stores can also focus on both the customers whose occupations are in 5, 19, 20; and the customers who stay in current cities for less than one year.  
# 3. Both our visualizations and statistical analyses show that the marriage variable is not the key determinant of the purchase because the t-test showed that it is not statistically significant. Another interesting finding that we may extend more analyzes in the future is the correlations between age and purchase because the result looks like a non-linear regression but the visualization part showed that age matters in analyzing the customer groups.
