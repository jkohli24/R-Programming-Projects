#Project 2
dir()
setwd("~/desktop/COURSEWORK/R/rossmann-store-sales")
# Load Libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(scales)
library(ggrepel)
library(lubridate)
library(gridExtra)

# We combine train.csv and store.csv into a single table called combined.csv using SQL 
# Step 1: Load Data
combined <- read.csv("combined.csv")
test <- read.csv("test.csv")

# Step 2: Exploring Data
dim(combined)
# Exploring some of the most important variables
# 1. The response variable: Sales
ggplot(data=combined[!is.na(combined$Sales),], aes(x=Sales)) +
  geom_histogram(fill="blue", binwidth = 1000) +
  scale_x_continuous(breaks= seq(0, 80000, by=10000), labels = comma)
# We need to continue dealing with zero sales.

# 2. The most important numeric predictors
# How many numeric variables do we have?
numericVars <- which(sapply(combined, is.numeric)) 
numericVarNames <- names(numericVars) 
cat('There are', length(numericVars), 'numeric variables')
# Which numeric variable has the highest correlation with the sales？
all_numVar <- combined[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'Sales'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.06)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
# Select only 2 high correlations
# Customers/Open have the highest correlation with Sales 

# We further investgate the two variables with the highest correlations with sales
# 3. Plot to see the relationship between customers numbers and sales
ggplot(data=combined[!is.na(combined$Sales),], aes(x=Customers, y=Sales))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 80000, by=10000), labels = comma) +
  geom_text_repel(aes(label = ifelse(combined$Customers[!is.na(combined$Sales)]>6000, rownames(combined), '')))
# We found a data with relatively high amount of customers but normal level of sales.
# We will not take it out yet as taking outliers can be dangerous.

# 4. Plot to see the relationship between sales and open
ggplot(data=combined[!is.na(combined$Sales),], aes(x=factor(Open), y=Sales))+
  geom_boxplot(col='blue') + labs(x='Open or Not') +
  scale_y_continuous(breaks= seq(0, 80000, by=10000), labels = comma)
# There is no sales when the store are closed. We choose to drop the data when store are closed.

# Step 3: Clean Data
# 1. Drop data when stores are closed & when stores are open but sales is zero 
clean_sales <- subset(combined, Open==1 & Sales!=0)
dim(clean_sales)

# 2. Check completeness of dataset
NAcol <- which(colSums(is.na(clean_sales)) > 0)
sort(colSums(sapply(clean_sales[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

# 3. Deal with variables with missing values(Promo2SinceWeek,Promo2SinceYear,CompetitionOpenSinceMonth,CompetitionOpenSinceYear,CompetitionDistance)
# a. Replace Nas in Promo2SinceWeek & Promo2SinceYear with 0
clean_sales$Promo2SinceWeek[is.na(clean_sales$Promo2SinceWeek)] <-0
clean_sales$Promo2SinceYear[is.na(clean_sales$Promo2SinceYear)] <-0
# b. Replace Nas in CompetitionOpenSinceMonth & CompetitionOpenSinceYear with 0
clean_sales$CompetitionOpenSinceMonth[is.na(clean_sales$CompetitionOpenSinceMonth)] <-0
clean_sales$CompetitionOpenSinceYear[is.na(clean_sales$CompetitionOpenSinceYear)] <-0
# c. Replace Nas in CompetitionDistance with median of Competition Distance (because it is skewed distributed)
clean_sales$CompetitionDistance[is.na(clean_sales$CompetitionDistance)] <- median(clean_sales$CompetitionDistance, na.rm = TRUE)

# Step 4: Visiualize Clean Data
# 1. Find out the relationship among store types and sales and promo
# Create a table of monthly sales per customer
# Extract year-month from Date and insert table
monthly_sales <- clean_sales
monthly_sales$year_month <- format(mdy(monthly_sales$Date), "%y-%m")
monthly_sales <- ddply(monthly_sales, .(StoreType, year_month, Promo), summarise, Sales = sum(Sales), Customers = sum(Customers))
monthly_sales$SalesPerCustomers <- (monthly_sales$Sales / monthly_sales$Customers) 
monthly_sales$Promo <- as.logical(monthly_sales$Promo)
# Plot
pic1 <- ggplot(data = subset(monthly_sales, monthly_sales$StoreType == 'a'), aes(x=year_month, y=SalesPerCustomers, group = Promo, color = Promo))  +
  geom_line() + 
  labs(title="Sales Per Customers(in Dollar) Vs Promotion", subtitle="Store Type A", x = "Sales Per Customers(in dollars)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
pic2 <- ggplot(data = subset(monthly_sales, monthly_sales$StoreType == 'b'), aes(x=year_month, y=SalesPerCustomers, group = Promo, color = Promo))  +
  geom_line() + 
  labs(title="Sales Per Customers Vs Promotion", subtitle="Store Type B", x = "Sales Per Customers(in dollars)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
pic3 <- ggplot(data = subset(monthly_sales, monthly_sales$StoreType == 'c'), aes(x=year_month, y=SalesPerCustomers, group = Promo, color = Promo))  +
  geom_line() + 
  labs(title="Sales Per Customers Vs Promotion", subtitle="Store Type C", x = "Sales Per Customers(in dollars)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
pic4 <- ggplot(data = subset(monthly_sales, monthly_sales$StoreType == 'd'), aes(x=year_month, y=SalesPerCustomers, group = Promo, color = Promo))  +
  geom_line() + 
  labs(title="Sales Per Customers Vs Promotion", subtitle="Store Type D", x = "Sales Per Customers(in dollars)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
grid.arrange(pic1,pic2,pic3,pic4, layout_matrix = rbind(c(1), c(2), c(3), c(4)))

# 2. Find out how promotion affect weekday's sales
daysofweek_sales_promo <- subset(clean_sales, DayOfWeek != 6 & DayOfWeek != 7 & Promo == 1)
daysofweek_sales_nopromo <- subset(clean_sales, DayOfWeek != 6 & DayOfWeek != 7 & Promo == 0)
daysofweek_sales_promo <- ddply(daysofweek_sales_promo, .(StoreType, DayOfWeek, Promo), summarise, Sales = sum(Sales), Customers = sum(Customers))
daysofweek_sales_nopromo <- ddply(daysofweek_sales_nopromo, .(StoreType, DayOfWeek, Promo), summarise, Sales = sum(Sales), Customers = sum(Customers))
daysofweek_sales_promo$SalesPerCustomers <- (daysofweek_sales_promo$Sales / daysofweek_sales_promo$Customers) 
daysofweek_sales_nopromo$SalesPerCustomers <- (daysofweek_sales_nopromo$Sales / daysofweek_sales_nopromo$Customers) 
# Plot
pic5 <- ggplot(data = subset(daysofweek_sales_promo, daysofweek_sales_promo$StoreType == 'a'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend with Promotion", subtitle = "Store Type A", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))
pic6 <- ggplot(data = subset(daysofweek_sales_promo, daysofweek_sales_promo$StoreType == 'b'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend with Promotion", subtitle = "Store Type B", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))
pic7 <- ggplot(data = subset(daysofweek_sales_promo, daysofweek_sales_promo$StoreType == 'c'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend with Promotion", subtitle = "Store Type C", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))
pic8 <- ggplot(data = subset(daysofweek_sales_promo, daysofweek_sales_promo$StoreType == 'd'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend with Promotion", subtitle = "Store Type D", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))
pic9 <- ggplot(data = subset(daysofweek_sales_nopromo, daysofweek_sales_nopromo$StoreType == 'a'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend without Promotion", subtitle = "Store Type A", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))
pic10 <- ggplot(data = subset(daysofweek_sales_nopromo, daysofweek_sales_nopromo$StoreType == 'b'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend without Promotion", subtitle = "Store Type B", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))
pic11 <- ggplot(data = subset(daysofweek_sales_nopromo, daysofweek_sales_nopromo$StoreType == 'c'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend without Promotion", subtitle = "Store Type C", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))
pic12 <- ggplot(data = subset(daysofweek_sales_nopromo, daysofweek_sales_nopromo$StoreType == 'd'), aes(x = DayOfWeek, y = SalesPerCustomers, fill = SalesPerCustomers)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(SalesPerCustomers, 2)), hjust=1.6, color="white", size=3.5) +
  labs(title = "Weekday Sales Trend without Promotion", subtitle = "Store Type D", y = "Sales Per Customers(in dollars)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 13))

grid.arrange(pic5,pic6,pic9,pic10, ncol = 2)
grid.arrange(pic7,pic8,pic11,pic12, ncol = 2)

#3.Highest Store Sales per day
popular_shops <-combined%>% group_by(Store) %>% summarize(ICcount = sum(Sales)) %>% ungroup() %>% arrange(desc(ICcount))
head(popular_shops,5)
options(scipen=999)
# Plot
head(popular_shops,10)%>% ggplot(aes(x = reorder(as.factor(Store), ICcount), y = ICcount,fill=as.factor(Store))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Per day sales', x = 'Store ID', title = 'Highest Store Sales per day') +
  coord_flip()

#4.Store Sales vs. Competition
Sales_Dist <- aggregate(clean_sales$Sales ~ clean_sales$Store + clean_sales$CompetitionDistance, clean_sales, sum)
#Plot
plot_theme <- theme(plot.title = element_text(family = "Palatino", colour = "dodgerblue4", face = "bold", size = (15)), 
                    axis.title = element_text(family = "Helvetica", size = (10), colour = "dodgerblue4"),
                    axis.text = element_text(family = "Helvetica", colour = "dodgerblue4", size = (10)))
Sales_Dist$pc <- predict(prcomp(~Sales_Dist$`clean_sales$CompetitionDistance`+Sales_Dist$`clean_sales$Sales`, Sales_Dist))[,1]
myplot <- ggplot(Sales_Dist, aes(Sales_Dist$`clean_sales$CompetitionDistance`, Sales_Dist$`clean_sales$Sales`, color = pc)) +
  geom_point(shape = 16, size = 4, show.legend = FALSE, alpha = .4) + ggtitle("Store Sales vs Competition") + labs(y="Total Sales", x = "Distance From The Nearest Competitor")+
  theme_minimal() +
  scale_color_gradient(low = "#5994f9", high = "#f0650e")
options(scipen = 999)

print(myplot + plot_theme)

# Step 5: Training Model
# 1.Splitting the dataset into the Training set and Test set
dataset <- clean_sales
library(caTools)
set.seed(123)
split = sample.split(dataset$Sales, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
# 2. Feature Scaling
training_set = scale(training_set)
test_set = scale(test_set)
# 3. Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Sales ~ Customers,
               data = dataset)
# 4. Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

write.csv(y_pred, file = "submission.csv")

