#Load data
#df<- read.csv("/cloud/project/HollywoodsMostProfitableStories.csv")

#Take a look at the data:
View(HMPS)

#Load Library:
install.packages("tidyverse")

#Import Library
library(tidyverse)

# Check data types:
str(HMPS)

# Check for missing values:
colSums(is.na(HMPS))

#Drop missing values
HMPS<- na.omit(HMPS)

# check to make sure that the rows have been removed
colSums(is.na(HMPS))

#Check for duplicates
#dim(HMPS[duplicated(HMPS$Film),])
HMPS <- HMPS [!duplicated(HMPS), ]

#round off values to 2 places
HMPS$Profitability <- round(HMPS$Profitability ,digit=2)
HMPS$Worldwide.Gross <- round(HMPS$Worldwide.Gross ,digit=2)

#View(df)
dim(HMPS)

#Check for outliers using a boxplot
library(ggplot2)

#Create a boxplot that highlights the outliers
ggplot(HMPS, aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

#Remove outliers in 'Profitability'
Q1 <- quantile(HMPS$Profitability, .25)
Q3 <- quantile(HMPS$Profitability, .75)
IQR <- IQR(HMPS$Profitability)

no_outliers <- subset(HMPS, HMPS$Profitability> (Q1 - 1.5*IQR) & HMPS$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers)

# Remove outliers in 'Worldwide.Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

HMPS1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(HMPS1)

#Summary Statistics/Univariate Analysis:
summary(HMPS1)

#bivariate analysis

#scatterplot
ggplot(HMPS1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))


#bar chart
ggplot(HMPS1, aes(x=Year)) + geom_bar()

#Export clean data
write.csv(HMPS1, "clean_HMPS.csv")
