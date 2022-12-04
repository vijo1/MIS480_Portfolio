setwd("/Users/vikus/Desktop/School Stuff/MIS 480/Data") 
admissions <- read.csv("adm.csv", stringsAsFactors = TRUE)
str(admissions)
summary(admissions)


#remove rows with missing values
admissions<-admissions[complete.cases(admissions[ , c('LSAT', 'GPA', 'resident', 'Gender', 'admit')]),]


#Removing rows with outlier values
admissions<- subset(admissions, LSAT<=180 & GPA<=4) 

summary(admissions)

#Plotting histograms of LSAT and GPA

par(mfrow=c(2,2))
hist(admissions$LSAT)
hist(admissions$GPA)

#creating pie charts


library(dplyr)

# Data transformation for admission percentages 
transformed <- admissions %>% 
  group_by(admit) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

library(ggplot2)

ggplot(transformed, aes(x = "", y = perc, fill = admit==1)) +
  geom_col(width = 1, color = 3) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  theme_void()


#creating a bar plot for race and admission

ggplot(data = admissions, aes(x = admit, fill=Race)) +
  geom_bar( position=position_dodge())

#creating a bar plot for gender and admission

ggplot(data = admissions, aes(x = admit, fill = Gender == 1)) +
  theme(legend.title=element_blank())+
  geom_bar( position=position_dodge())

#creating a bar plot for residence and admission

ggplot(data = admissions, aes(x = admit, fill = resident == 1)) +
  theme(legend.title=element_blank())+
  geom_bar()


#Creating a scatter plot of LSAT and GPA, split by admission status

ggplot(data = admissions, aes(x = GPA, y=LSAT, col = admit == 1)) +
  theme(legend.title=element_blank())+
  geom_point()

#Creating a scatter plot of LSAT and GPA, split by URM status

ggplot(data = admissions, aes(x = GPA, y=LSAT, col = URM == 1)) +
  theme(legend.title=element_blank())+
  geom_point()




# Prep data for the logistic regression. Partition data into training and testing partitions with 70% going into training and 30% into the testing set.
library(caret)

intrain<- createDataPartition(admissions$admit,p=0.7,list=FALSE)
set.seed(2017)
training<- admissions[intrain,]
testing<- admissions[-intrain,]


# Create a Logistic Model for Admission with training data

LogModel <- glm(admit ~ LSAT + GPA + resident + URM, data=admissions, family=binomial)

#displaying the summary of the model
summary(LogModel)

# Evaluate model with testing data
testing$admit <- as.character(testing$admit)
testing$admit[testing$admit=="No"] <- "0"
testing$admit[testing$admit=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$admit)
misClasificError

#Calculating accuracy of the model
accuracy<-1-misClasificError
print(accuracy)


#Display a confusion matrix 

table(testing$admit, fitted.results)

#saving example data into a data frame
new <- data.frame(GPA=c(4), LSAT=c(160), resident=c(1), URM=c(0))

#use fitted model to predict the response value for sample data
predict(LogModel, newdata=new, type="response")

















