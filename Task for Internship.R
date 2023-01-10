mydata <- read.csv(file="bank.csv", sep=';')
#1)      DATA MANIPULATION TASK
mydata[-which(is.na(mydata)), ] #checking if there is any missing data

##- select random subsample of data set;
library(data.table)
set.seed(10)
mydata <- data.table(mydata)
dt<-mydata[sample(.N, 1000)] #selecting random 1000 rows from dataset


unique(dt) #notice we have a lot of missing information in 'poutcome' variable
unique(dt$poutcome)
table(dt$poutcome=="unknown")
##- filter desired rows using simple and more complex conditions;
library(dplyr)
mydata %>% filter(housing=='yes' & loan=='yes' & age<25) ->curiosity #just for curiosity checking if there's 
#anyone with housing and personal loans in data under age of 25

##- drop unnecessary variables, rename some variables;
dt1 <- dt %>% select (!c("poutcome","day","month")) #droping time variables that is unnecessary in log regression 
#and poutcome because it's missing a lot of information
table(dt1=="unknown") # we can see that there is still some missing ingormation, so we drop those rows
dt1[dt1=="unknown"] <- NA #making unkown as a NA
dt1<-na.omit(dt1) #droped all NA
names(dt1)[names(dt1) == "y"] <- "subscribed" #renaming variable

##- calculate summarizing statistics (for full sample and by categorical variables as well);
summary(dt1) #basic statistics of data sample
tapply(dt1$balance, dt1$job, summary) #Average yearly balance grouped by job statistics

#checking some statistics for categorical variables
library(Hmisc)
describe(dt1$job) #we can see that in out data sample mostly appears people working with management.
#They consists of 25.5% of our sample
describe(dt1$education) #We notice that the majority, over 50.9% consists of secondary education level, 35.3% tertiary and 13.8% primary
##- create new variables using simple transformation and custom functions;
library(dplyr)
new <- mutate(dt1, average=duration/campaign) #added average campaign time.

library(data.table)
funkcija <- function(new_var){
  data <- dt1
  data$new_var <- data$duration/data$campaign
  return(data.table(data))
}
funkcija(naujas) #created new variable with custom function.
##- order data set by several variables.
order<-order(dt1$age, dt1$balance) #ordered data by age and balance
dt1[order,]

################################################
#2)      DATA VISUALISATION TASK
#Bar plot
library(Hmisc)
describe(dt1$subscribed)
subscribed<-table(dt1$subscribed)
A<-barplot(subscribed, 
           ylab="frequency",
           names = c("Subscribed","Not subscribed"), 
           ylim = c(0,600), 
           main='Amount of subscribtions',
           col = c("lightblue", "mistyrose"),
           las=1)
text(A, 10, subscribed,cex=1,pos=3)



#density
d <- density(dt1$age)
plot(d, xlab="Age", main="Age distribution")
axis(1, at = seq(10, 80, by = 10), las=1)
abline(v=c(30,40), col="blue")

#Pie chart
P<-data.frame(table(dt1$job))
Pd <- P[order(-P$Freq), ]
pct <- round(100*Pd$Freq/sum(Pd$Freq))

pie(Pd$Freq,
    labels = paste(Pd$Var1, sep = " ", pct, "%"), 
    col = rainbow(length(Pd$Freq)), 
    main = "Rate of different jobs in data")
#
#corelation plot
myPanel <- function(x, y, z, ...){
  lattice::panel.levelplot(x,y,z,...)
  my_text <- ifelse(!is.na(z), paste0(round(z, 4)), "")
  lattice::panel.text(x, y, my_text)}

#
kor <- dt1 %>% select (!c("job","marital","education","default","housing","loan","contact","y"))
#
mask = cor(kor, use = "complete.obs")
mask[upper.tri(mask, diag = TRUE)] <- NA
lattice::levelplot(mask, 
                   panel = myPanel, 
                   col.regions = viridisLite::viridis(100), 
                   main = 'Correlation of numerical variables')


#################################################################################################
#3)      MODELLING TASK
mydata <- read.csv(file="bank.csv", sep=';')#loaded raw dataset
#reusing some code from 1) task
library(data.table)
set.seed(10)
mydata <- data.table(mydata)
dt<-mydata[sample(.N, 2500)]
library(dplyr)
dt1 <- dt %>% select (!c("poutcome","day","month")) #droping time variables that is unnecessary in log regression 

dt1[dt1=="unknown"] <- NA #making unkown as a NA
dt1<-na.omit(dt1)


str(dt1)
#making char to factors
dt1$y<-as.factor(dt1$y)
dt1$job<-as.factor(dt1$job)
dt1$marital<-as.factor(dt1$marital)
dt1$education<-as.factor(dt1$education)
dt1$default<-as.factor(dt1$default)
dt1$housing<-as.factor(dt1$housing)
dt1$loan<-as.factor(dt1$loan)
dt1$contact<-as.factor(dt1$contact)


library(caTools)
ind = sample.split(Y = dt1$y, SplitRatio = 0.6)
training = dt1[ind,]
testing = dt1[!ind,]
#Spliting data into training and testing

mod_fit <- train(y ~ .,  data=training, method="glm", family="binomial") #making model with all variables
summary(mod_fit) #checking model with all variables
#we notice that housing, loan, duration and previous variables are significant,
#their P values are lower than 0.05(Previous is slightly higher but i'll keep them in model)


mod_fit_one <- glm(y ~ ., data=training, family="binomial")

mod_fit_two <- glm(y ~ housing + loan + duration + previous,  data=training, family="binomial")

mod_fit_three <- glm(y ~ housing + loan + duration,  data=training, family="binomial")
library(lmtest)
lrtest(mod_fit_one, mod_fit_two) #we get that P value is > 0.05, so we do not reject the null hypothesis
#and this indicates that the full model and the nested model do fit the data equally.
lrtest(mod_fit_one, mod_fit_three)
#it is better comparing with third model
summary(mod_fit_three)
# i will continue with 3rd model because all variables are significant and performs slighlty better than second.


library(pscl)
pR2(mod_fit_three)
#fitting null model for pseudo-r2
#     llh      llhNull           G2     McFadden         r2ML         r2CU 
#-366.4367026 -447.0153302  161.1572551    0.1802592    0.1444485    0.2494162 
#McFadden measure ranges from 0 to just under 1, 
#with values closer to zero indicating that the model has no predictive power.
#In this case its not the best predicting model but it has some power.


library(ROCR)
# Compute AUC for predicting Class with the model
prob <- predict(mod_fit_three, newdata=testing, type="response")
pred <- prediction(prob, testing$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.8238751
#value above 0.80 indicate that the model does a good job.

summary(mod_fit_three)

#final model: Subscribed=4.1679169+(-0.5579292)housingno+(-1.0811502)loanno+(-0.0036236)duration
