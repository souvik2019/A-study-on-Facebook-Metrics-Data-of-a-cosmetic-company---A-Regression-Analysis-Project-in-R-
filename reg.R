setwd("D:/IIT kanpur/2 nd sem/minerva regression/regression projet")
# rm(list=ls())
# getwd()
# setwd("/cloud/project")
MAPE = function(ycap,y){
  cat('MAPE',mean(abs((y - ycap)/y)))
}
# Data importing
X=read.csv("dataset_Facebook.csv",sep=";",header=TRUE,na.string = '?' )
attach(X)
colnames(X)

#shortening column names
colnames(X)[8:15]=c('Total.Reach','Total.Impressions','Lifetime.Engaged.Users','Consumers','Consumptions','Impressions.liked','Reach.liked','People.engaged.liked')
colnames(X)#checking column names after shortening

library(ggplot2)
library(RColorBrewer)
library(skimr)

#from the information in the it is given that the column 'Total.Interactions' is sum 
#of 'like', 'comment' and 'share'
#so, these four variables are redundant
#we only use 'Total.Interaction' and exclude other 3
X=X[,-c(which(colnames(X)=='like'),which(colnames(X)=='share'),which(colnames(X)=='comment'))]

#now we will check some summary and information in the data
skim(X)
#see that there is 500 obsn, 16 variables, one categorical, 15 numeric variable
#one missing value in 'Paid'

#Actually there are 6 categorical variable namely 'Type', 'Category', 'Post.Month'
#'Post.Weekday', 'Post.Hour' and 'Paid' needed to convert into categorical type
X$Type=as.factor(X$Type)
X$Category=as.factor(X$Category)
X$Paid=as.factor(X$Paid)
X$Post.Month=as.factor(X$Post.Month)
X$Post.Hour=as.factor(X$Post.Hour)
X$Post.Weekday=as.factor(X$Post.Weekday)


#We will exculde the single observation contains a missing value
which(is.na(X$Paid)==T) #500
model_m=glm(Paid~.,data = X[1:499,],family = binomial)
prob=predict(model_m,X[500,-7],type='response')
X[500,7]=round(prob)

#let's again see the variables and their summary
skim(X)
#now, no missing value 6 categorical , 10 numeric variable

#splitting into train and test
set.seed(197)
ind=sample(500,500*.8) # selecting randomly approx. 80% of observation from 499 observation
X_train=X[ind,]
X_test=X[-ind,]
dim(X_train)  #400 16
dim(X_test)   #100 16

#train set
y=X_train[,which(colnames(X)=='Lifetime.Engaged.Users')]
X=X_train[,-which(colnames(X)=='Lifetime.Engaged.Users')]

#test set
y_test=X_test[,which(colnames(X_test)=='Lifetime.Engaged.Users')]
X_test=X_test[,-which(colnames(X_test)=='Lifetime.Engaged.Users')]


dim(X)  #now dimension of X is 400 x 15

cat_col=c(2,3,4,5,6,7) # a Variable containing the column indices of categorical variable

#box plot and frequency tables for categorical variable
data=cbind.data.frame(y,X)
colnames(data)[1]="Lifetime.Engaged.Users"

ggplot(data,aes(x=Type,y=Lifetime.Engaged.Users,fill=Type))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,2])
#'Photo' consists many outliers. 'Status', 'Link', 'Video' have long tail
#clearly we see a ordering i.e. y increases from Link to Photo to Status to Video
#so, we can assign a increasing number to these labels of category
X[,2]=factor(X[,2],levels = c('Link','Photo','Status','Video'),labels = c(1,2,3,4))
X[,2]=as.numeric(X[,2])

ggplot(data,aes(x=Category,y=Lifetime.Engaged.Users,fill=Category))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,3])
#each label has outliers, category 1,2,3 contain 175,103,122 respectively
#category 2 has long tail. all levels have almost same central tendency


ggplot(data,aes(x=Paid,y=Lifetime.Engaged.Users,fill=Paid))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,7])
#each label has many outliers , paid 0 is more frequent (301)

ggplot(data,aes(x=Post.Month,y=Lifetime.Engaged.Users,fill=Post.Month))+
  geom_boxplot()+scale_fill_brewer(palette = "Paired")+theme_classic()
table(X[,4])
#most frequent months are oct(50), dec(40)
#seeing the graph we can group our months into three groups considering their central tendency
#(1,2),(3,4,5,6,,7),(8,9,10,11,12)
library(plyr)
X[,4]=revalue(X[,4],c('1'='group1','2'='group1','3'='group2','4'='group2','5'='group2','6'='group2','7'='group2','8'='group3','9'='group3','10'='group3','11'='group3','12'='group3'))

ggplot(data,aes(x=Post.Hour,y=Lifetime.Engaged.Users,fill=Post.Hour))+
  geom_boxplot()+theme_classic()
table(X[,6])
#it is clear that there are only one observation in hour 16,19,20,22,hour 23 has no value
#seeing the plot we can group the hours in evening(15-22),night(23-4),morning(5-9),day(10-14)
X[,6]=revalue(X[,6],c('23'='night','1'='night','2'='night','3'='night','4'='night','5'='morning','6'='morning','7'='morning','8'='morning','9'='morning','10'='day','11'='day','12'='day','13'='day','14'='day','15'='evening','16'='evening','17'='evening','18'='evening','19'='evening','20'='evening','22'='evening'))


ggplot(data,aes(x=Post.Weekday,y=Lifetime.Engaged.Users,fill=Post.Weekday))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,5])
#all the observations are almost equally distributed among the levels, their central tendency also near about same but among them day 4 has highest central tendency
# here we can group these into weekday and weekened
X[,5]=revalue(X[,5],c('1'='weekday','2'='weekday','3'='weekday','4'='weekday','5'='weekday','6'='weekend','7'='weekend'))

#now our categorical column has decreased
cat_col=c(3,4,5,6,7)

#Normalization
X[,-cat_col]=scale(X[,-cat_col])

#corelation among numeric variables
pairs(X[,-cat_col])
cor(X[,-cat_col])
cor(X[,-cat_col],y)

dat=cbind(y,X)
# indicator Variable
library(fastDummies)
length(cat_col) #5
X=dummy_cols(X,select_columns =colnames(X)[ cat_col],remove_first_dummy = T,remove_selected_columns = T)
dim(X)  #400 x 19

# a function for modification in test set which include categorical variable modification, normalization, indicator variable
convert=function(X){
  X[,2]=factor(X[,2],levels = c('Link','Photo','Status','Video'),labels = c(1,2,3,4))
  X[,2]=as.numeric(X[,2])
  X[,4]=revalue(X[,4],c('1'='group1','2'='group1','3'='group2','4'='group2','5'='group2','6'='group2','7'='group2','8'='group3','9'='group3','10'='group3','11'='group3','12'='group3'))
  X[,6]=revalue(X[,6],c('23'='night','1'='night','2'='night','3'='night','4'='night','5'='morning','6'='morning','7'='morning','8'='morning','9'='morning','10'='day','11'='day','12'='day','13'='day','14'='day','15'='evening','16'='evening','17'='evening','18'='evening','19'='evening','20'='evening','22'='evening'))
  X[,5]=revalue(X[,5],c('1'='weekday','2'='weekday','3'='weekday','4'='weekday','5'='weekday','6'='weekend','7'='weekend'))
  cat_col=c(3,4,5,6,7)
  X[,-cat_col]=scale(X[,-cat_col])
  X=dummy_cols(X,select_columns =colnames(X)[ cat_col],remove_first_dummy = T,remove_selected_columns = T)
  
  return(X)
}
X_test=convert(X_test)




p=(dim(X))[2]   
p     #no. of predictors   19
n=(dim(X))[1]
n     #no. of observation  400

library(olsrr)
my_data=cbind(y,X)
model=lm(y~.,data=my_data)
summary(model)      # multiple R squared=.9971,  Adjusted R square=0.9969
dim(my_data)  #399  20
predictions <- predict(model, X_test)
MAPE(predictions,y_test)  #MAPE 0.3506001  

#leverage points
hat_value=hatvalues(model)
par(mfrow=c(1,1))
plot(hat_value,xlab ="observation",ylab="hat_values",col=(hat_value>0.6)+1)
abline(h=0.6,col="red")
lev_point=which(hat_value>0.6)      # it provides row no of data corresponding to which high hat values
length(lev_point)  # 1
my_data1=my_data[-lev_point,]
dat=dat[-lev_point,]

row.names(my_data1)=as.character(seq(1,dim(my_data1)[1]))   # assigning row names to each row of data
model1=lm(y~.,my_data1)
summary(model1)  #adj rsq 0.9971 
dim(my_data1)  #399 x 20
predictions <- predict(model1, X_test)
MAPE(predictions,y_test) ##MAPE 0.3619461

# cooks distance
cd=cooks.distance(model1)
plot(cd,xlab="observation",ylab="cooks distance", pch=20, col=(cd>0.25)+1)
abline(h=0.25,col="red")
inf_cd=which(cd>0.25)
length(inf_cd) #3
ols_plot_cooksd_bar(model1)

# DFFITS
df_fit=dffits(model1)
plot(df_fit,xlab="observation",ylab="DFFIT",pch=20, col=(abs(df_fit-0.5)>1.5)+1)
abline(h=c(2,-1),col="red")
ols_plot_dffits(model1)
influence_DFFIT=which(abs(df_fit-0.5)>1.5)
length(influence_DFFIT)  #7

####DFBETA######
DFBETA_ij=dfbetas(model1)
dim(my_data1)  #399 20
influence_DFBETA=c()
for(j in 2:10)
  influence_DFBETA=c(influence_DFBETA ,which(abs(DFBETA_ij[,j])>2/sqrt(399)) )
influence_DFBETA = as.numeric(names(which( table(influence_DFBETA)>=7) ) )
length(influence_DFBETA) #13

#covratio
COVRATIO=covratio(model1)
par(mfrow=c(1,1))
plot(COVRATIO,main="Fig 3:COVRATIO for training data Red:influential points and Black :Non",
     col=(abs(COVRATIO-1)>1)+1,cex.main=0.85, pch=20)
abline(h=c( 1.5, 0.5) , col="red", lwd=2, lty="dotted")#check threshold
influence_COVRATIO1=which(abs(COVRATIO)>1.5)
influence_COVRATIO2=which(abs(COVRATIO)<0.4)
influence_COVRATIO=length(influence_COVRATIO1)+length(influence_COVRATIO2) # 6
influence_COVRATIO=c(which(abs(COVRATIO)<0.4),which(abs(COVRATIO)>1.5))

#####Final Influential#####
influential = sort(( c(inf_cd,influence_DFBETA,influence_DFFIT,influence_COVRATIO ) ))
final=as.numeric(names(which(table(influential)>=2)))
length(final)  #5
my_data2=my_data1[-final,]
dat=dat[-final,]
which(colSums(my_data2)==0) #0
#row.names(my_data2)=as.character(seq(1,dim(my_data2)[1]))
dim(my_data2)  #394  20
model2=lm(y~.,data=my_data2)
summary(model2)  #adj r sq 0.9979
predictions <- predict(model2, X_test)
MAPE(predictions,y_test) # MAPE 0.3582894 
my_data22=my_data2


##normality checking
y=my_data2[,1]

library(ggpubr)
ggdensity(y,fill='blue')+ggtitle('Density Curve')+labs(x='y')+theme_dark()
ggqqplot(y)+ggtitle('Normal Q-Q Plot')+theme_light()

# Box-Cox transformation
library(MASS)  
z=log(y)
ggqqplot(z)+ggtitle('Normal Q-Q Plot')+theme_light()
# box cox transformation on log(y)
my_data22[,1]=z
mod_bc=lm(y~.,data=my_data22)   # model for box-cox transformation
bc=boxcox(mod_bc,lambda = seq(-15,15))   # boc-cox transformation 
best.lambda= bc$x[which(bc$y==max(bc$y))]
best.lambda  #7.727273
shapiro.test((z^best.lambda-1)/best.lambda)    #  < 2e-16
plot(lm((z^best.lambda-1)/best.lambda~.,data=my_data22))



#excluding first 6 points
z1=sort(z)[7:394]
ggqqplot(z1)+ggtitle('Normal Q-Q Plot')+theme_light()
ggdensity(z1,fill='blue')+ggtitle('Density Curve')+theme_dark()
shapiro.test(z1) #p-value = 0.1199  so, it is normal
del_point=which(z<=sort(z)[6])

#new data with transformed response
my_data2[,1]=z
my_data2=my_data2[-del_point,]
dat=dat[-del_point,]
colnames(my_data2)[1]='z'
dim(my_data2)   #387  20
model3=lm(z~.,data=my_data2)   #Our new or transformed model
summary(model3)  #adj r sq 0.8308
predictions <- predict(model3, X_test)

MAPE(predictions,log(y_test))   # 0.06085284


#heteroscedasticity
library(lmtest)
bptest(model3)  #p-value =0.4946 implies no heteroscedasticity

#multicollinearity
# checking Multicollinearity
X_num<-my_data2[,2:11]   # containing only numerical vectors
cor(X_num)  # from this it is clear that X3,X4,X8,X7 are highly correlate also 
#X4 and X8 are highly correlated.(indication of Multicollinearity)

#Computation of X'X
X_num=as.numeric(unlist(X_num))
X_num=matrix(X_num,ncol=10,byrow = FALSE)
C=t(X_num)%*%(X_num)  # it is [10,10] matrix
eigen_C=(eigen(C))$values
k=max(eigen_C)/min(eigen_C)   
# here k is condition number and k=516.5971 
# evidence of Multicollinearity

library(GGally)

car::vif(model3)      
#Some of the VIFs are greater than 10 so we will look into Lasso
#Lasso to remove multicolinearity + variable selection
set.seed(197)
library(glmnet)
xtrain=as.matrix(my_data2[,-1])
ytrain=as.vector(my_data2[,1])
cv.out=cv.glmnet(xtrain,ytrain,
                 alpha=1)
a=coef(cv.out)[,1]
length(a[a!=0]) #4 non zero coefficients
selected_variable=names(a[a!=0])
selected_variable
plot(cv.out,xlab="log(lambda)",ylab="mean squared error")
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(cv.out,s=bestlam,newx = as.matrix(X_test))
MAPE(lasso.pred,log(y_test)) #0.05756424

lasso_model=lm(formula=z~Reach.liked+Consumers+Total.Interactions+Post.Month_group3,data=my_data2)
car::vif(lasso_model)
#All VIFs are less than 10 so this indicates multicollinearity has been 
#removed and Variable selection has been applied



#Anova test for categorical variable
dim(dat)
data1=dat[,c(1,4)]
mod1=aov(y~Category,data=data1)
summary(mod1) #no significant level

data2=dat[,c(1,5)]
mod2=aov(y~Post.Month,data=data2)
summary(mod2)  #0.000698
#there are significant levels
library(DescTools)
ScheffeTest(mod2,which = 'Post.Month', conf.level = 0.99) 
#each group3 and 1 is significant            


data3=dat[,c(1,6)]
mod3=aov(y~Post.Weekday,data=data3)
summary(mod3) 
#no effect

data4=dat[,c(1,7)]
mod4=aov(y~Post.Hour,data=data4)
summary(mod4) 
# no effect

data5=dat[,c(1,8)]
mod5=aov(y~Paid,data=data5)
summary(mod5) 
#no effect

























