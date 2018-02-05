library(ggplot2)
library(scales)
library(lubridate)
library(Hmisc)
library(gridExtra)
library(stringr)
library(corrplot)
library(GGally)
library(reshape2)
library(gdata)
library(dplyr)

loan<-read.csv(file.choose(),stringsAsFactors = F, na.strings=c("", " ", "NA", "N/A"))

#table(loan$loan_status)

#As Current loan status is not required for selection of Defaulter , therefore removing it from data frame.

loan<-loan[-which(toupper(loan$loan_status) == "CURRENT"), ]

# Get rid of all columns having only 1 unique value

loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

#colSums(is.na(loan))

#uRL is useless , without user id and password there nit required .
#desc ,title is also not needed as we are going to do analysis based on text . and it not
#going to effect in loan defaulter .

loan <- subset(loan,select = -c(url,desc,title))

#Therre are few columns with either 0 or NA . So it wont help in our analysis therefore 
#removing those fileds

loan <- subset(loan,select = -c(collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens))

#As we are not doing analysis on collected data , and not analyzing each loan borrower so  id is not required
#as every loan is individual loan , there member id should also be removed .

loan <- subset(loan,select = -c(member_id,id))

#colSums(is.na(loan))

#convering all the non numeric columns to upper case 

loan <- data.frame(lapply(loan, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


#converting int_rate and revol_util to numeric and replacing the % with " "

loan$int_rate <- as.numeric(gsub("%"," ",loan$int_rate))
loan$revol_util <- as.numeric(gsub("%"," ",loan$revol_util))


#we are not considering the title variable as it containing many different levels .

loan <- subset(loan , select = -c(emp_title))

#colSums(is.na(loan))

#Removing the columns having more than 50% NA values .

loan <- subset(loan, select = -c(mths_since_last_delinq,mths_since_last_record))

#Cleanig of date data
loan$issue_d <- as.character(loan$issue_d)
loan$issue_d <- paste(loan$issue_d,"-01",sep = "")
loan$issue_d <- parse_date_time(loan$issue_d,"myd")


loan$earliest_cr_line <- as.character(loan$earliest_cr_line)
loan$earliest_cr_line <- paste(loan$earliest_cr_line,"-01",sep = "")
loan$earliest_cr_line <- parse_date_time(loan$earliest_cr_line,"myd")


#table(is.na(loan$revol_util)) # we have a few na values here 

#Replacing all the missing with median of the data set for pub_rec_bancrupties and revol_util.

loan$pub_rec_bankruptcies <-impute(loan$pub_rec_bankruptcies,median)
loan$revol_util <- impute(loan$revol_util)

#colSums(is.na(loan))


#-----------------------DATA CLEANING ENDS HERE--------------------------------------------------- 

#write.csv(loan,file="loan_clean.csv")

#seperating the loan dat into 2 data frames ChargedOff and FullyPaid .

ChargedOff <- loan[loan$loan_status== "CHARGED OFF",]
FullyPaid <- loan[loan$loan_status == "FULLY PAID",]


#********************NOW DOING THE UNI VARIATE ANALYSIS**************************** 


#segmented univariate analysis on categorical variables

#segmented univariate analysis based on terms 

ggplot(ChargedOff, aes(ChargedOff$term , fill = term))+geom_bar(stat= "count")+geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+xlab("Types of loan based on terms")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on Loan terms")

#univariate analysis bsed on grade 

p1 <-ggplot(ChargedOff, aes(ChargedOff$grade , fill = grade))+geom_bar(stat= "count")+geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+xlab("Types of loan based on grade")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on grade(Interest rates)")
p2<-ggplot(ChargedOff, aes(ChargedOff$sub_grade , fill = sub_grade))+geom_bar(stat= "count")+geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+xlab("Types of loan based on grade")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on grade(Interest rates)")
grid.arrange(p1,p2,ncol=2)
rm(p1)
rm(p2)

#Employee length Analysis 

loan <- loan[loan$emp_length != "N/A",]
loan$emp_length <- gsub("< 1 YEAR","0 YEARS",loan$emp_length)
loan$emp_length <- gsub("10\\+ YEARS","10 YEARS",loan$emp_length)
ggplot(loan, aes(loan$emp_length , fill = emp_length))+geom_bar(stat= "count")+geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+xlab("Types of loan based on employ length")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on employee length")


#Home Ownership Analysis

ggplot(ChargedOff, aes(ChargedOff$home_ownership , fill = home_ownership))+
  geom_bar(stat= "count")+
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+
  xlab("Types of loan based home Ownership")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on Home_ownership on ChargedOff data")


#Verification Status Analysis

ggplot(ChargedOff, aes(ChargedOff$verification_status , fill = verification_status))+
  geom_bar(stat= "count")+
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+
  xlab("Types of loan based verification_status")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on verification_status")


#Loan purpose Analysis

ggplot(ChargedOff, aes(x=ChargedOff$purpose , fill = purpose))+
  geom_bar(stat= "count")+scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+
  xlab("Types of loan based purpose")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on purpose")


#open acc Analysis

ggplot(ChargedOff, aes(x=ChargedOff$open_acc , fill = open_acc))+
  geom_histogram(stat= "count")+
  geom_text(stat = "count",aes(label=(..count..)), vjust=-1,position = position_dodge(0.9))+
  xlab("Types of loan based on open_acc")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on open_acc")

#Analysis on deling 2years

p1 <-ggplot(ChargedOff, aes(x=ChargedOff$delinq_2yrs , fill = delinq_2yrs))+geom_bar(stat= "count")+geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+xlab("Types of loan based delinq_2yrs")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on delinq_2yrs")
p2 <-ggplot(FullyPaid, aes(x=FullyPaid$delinq_2yrs , fill = delinq_2yrs))+geom_bar(stat= "count")+geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9))+xlab("Types of loan based delinq_2yrs")+ylab("Total Applicant")+ggtitle("Univariate Analysis based on delinq_2yrs")
grid.arrange(p1,p2,ncol=2)
rm(p1)
rm(p2)

#no significance difference between the 2 graphs so cannot be considered as a deciding factor.

#Anaylsis on inq last 6 months 

p1<-ggplot(ChargedOff,aes(x=inq_last_6mths , fill = inq_last_6mths ))+geom_bar(stat = "count")+geom_text(stat = "count",aes(label=(..count..)),vjust=-1,position = position_dodge(0.9))+labs(x="Number of Enquiries last 6 month",y = "Total Applicant",title= "Univariate analysis based on Inquiry last 6 months")
p2<-ggplot(FullyPaid,aes(x=inq_last_6mths , fill = inq_last_6mths ))+geom_bar(stat = "count")+geom_text(stat = "count",aes(label=(..count..)),vjust=-1,position = position_dodge(0.9))+labs(x="Number of Enquiries last 6 month",y = "Total Applicant",title= "Univariate analysis based on Inquiry last 6 months")
grid.arrange(p1,p2,ncol=2)
rm(p1)
rm(p2)

#No any significance, that's why can be removed.

#DTI Ratio Analysis

boxplot(loan$dti)
summary(loan$dti)

#-----------------------------Bivariate analysis for segmented continous variables---------------------

#Based on interest rate and loan status

ggplot(loan,aes(loan_status,int_rate))+geom_boxplot() + labs(title = "Interest rate vs Loan Status")

#We can see that int_rate might be significant factor in deciding defaulters.


#Based on loan status and total_payment

ggplot(loan,aes(loan_status,total_pymnt))+geom_boxplot() + labs(title = "Total payment vs Loan Status")

#We can see that total_pymt  might be significant factor in deciding degfault


#Based on loan status and total_payment_inv

ggplot(loan,aes(loan_status,total_pymnt_inv))+geom_boxplot() + labs(title = "Total_payment_inv vs Loan Status")

#we can see that total_pymnt_inv might be used in decididing default


#Based on loan status and total_rec_prncp

ggplot(loan,aes(loan_status,total_rec_prncp))+geom_boxplot() + labs(title = "Total received principle vs Loan Status")

#we can see that total_rec_prncp can be a significant in deciding the loan_status.

#boxplot recoveries and loanstatus

ggplot(loan,aes(loan_status,loan$recoveries))+geom_boxplot() + labs(title = "Recoveries vs Loan Status", x = "Loan_status", y = "Revoveries")

# As for non-defaulters there is 0 recovery so it imples that it might not be useful for our analysis 
# opposite from boxplot 

#boxplot loan_status and loan_amnt

ggplot(loan,aes(loan_status,loan_amnt))+geom_boxplot()+ labs(title = "Loan Amount vs Loan Status")

#From boxplot it is intutive that that loan_amnt might not help as  to derive loan status
#it has not significant difference , but this will view changes when we the corrrelation matrix.

#boxplot loan_status and funded_amnt

ggplot(loan,aes(loan_status,funded_amnt))+geom_boxplot() + labs(title = "Funded Amount vs Loan Status")

#From boxplot it seems that funded_amnt is not  be significant for our analysis.
#We cant derive much from funded_amt to derive about loan status.

#boxplot loan_status and funded_amnt_inv

ggplot(loan,aes(loan_status,funded_amnt_inv))+geom_boxplot()+ labs(title = "Funded amount inv vs Loan Status")

##We cant derive much from funded_amt_inv to derive about loan status.

#boxplot loan_status installment 

ggplot(loan,aes(loan_status,installment))+geom_boxplot()+ labs(title = "Installment vs Loan Status")

## We can see that both plots are identical,we cant derive much from installment.

require(scales)
ggplot(loan,aes(loan_status,annual_inc))+geom_boxplot()+scale_y_continuous(labels = comma)+ labs(title = "Annual Income vs Loan Status")

# we can see that we there is not much significant differece So it might not be helpful.

#boxplot loan_status  open accounts

ggplot(loan,aes(loan_status,open_acc))+geom_boxplot() + labs(title = "Open Accounts vs Loan Status")

#We can see that both plots are identical,we cant derive much from these.


#boxplot loan_status and pub_rec

p1 <- ggplot(ChargedOff,aes(as.factor(pub_rec)))+geom_bar(stat= "count")+ geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9)) + labs(title = "On Charged off Data")
p2 <- ggplot(FullyPaid,aes(as.factor(pub_rec)))+geom_bar(stat= "count")+ geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9)) + labs(title = "On Fully paid data")
grid.arrange(p1,p2, ncol = 2)
rm(p1)
rm(p2)

#We can see that both plots are identical,we cant derive much from these.

#not much significance from the boxplot  of revol_bal

ggplot(loan,aes(loan_status,revol_bal))+geom_boxplot() + labs(title = "Revolving balance vs Loan Status")


#Might Be significant in derivering loan status revolving util 

ggplot(loan,aes(loan_status,revol_util))+geom_boxplot() + labs(title = "Revolving Utilization vs Loan Status")

 
# total account

ggplot(loan,aes(loan_status,total_acc))+geom_boxplot() + labs(title = "Total acounts vs Loan Status")

#Not much significant 

# total interst received

ggplot(loan,aes(loan_status,total_rec_int))+geom_boxplot() + labs(title = "Total recieved interest vs Loan Status")

#Not significant much

#total_rec_late fee 
ggplot(loan,aes(loan_status,total_rec_late_fee))+geom_boxplot() + labs(title = "Total recieved Late fee vs Loan Status")

#from boxplot also we can see it is not significant.

#as recovery isn't important it will be always zero  for fully paid.So,It is related term same behaviour compare dto recovery.
ggplot(loan,aes(loan_status,collection_recovery_fee))+geom_boxplot()+ labs(title = "Collection recovery fee vs Loan Status")


#As median is same is for both and its not make any intuition to use last payment amount.
ggplot(loan,aes(loan_status,last_pymnt_amnt))+geom_boxplot() +
  labs(title = "Last payment amount vs Loan Status")




#------------------------------Bivariate Analysis for Categorical Variables---------------------------------------------------

#home ownership vs loan status

ggplot(loan,aes(x=loan_status,group= home_ownership))+
  geom_bar(stat="count",aes(y=..prop.., fill = factor(..x..)))+
  geom_text(aes(label= scales::percent(..prop..),y=..prop..),stat = "count" ,vjust=-0.5 )+
  facet_grid(~home_ownership)+scale_y_continuous(labels= scales::percent)+scale_x_discrete(labels=abbreviate) +
  labs(y="Percent",fill = "loan_status",title="Bivariate Analysis on Loan status vs Home Ownership")

  
#bivariate loan status vs purpose 

ggplot(loan,aes(x=loan_status,group=purpose))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat= "count",vjust= -.5)+
  facet_grid(~purpose)+labs(y = "Percent", fill="Status") +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)+labs(y="Percent",fill = "loan_status",title="Bivariate Analysis on Loan status vs purpose")



#bivariate loan status vs employment length

ggplot(loan[loan$emp_length != "N/A",],aes(x=loan_status,group =emp_length))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat = "count",vjust= -.5)+
  facet_grid(~emp_length)+labs(y="Percent",fill="Status",title="Bivariate loan status vs employment length")+scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)
  
#bivariate loan status vs grade 

ggplot(loan[loan$emp_length != "N/A",], aes(x= loan_status,  group=grade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status",title="bivariate loan status vs grade") +
  facet_grid(~grade) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)

#Bivariate analysis on verification status 

ggplot(loan, aes(x= loan_status,  group=verification_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status",title="Bivariate analysis on verification status") +
  facet_grid(~verification_status) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)

#multivariate analysis on loan status , amount and purpose

ggplot(loan,aes(x=loan$loan_amnt,y=loan$purpose,col=loan$loan_status))+geom_point(alpha=0.5,position = "jitter")


#Bi-variate for Address State

ggplot(loan, aes(x=loan$addr_state, y = (..count..)/sum(..count..), fill=factor(loan_status))) + 
  geom_bar(position="fill") + scale_y_continuous(labels = percent) +
  xlab("Address State") + ylab("Applicants  Percentage")+
  ggtitle("Status of Loan on basis of State") + 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 


#Clearly One state (NEVADA) is showing maximum charged off cases. 

#-----------------------Correlation Matrix--------------------------------------------------

#Converting some numeric variable to factors as they have repeated datas.

loan$pub_rec_bankruptcies <-as.factor(loan$pub_rec_bankruptcies)
loan$pub_rec <-as.factor(loan$pub_rec)
loan$inq_last_6mths <-as.factor(loan$inq_last_6mths)
loan$delinq_2yrs <- as.factor(loan$delinq_2yrs)


#Deriving a new column credit loss n the chargedOff dataframe
ChargedOff$loss=ChargedOff$funded_amnt-ChargedOff$total_rec_prncp
loan$loss=loan$funded_amnt-loan$total_rec_prncp

#str(ChargedOff)
#Creating a data frame of all the numerical variables

numeric_ChargedOff <- ChargedOff[sapply(ChargedOff,is.numeric)]

numeric_loan <- loan[sapply(loan,is.numeric)]

cormat <- round(cor(numeric_ChargedOff),2)
head(cormat)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Leading Loan Parameters\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#-------------------Analysis based on Correleation Matrix-----------------------------------

#As we analysed from boxplot that interest rate, total payment recieved, total payment investement, total recieved principle and revolve utilization
#are significant in deciding the defaulters but these are highly correlated as we can infer from above heat map correlation by the red region of plot.
#which is the high amount of corelation existing because this variables are derived from each other. So, by elliminating the other variables, we are subsetting the numeric variables.
#To further drill down

sub_numeric_loan <- numeric_loan[ , c(2,4,12,13,14,10)]
cor(sub_numeric_loan)

#As we can see from above correlation matrix, total_paymnt and total_payment_inv are highly correlated with total_rec_prncp. So, we can take any one of these.
#We are taking total_rec_prncp for our analysis with combination of interest rate and revolve utilization.
#From box plot also, we found that total_rec_prncp might be most significant in deciding loan status.

ggplot(loan, aes(x = int_rate, y = total_rec_prncp, color = loan_status)) +
  geom_point() +labs(title = "Effect of interest rate and total_rec_prncp on loan status")

#as we can see that for less Total_rec_prncp and high interest rate, percentage of defaulters are most. 
#Also, we can infer that for less total_rec_prncp and moderate interest rate, defaulters are high.
#Also, we can infer that for moderate total_rec_prncp and moderate interest rate, defaulters are moderate.
#Also, we can infer that for high total_rec_prncp and any interest rate, defaulters are very less.

# But we cant use total payment received because it is known only after once the loan is sanctioned
##As per buisness understanding 
##so,we are picking the term which is highest correlated with total received principal
##that we can see from coorelation plot that Loan Amount is highly correlated(85%) with  total received pricipal

ggplot(loan, aes(x= int_rate, y= loan_amnt, color = loan_status)) +geom_point() + facet_wrap(~term)
ggplot(loan, aes(x= int_rate, y= loan_amnt, color = loan_status)) +geom_point() + facet_wrap(~purpose)


#Divding the loan Amount into different levels 

loan <- mutate(loan,loan_amnt_labels=if_else(loan_amnt<=10000,'low_amount',
                                                          if_else(total_rec_prncp>10000 & total_rec_prncp<=20000,'moderate_amount',
                                                                  if_else(total_rec_prncp>20000 & total_rec_prncp<=30000,'high_amount','extreme_high_amount'))))
#Dividing the interest rate into different levels

loan=mutate(loan,labels_int_rate=if_else(int_rate<=10,'low_interest',
                                         if_else(int_rate>10 & int_rate<=15,'mid_interest',
                                                 if_else(total_rec_prncp>15 & total_rec_prncp<=20,'high_interest','extreme_high_interest'))))

#creating a new dataframe for a selected number of significant variables , found .                                                                                                                                  if_else(total_rec_prncp>15 & total_rec_prncp<=20,'high_interest','extreme_high_interest'))))

new_loan <- loan[ , c(1,4,5,7,10,14,15,38,39,40)]

#Summarising the Values

grouped_amt_ir=as.data.frame(new_loan %>% group_by(loan_amnt_labels,labels_int_rate) %>% summarise(count=n()))
grouped_amt__ir_ls=as.data.frame(new_loan %>% group_by(loan_amnt_labels, labels_int_rate,loan_status) %>% summarise(count=n()))



merged_df_amt_ir=merge(grouped_amt__ir_ls,grouped_amt_ir,by=c('loan_amnt_labels','labels_int_rate'))
merged_df_amt_ir$default_per=merged_df_amt_ir$count.x*100/merged_df_amt_ir$count.y

#calulated percentage of defaulters
sub_merged=subset(merged_df_amt_ir,loan_status=='CHARGED OFF')
ggplot(data=sub_merged,aes(loan_amnt_labels,default_per))+geom_bar(stat="identity")+facet_wrap(~labels_int_rate)+labs(title="Analysis based on interest rate vs Loan Amount")

#we can see clearly that
#In case of low interest and extremly high amount given can lead to 45% default
#In case of extremely high amount and high interst and mid interest are contributing to 80% of defaults
#In case of low amount extremely high interst is significant






#TERM
grouped_df=as.data.frame(new_loan %>% group_by(loan_amnt_labels,labels_int_rate,term) %>% summarise(count=n()))
grouped_df1=as.data.frame(new_loan %>% group_by(loan_amnt_labels, labels_int_rate,term,loan_status) %>% summarise(count=n()))

df3=merge(grouped_df1,grouped_df,by=c('loan_amnt_labels','labels_int_rate', 'term'))
df3$percentage=df3$count.x*100/df3$count.y

df3 

#df3 dataframe tells us about the relatonship between , loan amount, interst rate , terms and loan status.

#PURPOSE
grouped_df=as.data.frame(new_loan %>% group_by(loan_amnt_labels,labels_int_rate,purpose) %>% summarise(count=n()))
grouped_df1=as.data.frame(new_loan %>% group_by(loan_amnt_labels, labels_int_rate,purpose,loan_status) %>% summarise(count=n()))

df4=merge(grouped_df1,grouped_df,by=c('loan_amnt_labels','labels_int_rate', 'purpose'))
df4$percentage=df4$count.x*100/df4$count.y
df4

#df4 dataframe tells us about the relatonship between , loan amount, interst rate , purpose and loan status.

#HOME_OWNERSHIP
grouped_df=as.data.frame(new_loan %>% group_by(loan_amnt_labels,labels_int_rate,home_ownership) %>% summarise(count=n()))
grouped_df1=as.data.frame(new_loan %>% group_by(loan_amnt_labels, labels_int_rate,home_ownership,loan_status) %>% summarise(count=n()))

df5=merge(grouped_df1,grouped_df,by=c('loan_amnt_labels','labels_int_rate', 'home_ownership'))
df5$percentage=df5$count.x*100/df5$count.y
df5

#df5 dataframe tells us about the relatonship between , loan amount, interst rate , home_ownership and loan status.

#ggplot(data=df5,aes(labels_int_rate,percentage,fill=loan_status))+geom_bar(stat="identity", position = "dodge")+facet_wrap(~home_ownership)

#Plotting High Loan amount with Interst rate grouped by home Ownership.

#df5_low <- df5[which(df5$loan_amnt_labels == "low_amount"),c(-1)]
#df5_moderate <- df5[which(df5$loan_amnt_labels == "moderate_amount"),c(-1)]
#df5_high <- df5[which(df5$loan_amnt_labels == "high_amount"),c(-1)]
df5_extreme <- df5[which(df5$loan_amnt_labels == "extreme_high_amount"),c(-1)]

ggplot(data=df5_extreme,aes(labels_int_rate,percentage,fill=loan_status))+geom_bar(stat="identity", position = "dodge")+facet_wrap(~home_ownership)

#As per the Plot , When the loan amount is high and home_ownership is Others in that case for low and high interst rates ,
# Defaulters is more likely to occur. 
#After analysing the correlation matrix, as credit loss is hightly correlated with loan amount, so we can take loan amount as one of the significant parameter while deciding defaulters
#You can visualize the above statement by below plot
ggplot(new_loan, aes(x= loan_status,  group=loan_amnt_labels)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status",title="Bivariate analysis on Loan Amount") +
  facet_grid(~loan_amnt_labels) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)



#ggplot(data=df5_high,aes(labels_int_rate,percentage,fill=loan_status))+geom_bar(stat="identity", position = "dodge")+facet_wrap(~home_ownership)
#ggplot(data=df5_moderate,aes(labels_int_rate,percentage,fill=loan_status))+geom_bar(stat="identity", position = "dodge")+facet_wrap(~home_ownership)
#ggplot(data=df5_low,aes(labels_int_rate,percentage,fill=loan_status))+geom_bar(stat="identity", position = "dodge")+facet_wrap(~home_ownership)


#--------------------------------END OF ALALYSIS-----------------------------------------------

