#### in here, we cleaned the data, did binning, proper conversion of data types
#### and Expploratory Data Analysis

install.packages("gmodels")

library(ggplot2)
library(dplyr)
library(skimr)
library(gmodels)
project_data<- read.csv("C:/Users/User/Desktop/final project/encoded further surv 13 vars.csv")
head(project_data)
attach(project_data)


## lemme try as.factor to convert numerical looking categorical to ... 
# ...categorical variables understandable by R

project_data$status <- as.factor(project_data$status)
project_data$gender <- as.factor(project_data$gender)
project_data$TOSecSch <- as.factor(project_data$TOSecSch)
project_data$livingINsch <- as.factor(project_data$livingINsch)
project_data$familystructure <- as.factor(project_data$familystructure)
project_data$Deptatsecsch <- as.factor(project_data$Deptatsecsch)
project_data$base <- as.factor(project_data$base)
project_data$sponsor <- as.factor(project_data$sponsor)
project_data$zone <- as.factor(project_data$zone)
project_data$Fathersedu <- as.factor(project_data$Fathersedu)
project_data$Mothersedu <- as.factor(project_data$Mothersedu)
project_data$Educationisthebestlegacy <- as.factor(project_data$Educationisthebestlegacy)
project_data$Schoolisscam <- as.factor(project_data$Schoolisscam)
project_data$olevel <- as.factor(project_data$olevel)

##skim the whole data for full data description - EDA
skim<- skim(project_data)
skim
variable<-skim$skim_variable
variable
level_count<-skim$factor.top_counts
level_count
tab<-data.frame(variable,level_count)
tab
### length of the variable status

x=length(project_data$'status'[project_data$'status'=='1'])

y=length(project_data$'status'[project_data$'status'=='0'])
z=c(x,y)
z
#### pie chart
png(file = "Status Proportion Pie Chart.jpg")

pie(z,labels = c("Gained Admission","Did Not Gain Admission"),main = "Admission Status Pie Chart",col = rainbow(length(z)))
dev.off()
#########



##########################
######count summary#######
##table of each variable##
####and proprotion####
##########################
### proportion table ###

p<-prop.table(summary(project_data$status))
p<-round(p*100,2)
p

gender_table<-summary(project_data$gender)
gender_table
g<-prop.table(gender_table)
g

tosecsch_table<-summary(project_data$TOSecSch)
tosecsch_table
to<-prop.table(tosecsch_table)
to

livinginsch_table<-summary(project_data$livingINsch)
livinginsch_table
li<-prop.table(livinginsch_table)
li<-round(li*100,2)
li

familystr_table<-summary(project_data$familystructure)
familystr_table
fam<-prop.table(familystr_table)
round(fam*100,2)

deptatsch_table<-summary(project_data$Deptatsecsch)
deptatsch_table
dept<-prop.table(deptatsch_table)
round(dept*100,2)

base_table<-summary(project_data$base)
base_table
b<-prop.table(base_table)
round(b*100,2)

sponsor_table<-summary(project_data$sponsor)
sponsor_table
spons<-prop.table(sponsor_table)
round(spons*100,2)

zone_table<-summary(project_data$zone)
zone_table
z<-prop.table(zone_table)
round(z*100,2)

fatheredu_table<- summary(project_data$Fathersedu)
fatheredu_table
f<-prop.table(fatheredu_table)
round(f*100,2)

motheredu_table<- summary(project_data$Mothersedu)
motheredu_table
m<-prop.table(motheredu_table)
round(m*100,2)

schisscam_table<- summary(project_data$Schoolisscam)
schisscam_table
sch<-prop.table(schisscam_table)
round(sch*100,2)

Educationisthebestlegacy_table<- summary(project_data$Educationisthebestlegacy)
Educationisthebestlegacy_table
edu<-prop.table(Educationisthebestlegacy_table)
round(edu*100,2)

Schoolisscam<- summary(project_data$Schoolisscam)
Schoolisscam
scam<-prop.table(Schoolisscam)
round(scam*100,2)

olevel_table<- summary(project_data$olevel)
olevel_table
olvl<-prop.table(olevel_table)
round(olvl*100,2)



##form a presentable dataframe that'll include variable name, categories decoded, 
##level count,level proportion, categories decoded,

#bind<-c(p,g,to,li,fam,dept,b,spons,z,f,m,edu,scam,olvl)

#bind_cent<-round(bind,2)
#bind_cent
#tab['ddd']<- "word"
#tab$ddd[1]<- p
#tab
#for (x in bind){x<-round(x*100,2)}
#class(b)
#p<-list(p)
#class(p)


#data is so unstructured, we couldn't get head out of it 
##that was when we were using .sav file, 
###now, using .csv file, we could get the head
str(project_data)
##while table works well for categorical data, 
#summary works for both numerical and categorical data

#kk<-prop.table(summary(project_data$gender))
#typeof(kk)

summary(project_data$survivaltime)

#class and level of a categorical variable
class(project_data$gender)
levels(project_data$gender)

quantile(project_data$survivaltime)
#iqr(project_data$survivaltime)
median(project_data$survivaltime)
###mean absolute deviation fot the survt
### mad=median(|x-median(x)|)
mad(project_data$survivaltime, na.rm = TRUE)

#####__________________________######
#### the contingency tables ####
CrossTable(project_data$status, project_data$gender)
CrossTable(project_data$status, project_data$zone)
CrossTable(project_data$Educationisthebestlegacy, project_data$Schoolisscam)
CrossTable(project_data$status, project_data$TOSecSch)
CrossTable(project_data$status, project_data$livingINsch)
CrossTable(project_data$status, project_data$olevel)

