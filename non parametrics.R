
#library(ROSE) #installed
library(caret)#installed
#library(smotefamily) ## installed


library(survival)
library(survminer)
library(survMisc)
library(dplyr)
library(ComparisonSurv)
#library(writexl)
#presenting the data from excel 
project_data<- read.csv("C:/Users/User/Desktop/final project/encoded further surv 13 vars.csv")
head(project_data)
#attach so you dont have to stress through $ no more
attach(project_data)

# Categorizing the data
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

project_data<-project_data[,c("survivaltime","status","gender","TOSecSch","livingINsch",
                              "familystructure","Deptatsecsch","base","sponsor","OlevelAttempts",
                              "zone","Fathersedu","Mothersedu","Educationisthebestlegacy","Schoolisscam",
                              "olevel")]
str(project_data)

#### pie chart
#png(file = "Status Proportion Pie Chart.jpg")

#pie(z,labels = c("Gained Admission","Did Not Gain Admission"),main = "Admission Status Pie Chart",col = rainbow(length(z)))

#dev.off()
######

#use with  and table to view the objects/ factors of the data 
#...and qty. of each class respectively
with(project_data,{
  print(table(status));
  print(table(zone));
  print(table(gender));
  print(table(TOSecSch));
  print(table(livingINsch));
  print(table(familystructure));
  print(table(Deptatsecsch));
  print(table(base));
  print(table(sponsor));
  print(table(OlevelAttempts));
})

#index of values in the minority and majority class
minority_class<-which(project_data$status=="0") #minority class of the dependent variable
majority_class<-which(project_data$status=="1")  #majority class of the dependent variable- Status
length(minority_class)
length(majority_class)

#using base R, boost up the miniority class up to the majority class
base_upsampling<-sample(minority_class,length(majority_class), replace = TRUE)
length(base_upsampling)

###create a new dataset from the initial majority class and the boosted (sampled)...
#...minority class to form a larger (191*2 = 382) dataset
new_data<- project_data[c(base_upsampling,majority_class),]
View(new_data)
str(new_data)

#using caret for the upsample
new.df<- upSample(project_data[,-2],yname = "status",project_data$status);new.df
str(new.df)
View(new.df)

#### pie chart for the new balanced data

status_cat<-table(new.df$status) # create a table for the categorical dependent ...
#... variable to be used in plotting pie chart
png(file = "Status Proportion Pie Chart.jpg") # for saving the plot

#the pie-plot -->
pie(status_cat,labels = c("Gained Admission","Did Not Gain Admission"),
    main = "Balanced Admission Status Chart",col = rainbow(length(status_cat)))
dev.off()

######



##Reduce covariates
#by relative importance, decided to use the variables:
#  **** gender, zone,tosecsch, opinion, livinginsch, olevelattempts
#gender,TOSecSch,livingINsch, zone, Educationisthebestlegacy, Schoolisscam, olevel

##define the variables
time<- project_data$survivaltime
status <- project_data$status
covariates<- cbind(gender,TOSecSch,livingINsch, zone, Educationisthebestlegacy, 
                   Schoolisscam, olevel)
head(covariates)
View(covariates)


## trying to find a way to convert my table into an excel file
# but got the error "cannot coerce class '"summary.survfit"' to a data.frame"
#s<-summary(kmsurv)
#typeof(s)
#as.data.frame(s)
#write_xlsx(s,"C:/Users/User/Desktop/final project/s.xlsx")
#write.csv(s,"C:/Users/User/Desktop/final project/s.csv")

covariates<-as.data.frame(covariates)
typeof(covariates)
write.csv(covariates,"C:/Users/User/Desktop/final project/covariates.csv")
## converted to csv successfully

##kaplan meier non-parametric analysis
kmsurv<- survfit(Surv(time, status) ~ 1, ctype = 1,data = project_data)
summary(kmsurv)
surv_median(kmsurv)
ggsurvplot(kmsurv, fun = "pct", color = "black",
           surv.median.line ="hv",
           conf.int = TRUE,
           conf.int.fill = 'red',
           title ='kaplan-meier survivor curve for the
     "time till first tertiary admission" data', xlim = c(0,30),
           font.title = c(16, "bold", "black")
           )


##non-parametric analysis by categorical variables-the relative important ones amongst them


# kmsurvgender<- km estimated curve for gender
kmsurvgender<- survfit(Surv(time, status) ~ gender,ctype = 1, data = project_data )
summary(kmsurvgender)
surv_median(kmsurvgender)##compare median times of levels
ggsurvplot(kmsurvgender, fun = "pct", linetype = c(1,5), 
           surv.median.line ="hv",
           legend.title = "gender", legend.labs = c("male", "female"),
           title ='Estimated Survivor Curve for Gender', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
### hypothesis testing 
#H0: there is no difference in survival functions (curve) between the two levelsof gender
#H0: s(t_male) = s(t_female) 
##H1: difference in at least one pair of survival function
#H1: s(t_male) != s(t_female)

###log rank test
##...and maybe other tests
#rho = 0 # log_rank
#rho = 1#generalized wilcoxon
#rho = 1.5 # tarone_ware test
survdiff(Surv(time, status) ~gender,rho= 0)
#**based on log_rank,[p=0.4] > [0.05], the test is sgft, :. we do not reject H0  


# kmsurv2<- km estimated curve for livinginsch
kmsurv2<- survfit(Surv(time, status) ~ livingINsch,ctype = 1, data = project_data )
summary(kmsurv2)
ggsurvplot(kmsurv2, fun = "pct", linetype = c(1,5), 
           surv.median.line ="hv",
           legend.title = "livinginsch", legend.labs = c("day", "boarder"),
           title ='Estimated Survivor Curve for Living in School Type', xlim = c(0,30))
surv_median(kmsurv2)##compare median times of levels
#hypothesis
#H0: s(t_day) = s(t_boarder)
#H1: s(t_day) != s(t_boarder)
#log_rank test for living in sch
survdiff(Surv(time, status) ~ livingINsch,rho= 0)

# kmsurvtos<- km estimated curve for type of ownership
kmsurvtos<- survfit(Surv(time, status) ~ TOSecSch,ctype = 1,data = project_data )
summary(kmsurvtos)
surv_median(kmsurvtos)##compare median times of levels
ggsurvplot(kmsurvtos, fun = "pct", linetype = c(1,5,9), 
           surv.median.line ="hv",
           legend.title = "Type of Ownership of Secondary School",
           legend.labs = c("federal govt. owned", "state govt. owned", "private"),
           title ='Estimated Survivor Curve for type of ownership of school', xlim = c(0,30))
#hypothesis
#H0: s(t_fed) = s(t_state) = s(t_private)
#H1: s(t_fed) != s(t_state) != s(t_private)
#log_rank test for ownership of sec sch
survdiff(Surv(time, status) ~ TOSecSch,rho= 0)
#interpretation

# kmsurv_zone<- km estimated curve for zone
kmsurvzone<- survfit(Surv(time, status) ~ zone,ctype = 1, data = project_data )
summary(kmsurvzone)
surv_median(kmsurvzone)##compare median times of levels
ggsurvplot(kmsurvzone, fun = "pct", linetype = c(1,5,6,7,8,3), 
           surv.median.line ="hv",
           legend.title = "zone", legend.labs = c("ne","nc","nw","ss","se","sw"),
           title ='Estimated Survivor Curve for Geo-political zone', xlim = c(0,30))
#hypothesis
#H0: s(t_NC) = s(t_NE) = s(t_NW) = s(t_SS) = s(t_SW) = s(t_SS) 
#H1: s(t_NC) != s(t_NE) != s(t_NW) != s(t_SS) != s(t_SW) != s(t_SS)
#log_rank test for zone
survdiff(Surv(time, status) ~zone,rho= 0)


# kmsurvedu<- km estimated curve for education is the best legacy
kmsurvedu<- survfit(Surv(time, status) ~ Educationisthebestlegacy, ctype = 1,data = project_data )
summary(kmsurvedu) 
ggsurvplot(kmsurvedu, fun = "pct", linetype = c(1,5,4,6,8), 
           surv.median.line ="hv",
           legend.title = "education is the best legacy",
           legend.labs = c("strongly agree", 
                           "agree",
                           "neutral",
                           "disagree",
                           "strongly disagree"),
           title ='Estimated Survivor Curve for education is the best legacy', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
surv_median(kmsurvedu)##compare median times of levels
#hypothesis
#H0: s(t_s+) = s(t_+) = s(t_neutral) = s(t_-) = s(t_s-) 
#H1: s(t_s+) != s(t_+) != s(t_neutral) != s(t_-) != s(t_s-)
#log_rank test for opinion
survdiff(Surv(time, status) ~Educationisthebestlegacy,rho= 0)


# kmsurvscam<- km estimated curve for school is scam
kmsurvscam<- survfit(Surv(time, status) ~ Schoolisscam, ctype = 1,data = project_data )
summary(kmsurvscam) 
ggsurvplot(kmsurvscam, fun = "pct", linetype = c(1,5,4,8,3),
           surv.median.line ="hv",
          legend.title = "opinion",
           legend.labs = c("strongly agree s", 
                           "agree s",
                           "neutral s",
                           "disagree s",
                           "strongly disagree s"),
           title ='Estimated Survivor Curve for school is scam', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
surv_median(kmsurvscam)##compare median times of levels
#hypothesis
#H0: s(t_s+) = s(t_+) = s(t_neutral) = s(t_-) = s(t_s-) 
#H1: s(t_s+) != s(t_+) != s(t_neutral) != s(t_-) != s(t_s-)
#log_rank test for sch is scam
survdiff(Surv(time, status) ~ Schoolisscam ,rho= 0)


# kmsurvexam<- km estimated curve for no of times respondents tried 0level examination
kmsurvexam<- survfit(Surv(time, status) ~ olevel,ctype = 1, data = project_data )
summary(kmsurvexam) 
ggsurvplot(kmsurvexam, fun = "pct", linetype = c(1,5), 
           surv.median.line ="hv",
           legend.title = "o-level", legend.labs = c("once", "more than once"),
           title ='Estimated Survivor Curve for O-level', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
##compare median times of levels
surv_median(kmsurvexam)
#hypothesis
#H0: s(t_once) = s(t_more) 
#H1: s(t_once) = s(t_more)
#log_rank test for olevel
survdiff(Surv(time, status) ~olevel,rho= 0)


##Nelson-Aalen non-parametric analysis
#cummulative hazard rate
ggsurvplot(kmsurv, fun = "cumhaz",
           title ='Nelson-Aalen cummulative hazard Curve', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
ggsurvplot(kmsurvgender, fun = "cumhaz",legend.title = "gender", legend.labs = c("male", "female"),
           title ='Nelson-Aalen cummulative hazard Curve for gender', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
ggsurvplot(kmsurvedu, fun = "cumhaz", legend.labs = c("strongly agree", "agree", "neutral", "disagree","strongly disagree"),
           title ='cummulative hazard Curve for education is the best legacy', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
ggsurvplot(kmsurvscam, fun = "cumhaz",legend.title = "number of times", legend.labs = c("strongly agree s", "agree s", "neutral s", "disagree s","strongly disagree s"),
           title ='cummulative hazard Curve for school is scam', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
ggsurvplot(kmsurv2, fun = "cumhaz",legend.title = "living in school type", legend.labs = c("day", "boarder"), 
           linetype = c(1,4),
           title ='cummulative hazard Curve for type of living in school', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
ggsurvplot(kmsurvexam, fun = "cumhaz",legend.title = "number of times", legend.labs = c("once", "more than once"),
           linetype = c(1,9),
           title ='cummulative hazard Curve for olevel attempts', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
ggsurvplot(kmsurvtos, fun = "cumhaz",legend.title = "ownership", legend.labs = c("federal govt", "state", "private"),
           title ='cummulative hazard Curve for school ownership type', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
ggsurvplot(kmsurvzone, fun = "cumhaz",legend.title = "zone", legend.labs = c("NC", "NE",'NW','SE','SS','SW'),
           title ='Nelson-Aalen Curve for geo-political zone', xlim = c(0,30),
           font.title = c(16, "bold", "blue"))
#cummulative hazard can be plotted for all the covariates 
#Cumhazard.plot(time, status,olevel)
#Hazard.plot(time, status,olevel)

#chedu<-survfit(Surv(time, status) ~ Educationisthebestlegacy,ctype = 1,data = project_data )
#chscam<-survfit(Surv(time, status) ~ Schoolisscam,ctype = 1,data = project_data )

##dummy encoding

##dummy for zone where nc is the reference variable
sw<-ifelse(zone=='6',1,0)
ss<-ifelse(zone=='5',1,0)
se<-ifelse(zone=='4',1,0)
nw<-ifelse(zone=='3',1,0)
ne<-ifelse(zone=='2',1,0)

####dummy variable of 2 categorical levels can either be dummied or not,
##if it is not dummied, it automatically takes a level as the dummy
##dummy for gender where male is the reference variable
female<-ifelse(gender=='2',1,0)

##dummy for type of secondary school where fed is the reference variable
state<-ifelse(TOSecSch=='2',1,0)
private<-ifelse(TOSecSch=='3',1,0)

##dummy for livinginsch where boarder is the reference variable
day<-ifelse(livingINsch=='1',1,0)

##dummy for education is the best legacy where strongly disagree is the reference variable
strongly_agree<-ifelse(Educationisthebestlegacy=='1',1,0)
agree<-ifelse(Educationisthebestlegacy =='2',1,0)
neutral<-ifelse(Educationisthebestlegacy=='3',1,0)
disagree<-ifelse(Educationisthebestlegacy=='4',1,0)

##dummy for school is scam where strongly disagree is the reference variable
strongly_agree_s<-ifelse(Schoolisscam=='1',1,0)
agree_s<-ifelse( Schoolisscam=='2',1,0)
neutral_s<-ifelse(Schoolisscam=='3',1,0)
disagree_s<-ifelse(Schoolisscam=='4',1,0)

##dummy for olevel exam where more is the reference variable
once<-ifelse(olevel=='0',1,0)

dummy_covariates<- cbind(sw,ss,se,nw,ne,
                    female,
                    state,private,
                    day,
                    strongly_agree,agree,neutral,disagree,
                    strongly_agree_s,agree_s,neutral_s,disagree_s,
                    once)

head(dummy_covariates)


##cox ph model-->coefficient and hazard rates
coxmodel<- coxph(Surv(time, status)~ dummy_covariates, data = project_data, method = "breslow")
cfit<- coxph(Surv(time, status)~ sw+ss+se+nw+ne+gender+state+private + livingINsch + 
               strongly_agree+agree+neutral+disagree+strongly_agree_s+agree_s+neutral_s+disagree_s+once,
             data = project_data, method = "breslow")
#coxmodel1<- coxph(Surv(time, status)~ covariates, data = project_data, method = "breslow")
#cfit1<- coxph(Surv(time, status)~ gender + TOSecSch +livingINsch + zone + opinion +olevel,
#              data= project_data,)

coxmodel
cfit
#coxmodel1
#cfit1


#summary(coxmodel)# summary will  bring result for #concordance #wald statistics
#summary(coxmodel1)#... #score (log-rank) test, all of which I can't interprete yet
#summary(cfit1)

cox.zph(coxmodel)
plot(cox.zph(coxmodel))
survfit(cfit)#baseline survival
?coxph 

### hypothesis


###testing model... LR, wald, AIC...

##data splicing
#HR