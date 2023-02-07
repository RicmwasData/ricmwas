#load the libraries
library(tidyverse)
library("lmtest")
library("multiwayvcov")
library(gridExtra)
library(sandwich)
options(scipen=999)

#load the data
Data_final  <-load("data_final_25Jan2022.RData")

#Look into the data. 
"I used the str function to inspect the structure of the data. My data is a dataframe 
Data type with 962 obs and 48 variables. The data contains three data tpye format i.e 
int= interger data tpye, chr= character data type and Factor= factor data type."
str(data_final)

#summary stat.
"I also used the sumary function to inpesct the data further. The summary fuction gives
a summary statics of the data e.g mean, median, maximum value and minimum value.
For character variables the fuction return the mode, class and length of the variable.
The function also gives information on how many miss values are in each variable"

summary(data_final)

#clean the data. 
"From the summary and srt function is was clear that the data had missing values. There
might also be outliers in the data which suggest that the data needs some cleaning. However,
the data had been prepared to much the analysis of the paper and I did not see any reason
to remove the missing values or the outliers. The boxplot below demonstartes precence of 
outliers in the data"
boxplot(data_final$word_count, main="Word count")

#distribution. 

hist(data_final$word_count, main = "Word Count per report",
     xlab = "Word Count")
#Dependent variables
hist(data_final$prop_attn, main = "Overall", xlab = "Overall")
hist(data_final$prop_lead, main = "Leadership", xlab = "Leadership")
hist(data_final$prop_pay, main = "Pay", xlab = "Pay")
hist(data_final$prop_family, main = "Family Care", xlab = "Family Care")
hist(data_final$prop_harass, main = "Discrimination/Harassment",
     xlab = "Discrimination/Harassment")
"The wordcount and all the dependent variables are srongly skewed to the right and 
tis do not follow a normal distribution"

#Important Ivs

#Quota
ggplot(data = data_final, aes(x= factor(quota), fill= factor(quota) ))+
  geom_bar()+
  labs(title = "Control vs Treatement Group",
       fill= "",x="")+
  theme_bw()
"Most observations falls under the control group"

#Sustainability
ggplot(data = data_final, aes(x= factor(sustain), fill= factor(sustain) ))+
  geom_bar()+
  labs(title = "Sustainability Vs  annual report",
       fill= "",x="")+
  theme_bw()
"The barplot shows that majority of the reports used were annual report"

hist(data_final$pct_rev_change, main = '% Revenue Change',
     xlab = '% Revenue Change')
"The % Revenue Change in revenue is strongly skewed to the right, Majority of the conpanies
fall under 100% with a few cases above 100%"


########## Recreating figure 1 ##########
'''
The first thing I need to recreate is figure 1, which can be divided into:
fig1_overall and fig1_leadership

fig1_overall is a plot over the variable prop_attn x years grouped by country, 
where x= years, y=prop_attn, 
To pplot the data ggplot is used, col=country is added to distinguish between the two countries
I add the regression lines (prop_attn~year) for data less than 2011 and greater 
than 2010 using the geom_smooth function
'''
fig1_overall <- ggplot(data=data_final, aes(x=year, y=prop_attn, col=country)) +
  geom_point() +  theme_bw() +
  #I add the function scale_x_continuous to add breaks and limits on the x-axis.
  scale_x_continuous(breaks=c(2007, 2011, 2017), lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,], show.legend = F) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,], show.legend = F) + 
  #I then add titles, and colors - I decide to use different colors than grey to make it easier to read
  #from this point Greece is orange:#E69F00 and Italy is turquoise:"#56B4E9"
  labs( title = "Overall attention to gender equality issues,
          before and after quota law (2011)",
        x="Year", y="Attention")+ 
  theme() + scale_color_manual(values=c( "#E69F00", "#56B4E9"))
fig1_overall



#I now do the same for fig1_leadership, using the variable prop_attn_no_lead

fig1_leadership <- ggplot(data=data_final, aes(x=year, y= prop_attn, col=country)) +
  geom_point() + theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,], show.legend = F) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,], show.legend = F) + 
  theme(legend.title = element_blank()) +
  labs( title = "Attention to gendergap in leadership,
          before and after quota law (2011)",
        x="Year", y="Attention") + theme()+scale_color_manual(values=c( "#E69F00", "#56B4E9"))

fig1_leadership

#Finally I use the the grid.arrange function to put the 2 plots side by side. 
grid.arrange(fig1_overall, fig1_leadership, nrow = 1)

#To extend the study i repeat for the other DV

fig1_pay <- ggplot(data=data_final, aes(x=year, y=prop_pay, col=country)) +
  geom_point() +  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,], show.legend = F) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,], show.legend = F) + 
  #I then add titles, and colors - I decide to use different colors than grey to make it easier to read
  labs( title = "Overall attention to gendergap in pay,
          before and after quota law (2011)",
        x="Year", y="Attention") + theme() + scale_color_manual(values=c( "green", "orange"))

fig1_family <- ggplot(data=data_final, aes(x=year, y=prop_family, col=country)) +
  geom_point() +  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,], show.legend = F) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,], show.legend = F) + 
  #I then add titles, and colors - I decide to use different colors than grey to make it easier to read
  labs( title = "Overall attention to family care,
          before and after quota law (2011)",
        x="Year", y="Attention")+ theme() + scale_color_manual(values=c( "green", "orange"))

fig1_Harass <- ggplot(data=data_final, aes(x=year, y=prop_harass, col=country)) +
  geom_point() +  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,], show.legend = F) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,], show.legend = F) + 
  #I then add titles, and colors - I decide to use different colors than grey to make it easier to read
  labs( title = "Overall attention to harrasment,
          before and after quota law (2011)",
        x="Year", y="Attention")+ theme() + scale_color_manual(values=c( "green", "orange"))
fig1_Harass

#### Table 2 ###
#for table 2 we run 5 linear regression analysis with independent variables as
# (year_f + company + quota + sustain + pct_rev_change) for all the 5 models.
# The dependent variables are prop_attn,prop_lead,prop_pay,prop_family,prop_harass,
#for model 1 to 5. 

#We then use the function cluster.vcov in R to computes robust standard errors 
#for each linear regression models. The uses the cluster-robust estimator of 
#variance-covariance matrix, which adjusts the standard errors to account for 
#the non-independence of observations within clusters.
# We used the coeftest() function  to perform hypothesis tests on the coefficients
#for the regression model and  robust model, the results are saved as a matrix. 

############### recreating table 2 #################
'''
I want to recreate model 1-5
All the models will be names modela1,modela2 ect. (models for table 3 & 4 will be named b and c) 

Modela1:
First I create the DID estimator using lm() in R. I do this based of the specifikation in the article. 
This gived the model: lm(outcome/DV ~ time + company + treatment + controls) 
Using the Code book i find the variables For Modela1,
 outcome=prop_attn, time=year_f (obs: looking at the dataset there was also a year variable, 
 - but i could not find any difference between the 2 and looked to the original code to see which was used),
 company=company, treatment=quota and controls= sustain and pct_rev_change.
'''
modela1<-lm(prop_attn ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final)
# I now need to add the standard errors, which the article clusters at the company level. 
Cluster1_com <- vcovCL(modela1, cluster = data_final$company)
#Finally I perform a t-test using coeftest,
modela1_final <- coeftest(modela1, vcov = Cluster1_com)
# I run the model
modela1_final
#Looking at the last part of the model i find:
#Quota = 0.0328 **, sustain= 0,122***, pct_rec_change=-0.000
#I check the output with the article and find that both coefficients and std. error match

#I now repeat for the other models , 
modela2<-lm(prop_lead ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final)
Cluster2_com <- vcovCL(modela2, cluster = data_final$company)
modela2_final <- coeftest(modela2, vcov = Cluster2_com) 
modela2_final
#Quota = 0.0123 ***, sustain= 0,017***, pct_rec_change=-0.000

modela3<-lm(prop_pay ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final)
Cluster3_com <- vcovCL(modela3, cluster = data_final$company)
modela3_final <- coeftest(modela3, vcov = Cluster3_com) 
modela3_final 
#Quota = 0.002*, sustain=0.001, pct_rev_change=0.000***

modela4<-lm(prop_family ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final)
Cluster4_com <- vcovCL(modela4, cluster = data_final$company)
modela4_final <- coeftest(modela4, vcov = Cluster4_com) 
modela4_final
# Quota=0.020*, sustain=0.101***, pct_rev_change=0.000

modela5<-lm(prop_harass ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final)
Cluster5_com <- vcovCL(modela5, cluster = data_final$company)
modela5_final <- coeftest(modela5, vcov = Cluster5_com) 
modela5_final
## Quota=-0.001*, sustain=0.003, pct_rev_change=0.000
#I add the results and std error to my table 1 in assignment


########## Recreating figure 2 ##########
'''
I need to make two graphs, first i code for the first which i name: fig2meanNr
To recreate figure 2 I first group the data by year and country
I then need to get the mean for number/sum of female board members (fem_sum)
'''
mean_sum_FBM <- data_final %>% 
  group_by(year, country) %>%
  summarize(mean_years_sum = mean(fem_sum, na.rm=T), .groups = 'drop')%>%
  as.data.frame(mean_sum_FBM)
#explain the last part groups and drop and as.data.frame to me :) 

# I now plot the data using ggplot  
fig2mean1 <- ggplot(mean_sum_FBM,aes(x= year, y= mean_years_sum , col= country, group= country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  theme() + scale_x_continuous(breaks = c(2007:2017)) +
  # I add a title for the graph and for the X-axis and change the y-axis intervals to make it easier to read the values
  # I Also change the color to match the previous graphs
  labs(y= "Mean number of Female Board Members",
       x="Year",
       title="Mean number of female Board Members over time") +
  scale_y_continuous(breaks=seq(0.0, 10, 1), limits=c(0, 10))+  theme_classic()+ scale_color_manual(values=c( "#E69F00", "#56B4E9"))
fig2mean1

# I now move on to the second graph: fig2share, 
# but where I get the mean for the share of female board members (fem_share)

mean_share_FBM <- data_final %>% 
  group_by(year, country) %>%
  summarize(mean_years_share = mean(fem_share, na.rm=T), .groups = 'drop')%>%
  as.data.frame(mean_share_FBM)

# I now plot the data 
fig2share <- ggplot(mean_share_FBM,aes(x=year, y=mean_years_share, col=country, group=country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  theme() + scale_x_continuous(breaks = c(2007:2017)) + 
  # I add a title for the graph and for the X-axis and change the y-axis intervals to make it easier to read the values
  # I Also change the color to match the previous graphs
  labs(y="Mean Share of Women Board Members",
       x="Year", title="Mean share of Woman Board Members over time") +
  scale_y_continuous(breaks=seq(0.0, 50, 5), limits=c(0, 50)) + theme_classic() + scale_color_manual(values=c( "#E69F00", "#56B4E9"))
fig2share


#Finally the grid function puts the plots side by side.
grid.arrange(fig2meanNr, fig2share, nrow = 1) 

########## Recreating table 3 ############
'''
To test for spilover effects the article uses an interactive model
The specifikation is the same as for table 2, except that the quota variable is interacted with each year post quota year
lm(outcome/DV ~ time + company + treatment:time (done for each year post 2011) + controls) 
The model is only tested on the collapsed DP that meassures the total attention (prop_attn)
(note: My initial model i added year in the place of time, as I had done in the previous model, however
when running the model, the output was slightly off compared to the article. I then tried adding each year seperatly 
which gave me the correct output, due to xxx) 
'''
#The first model estimates the effects of Gender quota law on the prop_attn variable
modelb1<-lm(prop_attn ~ company + quota:year2011 + quota:year2012 + 
              quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017
            + year2007 + year2008 + year2009 + year2011 + year2012
            + year2013 + year2014 + year2015 + year2016 + year2017 + 
              sustain + pct_rev_change, data=data_final)
Clusterb1_com <- vcovCL(modelb1, cluster = data_final$company)
modelb1_final <- coeftest(modelb1, vcov = Clusterb1_com) 
modelb1_final
#quota:year2011=0.022**, quota:year2012=0.026 (significant at 0.1),quota:year2013=0.036**
#quota:year2014=0.027*, quota:year2015= 0.033*, quota:year2016= 0.043**, quota:year2017=0.046*
#sustain=0.012***, pct_rev_change=-0.012


#note: i need to find the r values and r squared for all the models :) 
#also a way to get out number of observations!
#why do you have to add year each time and not like?: 

modelb1Trial<-lm(prop_attn ~ year + company + quota:year2011 + quota:year2012 + 
                   quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 +
                   quota:year2017 + sustain + pct_rev_change, data=data_final)
vcov_company <- cluster.vcov(modelb1Trial, data_final$company)
m1_dd2 <- as.matrix(coeftest(modelb1Trial, vcov_company)) 
m1_dd2


# I now repeat for the second model in the table
#I keep the same formula but change the DP to the share of female board memebers (fem_share)
modelb2<-lm(fem_share ~ company + quota:year2011 + quota:year2012 + 
              quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
              year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
              sustain + pct_rev_change, data=data_final)
Clusterb2_com <- vcovCL(modelb2, cluster = data_final$company)
modelb2_final <- coeftest(modelb2, vcov = Clusterb2_com) 
modelb2_final 
#add the output

#Finally I repeat for the final model in the table 
# While the table name 'all' can be a bit confusing as that is the same name they have given the first estimation -
# You can read in the text that the final estimation is the same as modelb1, but with the variable fem_share added as a covariate
#therefor i copy modelb1 and add fem_share to the end after the controls: 

modelb3 <-lm(prop_attn ~ company + quota:year2011 + quota:year2012 + 
               quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
               year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
               sustain + pct_rev_change + fem_share, data=data_final)
Clusterb3_com <- vcovCL(modelb3, cluster = data_final$company)
modelb3_final <- coeftest(modelb3, vcov = Clusterb3_com) 
modelb3_final 
#add the output


########## Recreating table 4 ############
'''
The final model is specified as ln(outcome/dp ~ company + year + quotashock + controls)
BY looking through the data set i found the following variables: chg_14_10, below_mean_change_14_10 and 
above_mean_change_14_10, there is no mention of theese variables in the codebook, but based of the 
article and the years 2010 and 2014 I assume this is the varible used for quotashock. 
'''
# I can now create the 3 models for the quotashock 
#Based of the names of the variables and the 3 specfikations in the article, the first specification uses the variable chg_14_10
modelc1<-lm(prop_attn ~ company + year2007 + year2008 + year2009 + year2011 + year2012
            + year2013 + year2014 + year2015 + year2016 + year2017 + chg_14_10 + sustain +
              pct_rev_change, data=data_final)
Clusterc1_com <- vcovCL(modelc1, cluster = data_final$company)
modelc1_final <- coeftest(modelc1, vcov = Clusterc1_com) 
modelc1_final 

# I now repeat for the two next models in the tabel first using below_mean_change_14_10 and then above_mean_change_14_10
modelc2<-lm(prop_attn ~ company + year2007 + year2008 + year2009 + year2011 + year2012
            + year2013 + year2014 + year2015 + year2016 + year2017 + chg_14_10 + sustain +
              below_mean_change_14_10, data=data_final)
Clusterc2_com <- vcovCL(modelc2, cluster = data_final$company)
modelc2_final <- coeftest(modelc2, vcov = Clusterc2_com) 
modelc2_final 

#and 
modelc3<-lm(prop_attn ~ company + year2007 + year2008 + year2009 + year2011 + year2012
            + year2013 + year2014 + year2015 + year2016 + year2017 + chg_14_10 + sustain +
              above_mean_change_14_10, data=data_final)
Clusterc3_com <- vcovCL(modelc3, cluster = data_final$company)
modelc3_final <- coeftest(modelc3, vcov = Clusterc3_com) 
modelc3_final 

############## Extension ####################
#a) Fisher permutation test 
"I create a function that allows you to take a random sample of 
elements from the dependent varaiable. Every time the fuction rsamp2 the
DP variable is rearranged."
rsamp2 <- function(respvar="prop_attn",data=data_final) {
  perm_df <- data
  perm_df[[respvar]] <- sample(perm_df[[respvar]])
  perm_df
}

"I then creat a function (simfun) that runs a regression model of the DP on the most important
IV (quota) and retuns the coeffients for quota"
simfun <- function(dat) {
  coef(lm(prop_attn~quota,data=dat))["quota"]
}

set.seed(101)
"I then chain the two fuction. this mean that I run the regresion model with a resampled
Depende varaible and repeat the process 16000 times, each time a get a coefficient and 
save it in a vector called permdist_skew. To get the pvalue i calculate propotion using 
mean(permutations>=obs)). The logical statement returns a logical (FALSE/TRUE) vector,
which then gets converted to a 0/1 vector when you ask R to take the mean,
so this is equivalent to counting the number of true values and dividing by the
length "
permdist_skew <- replicate(16000,simfun(rsamp2()))
(skew_pval <- mean(abs(permdist_skew)>=abs(simfun(data_final))))

"The code below simplifies the process above,"
#Using a library
library(lmPerm)
summary(lmp(prop_attn ~ quota,data=data_final, maxIter=16000))

"The p-value was less than 0.05 therefore we reject the null hypothesis 
( that there is no difference between the two groups being compared) and conclude that
there is a significant diffenrce in between the treatment group and control group."

## Adding the counter 
"I sued same data used for figure two right side"
mean_sum_FBM <- data_final %>% 
  group_by(year, country) %>%
  summarize(mean_years_sum = mean(fem_sum, na.rm=T), .groups = 'drop')%>%
  as.data.frame(mean_sum_FBM)

# I pull the control value (Greece 2011) form the data
control2011 = mean_sum_FBM%>%
  filter(country=="Greece", year==2011)%>%
  pull(mean_years_sum)

# I pull the treat value (Italy 2011) form the data
treat2011 = mean_sum_FBM%>%
  filter(country=="Italy", year==2011)%>%
  pull(mean_years_sum)

'I created a new data by filtering on Greece from mean_sum_FBM data
I then create a new variable (counter) with the formula,
Countervalue=Controlvalueyear + (Treatment2011 - controlvalue2011). 
all values before year 2011 were converted to NA' 

control_data<- mean_sum_FBM%>%
  filter(country=="Greece")%>%
  mutate(counter= mean_years_sum+ (treat2011- control2011),
         counter= ifelse(year<2011, NA, counter))


"The graph is the same as the one in figure two, Howerver in this case 
I added another geom_line to plot the counterfactual data from control_data. 
The line type is set to data and colour was added to much italy."

ggplot(mean_sum_FBM,aes(x= year, y= mean_years_sum , col= country, group= country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  geom_line(data=control_data, aes(x= year, y= counter),col="#56B4E9",
            linetype = "dashed")+
  theme() + scale_x_continuous(breaks = c(2007:2017)) +
  labs(y= "Mean number of Female Board Members",
       x="Year",
       title="Mean number of female Board Members over time",
       subtitle = "With Counterfactual") +
  scale_y_continuous(breaks=seq(0.0, 10, 1), limits=c(0, 10))+ 
  theme_classic()+ scale_color_manual(values=c( "#E69F00", "#56B4E9"))




#C) change the quota to 2012 and drop year 2017. / do model 1-5
"using the ifels fucntion we create a quota varible whereby if the year is greater that 
2012 and the company is fron italy quota is codes as 1 and 0 otherwise. I then filter out
year 2017 using the filter function. The data is now saved as data_final2"
data_final2<- data_final%>%
  mutate(quota= ifelse(year>2011 & country=="Italy",1,0))%>%
  filter(year!=2017)

#I then repeat the 5 models using the new data. 

New_m_a1<-lm(prop_attn ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final2)
New_Cluster1_com <- vcovCL(New_m_a1, cluster = data_final2$company)
#Finally I perform a t-test using coeftest,
New_modela1_final <- coeftest(New_m_a1, vcov = New_Cluster1_com)
# I run the model
New_modela1_final

#Looking at the last part of the model i find:
#Quota = 0.0263 **, sustain= 0.123***, pct_rec_change=-0.000

#I now repeat for the other models , 
New_m_a2<-lm(prop_lead ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final2)
New_Cluster2_com <- vcovCL(New_m_a2, cluster = data_final2$company)
New_modela2_final <- coeftest(New_m_a2, vcov = New_Cluster2_com) 
New_modela2_final
#Quota = 0.001 ***, sustain=  0.014***, pct_rec_change=-0.000

New_m_a3<-lm(prop_pay ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final2)
New_Cluster3_com <- vcovCL(New_m_a3, cluster = data_final2$company)
New_modela3_final <- coeftest(New_m_a3, vcov = New_Cluster3_com) 
New_modela3_final 
#Quota = 0.001*, sustain=-0.000, pct_rev_change=-0.000***

New_m_a4<-lm(prop_family ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final2)
New_Cluster4_com <- vcovCL(New_m_a4, cluster = data_final2$company)
New_modela4_final <- coeftest(New_m_a4, vcov = New_Cluster4_com) 
New_modela4_final
# Quota=0.016, sustain=0.103***, pct_rev_change=0.000

New_m_a5<-lm(prop_harass ~ year_f + company + quota + sustain + pct_rev_change, 
            data=data_final2)
New_Cluster5_com <- vcovCL(New_m_a5, cluster = data_final2$company)
New_modela5_final <- coeftest(New_m_a5, vcov = New_Cluster5_com) 
New_modela5_final
# Quota=-0.001, sustain=0.006*, pct_rev_change=0.000



# I have adde some code
