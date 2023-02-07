#load the libraries
library(tidyverse)
library("lmtest")
library("multiwayvcov")
library(stargazer)
library(gridExtra)

#load the data
load("data_final_25Jan2022.RData")

#Look into the data. 

#summary stat.

#clean the data. 

#distribution. 


#The plots shows year vs prop_attn grouped by country. 
#We have add the regression lines (prop_attn~year) for data less than 2011 and greater
#than 2010 using the geom_smooth function.
#The plot is then saved as p1

p1 <- ggplot(data=data_final, aes(x=year, y=prop_attn, col=country)) +
  geom_point() + 
  theme_bw() +
  #We used the function scale_x_continuous so as to add the break and the x-axis limit.
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,], show.legend = F) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,], show.legend = F) + 
  labs( title = "Overall attention to gender equality issues,
          before and after quota law (2011)",
        x="Year", y="Attention")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c( "#E69F00", "#56B4E9"))

p1

#The plots shows year vs prop_attn grouped by country. 
#We have add the regression lines (prop_attn~year) for data greater than 2011 and lessr
#than 2010 using the geom_smooth function.
#The plot is then saved as p2

p2 <- ggplot(data=data_final, aes(x=year, y=prop_attn, col=country)) +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year > 2011,], show.legend = F) + 
  geom_smooth(method="lm", data=data_final[data_final$year < 2010,], show.legend = F) + 
  theme(legend.title = element_blank()) +
  labs( title = "Overall attention to gender equality issues,
          before and after quota law (2011)",
        x="Year", y="Attention") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c( "#E69F00", "#56B4E9"))

p2

#the grid.arrange fuction puts the 2 plots side by side. 
grid.arrange(p1, p2, nrow = 1)


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

model1<-lm(prop_attn ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(model1, data_final$company)
model1_se <- as.matrix(coeftest(model1, vcov_company)) 

model2<-lm(prop_lead ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(model2, data_final$company)
model2_se <- as.matrix(coeftest(model2, vcov_company)) 

model3<-lm(prop_pay ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(model3, data_final$company)
model3_se <- as.matrix(coeftest(model3, vcov_company)) 

model4<-lm(prop_family ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(model4, data_final$company)
model4_se <- as.matrix(coeftest(model4, vcov_company)) 

model5<-lm(prop_harass ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(model5, data_final$company)
model5_se <- as.matrix(coeftest(model5, vcov_company))


# To formulate the table we used the stargazer function 
stargazer(model1, model2, model3, model4, model5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c("Overall","Leadership","Pay", "Family Care", 
            "Discrim/Harass"),
          se = list(model1_se[,2],model2_se[,2],model3_se[,2],
                    model4_se[,2],model5_se[,2]),
          p = list(model1_se[,4], model2_se[,4],model3_se[,4],
                   model4_se[,4],model5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")),
          type = "html",  out="Table2.doc")

#### FIGURE 2 ##

#We group the data by year and country then get the mean for fem_sum. 
#We save the data as df_fm_num
df_fm_num <- data_final %>% 
  group_by(year, country) %>%
  summarize(Avg_no_year = mean(fem_sum, na.rm=T), .groups = 'drop')%>%
  as.data.frame(df_fm_num )


p3<- ggplot(df_fm_num,
                   aes(x=year, y=Avg_no_year , col=country, 
                       group=country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = c(2007:2017)) +
  labs(y="Mean Number of Women Board Members",
       x="",
       title="") +
  scale_y_continuous(breaks=seq(0.0, 10, 1), limits=c(0, 10))+
  theme(plot.title = element_text(size=12))+  theme_classic()+
  scale_color_manual(values=c( "#E69F00", "#56B4E9"))
p3

#left

#We group the data by year and country then get the mean for fem_share. 
#We save the data as df_fm_num
df_fm_share <- data_final %>% 
  group_by(year, country) %>%
  summarize(Avg_share_year = mean(fem_share, na.rm=T), .groups = 'drop')%>%
  as.data.frame(plot_female_share)

#We create a plot with x axis as year and y axis as Avg_share_year
# The graph groups the data by country
p4 <- ggplot(df_fm_share,
                    aes(x=year, y=Avg_share_year, 
                        col=country, group=country)) +
  geom_point(stat="identity") +
  geom_line(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0, 50) +
  scale_x_continuous(breaks = c(2007:2017)) +
  labs(y="Mean Share of Women Board Members",
       x="") +
  theme_classic()+
  scale_color_manual(values=c( "#E69F00", "#56B4E9"))
p4

#The grid function puts the plots side by side.
grid.arrange(p3, p4, nrow = 1) 

###### Table 3 ###
m1dd<-lm(prop_attn ~ company + quota:year2011 + quota:year2012 + 
           quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
           year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
           sustain + pct_rev_change, 
         data=data_final)
vcov_company <- cluster.vcov(m1dd, data_final$company)
m1_dd2 <- as.matrix(coeftest(m1dd, vcov_company)) 


m1ddb<-lm(fem_share ~ company + quota:year2011 + quota:year2012 + 
            quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
            year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
            sustain + pct_rev_change, 
          data=data_final)
vcov_company <- cluster.vcov(m1ddb, data_final$company)
m1_dd4 <- as.matrix(coeftest(m1ddb, vcov_company)) 


m1ddc<-lm(prop_attn ~ company + quota:year2011 + quota:year2012 + 
            quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
            year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
            sustain + pct_rev_change + fem_share, 
          data=data_final)
vcov_company <- cluster.vcov(m1ddc, data_final$company)
m1_dd6 <- as.matrix(coeftest(m1ddc, vcov_company)) 


### Note: Need to manually delete year FE from stargazer output  

stargazer(m1dd, m1ddb, m1ddc,
          font.size = "small",
          title="Table 3: Effects of Gender Quota Law Over Time",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Share women", "Overall"),
          order=c("quota:year2011", "quota:year2012",  
                  "quota:year2013", "quota:year2014", "quota:year2015", "quota:year2016", "quota:year2017",
                  "fem_share", "sustain", "pct_rev_change"),
          se = list(m1_dd2[,2], 
                    m1_dd4[,2],
                    m1_dd6[,2]  ),
          p = list(m1_dd2[,4], 
                   m1_dd4[,4],
                   m1_dd6[,4]  ),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("company", "Constant"),
          column.sep.width = "-15pt",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes")),
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          type = "html",
          out="Table3.doc")

#### Table 4 ###
mshock<-lm(prop_attn ~ company + factor(year) + chg_14_10 + sustain  + pct_rev_change, 
           data=data_final)
vcov_company <- cluster.vcov(mshock, data_final$company)
mshock_se <- as.matrix(coeftest(mshock, vcov_company)) 

mschock2<-lm(prop_attn ~ company+ factor(year) + below_mean_change_14_10 + sustain + pct_rev_change, 
             data=data_final)
vcov_company <- cluster.vcov(mschock2, data_final$company)
mschock2_se <- as.matrix(coeftest(mschock2, vcov_company)) 


mshock3<-lm(prop_attn ~ company + factor(year) + above_mean_change_14_10 +  sustain + pct_rev_change, 
            data=data_final)
vcov_company <- cluster.vcov(mshock3, data_final$company)
mshock3_se <- as.matrix(coeftest(mshock3, vcov_company)) 


stargazer(mshock, mschock2, mshock3,
          font.size = "small",
          title="Table 4: Effects of Quota Shocks",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Overall",
            "Overall"),
          se = list(mshock_se[,2], 
                    mschock2_se[,2], 
                    mshock3_se[,2]),
          p = list(mshock_se[,4], 
                   mschock2_se[,4], 
                   mshock3_se[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year", "company", "Constant"),
          column.sep.width = "-15pt", star.cutoffs=c(.05, .01, .001),
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes")),
          digits = 3, digits.extra=0,
          notes = "Robust standard errors clustered around company in parentheses.",
          type = "html",  out="Table4.doc")

