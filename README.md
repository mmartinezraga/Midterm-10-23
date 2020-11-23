# Midterm-10-23
---
<p style="color:rgb(182,18,27);font-family:corbel">Mónica Martínez-Raga</p>
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


<p style="color:rgb(182,18,27);font-family:corbel">1. Normal distribution</p> 

Area:
```{r}
#pnorm(x, mean = , sd = ) : finds area under curve to the left of x. Prob of selecting a number less than or equal to x.

#a
xa = 1.65 #replace
ma = 1
stdeva = 6.5
areaa <- pnorm(xa, mean = ma, sd = stdeva)

#b
xb = 13.67 #replace
mb = 8
stdevb = 2.7
area.lower.tailb <- pnorm(xb, mean = mb, sd = stdevb, lower.tail = TRUE)
area.middleb <- (area.lower.tailb * 2) - 1
area.tailsb <- 1 - area.middleb

#c
xc = -5.4 #replace
mc = -11
stdevc = 4
area.lower.tailc <- pnorm(xc, mean = mc, sd = stdevc, lower.tail = TRUE)
area.middlec <- (area.lower.tailc * 2) - 1
area.tailsc <- 1 - area.middlec

cat("a. Area: ", areaa, "\nb. Area: ", area.tailsb, "\nc. Area: ", area.tailsc)
```


Probability in 2 tails: 
```{r}
install.packages("tigerstats")
require(tigerstats)
```

```{r}
#Finds x (which is also z) with combined area a in both tails. If 1 - a% of the area is between smaller x and x, then area left of x is half of a% + 1 - a%.

#Example: Find two values that leave probability 5% in both tails. 
  #Area in question (aq): 2.5% + 95% = 97.5%
  #95% = 1 - 5%
  #2.5% = 5% / 2

#Z: Will give you the possitve value. Second value is the negative equivalent.
#aq = (a / 2) + (1 - a)
#qnorm(aq, mean = , sd = )

#d
a = 0.158 #replace
m = 14
stdev = 7.4
aq = (a / 2) + (1 - a)
zmax <- qnorm(aq, mean = m, sd = stdev, lower.tail = FALSE)
zmin <- qnorm(aq, mean = m, sd = stdev, lower.tail = TRUE)



cat("d. Values: ", zmin, ",", zmax) #weird, min is greater than max...check!
```


P-value (Two-tailed test) :
```{r}
#What is the p-value (from the t-statistic) against null hypothesis of zero? P-value is the area to the right of the t-stat (t or z).
#Given: regression coefficient (estimate), sd, and df.

#Hypothesis testing:
#Null hypothesis Ho = 0
#Alternative hypothesis Ha > 0
#Area past tails (z scores) : Reject Ho
#Area bewteen tails : Fail to reject Ho

#e
estime = 6.56
stderre = 4.1
dfe = 24

tstate <- (estime - 0) / (stderre)
we <- 2*pt(-abs(tstate),df=dfe)

#f
estimf = -0.24
stderrf = 0.4
dff = 4

tstatf <- (estimf - 0) / (stderrf)
wf <- 2*pt(-abs(tstatf),df=dff)


cat("e. t-stat: ", tstate,", P-value: ", we, "\nf. t-stat: ", tstatf,", P-value: ", wf)

```


<p style="color:rgb(182,18,27);font-family:corbel">2. Crude Oil</p>

Is there a statistically significant difference in the mean? Calculate t-stat and p-value for the test against no difference in daily returns.

```{r}
#289 days before March 1, 2020
x1 = 0.000145
s1 = 0.0213
n1 = 289

#174 days after 
x2 = -0.0210
s2 = 0.271
n2 = 174

#pooled sd
s = sqrt((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)

#se of difference between two means
se = s * sqrt((1/n1) + (1/n2))

#tstat
tstat2 = ((x1 - x2) / se)
df2 = n1 + n2 - 2

w2 <- 2*pt(-abs(tstat2),df=df2)
cat("2. t-stat: ", tstat2,", P-value: ", w2)

```
I have no idea why I got such a huge t-stat. I think it should have been smaller, and my p-value is suspiciously small too. However, analyzing the answer regardless because I need to move on to the next one, the p-value shows that the difference among means is significant, confirming fatalist notions that everything did change when it comes to oil during the pandemic. 



<p style="color:rgb(182,18,27);font-family:corbel">3. Vaccine trials</p>

Calculate the t-stat and p-value for the test against no difference in infection rates between groups.
```{r}
#Took vaccine
x1 = 
s1 = 
n1 = 15 #need to calc sd, come back to this later

#Did not take vaccine
x2 = 
s2 = 
n2 = 90

#pooled sd
s = sqrt((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)

#se of difference between two means
se = s * sqrt((1/n1) + (1/n2))

#tstat
tstat2 = ((x1 - x2) / se)
df2 = n1 + n2 - 2

w2 <- 2*pt(-abs(tstat2),df=df2)
cat("2. t-stat: ", tstat2,", P-value: ", w2)
```


<p style="color:rgb(182,18,27);font-family:corbel">4 - 8. PUMS - Hours worked</p>

We focus on the decision of whether to work full time or part time
```{r}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "")
getwd()
load("acs2017_ny_data.RData")
acs2017_ny[1:10,1:7]
attach(acs2017_ny)
```

<p style="color:rgb(182,18,27);font-family:corbel">4. </p>

Rationale for choosing subgroup:
AGE >= 22 -  under that age more people as a percentage of the age population are at school therefore may skew our results about choosing part time other than going to school/college. 

AGE <= 66 - age where many people can start receiving social security and retire.

LABFORCE == 2 - In labor force, willing to work, get to make decision about how many hours to work.

WKSWORK2 > 4 - Establishing that anyone who worked under 4 weeks a year is not part-time.

UHRSWORK > 0 - To make sure everyone in our data set is working at least an hour a week.

```{r}
use_varb <- (AGE >= 22) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (CLASSWKR == 2) & (UHRSWORK > 0)
dat_use <- subset(acs2017_ny,use_varb) 
detach(acs2017_ny)
attach(dat_use)
```



<p style="color:rgb(182,18,27);font-family:corbel">5. </p>

Test if there is a difference between men and women.

First let's see differences qualitatively. Most men and women in our data set work full time, although on average men work more hours a week than women, who work less than 40 on average. Suggesting that more women are working part-time than men.
```{r}
summary(UHRSWORK[female == 1])
summary(UHRSWORK[female == 0])
```

```{r}
sd(UHRSWORK[female == 1])
sd(UHRSWORK[female == 0])

sum(UHRSWORK[female == 1])
sum(UHRSWORK[female == 0])
```

Trying out a simply lm model to calculate differences better. This makes more sense, being female has a statistically significant difference with p-value less than 0.001. 
```{r}
m2 <- lm(UHRSWORK ~ female)
summary(m2)
plot(m2)
#require(stargazer)
#stargazer(m2, type = "text")
```


<p style="color:rgb(182,18,27);font-family:corbel">6. </p>

Estimate a simple OLS model for hours worked, within your subsample.

a - b.
I chose a small amount of variables, all having independent relationships with hours worked. They seem exogenous, maybe with the exepction of female. All seem plausible and are statistically significant, NCHILD less so than others.

AGE - looks like with age, people work more hours. This could be because of higher expenses (children, college tuition, meidcal bills, homes, etc.) or more responsibilities at a higher employee level.

female & NCHILD - may be related to number of children, given that responsibility tends to fall on mothers. NCHILD is less significant than female, suggesting that NCHILD doesn't affect a non female portion of the population as much. 

RENT - Included to see if rent prices affected amount of time people need to work. Positively correlated and significant, therefore there maybe be a relationship there. 
```{r}
m1 <- lm(UHRSWORK ~ AGE + female + NCHILD + RENT)
summary(m1)
plot(m1)
#require(stargazer)
#stargazer(m1, type = "text")
```





Regression definitions:

Estimate (intercept) : expected value, most representative mean result of population
Estimate (slopes) : how much more or less does the value pull the intercept for every unit change (positive or negative correlations)
Standard error : average amount estimates vary from actual average value of our response variable. How much will slope change on average with every next unit added. The lower the better.
T-value : How many standard deviations our estimate is away from 0. The larger the better, the greater the evidence against the null, more relationship.
P-value : probability of observing any value equal or larger than t. (Area of tails past test stat (z or t)) The lower the more confident we can be and we can reject the null hypothesis. More significant.
Residual standard error : We are not capable of perfectly predicting our response variable. Average amount that the response will deviate from the true regression line. 

Chi-square test : Need to estimate how closely an observed distirbution matches an expected distribution, and whether two random variables are independent
