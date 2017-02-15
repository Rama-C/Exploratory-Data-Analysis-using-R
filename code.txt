Red Wine Quality by Rama
========================================================
Which chemical properties influence the quality of red wines?

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages that you end up using
# in your analysis in this code chunk.

# Notice that the parameter "echo" was set to FALSE for this code chunk.
# This prevents the code from displaying in the knitted HTML output.
# You should set echo=FALSE for all code chunks in your file.

#install.packages('memisc')
#install.packages('ggplot2')
#install.packages('gridExtra')
#install.packages('GGally')
#install.packages("RColorBrewer")
#install.packages("psych")
suppressMessages(library("psych"))
suppressMessages(library(RColorBrewer))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(GGally))
suppressMessages(library(memisc))
suppressMessages(library(plyr))
```


```{r echo=FALSE, message=FALSE, warning=FALSE, Load_the_Data}
# Loading the Data

red_wine <-read.csv('wineQualityReds.csv', header = T)

str(red_wine)

red_wine$quality_cat <-cut(red_wine$quality,breaks=c(0,4,7,10),labels=c('Bad','Average','good'))

red_wine$wine_quality <-factor(red_wine$quality)

```

The original dataset has 1599 instances with 13 variables. A variable(quality_cat) is added to the dataset to categorize the quality of the wine into Bad,Average and Good corresponding to the values - upto 4, 5 to 7 and 8 and above respectively.Another variable(wine_quality) is added to store the factor of quality variable.

# Univariate Plots Section
```{r echo=FALSE, message=FALSE, warning=FALSE,  Structure}

# structure of the dataset

dim(red_wine)
str(red_wine)

summary(red_wine)
```



```{r echo=FALSE, message=FALSE, warning=FALSE,Univariate_Plots}

# Distribution of fixed.acidity variable.

summary(red_wine$fixed.acidity)

p1 <-ggplot(aes(x=fixed.acidity), data = red_wine ) +
     geom_bar()

p2 <-ggplot(aes(x=fixed.acidity,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
     geom_histogram()

grid.arrange(p1,p2,ncol=1)
```

In the  plot of fixed.acidity ,there is a peak around 7. when we change the binwidth in histogram, the shape of the distribution changes.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of citric.acid variable.

summary(red_wine$citric.acid)

p1 <-ggplot(aes(x=(citric.acid)), data = red_wine ) +
     geom_bar()

p2 <-ggplot(aes(x=log(citric.acid)), data = red_wine ) +
     geom_bar()

p3 <-ggplot(aes(x=citric.acid,color = I('black'),fill=I('orange')), data = red_wine ) +
     geom_histogram()

grid.arrange(p1,p2,p3,ncol=2)
```

The value of citric acid ranges from 0 to 1. Most of the wines have zero citric acid .The second peak is at 0.48/.49 .  
The median is at 0.260 ,the 3rd quartile is at 0.420 and the max is at 1.000  which indicates the presence of the outliers. Did you notice the plot at 1 ?. yes, it's an outlier.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of volatile.acidity

p1 <-ggplot(aes(x=log(volatile.acidity)), data = red_wine ) +
     geom_bar()

p2 <-ggplot(aes(x=volatile.acidity,color = I('black'),fill=I('red')), data = red_wine ) +
     geom_histogram()

grid.arrange(p1,p2,ncol=1)

summary(red_wine$volatile.acidity)

```
The distribution of the volatile.acidity shows there are two peaks at 0.6 and 0.4 in the histogram plot. summary results shows the presence of outliers.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of residual.sugar

p1 <-ggplot(aes(x=residual.sugar), data = red_wine ) +
     geom_bar()

p2 <-ggplot(aes(x=residual.sugar,color = I('black'),fill=I('pink')), data = red_wine ) +
     geom_histogram()

grid.arrange(p1,p2,ncol=1)

summary(red_wine$residual.sugar)

```

The distribution of the residual.sugar has a highest peak at 2. The max is at 15.50 , Median is at 2.200 and 3rd quartile is at 2.600 , these values indicates the presence of outliers.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of chlorides

p1 <-ggplot(aes(x=chlorides), data = red_wine ) +
     geom_bar()

p2 <-ggplot(aes(x=chlorides,color = I('black'),fill=I('blue')), data = red_wine ) +
     geom_histogram()

grid.arrange(p1,p2,ncol=1)

summary(red_wine$chlorides)
```

The distribution of chlorides has a peak at 0.08/0.09 . The value of chlorides ranges from 0.01 to 0.6. The result summaries shows the presence of outliers.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of free.sulfur.dioxide

p1 <-ggplot(aes(x=free.sulfur.dioxide), data = red_wine ) +
     geom_bar()

p2 <-ggplot(aes(x=log(free.sulfur.dioxide)), data = red_wine ) +
     geom_bar()

p3 <-ggplot(aes(x=free.sulfur.dioxide,color = I('black'),fill=I('green')), data = red_wine ) +
     geom_histogram()

grid.arrange(p1,p2,p3,ncol=1)

summary(red_wine$free.sulfur.dioxide)
```

The distribution of free.sulfur.dioxide has a peak at 5 in the histogram plot. There are values even beyond 60. The summary result  indicates the presence of outliers.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of density

p1 <-ggplot(aes(x=density,color = I('black'),fill=I('red')), data = red_wine ) +
     geom_histogram()

p2 <-ggplot(aes(x=density), data = red_wine ) +
     geom_bar()

p3 <-ggplot(aes(x=log(density)), data = red_wine ) +
     geom_bar()

grid.arrange(p1,p2,p3,ncol=1)

summary(red_wine$density)

```

The distribution of density in the bar plot has different shades of lines. When there are more datapoints at particular x value ,the line has a darker shade. When there are few datapoints at a particular x value, the line has a lighter shade. Also, each scale in x-axis is very small.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of total.sulfur.dioxide

p1<-ggplot(aes(x=total.sulfur.dioxide,color = I('black'),fill=I('orange')), data = red_wine ) +
    geom_histogram()

p2<-ggplot(aes(x=total.sulfur.dioxide), data = red_wine ) +
    geom_bar()

p3<-ggplot(aes(x=log(total.sulfur.dioxide)), data = red_wine ) +
    geom_bar()

grid.arrange(p1,p2,p3,ncol=1)

summary(red_wine$total.sulfur.dioxide)
```
The distribution of total.sulfur.dioxide in the histogram plot has a peak value around 20.  The median,mean ,3rd quartile and the max in summary result shows the presence of outliers.

I got curious to know the difference between the total.sulfur.dioxide and free.sulfur.dioxide . free.sulfur.dioxide is the amount of free form of so2 ,whereas total.sulfur.dioxide is the amount of free and bound forms of so2.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of pH

p1 <-ggplot(aes(x=pH,color = I('black'),fill=I('black')), data = red_wine ) +
     geom_bar()

p2 <-ggplot(aes(x=pH,color = I('black'),fill=I('blue')), data = red_wine ) +
     geom_histogram()

grid.arrange(p1,p2,ncol=1)

summary(red_wine$pH)

```
Most of the pH values falls between 3.0 and 3.5. The pH value ranges from 2.74 to 4.01 . The pH describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic). 

The summary results shows that the mean and median values are pretty close which means we have a normal distribution .

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of sulphates

p1 <-ggplot(aes(x=sulphates), data = red_wine ) +
     geom_bar()

p2<-ggplot(aes(x=sulphates,color = I('black'),fill=I('red')), data = red_wine ) +
    geom_histogram()

grid.arrange(p1,p2,ncol=1)

summary(red_wine$sulphates)
```
The distribution of sulphates has a peak at 0.6 . There are visible plots even beyond 2.0 . The summary results indicates the presence of outliers.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of alcohol

p1<-ggplot(aes(x=alcohol,color = I('black'),fill=I('orange')), data = red_wine ) +
    geom_histogram()

p2 <-ggplot(aes(x=alcohol), data = red_wine ) +
     geom_bar()

grid.arrange(p1,p2,ncol=1)

summary(red_wine$alcohol)
```
The distribution of alcohol has a peak at 9.4 . The value of alcohol ranges from 8.40 to 14.90 . The summary results indicates the presence of outliers.



```{r echo=FALSE, message=FALSE, warning=FALSE}
# Distribution of quality

ggplot(aes(x=wine_quality,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram(stat="count")

summary(red_wine$wine_quality)
```


The distribution of quality has the highest peak at 5 and the second highest at 6. The quality ranges from 3( bad) to 8 (very good) . quality is the output variable.



```{r echo=FALSE, message=FALSE, warning=FALSE}
# Distribution of quality_cat

ggplot(aes(x=quality_cat,color = I('black'),fill=I('#066DD7')), data = red_wine ) +
  geom_histogram( stat = "count")

summary(red_wine$quality_cat)
```
A variable(quality_cat) is added to categorize the quality of the wine into Bad,Average and Good.
A wine is categorized into Bad,when its value is 4 or less. 
A wine is categorized into Average,when its value falls in the range of 5 to 7.
A wine is categorized into good,when its value is  8 and above. 

The distribution of quality_cat shows that most of the wines falls under the average wine quality.
The summary clearly shows the observations in each category. Above 90% of the wines falls in average category of wine.


# Univariate Analysis

### What is the structure of your dataset?
The red wine dataset consists of 1599 observations with 13 variables. The variables are id, fixed.acidity, volatile.acidity , citric.acid ,residual.sugar ,chlorides ,free.sulfur.dioxide , total.sulfur.dioxide,density, pH,sulphates, alcohol and quality  .        



### What is/are the main feature(s) of interest in your dataset?
I would like to determine "Which chemical properties influence the quality of red wines?".The main features in the data are Alcohol and pH. 
I think,
- alcohol decides the quality and taste of the wine 
- pH describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic). so a right pH will influence the quality.


### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?

The other features like citric acid ,density ,Fixed .acidity, volatile.acidity ,sulphates ,free.sufur.dioxide, total .sufur.dioxide,chlorides and sulphates might support the investigation. These variables can be used to build a predictive model to determine the influence on the quality of the red wines.

### Did you create any new variables from existing variables in the dataset?

 A variable(quality_cat) is added to categorize the quality of the wine into Bad,Average and Good corresponding to the values - upto 4, 5 to 7 and 8 to 10 respectively. Another variable (wine_quality) is added to store the factor of quality variable.

### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?
The chosen dataset is in tidy format.so there wasn't any need to change the form  of the data. pH had a normal distribution which is different from other variables distribution.
The log transformations is calculated on some variables.

# Bivariate Plots Section
```{r echo=FALSE, message=FALSE, warning=FALSE, correlation}

# pairwise correlation and ggpairs 
Exclude <-c("X", "quality_cat", "wine_quality")
Exploring_var<- red_wine[,-which(names(red_wine) %in% Exclude)]
names(Exploring_var)
names(Exploring_var)[names(Exploring_var)=="total.sulfur.dioxide"] <- "total.so2"
names(Exploring_var)[names(Exploring_var)=="free.sulfur.dioxide"] <- "free.so2"


cor(Exploring_var, use="pairwise", method="pearson")

```



```{r echo=FALSE, message=FALSE, warning=FALSE , fig.width=10,fig.height=10}
# correlation matrix or plot

pairs.panels(Exploring_var,bg=c("yellow","green"),pch=21)

summary(Exploring_var$density)
```

I used pairs.panels from psych package to create a correlation plot.The markings on the top or bottom of each column are the value ranges of the variable described in that particular  column. for example, density has a minimum value of 0.9901 and maximum value of 1.0040 in the summary  and you can notice , density has a marking of 0.9900 to 1.00 in the plot. setting fig.width and fig.height in the r-chunks greatly helped in the visualization of the plot.

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Distribution of quality vs Alcohol

ggplot(aes(x=alcohol),data=red_wine)+
     geom_bar(aes(fill=wine_quality))
  

by(red_wine$alcohol,red_wine$quality,summary)
```

In the bar plot,most of the wines falls into the average quality . It clearly demonstrates the predominant distribution of wine_quality values  of 5,6 and 7. The Alcohol vs wine quality have a positive correlation than quality - any other variable combination.

The summary results shows that the wine quality 8 have  highestmedian value of 12.15.

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots}

# Distribution of quality_cat vs fixed.acidity,citric.acid,sulphates,alcohol

u1 <-ggplot(aes(y=fixed.acidity,x=quality_cat ),data=red_wine)+
     geom_boxplot() +ylim(7,14)

u2 <-ggplot(aes(y=citric.acid,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('orange'))

u3 <-ggplot(aes(y=sulphates,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('blue')) + ylim(0,1)

u4 <-ggplot(aes(y=alcohol,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('red')) +ylim(9,14)

grid.arrange(u1,u4,u2,u3,ncol=2)
```

The variables like citric.acid,fixed.acidity,alcohol and sulphates show positive correlation with the quality_cat. As the value of these variables increases the quality_cat also increases.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of quality_cat vs pH,chlorides,volatile.acidity

ggplot(aes(y=volatile.acidity,x=quality ),data=red_wine)+
  geom_jitter(color =I('blue'), alpha = 1/4)

d1 <-ggplot(aes(y=volatile.acidity,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('red'))

d2 <-ggplot(aes(y=chlorides,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('orange'))+ylim(0,0.15)

d3 <-ggplot(aes(x=quality_cat ,y=pH ),data=red_wine)+
     geom_boxplot(color =I('black'))

grid.arrange(d1,d2,d3,ncol=2)

by(red_wine$chlorides,red_wine$quality_cat,summary)
```

The quality of the wine decreases when the variables like pH,volatile.acidity and chlorides increases.In the chlorides-quality_cat boxplots, the median of Bad and Average categories are pretty close. The summary of chlorides with quality_cat shows their median values.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of quality_cat vs density,residual.sugar,total.sulfur.dioxide,free.sulfur.dioxide



m1 <-ggplot(aes(y=density,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('orange'))

m2 <-ggplot(aes(y=residual.sugar,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('blue')) +ylim(1,5)

m3 <-ggplot(aes(y=free.sulfur.dioxide,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('black')) +ylim(0,40)

m4 <-ggplot(aes(y=total.sulfur.dioxide,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('red')) +ylim(0,150)

grid.arrange(m1,m2,m3,m4,ncol=2)

by(red_wine$density,red_wine$quality_cat,summary)
```

There is a mixed trend of quality_cat with the variables like density,residual.sugar, free.sulfur.dioxide and total.sulfur.oxide. The median values improves from bad to average and then drops from average to good.



```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of pH with other variables.

p1<-ggplot(aes(y=citric.acid,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('blue'))

p2<-ggplot(aes(y=alcohol,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('red'))

p3<-ggplot(aes(y=fixed.acidity,x=pH ),data=red_wine)+
   geom_jitter(alpha = 1/10,color =I('black'))

p4<-ggplot(aes(y=residual.sugar,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('orange'))

p5<-ggplot(aes(y=chlorides,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('blue'))

p6<-ggplot(aes(y=sulphates,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('red'))

grid.arrange(p1,p2,p4,p3,p5,p6,ncol=2)

p7<-ggplot(aes(y=total.sulfur.dioxide,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('blue'))

p8<-ggplot(aes(y=free.sulfur.dioxide,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('red'))

p9<-ggplot(aes(y=volatile.acidity,x=pH ),data=red_wine)+
    geom_jitter(alpha = 1/10,color =I('black'))

p10<-ggplot(aes(y=density,x=pH ),data=red_wine)+
     geom_jitter(alpha = 1/10,color =I('orange'))


grid.arrange(p7,p8,p9,p10,ncol=2)

```

The scatterplots of citric.acid vs pH ,fixed.acidity vs pH and density vs pH  shows a linear association.  However, there is still quite a bit of scatter around the pattern. 
A positive correlation value indicates a positive linear association and negative value indicates a negative linear association 
Consequently, a correlation values of -0.54, 0.66, 0.67 and 0.68 are reasonable to strong. It is common for a correlation to decrease as sample size increases.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of fixed.acidity vs citric.acid, fixed.acidity vs pH and fixed.acidity vs density

Include <-c("fixed.acidity","citric.acid"  , "density","pH")

red_wine_cor<- red_wine[,(names(red_wine) %in% Include)]

cor(red_wine_cor, use="pairwise", method="pearson")

f1<-ggplot(aes(y=citric.acid,x=fixed.acidity ),data=red_wine)+
    geom_point(color =I('red'))+
    geom_smooth(method='lm' )

f2<-ggplot(aes(x=fixed.acidity,y=pH ),data=red_wine)+
    geom_point(color =I('green'))+
    geom_smooth(method='lm' )

f3<-ggplot(aes(y=density,x=fixed.acidity ),data=red_wine)+
    geom_point(color =I('orange'))+
    geom_smooth(method='lm' )

grid.arrange(f1,f2,f3,ncol=2)

```

fixed.acidity has a linear association with density,citric.acid and pH .The scatterplots of these variables are shown above.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of residual.sugar vs citric.acid 

rs1 <-ggplot(aes(y=residual.sugar,x=citric.acid ),data=red_wine)+
      geom_point(color='blue')

rs2 <-ggplot(aes(y=residual.sugar,x=citric.acid ),data=red_wine)+
      geom_jitter(alpha=1/5,color =I('blue')) +
      xlim(0,quantile(red_wine$citric.acid,0.99))

grid.arrange(rs1,rs2,ncol=1)

```

The scatterplot of residual.sugar vs citric.acid is overplotting. when geom_jitter is used,it reduces the overplotting .

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of free.sulfur.dioxide vs quality_cat

s1 <-ggplot(aes(y=free.sulfur.dioxide,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('blue'))

s2 <-ggplot(aes(y=total.sulfur.dioxide,x=quality_cat ),data=red_wine)+
     geom_boxplot(color =I('red'))+
     coord_cartesian(ylim=c(0,175))

grid.arrange(s1,s2,ncol=1)
```

The boxplots of free.sulfur.dioxide vs quality_cat and total.sulfur.dioxide vs quality_cat  demonstrates same type of distribution but their values varies.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of free.sulfur.dioxide vs total.sulfur.dioxide

t1 <-ggplot(aes(y=free.sulfur.dioxide,x=total.sulfur.dioxide ),data=red_wine)+
     geom_point(color =I('blue'))+geom_smooth(method='lm',color='red' )

t2 <-ggplot(aes(y=free.sulfur.dioxide,x=total.sulfur.dioxide ),data=red_wine)+
     geom_jitter(alpha = 1/10,color =I('blue')) +xlim(0,200)

grid.arrange(t1,t2,ncol=1)
```

I was curious to see the distribution of Free.sulfur.dioxide vs total.sulfur.dioxide.
Free.sulfur.dioxide and total.sulfur.dioxide have a  strong relationship with a correlation value of 0.667



# Bivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?

The correlation tables demostrates the relationships among various variables.


The variable,quality has a positive relationship with fixed.acidity,citric.acid,alcohol and residual.sugar.
The quality has a negative relationship  with volatile.acidity,pH and chlorides. 

A relationship between two variables are said to be

- strong when the correlation values are between ( 0.5 and 1) or ( -1 and -0.5)

- moderate when the correlation values are between ( 0.3 and 0.5) or ( -0.3 and -0.5)

The correlation value of alcohol - quality has a moderate relationship with the value of 0.476 and the volatile.acidity - quality pair has a moderate relationship with the value of -0.3905 .
I was disappointed to know that the quality has 0.476 as its highest correlation value and the variable that is paired with is Alcohol.


A strong relationship is seen in pH- fixed.acidity (-0.6829782) and pH- citric.acid(-0.5419041) plots.



### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?

A strong relationship is seen in pH- fixed.acidity(-0.6829782) and pH- citric.acid(-0.5419041) plots.

fixed.acidity has a strong relationship with density,citric.acid and pH.

The values are  as follows 
              
citric.acid  -fixed.acidity      0.6717034   
density     - fixed.acidity      0.6680473  
pH          - fixed.acidity     -0.6829782  

### What was the strongest relationship you found?

The strongest relationship is pH-Fixed.acidity with a correlation value of -0.683  .

# Multivariate Plots Section

```{r echo=FALSE, message=FALSE, warning=FALSE, Multivariate_Plots}

# Distribution of pH - fixed.acidity based on Quality_cat
  
ggplot(aes(y=fixed.acidity,x=pH ),data=red_wine)+
  geom_jitter(color='blue',alpha=1/5) + 
  facet_wrap(~quality_cat)

ggplot(aes(y=fixed.acidity,x=pH ),data=red_wine)+
  geom_point(aes(color=quality_cat)) + 
  scale_color_brewer(palette="RdYlBu","Quality\nCategory") + 
  xlim(2.9,quantile(red_wine$pH,0.99))+
  ylim(4.5,quantile(red_wine$fixed.acidity ,0.99))+
  theme_dark()
```

Multivariate plotting starts with the strongest relationship pair pH-fixed.acidity based on the quality_cat. The scatterplot shows that the  maximum datapoints are from the average quality of wine.


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of citric.acid,pH ,quality

ggplot(aes(y=citric.acid,x=pH ),data=red_wine)+
  geom_point(aes(color =wine_quality),stat='identity')+
  scale_color_brewer(palette="RdYlBu","Wine Quality")+
  xlim(2.9,quantile(red_wine$pH,0.99))+
  ylim(0,quantile(red_wine$citric.acid ,0.99))+
  theme_dark()

ggplot(aes(y=citric.acid,x=pH ),data=red_wine)+
  geom_bar(aes(fill=quality_cat),stat='identity')+
  scale_fill_brewer(palette='RdYlGn',"Quality\nCategory") 
  

```  

Distribution of Citric acid - pH  based on the quality_cat , have most of the values in average quality. citric acid-pH  have a correlation value of -0.542

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of citric.acid vs fixed.acidity

ggplot(aes(y=citric.acid,x=fixed.acidity ),data=red_wine)+
  geom_bar(aes(fill=quality_cat),stat='identity')+
  scale_fill_brewer(palette='RdYlGn',"Quality\nCategory") 
  

ggplot(aes(y=citric.acid,x=fixed.acidity ),data=red_wine)+
  geom_point(stat='identity',aes(color=wine_quality))+
  xlim(6,quantile(red_wine$fixed.acidity,0.99))+
  ylim(0,quantile(red_wine$citric.acid ,0.99))+
  scale_color_brewer(palette="RdYlGn","Wine Quality")+
  theme_dark()
 
  

```

Distribution of Citric.acid - fixed.acidity based on the quality show a strong relationship with a correlation value of 0.67.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of fixed.acidity - ph - citric.acid -quality_cat

ggplot(aes(y=pH,x=fixed.acidity ),data=red_wine)+
  geom_bar(stat='identity',aes(color=citric.acid)) + facet_wrap(~quality_cat)
```

Distribution of pH- Fixed.acidity with citric.acid as color and face_wrap with quality_cat has a four variable distribution.when I used factor of pH values, I had a huge list of values in different colours(created different levels). so, I used pH values without the factor. 
I was so curious to explore this combo because of the strong relationship between the variables  pH, Fixed.acidity and  citric.acid .


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of density - ph - wine_quality

ggplot(aes(y=density,x=pH ),data=red_wine)+
  geom_bar(stat='identity',aes(fill =wine_quality))+
  scale_fill_brewer("Wine Quality",palette="YlGnBu")+
  theme_dark()

# Distribution of density - citric.acid -quality


ggplot(aes(y=density,x=citric.acid ),data=red_wine)+
  geom_bar(stat='identity',aes(fill =wine_quality))+
  scale_fill_brewer("Wine Quality",palette="YlGnBu")+
  theme_dark()

```

Density-pH and Density - citric.acid based on the wine quality have a moderate relationship.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Distribution of alcohol  - sulphates/volatile.acidity/density based on the quality

  
a1 <-ggplot(aes(y=alcohol,x=sulphates),data=red_wine)+
     geom_bar(stat='identity',aes(fill = wine_quality))+
     scale_fill_brewer("Wine Quality",palette="YlGnBu")+
     theme_dark()

a2 <-ggplot(aes(y=alcohol,x=volatile.acidity),data=red_wine)+
     geom_bar(stat='identity',aes(fill = wine_quality))+
     scale_fill_brewer("Wine Quality",palette="RdYlGn")+
     theme_dark()

grid.arrange(a1,a2,ncol=1)

ggplot(aes(y=alcohol,x=density),data=red_wine)+
     geom_point(stat='identity',aes(color = wine_quality))+
     scale_color_brewer("Wine Quality",palette="YlGnBu")+
     xlim(0.990,quantile(red_wine$density,0.99))+
     ylim(9,quantile(red_wine$alcohol ,0.99))+
     theme_dark()


```


Quality have a higher relationship with Alcohol than with other variables. so I decided to add one more variable like density,volatile.acidity ,sulphates and check for the interesting plots. most of the datapoints from average wine quality(of values 5,6,7) 



# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?

The distribution of pH - Fixed.acidity with citric.acid values as colour and face wrap with Quality_cat. I was so curious to explore this combo because of the strong relationship between the variables  pH, Fixed.acidity and  citric.acid .

Quality have a better linear association  with Alcohol than with any other variables. Even though it is better ,but it isn't strong.
Quality have  a second better relationship with volatile.acidity .

 
citric.acid- fixed.acidity  and  pH-fixed.acidity also have strong relationships.

### Were there any interesting or surprising interactions between features?

Alcohol- density-quality plot have a surprising relationship with a correlation value of -0.496 . Alcohol have a better relationship with density than with  any other variables.

### OPTIONAL: Did you create any models with your dataset? Discuss the strengths and limitations of your model.
```{r echo=FALSE, message=FALSE, warning=FALSE, Linear_model}

# linear model 

m1 <-lm(quality ~ alcohol, data = red_wine)

m2 <-update(m1 , ~ .+volatile.acidity+residual.sugar)

m3 <-update(m2 , ~ .+fixed.acidity+chlorides )

m4 <-update(m3 , ~ .+sulphates+pH )

m5 <-update(m4 , ~ .+citric.acid+density )

m6 <-update(m5 , ~ .+free.sulfur.dioxide + total.sulfur.dioxide)

mtable(m1,m2,m3,m4,m5,m6)

```
I built a linear model for quality  using all variables describing chemical properties of the wine. R -squared values are 0.4(max) and 0.2(min). R squared values helps in drawing conclusions about how changes in the predictor values are associated with changes in the response value.

This model has a limitation. The red wine dataset does not include the wine quality over 8 and winequality  under 3 . when the data with winequality under 3 and wine quality over 8 are added , this will improve the linear model's outcome.

------

# Final Plots and Summary

### Plot One
```{r echo=FALSE, message=FALSE, warning=FALSE, Plot_One}

# Distribution of wine quality

ggplot(aes(x=wine_quality,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram(stat="count")+
  ggtitle("Histogram for Quality")+
  labs(x= 'Wine Quality')

summary(red_wine$wine_quality)

```


### Description One

The distribution of quality has the highest peak at 5 and the second highest at 6. The quality ranges from 3( bad) to 8 (very good) . quality is the output variable.

The summary shows that the distribution of 1599 observations in the wine quality.  681 observations falls in the wine quality of 5 and 638 observations falls in the wine quality of 6.


### Plot Two
```{r echo=FALSE, message=FALSE, warning=FALSE, message=FALSE, warning=FALSE ,Plot_Two}


# Distribution of alcohol vs wine quality

ggplot(aes(x=alcohol),data=red_wine)+
     geom_bar(aes(fill=wine_quality))+
     scale_fill_brewer(palette="RdYlGn","Wine Quality")+
     theme_dark()+
     ggtitle("Distribution of Alcohol percentage based on the wine quality")+
     labs(x= 'Alcohol(% by volume)')


by(red_wine$alcohol,red_wine$wine_quality,summary)

```

### Description Two

In the boxplot for alcohol percentage based on the wine quality plot, the wine of good quality have a higher percentage of alcohol content(over 11.3). The medians of bad,average and good quality wines are gradually increasing. But , bad quality wines and average quality wines share almost same alcohol ranges. As you can see,the average quality wine has a lower first quartile value than the bad quality wine. This plot gives the impression that alcohol plays an important role in deciding the wine quality.

In the bar plot,most of the wines falls into the average quality . It clearly demonstrates the predominant distribution of wine_quality values  of 5,6 and 7. The Alcohol vs wine quality have a positive correlation than quality - any other variable combination.

The summary results of Alcohol vs quality has a highest median value of 12.15 for the wine quality value of 8. The wine quality of 5 has the  lowest median value of 9.7 .


### Plot Three
```{r echo=FALSE, message=FALSE, warning=FALSE, Plot_Three}
# Distribution of  fixed.acidity and pH based on the  quality category

ggplot(aes(y=citric.acid,x=fixed.acidity ),data=red_wine)+
  geom_point(stat='identity',aes(color=wine_quality))+
  xlim(6,quantile(red_wine$fixed.acidity,0.99))+
  ylim(0,quantile(red_wine$citric.acid ,0.99))+
  scale_color_brewer(palette="RdYlGn","Wine Quality")+
  theme_dark()+
  ggtitle("Distribution of fixed.acidity and Citric acid based on the Wine Quality")+
  labs(x= 'Fixed acidity (tartaric acid - g / dm^3)' , y= 'Citric.acid (g / dm^3)')+
  theme_dark()
  

keep <-c("fixed.acidity","quality" ,"citric.acid")

red_wine_MV<- red_wine[,(names(red_wine) %in% keep)]

cor(red_wine_MV, use="pairwise", method="pearson")
```

### Description Three

This scatterplot shows the distribution of fixed acidity and citric.acid based on the wine quality.citric.acid can add 'freshness' and flavor to wines.most acids involved with wine do not evaporate readily.
Chemically the acids influence titrable acidity which affects the taste of the wine. As you can see, there is a positive correlation between citric.acid and Fixed.acidity.when these two variables increases ,it affects the taste and quality of the wine.

The correlation value of these variables shows that 
- fixed.acidity- citric.acid have a correlation value of 0.672
- quality has a better correlation value with citric.acid than the fixed.acidity as it can add freshness and flavor to wines.Thus improving the quality.
- when  fixed.acidity and citric.acid increases ,it increases the titrable acidity thus affecting the taste and quality of the wine.



-----

# Reflection

I had trouble in the correlation matrix or plot section. I used ggpairs to create the correlation plot. The plot was fine but the correlation values aren't clearly /fully visible in some cells. Also the variables like free.sulfur.dioxide and total.sulfur.dioxide didn't fit within the column cell.I tried different ways to solve this issue. But had no success.I submitted the project without solving that issue.

In the review, I was suggested to use fig.width and fig.height for this issue.I applied this, in the correlation plot.It did help .The correlation values are readable  and of same size now  but I had problem with plot. Nearly 30 % of the matrix was not visible .so I decided to search for other options.

I found a chart.Correlation from the package("PerformanceAnalytics"), I got a pretty matrix but had a problem with correlation value text in different sizes. The strong correlation values are in appropriate font size whereas weak correlation values were in  small font size .

Then I noticed the pretty correlation plot (using psych package) given in the example project of the project description section. I renamed the variables like free.sulfur.dioxide and total.sulfur.dioxide as free.so2 and total.so2 respectively. fig.width and fig.height greatly helped in restoring its size and the plot looks pretty now. 

The dataset lacks the wine quality values under 3 and over 8. These values would have impacted the outcome of the output variable. when a dataset includes the wine quality values ranging from 0 to 10, the outcome would be more precise than the current one. 

There is no data about grape types, wine brand, wine selling price, etc. 
Each grape type vary with the sweetness,chemical properties,etc from other grape types.This would have helped in understanding the chemical properties better and hence the influence on the wine quality.

Each wine brand has a unique way of making wines or following the standard way of making wine with interesting twist on some/all processes. So ,any chemical property change because of the wine making process might have influenced the wine quality.

weather also plays an important reason for the end result of wine quality. A good weathered year produces better tasting,healthy  grapes,thus influencing on the wine quality.

Moreover, more sophisticated prediction models should be able to provide more accurate predictions for the quality of wine based on its chemical characteristics.
