# Easy Cost
Clement Ponsonnet  

A common approach to determine the cost of products is the should cost method. It consists in estimating what a product should cost based on materials, labor, overhead, and proﬁt margin. Although this strategy is very accurate, it has the drawback of being tedious and it requires expert knowledge of industrial technologies and processes. To get a quick estimation, it is possible to build a statistical model to predict the price of products given their characteristics. With such a model, it would no longer be necessary to be an expert or to wait several days to assess the impact of a design modiﬁcation, a change in supplier or a change in production site.  
Before builing a model, it is important to explore the data which is the aim of this case study. 

This study was commissioned by a cosmetics company that wants to estimate the price of Screw Caps (bouchon Ã vis) of shampoo bottles:


##Preparation

Load libraries


```r
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(scatterplot3d)
```


Load the data, and have a look at it.


```r
screw <- read.csv("ScrewCaps.csv")
#Take out the first column (it is just indices)
screw <- screw[-1]
head(screw)
```

```
##     Supplier Diameter weight nb.of.pieces   Shape Impermeability
## 1 Supplier A    3.780  3.780            2 Shape 1         Type 1
## 2 Supplier A    3.994  4.768            3 Shape 2         Type 2
## 3 Supplier A    3.994  4.768            3 Shape 2         Type 2
## 4 Supplier A    1.066  1.917            4 Shape 2         Type 2
## 5 Supplier A    1.179  1.261            5 Shape 3         Type 1
## 6 Supplier A    1.079  1.250            5 Shape 2         Type 1
##      Finishing Mature.Volume Raw.Material    Price    Length
## 1   Lacquering         60000           PS 28.19338 30.118312
## 2   Lacquering         49000           PS 35.34190 31.693726
## 3   Lacquering         55000           PS 41.94724 31.738166
## 4   Lacquering          1500           PS 26.76473  8.472485
## 5 Hot Printing        215000           PP 13.84152  9.222997
## 6 Hot Printing         75000           PP 16.02589  8.764609
```

```r
summary(screw)
```

```
##        Supplier      Diameter          weight       nb.of.pieces   
##  Supplier A: 31   Min.   :0.4458   Min.   :0.610   Min.   : 2.000  
##  Supplier B:150   1st Qu.:0.7785   1st Qu.:1.083   1st Qu.: 3.000  
##  Supplier C: 14   Median :1.0120   Median :1.400   Median : 4.000  
##                   Mean   :1.2843   Mean   :1.701   Mean   : 4.113  
##                   3rd Qu.:1.2886   3rd Qu.:1.704   3rd Qu.: 5.000  
##                   Max.   :5.3950   Max.   :7.112   Max.   :10.000  
##      Shape     Impermeability        Finishing   Mature.Volume   
##  Shape 1:134   Type 1:172     Hot Printing: 62   Min.   :  1000  
##  Shape 2: 45   Type 2: 23     Lacquering  :133   1st Qu.: 15000  
##  Shape 3:  8                                     Median : 45000  
##  Shape 4:  8                                     Mean   : 96930  
##                                                  3rd Qu.:115000  
##                                                  Max.   :800000  
##  Raw.Material     Price            Length      
##  ABS: 21      Min.   : 6.477   Min.   : 3.369  
##  PP :148      1st Qu.:11.807   1st Qu.: 6.161  
##  PS : 26      Median :14.384   Median : 8.086  
##               Mean   :16.444   Mean   :10.247  
##               3rd Qu.:18.902   3rd Qu.:10.340  
##               Max.   :46.610   Max.   :43.359
```

##Questions

**2) We start with univariate and bivariate descriptive statistics. Using appropriate plot(s) or summaries** 
**answer the following questions**

* *How is the distribution of the Price? Comment your plot with respect to the quartiles of the Price*

```r
plot(density(screw$Price), xlab = "Price", main="Price distribution")
```

![](full_analysis_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
boxplot(screw$Price, ylab = "Price", main = "Boxplot of Price")
```

![](full_analysis_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

The distribution is not symmetric: its right tail is longer than the left tail. We see this in the quartiles as well: the distance between the distance between the quartiles increases. The distribution has a peak around a price of 15, and another much smaller peak around 30. 
This looks like it could be a mixture of two gaussian randon variables

* *Does the Price depend on the Length? weight?*

```r
cor(screw$Price, screw$Length)
```

```
## [1] 0.7997892
```

```r
cor(screw$Price, screw$weight)
```

```
## [1] 0.787921
```

```r
cor(screw$Length,screw$weight)
```

```
## [1] 0.9629389
```
There appears to be a positive relationship between Price and both Length and weight. We should note that Length and weight are very highly correlated.

* *Does the Price depend on the Impermeability? Shape?*

```r
summary(lm(screw$Price ~ screw$Impermeability))
```

```
## 
## Call:
## lm(formula = screw$Price ~ screw$Impermeability)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.4106  -3.0187  -0.6286   2.4897  25.0638 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                 14.7236     0.4117   35.77   <2e-16 ***
## screw$ImpermeabilityType 2  14.5846     1.1986   12.17   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.399 on 193 degrees of freedom
## Multiple R-squared:  0.4341,	Adjusted R-squared:  0.4312 
## F-statistic:   148 on 1 and 193 DF,  p-value: < 2.2e-16
```

```r
summary(lm(screw$Price ~ screw$Shape))
```

```
## 
## Call:
## lm(formula = screw$Price ~ screw$Shape)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.098  -3.850  -1.025   3.055  25.587 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         14.2006     0.5406  26.267  < 2e-16 ***
## screw$ShapeShape 2   8.1403     1.0782   7.550 1.75e-12 ***
## screw$ShapeShape 3   1.4510     2.2777   0.637  0.52485    
## screw$ShapeShape 4   7.4393     2.2777   3.266  0.00129 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.258 on 191 degrees of freedom
## Multiple R-squared:  0.2475,	Adjusted R-squared:  0.2357 
## F-statistic: 20.94 on 3 and 191 DF,  p-value: 9.008e-12
```
The F-statistic is a measure of the difference between our linear model and an intercept only model. Because it is in both cases very significant, we can say that Price depends on both Impermeability and Shape

* *Which is the less expensive Supplier?*


```r
screw %>%
  group_by(Supplier)%>%
    summarise(mean = mean(Price))
```

```
## # A tibble: 3 x 2
##     Supplier     mean
##       <fctr>    <dbl>
## 1 Supplier A 18.02897
## 2 Supplier B 16.26141
## 3 Supplier C 14.88869
```
And so the least expensive supplier on average is supplier C. This does not tell us if the difference is statistically significant.

**3) One important point in exploratory data analysis consists in identifying potential outliers. Could you give points which are suspect regarding the Mature.Volume variable? Give the characteristics (other features) of the observations that seem suspect.**


```r
boxplot(screw$Mature.Volume, ylab = "Mature Volume", main = "Boxplot of Mature Volume (with outliers")
```

![](full_analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
outlier_values <- boxplot.stats(screw$Mature.Volume)$out
paste("There are", length(outlier_values), "outliers")
```

```
## [1] "There are 16 outliers"
```

```r
print("Here is the list of outliers")
```

```
## [1] "Here is the list of outliers"
```

```r
screw %>%
  filter(Mature.Volume %in% outlier_values)
```

```
##      Supplier Diameter weight nb.of.pieces   Shape Impermeability
## 1  Supplier A   0.7900 1.0600            4 Shape 1         Type 1
## 2  Supplier A   0.7900 1.0600            4 Shape 1         Type 1
## 3  Supplier A   0.5220 0.7670            3 Shape 1         Type 1
## 4  Supplier B   0.7900 1.0600            4 Shape 1         Type 1
## 5  Supplier B   0.7900 1.0600            4 Shape 1         Type 1
## 6  Supplier B   1.2200 1.4690            5 Shape 4         Type 1
## 7  Supplier B   1.2200 1.4690            5 Shape 4         Type 1
## 8  Supplier B   0.8570 1.0750            4 Shape 1         Type 1
## 9  Supplier B   0.7670 1.0250            2 Shape 1         Type 1
## 10 Supplier B   0.4458 0.6252            4 Shape 1         Type 1
## 11 Supplier B   0.6440 0.8240            3 Shape 1         Type 1
## 12 Supplier B   0.7420 0.9910            2 Shape 1         Type 1
## 13 Supplier B   0.7520 1.0570            4 Shape 2         Type 1
## 14 Supplier B   1.2200 1.4690            6 Shape 4         Type 1
## 15 Supplier B   1.2200 1.4690            5 Shape 4         Type 1
## 16 Supplier B   0.5270 0.6960            3 Shape 1         Type 1
##       Finishing Mature.Volume Raw.Material     Price   Length
## 1    Lacquering        800000           PP 12.256185 6.209053
## 2    Lacquering        800000           PP  9.416743 6.412783
## 3    Lacquering        270000           PP  8.513328 4.050041
## 4    Lacquering        800000           PP 11.113649 6.421937
## 5    Lacquering        800000           PP 12.269223 6.212285
## 6  Hot Printing        270000           PP 17.923219 9.660626
## 7  Hot Printing        270000           PP 18.539915 9.936761
## 8    Lacquering        300000           PP  9.563779 6.753866
## 9  Hot Printing        415000           PP  7.856303 6.369959
## 10   Lacquering        266784           PP  7.966986 3.369243
## 11   Lacquering        330000           PP  8.994384 5.120182
## 12 Hot Printing        413000           PP 11.164396 5.764941
## 13 Hot Printing        374000           PP 11.243123 5.961133
## 14 Hot Printing        300000           PP 19.990634 9.521870
## 15 Hot Printing        270000           PP 29.160137 9.682274
## 16   Lacquering        287500           PP 11.540327 4.238028
```
So we have 16 outliers, which all have extremely high volumes. They are all of type 1 and mostly shape 1. We would like to verify that it is not simply the whole group of type 1 and shape 1 that exhibits a high volume. If that were the case, we should not treat this data as outliers.


```r
sum((boxplot.stats((screw %>%
          filter(Impermeability == "Type 1" && Shape == "Shape 1"))$Mature.Volume)$out) == outlier_values)
```

```
## [1] 16
```

What the previous analysis shows is that all the outliers we found are still outliers, even after when we look only at the group (type1, shape1). So these datapoints should be treated as outliers.

**4 - Perform a PCA on the dataset ScrewCap, explain brieﬂy what are the aims of PCA and how categorical variables are handled?**

The PCA consists in finding a low dimensional representation of our data in a way that will best retain the variability of the data. Specifically, it finds  orthogonal "directions" along which our data has the highest variance possible. 

Only numerical variables are used to find these directions. Categorical data is typically used as supplementary information: we study the  projection of the categories at the barycentre of the observations which take the categories.  
Here we also treat price as a supplementary variable since the goal of our analysis is to estimate price


```r
res.pca <- PCA(screw, scale.unit = TRUE, quali.sup = c(1,5,6,7,9), quanti.sup = 10)
```

![](full_analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

**Compute the correlation matrix between the variables and comment it with respect to the correlation circle.  **


```r
cor(screw %>% select_if(is.numeric))
```

```
##                 Diameter     weight nb.of.pieces Mature.Volume       Price
## Diameter       1.0000000  0.9624561  -0.14742721   -0.24777035  0.80034164
## weight         0.9624561  1.0000000  -0.16736670   -0.26659464  0.78792102
## nb.of.pieces  -0.1474272 -0.1673667   1.00000000   -0.05820704  0.04980782
## Mature.Volume -0.2477703 -0.2665946  -0.05820704    1.00000000 -0.30192234
## Price          0.8003416  0.7879210   0.04980782   -0.30192234  1.00000000
## Length         0.9996960  0.9629389  -0.14512574   -0.24874382  0.79978917
##                   Length
## Diameter       0.9996960
## weight         0.9629389
## nb.of.pieces  -0.1451257
## Mature.Volume -0.2487438
## Price          0.7997892
## Length         1.0000000
```
What we see from the correlation matrix is that Diameter, weight, and Length are extremely highly correlated to each other. Price is also positively correlated to these 3 variables. Number of pieces and Mature Volume are not strongly correlated to any other variables.  
This is confirmed by the PCA correlation circle. We see that the first direction computed by the PCA is parallel to diameter, weight and length. The projection of price depends mostly on the first price. The second direction is parallel to number of pieces.

**6 - On what kind of relationship PCA focuses? Is it a problem?**

PCA only focuses on linear relationships between the data. This can be a problem if there exist non-linear relationships between our data => PCA would miss them.

**7 -  Comment the PCA outputs.**

As we have said, the first principal component is extremely correlated to diameter weight and length. It is strongly correlated to price.  From the individuals factor map we see that individuals are not evenly distributed along this component: Most individuals are slightly below the mean, and a few are far above the mean.  
The second principal component is quite positively correlated to number of pieces.

Price is not perfectly explained by the two first principal components, as the length of the vector on the variables factor map is smaller than 1. This is even  more true for Number of pieces.

* **Comment the position of the categories Impermeability=type 2 and Raw.Material=PS**


```r
plot.PCA(res.pca, choix = c("ind"), label = "quali", col.ind = "pink", col.quali = "black", autoLab = "yes", cex = 0.8)
```

![](full_analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

* So we see that Impermeability = type 2 and Raw.material = PS are quite close on the individuals factor map, indicating that those two groups are similar. They are both along the first principal component, with quite high values for this component compared to the rest of the data. Their projection onto the second component is close to 0. So these groups depend mostly on the first component.

* **Comment the percentage of inertia**
We see that 61.49 % of the variability of our data is captured by the first component, while the second one captures an additional 21.09 % of the total variability.  
So overall, with the two first components alone we capture over 82 % of the variability of our data. This is satisfying: we have found a quite good low dimensional representation of our data. 

**8) Give the the R object with the two principal components which are the synthetic variables the most correlated to all the variables.**


```r
res.pca$var$coord[,1:2]
```

```
##                    Dim.1       Dim.2
## Diameter       0.9867048 -0.01712271
## weight         0.9783345 -0.02105079
## nb.of.pieces  -0.2080866  0.81573137
## Mature.Volume -0.3560437 -0.62299636
## Length         0.9868154 -0.01477619
```

**9) PCA is often used as a pre-processing step before applying a clustering algorithm, explain the rationale of this approach and how many components k you keep**

Preprocessing our data with a PCA is often used to 'denoise the data' before applying a clustering algorithm, and makes the clustering more stable. In general, we will choose the minimum number of principal components necessary to retain a certain amount (often 95%) of the variation of our data.  
We could also use the elbow rule.


```r
res.pca$eig
```

```
##          eigenvalue percentage of variance
## comp 1 3.0745963906           61.491927811
## comp 2 1.0544967801           21.089935603
## comp 3 0.8218979343           16.437958687
## comp 4 0.0487098555            0.974197111
## comp 5 0.0002990394            0.005980788
##        cumulative percentage of variance
## comp 1                          61.49193
## comp 2                          82.58186
## comp 3                          99.01982
## comp 4                          99.99402
## comp 5                         100.00000
```

```r
plot(res.pca$eig[,1], type="l", ylab = "Eigenvalue")
```

![](full_analysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Here in fact, we see that there is no clear "elbow" in our graph, so we will stick to the first heuristic: retain at least 95% of the total variance of our dataset.  
By looking at the cumulative percentage of variance column in our table, we see that this is achieved by keeping the first 3 principal components, and in fact we retain 99% of the variance.

**10) Perform a kmeans algorithm on the selected k principal components of PCA. How many cluster are you keeping? Justify**


```r
res.pca_3c <- PCA(screw, ncp=3, scale.unit = TRUE, quali.sup = c(1,5,6,7,9), quanti.sup = 10)
```

![](full_analysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-14-2.png)<!-- -->
We will be doing the clustering on this projection.  
To choose the number of clusters, we use the elbow method (we plot only up to 8 clusters)


```r
wss <- sapply(1:8, 
              function(k){kmeans(res.pca_3c$ind$coord, k, nstart=50,iter.max = 15 )$tot.withinss})

plot(1:8, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
```

![](full_analysis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

The plot does not exhibit a clear "elbow", which indicates that our data is not very clustered originally. We choose to have 2 clusters as this is where the inflexion point seems to be.



```r
res.kmeans <- kmeans(res.pca_3c$ind$coord, 2)

print("These are the center of the clusters we have found ")
```

```
## [1] "These are the center of the clusters we have found "
```

```r
res.kmeans$centers
```

```
##        Dim.1       Dim.2       Dim.3
## 1  4.5230724 -0.39618106  0.15057123
## 2 -0.5458881  0.04781496 -0.01817239
```

We plot our clustering in 3D using the *scatterplot3d* library. (Note: the plot may slow down our notebook. If this is the case, comment out the plot lines)


```r
proj <- data.frame(res.pca_3c$ind$coord)

scatterplot3d(x = proj[,1], y = proj[,2], z = proj[,3], color = res.kmeans$cluster,
              xlab="Principal Component 1" , ylab="Principal Component 2" , zlab="Principal Component 3" )
```

![](full_analysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


**11) Performs an AHC on the selected k principal components of PCA.**


```r
res.hcpc <- HCPC(res.pca_3c, nb.clust=-1)
```

![](full_analysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-18-2.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-18-3.png)<!-- -->

**12) Comments the results and describe precisely one cluster.**

Comments: by specifying *nb.clust=-1* in our call, the HCPC used cross-validation to find the optimal number of clusters for our data. We see that the best number of clusters is **3**, which indicates that we chose the wrong number of clusters with the elbow rule in question 10.  
We can also note that the projection of our clustered data onto the first two principal components (as seen in the factor map) seems to remain well clustered. This makes sense since these two principal components explain more than 80% of the variance in our data.

To describe precisely the first cluster, we can do the following:


```r
res.hcpc$desc.var$quanti$'1'
```

```
##                  v.test Mean in category Overall mean sd in category
## Mature.Volume  9.826446     3.093599e+05 96930.005128   1.948710e+05
## Diameter      -3.523930     7.379455e-01     1.284287   1.847906e-01
## Length        -3.559146     5.828560e+00    10.247218   1.512553e+00
## weight        -3.782495     1.000052e+00     1.700704   2.475292e-01
## Price         -4.908506     1.086889e+01    16.443852   1.836258e+00
## nb.of.pieces  -5.409898     2.909091e+00     4.112821   7.925271e-01
##                 Overall sd      p.value
## Mature.Volume 1.359000e+05 8.662182e-23
## Diameter      9.746250e-01 4.251971e-04
## Length        7.804498e+00 3.720626e-04
## weight        1.164460e+00 1.552643e-04
## Price         7.139907e+00 9.177266e-07
## nb.of.pieces  1.398750e+00 6.306061e-08
```

We see that the first centroid exhibits very high mature.volume, low diameter, low length, low weight, low price and high number of pieces, compared to the overall average. All p-values are highly significant indicating that the first cluster is significantly different to the rest of the data.

**13) If someone ask you why you have selected k components and not k +1 or k−1, what is your answer? (could you suggest a strategy to assess the stability of the approach? - are there many diﬀerences between the clustering obtained on k components or on the initial data)**

We chose 3 components in order to retain 99% of the total variance of our data, and remove some of the noise associated to the remaining 1%.

To assess the stability of the approach, we cluster the initial data. Note we can just cluster the data projected onto all principal components (in the res.pca object) and get the same clustering. The advantage of doing this is that the projected data is already scaled, which is important for the clustering.

```r
scaled <- as.data.frame(scale(screw %>% select_if(is.numeric)))

res.hcpc_init <- HCPC(res.pca, nb.clust=-1)
```

![](full_analysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-20-2.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-20-3.png)<!-- -->

We notice that the optimal number of clusters is still 3. 

We want to compute partition quality for both clusterings. The partition quality is defined as $$\frac{\text{between inertia}}{\text{total inertia}}$$
Remember that total inertia corresponds to the sum of the eigenvalues found in the pca.

```r
#between inertia

total_inertia <- sum(res.pca$eig[1:3,1])
total_inertia2 <- sum(res.pca$eig[,1])

print("The partition quality for the clustering on the 3 principal components (ratio of between inertia to total inertia) is:")
```

```
## [1] "The partition quality for the clustering on the 3 principal components (ratio of between inertia to total inertia) is:"
```

```r
res.hcpc$call$bw.after.consol / total_inertia
```

```
## [1] 0.644673
```

```r
print("The partition quality for the clustering on the inital data is: ")
```

```
## [1] "The partition quality for the clustering on the inital data is: "
```

```r
res.hcpc_init$call$bw.after.consol / total_inertia2
```

```
## [1] 0.6383859
```

And we see that the partition quality is slightly better for the clustering on the 3 first principal components. However the values are very close, and overall the approach is stable. 

**14) The methodology that you have used to describe clusters can also be used to describe a categorical variable, for instance the supplier. Use the function catdes(data, num.var=1) and explain how this information can be useful for the compagny**


```r
catdes(screw, num.var = 1)
```

```
## $test.chi2
##                     p.value df
## Raw.Material   0.0001649167  4
## Impermeability 0.0166850632  2
## 
## $category
## $category$`Supplier A`
##                        Cla/Mod  Mod/Cla   Global      p.value    v.test
## Raw.Material=PS       42.30769 35.48387 13.33333 0.0005137133  3.473502
## Impermeability=Type 2 34.78261 25.80645 11.79487 0.0181216728  2.363123
## Shape=Shape 2         26.66667 38.70968 23.07692 0.0337926808  2.122537
## Raw.Material=ABS       0.00000  0.00000 10.76923 0.0211426258 -2.305428
## Impermeability=Type 1 13.37209 74.19355 88.20513 0.0181216728 -2.363123
## 
## $category$`Supplier B`
##                    Cla/Mod Mod/Cla   Global     p.value    v.test
## Raw.Material=ABS 100.00000      14 10.76923 0.002847262  2.983764
## Raw.Material=PS   57.69231      10 13.33333 0.020029829 -2.325789
## Shape=Shape 2     60.00000      18 23.07692 0.003587698 -2.912307
## 
## $category$`Supplier C`
##                  Cla/Mod Mod/Cla   Global    p.value  v.test
## Raw.Material=PP 9.459459     100 75.89744 0.01799505 2.36572
## 
## 
## $quanti.var
##                   Eta2      P-value
## nb.of.pieces 0.2135959 9.593876e-11
## 
## $quanti
## $quanti$`Supplier A`
## NULL
## 
## $quanti$`Supplier B`
##                 v.test Mean in category Overall mean sd in category
## nb.of.pieces -2.778316             3.96     4.112821       1.232234
##              Overall sd     p.value
## nb.of.pieces    1.39875 0.005464138
## 
## $quanti$`Supplier C`
##                v.test Mean in category Overall mean sd in category
## nb.of.pieces 6.413237         6.428571     4.112821       1.720228
##              Overall sd      p.value
## nb.of.pieces    1.39875 1.424622e-10
## 
## 
## attr(,"class")
## [1] "catdes" "list "
```

This function is very useful. It allows us, for a given categorical variable in our data, to investigate:
* the variable's dependence to other categorical variables (chi2 test)
* the variable's dependence (correlation) to the numerical variables in our dataset
* the description of each  category of the variable, by the other variables in the dataset which exhibit a significant dependence to that variable.

For example, our call to the catdes function for the supplier variable taught us that :
* There is a significant dependence between supplier and the variables Raw material, impermeability, and number of pieces. Products which use supplier C have the highest number of pieces
* There is no significant dependence between supplier and price.

This is all important information for the company.

**15) To simultaneously take into account quantitative and categorical variables in the clustering you should use the HCPC function on the results of the FAMD ones. FAMD stands for Factorial Analysis of Mixed Data and is a PCA dedicated to mixed data. Explain what will be the impacts of such an analysis on the results?**

The impact will be that our clustering will now depend on the categorical variables as well as the quantitative ones.  
Let's perform FAMD and see what happens:


```r
FAMD(screw, sup.var = 11, ncp = 14)$eig
```

![](full_analysis_files/figure-html/unnamed-chunk-23-1.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-23-2.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-23-3.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-23-4.png)<!-- -->![](full_analysis_files/figure-html/unnamed-chunk-23-5.png)<!-- -->

```
##         eigenvalue percentage of variance
## comp 1  4.36407258             31.1719470
## comp 2  1.69048977             12.0749269
## comp 3  1.37536582              9.8240415
## comp 4  1.36809110              9.7720793
## comp 5  1.02193725              7.2995518
## comp 6  0.84387339              6.0276671
## comp 7  0.78048925              5.5749232
## comp 8  0.70393220              5.0280871
## comp 9  0.62061456              4.4329612
## comp 10 0.45560094              3.2542924
## comp 11 0.33280768              2.3771977
## comp 12 0.24426846              1.7447747
## comp 13 0.16594699              1.1853357
## comp 14 0.03251001              0.2322144
##         cumulative percentage of variance
## comp 1                           31.17195
## comp 2                           43.24687
## comp 3                           53.07092
## comp 4                           62.84299
## comp 5                           70.14255
## comp 6                           76.17021
## comp 7                           81.74514
## comp 8                           86.77322
## comp 9                           91.20619
## comp 10                          94.46048
## comp 11                          96.83768
## comp 12                          98.58245
## comp 13                          99.76779
## comp 14                         100.00000
```

We see that when we do FAMD on our data, the first principal components calculated do not retain much of the total variance of our data: the variance is split between many principal components. So we cannot find a good low dimensional representation of our data. 

To retain enough variation, we could do the clustering on a large number of principal components. However, clustering performs poorly in high dimensional spaces (curse of dimensionality).  
Or we could choose only the first few principal components to do the clustering. In this case however, we will have lost a significant portion of the variance of our data set : the projection on the principal components will be a poor approximation of our data set, and the clustering obtained may not be useful to describe the original dataset. 

**16) Perform a model to predict the Price. Explain how the previous analysis can help you to interpret the results.**

We will fit a linear model. We remember Diameter, weight and length are all extremely correlated, with pairwise correlation coefficients > 95%. To avoid multicollinearity, we do not include the weight and length variables in our model, only Diameter.

Because the model space is very high dimensional, we cannot explore all possible linear models. We will use a step-wise approach.  
Let's first fit the model with all variables (except wight and length), but no interaction terms. 


```r
res.lm1 <- lm(data = screw, formula = Price ~ Supplier + Diameter + nb.of.pieces + Shape + Impermeability + Finishing + Mature.Volume + Raw.Material)
summary(res.lm1)
```

```
## 
## Call:
## lm(formula = Price ~ Supplier + Diameter + nb.of.pieces + Shape + 
##     Impermeability + Finishing + Mature.Volume + Raw.Material, 
##     data = screw)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.3560  -2.2659  -0.3116   1.8095  16.4901 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           9.575e+00  1.703e+00   5.621 7.03e-08 ***
## SupplierSupplier B   -8.049e-01  7.806e-01  -1.031 0.303856    
## SupplierSupplier C   -2.653e+00  1.375e+00  -1.930 0.055168 .  
## Diameter              4.761e+00  4.111e-01  11.581  < 2e-16 ***
## nb.of.pieces          9.186e-01  2.499e-01   3.676 0.000311 ***
## ShapeShape 2         -6.141e-01  9.227e-01  -0.666 0.506521    
## ShapeShape 3          1.783e+00  1.381e+00   1.291 0.198346    
## ShapeShape 4          5.127e+00  1.487e+00   3.447 0.000704 ***
## ImpermeabilityType 2  6.097e+00  1.482e+00   4.115 5.85e-05 ***
## FinishingLacquering  -6.637e-01  6.097e-01  -1.089 0.277767    
## Mature.Volume        -4.559e-06  2.137e-06  -2.133 0.034230 *  
## Raw.MaterialPP       -2.301e+00  9.639e-01  -2.387 0.017993 *  
## Raw.MaterialPS       -3.253e+00  1.361e+00  -2.390 0.017850 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.731 on 182 degrees of freedom
## Multiple R-squared:  0.7451,	Adjusted R-squared:  0.7283 
## F-statistic: 44.34 on 12 and 182 DF,  p-value: < 2.2e-16
```

We see that we can drop the finishing variable


```r
res.lm2 <- lm(data = screw, formula = Price ~ Supplier + Diameter + nb.of.pieces + Shape + Impermeability + Mature.Volume + Raw.Material)
summary(res.lm2)
```

```
## 
## Call:
## lm(formula = Price ~ Supplier + Diameter + nb.of.pieces + Shape + 
##     Impermeability + Mature.Volume + Raw.Material, data = screw)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.9765  -2.1351  -0.1484   1.7410  17.0579 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           9.009e+00  1.623e+00   5.551 9.86e-08 ***
## SupplierSupplier B   -7.701e-01  7.803e-01  -0.987 0.325004    
## SupplierSupplier C   -2.805e+00  1.368e+00  -2.050 0.041795 *  
## Diameter              4.745e+00  4.110e-01  11.544  < 2e-16 ***
## nb.of.pieces          9.203e-01  2.500e-01   3.681 0.000305 ***
## ShapeShape 2         -4.773e-01  9.146e-01  -0.522 0.602371    
## ShapeShape 3          1.985e+00  1.369e+00   1.450 0.148834    
## ShapeShape 4          5.414e+00  1.465e+00   3.697 0.000289 ***
## ImpermeabilityType 2  6.051e+00  1.482e+00   4.084 6.61e-05 ***
## Mature.Volume        -4.660e-06  2.136e-06  -2.182 0.030419 *  
## Raw.MaterialPP       -2.197e+00  9.596e-01  -2.289 0.023217 *  
## Raw.MaterialPS       -3.291e+00  1.361e+00  -2.418 0.016597 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.733 on 183 degrees of freedom
## Multiple R-squared:  0.7435,	Adjusted R-squared:  0.7281 
## F-statistic: 48.22 on 11 and 183 DF,  p-value: < 2.2e-16
```

We cannot drop anymore predictors (if at least one level of a categorical variable is statistically significant, we must keep all levels). We will stop here though we could have tried to fit models with interaction terms.

Our predictive model is described in the summary of the lm output. We are interested in the estimates of the coefficients.

What we see is that Unit Price depends on the categorical variables Supplier, Shape, Impermeability and Raw Material. 
Unit Price increases with Diameter and Number of pieces - as well as with length and weight which are correlated to diameter. This makes sense.
Unit Price decreases with Mature Volume, which indicates that the company gives discounts for large orders.

**If someone ask you why you did one global model and not one model per supplier, what is your answer? **

If we had done one model per supplier, we would have had much less data for each model, which is generally bad.  
More importantly, we would have had different estimates for the coefficients in each model. How would we make sense of different mature.volume coefficients for different suppliers ? We also would not be able to measure the effect on price of a change in supplier: our whole model would change as soon as we change suppliers !
Our model is more parcimonious: all the other coefficients stay the same regardless of the supplier, only the supplier coefficient changes, when we change supplier. In effect, a change of supplier will induce an additive change in price. This is more easy to interpret than 3 different models for the 3 suppliers 

**18) These data contained missing values.** 

**One representative in the compagny suggests either to put 0 in the missing cells or to impute with the median of the variables. Comment.**  
This would be a terrible idea. Imputing by the median or 0 would in both cases cause us to misestimate the mean of our data, as their is no reason the median or 0 would be equal to the mean. We would also underestimate the variance and correlation of our data.
  
**For the categorical variables with missing values, it is decided to create a new category “missing”. Comment**  
This is another bad idea. Our categorical variables can take on a set of levels, and nothing else. Though it was not recorded for some reason, a missing value corresponds to one of those levels. In our "missing" category, we would therefore have a bunch of observations that correspond to completely different levels. Artificially introducing this new level is bad statistical practice.
