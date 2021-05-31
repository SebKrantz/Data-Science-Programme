#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 3: Linear Modelling and Analysis + Extensions in R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Session 2

# Today:
# (1) Panel Data Regression
# (2) Elementary machine learning (some links are old, see also the main resources for newer materials)
# (3) Main resources


# (1) Panel Data Regression ------------------------------------------------
#***************************************************************************

library(collapse)
library(jtools) # I will be using jtools::summ instead of summary() to summarise regression models
View(wlddev)

qsu(wlddev, pid = ~ country)

# Lets say we are interested in measuring the average effect of GDP on Life Expectancy on this panel of countries
# We could be inclined to run a regression like this:

summ(lm(LIFEEX ~ PCGDP, wlddev), digits = 5)

# But this regression is problematic for a number of reasons.
# In particular 2 reasons come to mind that have to do with the zero conditional mean assumption: cor(X, e) = 0  (The model is y = ßX + e)
# (1) There are lots of factors that impact both income and life expectancy that are omitted from this regression 
#     -> Issue of Endogeneity of the independent variable (GDP) / Omitted variable bias: We would need to control for lots of things or find a valid
#        instrument for GDP to argue that cor(X, e) = 0 is satisfied, that is that GDP is uncorrelated with all other factors affecting Life Expectancy 
#        that we have omitted from the regression, so that we can argue we are truly capturing the effect of GDP on Life Expectancy and not also the effect 
#        of many things correlated with both GDP and Life Expectancy. 
# (2) A lot of these omitted factors are country specific, causing some countries to have high GDP and life expectancy and others to have low GDP and Life expectancy 
#     Such as history / colonialism, spread of technology, quality of institutions, missionary activity etc.

# To get a closer look at this, let's visualize the data
library(magrittr)
wlddev %>% psmat(PCGDP + LIFEEX ~ iso3c, ~ year) %>% plot(colour = TRUE)
# We see that GDP and life expectancy in most countries increase over time, which 
# may be due to a number of omitted factors. 

# Lets plot the logarithm of GDP per capita against life expectancy
wlddev %$% plot(log(PCGDP), LIFEEX, col = iso3c, pch = 20, 
                xlab = vlabels(PCGDP), ylab = vlabels(LIFEEX),
                ylim = c(30, 85), # setting bounds to remove outliers
                main = "Log GDP Per Capita and Life Expectancy, 1960-2018") 
# -> Each sequence of dots of a certain colour
# Lets run a corresponding regression
pooled_model <- lm(LIFEEX ~ log(PCGDP), wlddev)         # Pooled OLS estimator
summ(pooled_model, digits = 5)  

# This function adds a regression line + formula to the plot
add_regline <- function(x, pos = "topleft", col = "blue") {
  abline(x, col = col)
  cf <- coef(x)
  formula_text <- sprintf("y = %.1f + %.1f x, R2 = %.3f", cf[1L], cf[2L], summary(x)$r.squared)
  legend(pos, formula_text, col = col, lty = 1, bty = "n")
}

add_regline(pooled_model)

# We can visualize the cross-country aspect of this relationship by computing the mean for each country over time
wlddev_means <- wlddev %>% gby(iso3c) %>% slt(PCGDP, LIFEEX) %>% fmean
wlddev_means %$% plot(log(PCGDP), LIFEEX, col = iso3c, pch = 20, 
                      xlab = vlabels(PCGDP), ylab = vlabels(LIFEEX),
                      ylim = c(30, 85),
                      main = "Log GDP Per Capita and Life Expectancy, 1960-2018")
# -> Looks similar: Evidently a big part of this relationship is due to factors between countries. 
means_model <- lm(LIFEEX ~ log(PCGDP), wlddev_means)  # Country means estimator
summ(means_model, digits = 5)   
add_regline(means_model)

# Now we can get rid of the cross-country aspect by subtracting the mean GDP and life expectance of
# each country from all the data points for that country (centering or within-transformation), 
# thus giving each country the same average GDP per capita and life expectancy over time
wlddev_demeaned <- wlddev %>% slt(iso3c, PCGDP, LIFEEX) %>% na_omit %>% gby(iso3c) %>%
                   tfm(PCGDP = log(PCGDP)) %>% fwithin 
wlddev_demeaned %$%  plot(PCGDP, LIFEEX, col = iso3c, pch = 20, 
                          xlab = vlabels(PCGDP), ylab = vlabels(LIFEEX),
                          main = "Log GDP Per Capita and Life Expectancy, 1960-2018")
# -> we can see now more clearly the relationship between GDP and life expectancy over time:
#    Some countries increase faster on GDP, others faster on life expectancy. 
within_model <- lm(LIFEEX ~ PCGDP, wlddev_demeaned)  # Country means estimator (log of PCGDP already taken)
summ(within_model, digits = 5)   
add_regline(within_model)

# So we can see that demeaning the data by country gets rid of unobserved country specific effects 
# that cause countries to have different levels of GDP and life expectancy, and that also influence
# the relationship between GDP and life expectancy

# Note that instead of demeaning the data, we can also include a dummy for each country in the regression
summ(lm(LIFEEX ~ PCGDP + iso3c, wlddev)) # Least-Squares Dummy variable Estimator
# Coefficient is the same as in the within model, but this is inefficient for large data
View(model.matrix(LIFEEX ~ PCGDP + iso3c, wlddev)) # This is what happens internally: iso3c is a factor variable, and lm calls model.matrix which creates a dummy for each country / factor level

# A final possibility that we have that also gets rid of country specific effects is taking first differences of the data
wlddev_FD <- wlddev %>% slt(iso3c, year, PCGDP, LIFEEX) %>% gby(iso3c) %>% 
             tfm(PCGDP = log(PCGDP)) %>% fdiff(t = year) 
wlddev_FD %$% plot(PCGDP, LIFEEX, col = iso3c, pch = 20, 
                   xlab = vlabels(PCGDP), ylab = vlabels(LIFEEX),
                   ylim = c(-2, 2), xlim = c(-0.5, 0.5),
                   main = "Log GDP Per Capita and Life Expectancy, 1960-2018")
FD_model <- lm(LIFEEX ~ PCGDP, wlddev_FD)  # First-difference estimator
summ(FD_model)   # Negative coefficient !
add_regline(FD_model)

# Another nice function in the jtools package
plot_coefs(pooled_model, means_model, within_model, FD_model, 
           model.names = c("Pooled", "Means", "Within", "FD"))
# 'modelsummary" also provides a set of related functions (also for exporting)
modelsummary::modelplot(list(Pooled = pooled_model, Means = means_model, Within = within_model, FD = FD_model))
modelsummary::modelsummary(list(Pooled = pooled_model, Means = means_model, Within = within_model, FD = FD_model))

# More formally, we can test the significance of country specific factors predicting LIFEEX and PCGDP
wlddev %$% fFtest(LIFEEX, iso3c) # fFtest efficiently deals with the dummies
wlddev %$% fFtest(PCGDP, iso3c)
# -> Both significant: At this point we know we have an omitted variable bias problem, there are country specific
# shifters that influence both the dependent and independent variable in the regression 
# We can also see that including country dummies improves the fit of the regression a lot
wlddev %$% fFtest(LIFEEX, iso3c, PCGDP)


# Now Lecture: https://sites.google.com/site/econometricsacademy/econometrics-models/panel-data-models
# ...and work through example + code provided. 

# Packages to implement panel data models in R: 
library(plm)
phtest(PCGDP ~ LIFEEX, data = wlddev, index = c("iso3c", "year"))
summary(plm(PCGDP ~ LIFEEX, data = wlddev, model = "within", index = c("iso3c", "year")))
options(plm.fast = TRUE)
summary(plm(PCGDP ~ LIFEEX, data = wlddev, model = "within", 
            index = c("iso3c", "year"), effect = "twoway"))

# Higher-dimensional fixed effects + more speed. 
library(fixest)
summary(fixest::feols(PCGDP ~ LIFEEX, data = wlddev, panel.id = c("iso3c", "year"), 
              fixef = c("iso3c", "year")))


#****************************
### In-Class Exercise 1 -----
#****************************

# Lets get the census data: 
CENS <- readRDS("Course 2 - Advanced Data Manipulation and Visualization in R/data/UBOS 2014 census.rds")
View(namlab(CENS))

# Estimate our model you developed for Exercise 1 in C3_S1.R with and without district fixed effects. 
# Follow the lecture on panl data on Econometrics Academy, and do the Hausman Test of the fixed effects against the random effects model. 
# using plm::phtest()
# Which is the appropriate specification to apply for your model?




# (2) Elementary Machine Learning ------------------------------------------
#***************************************************************************

# General Note: If you have no background in Machine Learning, consider some basic resources.

# An Introduction to Statistical Learning with Applications in R (free): http://www-bcf.usc.edu/~gareth/ISL/ (esay self study)
# Ho to become a data scientist infographic: https://www.datacamp.com/community/tutorials/how-to-become-a-data-scientist
# data camp: https://www.datacamp.com/

# great Online courses:
# Pratical (applied R): https://www.coursera.org/learn/practical-machine-learning or https://www.udemy.com/machinelearning/
# Great theoretical course (Stanford): https://www.coursera.org/learn/machine-learning?utm_source=gg&utm_medium=sem&campaignid=685340575&adgroupid=32639001341&device=c&keyword=coursera%20machine%20learning&matchtype=p&network=g&devicemodel=&adpostion=1t1&creativeid=273169971736&hide_mobile_promo&gclid=EAIaIQobChMIs_-NqcqA3QIVLbXtCh26EAVAEAAYASAAEgJYO_D_BwE
# Note: This is the most poopular machine learning course on the planet, course website with original video lectures: https://see.stanford.edu/Course/CS229
# VIP cheatsheets from one of the tutors of this course: https://stanford.edu/~shervine/teaching/cs-229.html
# MIT also offers a good course: https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-034-artificial-intelligence-fall-2010/

# A more advanced free resource: https://web.stanford.edu/~hastie/ElemStatLearn/

# Unsuperivsed Learning: PCA and Clustering ------------------------------------

## PCA: There are a number of packages allowing you to do PCA in R. But doing it manually ais also very easy.

# PCA Theory:
# https://towardsdatascience.com/a-one-stop-shop-for-principal-component-analysis-5582fb7e0a9c
# http://setosa.io/ev/principal-component-analysis/
# https://stats.stackexchange.com/questions/134282/relationship-between-svd-and-pca-how-to-use-svd-to-perform-pca

# A Practical R tutorial:
# https://sites.google.com/site/econometricsacademy/econometrics-models/principal-component-analysis


# Principal component analysis using eigenvalue decomposition on Correlation Matrix:
pca1 <- princomp(scale(mtcars)) # some functions standardize the data automatically, some don't (if in doubt look up the documentation). I always standardize manually using the scale function, to be sure.
summary(pca1)
# Loadings of principal components
loadings(pca1)
# or pca1$loadings
# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")
# Biplot of score variables
biplot(pca1)
# Scores of the components
pca1$scores
# Rotation
varimax(pca1$loadings)
promax(pca1$loadings)

# Let's do the same manually:
X=scale(mtcars) # This standardizes all columns of the dataset
eig= eigen(cor(X))$values # Eigenvalues
PCs= X %*% eigen(cor(X))$vectors # This computes the PC's (scores). Make sure data matrix is standardized.
# Check equivalence to Princomp Output
round(PCs,4)==round(pca1$scores,4) # Manual scores and scores using princomp only differ by som signs (i.e. some components are scaled by -1)

x=1:length(eig)
plot(x,eig, type = "b", xlab = "Component", ylab = "Eigenvalue", main = "Screeplot") # Screeplot. Note: The eigenvalues are the variances in each dorection. The eigenvectors are the base vectors spanning the rotated plane.

pve= eig/sum(eig)*100 # Percent Variance Explained

par(mfrow=c(1,2)) # specify graphics grid with 1 row, 2 columns
plot(x,pve, type = "b", xlab = "Component", ylab = "% Variance Explained", main = "% Variance Explained")
plot(x,cumsum(pve), type = "b", xlab = "Number of Components", ylab = "% Variance Explained", main = "Cumulative % Variance Explained")
par(mfrow=c(1,1))

# Factor loadings (factor or component correlation coefficients): https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r
loadings=sapply(data.frame(PCs), function(z)cor(X,z))
rownames(loadings)=colnames(X)
round(loadings,3)
# Rotate loadings:
varimax(loadings)$loadings
promax(loadings)$loadings
# Save varimax rotated scores:
RotPCs= PCs %*% varimax(loadings)$rotmat


# Principal Component Analysis Using Singular Value Decomposition (SVD). This is Considered the better method, also more efficient.
# I also like the prcomp function better than princomp. It's simpler and allows to explicityly set center and scale (i.e. standardization) options, and you can limit the number of components retaind.
PC <- prcomp(mtcars, center = TRUE, scale. = TRUE, rank. = 3) # We only retain 3 components
View(PC) # look at this object
summary(PC)
screeplot(PC, type="line", main="Scree Plot")
biplot(PC, choices=1:2, scale =0)
# Loadings
PC$rotation %*% diag(PC$sdev,3,3) # This is the only tricky thing with this function you have to optain the loadings by scaling the rotation matrix by these standard deviations. you can however also just compute the PC's using the predict function as below and then calculate the correlations of the PC's and the original variables. (for that see sapply command below)
# get Principal components (scores):
PCs2=predict(PC,mtcars)
# This also gives the loadings (Just calculating correlations between component scores and original variables)
sapply(data.frame(PCs2), function(z)cor(X,z))

PCs2FULL=predict(prcomp(mtcars, center = TRUE, scale. = TRUE),mtcars) # get all PC scores, not only 3
round(PCs,4)==round(PCs2FULL,4) # apart from machine precision and sign inversions, the SVD method gives same results as eigendecomposition method.

# Again, it is easy to implement SVD based PCA manually:
sv= svd(X)$d # Singular Values
PCs2m= X%*%svd(X)$v # svd(X)$v are also the eigenvectors of cor(X)
table(round(PCs2m,4)==round(PCs2FULL,4)) # both should give the same

eig = sv^2/(nrow(X)-1) # Get Eigenvalues from singular Values, rerun the plotting commands above (in manual eigensdecomposition section)


# Another package that provides PCA methods, and lots of other statistical tools used by psychologists:
library(psych)
# See amongst other things the KMO() function, computes Kaiser-Meyer-Olkin Measure of Sampling Adequacy


# Clustering:

# Reference: ISLR: # http://www-bcf.usc.edu/~gareth/ISL/

# Kmeans Clustering:
X=predict(prcomp(mtcars, center = TRUE, scale. = TRUE, rank.=2), mtcars) # Get First and second principal component for plotting
# 2 clusters
set.seed(4)
km.out=kmeans(scale(mtcars),2,nstart=20) # scale standardizes the data, important since we want to give all variables the same weight.
km.out
km.out$cluster
km.out$centers
plot(X, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", pch=20, cex=2)
text(X, labels = rownames(X), pos = 4)
# 3 clusters
set.seed(4)
km.out=kmeans(scale(mtcars),3,nstart=20)
km.out
km.out$tot.withinss
plot(X, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", pch=20, cex=2)
text(X, labels = rownames(X), pos = 4)

# Choose how many clusters? -> Plot total within cluster sum of squares for each number of clusters and Look for elbow in plot.
tot.withinss=sapply(1:20, function(x)kmeans(scale(mtcars),x,nstart=20)$tot.withinss)
plot(1:20,tot.withinss, xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")


# Hierarchical Clustering:

# Reference: ISLR: # http://www-bcf.usc.edu/~gareth/ISL/

hc.complete=hclust(dist(mtcars), method="complete")
hc.average=hclust(dist(mtcars), method="average")
hc.single=hclust(dist(mtcars), method="single")
par(mfrow=c(1,3)) # Sets graphical parameters: 1 row, 3 columns
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)
plot(hclust(dist(scale(mtcars)), method="complete"), main="Hierarchical Clustering with Scaled Features")
dd=as.dist(1-cor(t(scale(mtcars))))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
dd2=as.dist(1-abs(cor(t(scale(mtcars)))))
plot(hclust(dd2, method="complete"), main="Complete Linkage with Abs. Correlation-Based Distance", xlab="", sub="")


# Superivsed Learning: Tree's, Random Forests, Support Vector Machines, Subset Selection, LASSO & RIDGE, caret package -----------------------------------------

dev.off() # clear graphics environment

# Decision Trees:

# Reference: ISLR: # http://www-bcf.usc.edu/~gareth/ISL/

# Classification Tree with rpart
library(rpart)

# grow tree
fit <- rpart(carb ~ ., method="class", data=mtcars) # method="nova" gives regression tree

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE, main="Classification Tree for Mtcars")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Better Plot
library(rattle) # A number of plot for data science + GUI. Also install package RGtk2
#rattle() graphical GUI for machine learning

fancyRpartPlot(fit)

library(rpart.plot) # another library for plotting trees
rpart.plot(fit, fallen.leaves = FALSE)


# Random Forests:

# Reference: ISLR: # http://www-bcf.usc.edu/~gareth/ISL/

library(randomForest)
set.seed(1)
rfmodel=randomForest(factor(cyl)~.,data=mtcars, ntree=1000, importance=TRUE)
rfmodel
plot(rfmodel)
importance(rfmodel)
varImpPlot(rfmodel)

# Support Vector Machine:

# Reference: ISLR: # http://www-bcf.usc.edu/~gareth/ISL/

library(e1071)
svm = svm(cyl ~ ., data = mtcars, type = 'C-classification', kernel = 'radial')
print(svm)
summary(svm)

pred <- predict(svm,mtcars)
# Check accuracy: (Confusion matrix)
table(pred, mtcars$cyl) # looks like 100% accuracy, but of course we might have overfitted



# Regression Subset Selection:

# Reference: ISLR: # http://www-bcf.usc.edu/~gareth/ISL/

library(leaps)
# Functions that will help assess the results:
plotstats<-function(reg.summary) {
  par(mfrow=c(2,2),mar=c(3,5,1,1))
  #plot(reg.summary$rss,xlab="Number of Variables",ylab="Residual sum of squares",type="l")
  plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="l")
  plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R-squared",type="l")
  maxadjr2=which.max(reg.summary$adjr2)
  points(maxadjr2,reg.summary$adjr2[maxadjr2], col="red",cex=2,pch=20)
  text(maxadjr2,reg.summary$adjr2[maxadjr2], labels = paste0("Max (",maxadjr2,")"), pos = 1, col= "red")
  plot(reg.summary$cp,xlab="Number of Variables",ylab="Mallows' Cp",type='l')
  mincp=which.min(reg.summary$cp)
  points(mincp,reg.summary$cp[mincp],col="red",cex=2,pch=20)
  text(mincp,reg.summary$cp[mincp], labels = paste0("Min (",mincp,")"), pos = 3, col= "red")
  minbic=which.min(reg.summary$bic)
  plot(reg.summary$bic,xlab="Number of Variables",ylab="Schwartz's information criterion, BIC",type='l')
  points(minbic,reg.summary$bic[minbic],col="red",cex=2,pch=20)
  text(minbic,reg.summary$bic[minbic], labels = paste0("Min (",minbic,")"), pos = 3, col= "red")
  par(mfrow=c(1,1),mar=c(3,3,3,3))
}
cvplot<-function(x,y,k=10,nvmax=ncol(x),method = "forward") {
  #set.seed(1)
  xx<-cbind.data.frame(y,x)
  xx<-model.matrix(as.formula(paste0(names(xx)[1],"~.")),data = xx)
  
  folds=sample(1:k,nrow(x),replace=TRUE)
  
  cv.errors=matrix(NA,k,ncol(x), dimnames=list(NULL, paste(1:ncol(x))))
  cv.PVE=matrix(NA,k,ncol(x), dimnames=list(NULL, paste(1:ncol(x))))
  
  predict.regsubsets=function(object,xx,id,...){ # create a function that takes a regsubsets object
    coefi=coef(object,id=id)
    xvars=names(coefi)
    wi<-which(gsub("`","",colnames(xx)) %in% xvars)
    xx[,wi]%*%coefi
  }
  
  for(j in 1:k){
    best.fit=regsubsets(y=y[folds!=j], x=x[folds!=j,], nvmax=nvmax, method = method)
    mvar=mean((y[folds==j]-mean(y[folds==j]))^2) # variance of hitters
    for(i in 1:ncol(x)){
      pred=predict(best.fit,xx[folds==j,],id=i) #y+x?
      cv.errors[j,i]=mean( (y[folds==j]-pred)^2)
      cv.PVE[j,i]=round( ((mvar-cv.errors[j,i])/mvar)*100 ,2)
    }
  }
  
  mean.cv.errors=apply(cv.errors,2,mean)
  mean.cv.PVE=apply(cv.PVE,2,mean)
  
  par(mfrow=c(1,2),mar=c(3,5,3,1))
  plot(mean.cv.errors,type='b', main = "Evaluation using 10-fold Cross-Validation", ylab = "Mean Cross-Validation Error", xlab = "Size of Model")
  mincv=which.min(mean.cv.errors)
  points(mincv,mean.cv.errors[mincv],col="red",cex=2,pch=20)
  text(mincv,mean.cv.errors[mincv], labels = paste0("Min (",mincv,")"), pos = 3, col= "red")
  
  plot(mean.cv.PVE,type='b', main = "Evaluation using 10-fold Cross-Validation", ylab = "Mean Cross-Validated % Variance Explained", xlab = "Size of Model")
  maxcv=which.max(mean.cv.PVE)
  points(maxcv,mean.cv.PVE[maxcv],col="red",cex=2,pch=20)
  text(maxcv,mean.cv.PVE[maxcv], labels = paste0("Max (",maxcv,")"), pos = 1, col= "red")
  par(mfrow=c(1,1),mar=c(3,3,3,3))
}

regfit.full<-regsubsets(x=mtcars[,-1],y=mtcars$mpg, nvmax = ncol(mtcars), method = "exhaustive")
reg.summary=summary(regfit.full)
reg.summary
plotstats(reg.summary)
cvplot(x=mtcars[,-1],y=mtcars[,1], nvmax = ncol(mtcars[,-1]))

# LASSO and RIDGE Regression

# Reference: ISLR: # http://www-bcf.usc.edu/~gareth/ISL/

library(glmnet)
library(plotmo) # for plot_glmnet
# Function to Rank Predictors based on Lasso.
LASSOrank<-function(x,y,lasso.mod) {
  x=scale(as.matrix(x))
  selecmat=as.matrix(lasso.mod$beta)
  lambda=lasso.mod$lambda
  #View(selecmat) # full matrix!!
  res=apply(selecmat,1,function(z){ll<-sum(ifelse(z!=0,1,0)); lam<-lambda[length(lambda)-ll];
  lasso.pred=predict(lasso.mod,s=lam,newx=x)
  lr2=round(sum((lasso.pred-mean(y))^2)/sum((y-mean(y))^2),3)
  rres<-c(lam,ll,lr2);
  rres})
  rownames(res)<-c("Lambda","Survived Intervals","R2"); res<-t(res)
  
  lassores=data.frame(Predictors=rownames(res),round(res,3))
  lassores=lassores[order(lassores$Lambda, decreasing = T),]
  return(lassores)
}

# LASSO
lasso.mod=glmnet(x=scale(as.matrix(mtcars[,-1])),y=mtcars[,1],alpha=1)
cv.out=cv.glmnet(x=scale(as.matrix(mtcars[,-1])),y=mtcars[,1],alpha=1,type.measure="mse")
bestlam=cv.out$lambda.min
bestlam # Best lambda parameter according to 10-fold cross-validation.
dev.off()
par(mfrow=c(1,2))
plot_glmnet(lasso.mod, s=bestlam)
plot(cv.out)

# Ranking of Predictors + cumulative regulariyed R^2 of the model.
LASSOrank(x=scale(as.matrix(mtcars[,-1])),y=mtcars[,1], lasso.mod)

# FOR RIDGE Regression set alpha=0
ridge.mod=glmnet(x=scale(as.matrix(mtcars[,-1])),y=mtcars[,1],alpha=0)
cv.out.ridge=cv.glmnet(x=scale(as.matrix(mtcars[,-1])),y=mtcars[,1],alpha=0,type.measure="mse")
par(mfrow=c(1,2))
plot_glmnet(ridge.mod, s=cv.out.ridge$lambda.min)
plot(cv.out.ridge)
par(mfrow=c(1,1))

# Caret: Super easy to use package implementing all kinds of machine-learning methods

# See documentatio: http://topepo.github.io/caret/index.html
# And cheat sheet: https://github.com/rstudio/cheatsheets/raw/master/caret.pdf

library(caret)
# Random Forest:
rf <- train(factor(cyl) ~ ., data = mtcars, method = "rf", preProc = c("center", "scale"))
rf
summary(rf)
confusionMatrix(rf)
# Linear SVM:
svm <- train(factor(cyl) ~ ., data = mtcars, method = "svmLinear", preProc = c("center", "scale"))
svm
summary(svm)
confusionMatrix(svm)
# Neural Network:
nnet <- train(factor(cyl) ~ ., data = mtcars, method = "nnet", preProc = c("center", "scale"))
nnet
summary(nnet)
confusionMatrix(nnet)








# Top Resource: For this second part  -----------------------------------
# Econometrics Academy on Panel Data: https://sites.google.com/site/econometricsacademy
  # See also: https://www.econometrics-with-r.org/10-rwpd.html
  # And: https://bookdown.org/ccolonescu/RPoE4/panel-data-models.html
# Machine Learning Essentials in R: http://www.sthda.com/english/articles/11-machine-learning/
  # In particular:
  # http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/
  # http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/
  # http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/
# Introduction to statistical learning with applications in R (highly recommended book to download and read): https://www.statlearning.com/



