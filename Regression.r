---
title: "assignment 2"
author: "Qirui Liu, Ashwuni Kumar, Gunchica Bhalla"
geometry: margin=.75in
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
    theme: cosmo
header-includes:
- \usepackage{graphicx}
- \usepackage{color}
graphics: yes
fontsize: 11pt
---

```{r}
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
library('tidyverse')
songs <- read.csv('spotify_songs.csv')
head(songs)
```

## Question 1: Nonlinear Regression

1.1. Process your data

output:energy

input categorical: playlist_genre

input categorical: track_popularity,energy, speechiness, loudness, key, acousticness,instrumentalness, liveness, valence, tempo, instrumentalness,
liveness, valence, tempo, duration_ms,danceability
```{r}
data <- select(songs, c('danceability','playlist_genre', 'track_popularity',
'energy','speechiness','loudness','key','acousticness','instrumentalness','liveness','valence','tempo','instrumentalness','liveness','valence','tempo','duration_ms'))

data <- na.omit(data)

data <- data[sample(5000), ]
```
1.2. Train/split
```{r}
set.seed(156) # Set seed is needed if we want 
# to get the same random numbers always
train_size <- floor(0.8 * nrow(data))
train_inds <- sample(1:nrow(data), size = train_size)
test_inds  <- setdiff(1:nrow(data), train_inds)

train <- data[ train_inds , ] 
test  <- data[ test_inds , ]

cat('train size:', nrow(train), '\ntest size:', nrow(test))
```
1.3 Visualize the data
```{r}
library('gridExtra')
g1 <- ggplot(data, aes(x=energy)) + 
  geom_histogram(alpha=.5) 


g2 <- ggplot(data, aes(x=loudness)) + 
  geom_histogram(alpha=.5) 


grid.arrange(g1,g2, ncol=2)
```

```{r}
g3 <- ggplot(data, aes(y=energy, x=loudness)) + 
  geom_point(alpha=.3) 
g3
# numeric.cols <- summarize_all(data, is.numeric) %>% unlist()
# pairs(data[,numeric.cols], col = adjustcolor('firebrick', .3), pch=16)
```
Discussion: It is positive linear relationship, there is some outliers near the origin point. Mostly between -17 loudness and -27 loudness.

1.4 Fit 4 models
1
```{r}
fit1 <- lm(energy ~ loudness, data = data)
fit1
#summary(fit1)
#coef(fit1)

pred1 <- predict(fit1, newdata=train)
pred2 <- predict(fit1, newdata=test)

```
2
```{r}
fit2 <- lm(energy ~ track_popularity + speechiness+ loudness+ key+ acousticness+instrumentalness+ liveness+ valence+ tempo+ instrumentalness+
liveness+ valence+ tempo+ duration_ms+danceability, data = data)
#summary(fit2)
#coef(fit2)

pred3 <- predict(fit2, newdata=train)
pred4 <- predict(fit2, newdata=test)
```
3
```{r}
fit.poly1 <- lm(energy ~ I(loudness^2), data = data)
sigma(fit.poly1)
pred5 <- predict(fit.poly1, newdata=train)
pred6 <- predict(fit.poly1, newdata=test)
```
4
```{r}
ggplot(data, aes(y=energy, x=loudness)) + 
  geom_point(alpha=.8, position = position_jitter())
```

```{r}
lm.fit  <- lm(energy ~ loudness, data=data)
wlm.fit <- loess(energy ~ loudness, data = data)

preds.lm  <- predict(lm.fit)
preds.wlm <- predict(wlm.fit)
preds.wlm1 <- predict(fit.poly1, newdata=train)
preds.wlm2 <- predict(fit.poly1, newdata=test)


ggplot(data, aes(y=energy, x=loudness)) + 
  geom_point(alpha=.6, position = position_jitter()) +
  geom_line(aes(y=preds.lm), colour = 'green', alpha=1) +
  geom_line(aes(y=preds.wlm), colour = 'firebrick', alpha=1)


```

```{r}
library('caret')
rmse1 <- RMSE(pred1, train$energy)
rmse2 <- RMSE(pred2, test$energy)
rmse3 <- RMSE(pred3, train$energy)
rmse4 <- RMSE(pred4, test$energy)
rmse5 <- RMSE(pred5, train$energy)
rmse6 <- RMSE(pred5, test$energy)
rmsewlm1 <-RMSE(preds.wlm1, train$energy)
rmsewlm2 <-RMSE(preds.wlm2, test$energy)

c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmsewlm1,rmsewlm2)
```
Train set ranking (Best to worst): 
1. RMSE2 - multilinear regression model
2. RMSE1 - simple linear regression model
3. RMSE3 - polynomial regression model
4. RMSE4 - Locally Weighted Regression model

Test set ranking (Best to worst) :
1. RMSE2 - multilinear regression model
2. RMSE1 - simple linear regression model
3. RMSE3 - polynomial regression model
4. RMSE4 - Locally Weighted Regression model

The order of models did not change when ranked using training and test error. multilinear regression model is the best and locally weighted regression model is the worst

1.5
```{r}
set.seed(15)

val_inds <- sample(1:nrow(train), size = floor(2/8 *nrow(train)))
val      <- train[val_inds,]
train5    <- train[-val_inds,]

c(nrow(train5),nrow(val),nrow(test))
```
```{r}
rmse_test <- function(model, data=test) {
  preds <- predict(model,newdata=data)
  rmse  <- RMSE(preds,test$energy)
  return(rmse)
}
```


```{r}

ctrl <- trainControl(method = "cv", number = 10)

cv_fit <- train(
  energy ~ loudness, 
  data = train5, 
  method = "lm",
  trControl = ctrl
)

print("simple linear regression")
rmse_test(cv_fit)
```

```{r}
multi <- train(
  form = energy ~ track_popularity + speechiness+ loudness+ key+ acousticness+instrumentalness+ liveness+ valence+ tempo+ instrumentalness+
liveness+ valence+ tempo+ duration_ms+danceability, 
  data = train5,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

print("multi linear regression")
rmse_test(multi)

```
```{r}
poly <- train(
  form = energy ~ poly(loudness,2), 
  data = train5,
  method = "lm",
  trControl = trainControl(method = "CV", number = 10)
)
print("poly")
rmse_test(poly)
```
```{r}
lw <- train(
  x = model.matrix(energy~loudness,train),
  y = train$energy,
  method = "gamLoess",
  trControl = trainControl(method = "CV", number = 10)
)
print("Linear Wieghted Regression")
rmse_test(lw)
```
The order changed. The second best model before was linear regression model and now it is the worst. 

1.6
```{r}
library('glmnet')
  print("linear regression lasso")

x   <- model.matrix(energy ~ loudness,train)
y   <- train$energy

fit <- glmnet(x, y, alpha=1, nfolds = 10)
min = fit$lambda.min
fit <- glmnet(x, y, alpha=1, lambda = min)

formula = energy ~ loudness
newX <- model.matrix(formula, test)

prednew11 <- predict(fit,newx=newX,s=fit$lambda.min, newdata=test)
rmsenew11 <- RMSE(prednew11, test$energy)
c(rmsenew11)


```
```{r}
  print("linear regression ridge")

x1   <- model.matrix(energy ~ loudness,train)
y1   <- train$energy

fit1 <- glmnet(x1, y1, alpha=0, nfolds = 10)
min = fit1$lambda.min
fit1 <- glmnet(x1, y1, alpha=0, lambda = min)

formula = energy ~ loudness
newX <- model.matrix(formula, test)

prednewlr <- predict(fit1,newx=newX,s=fit1$lambda.min, newdata=test)
rmsenewlr <- RMSE(prednewlr, test$energy)
c(rmsenewlr)
```


```{r}
  print("Multi regression lasso")

x   <- model.matrix(energy ~ track_popularity + speechiness+ loudness+ key+ acousticness+instrumentalness+ liveness+ valence+ tempo+ instrumentalness+
liveness+ valence+ tempo+ duration_ms+danceability,train)
y   <- train$energy

fitmulti <- glmnet(x, y, alpha=1, nfolds = 10)
min2 = fitmulti$lambda.min
fitmulti <- glmnet(x, y, alpha=1, lambda = min2)

formula2 = energy ~ track_popularity + speechiness+ loudness+ key+ acousticness+instrumentalness+ liveness+ valence+ tempo+ instrumentalness+
liveness+ valence+ tempo+ duration_ms+danceability
newXmulti <- model.matrix(formula2, test)

prednewm <- predict(fitmulti,newx=newXmulti,s=fitmulti$lambda.min, newdata=test)
rmsenewm <- RMSE(prednewm, test$energy)
c(rmsenewm)
```

```{r}
  print("Multi regression ridge")

x   <- model.matrix(energy ~ track_popularity + speechiness+ loudness+ key+ acousticness+instrumentalness+ liveness+ valence+ tempo+ instrumentalness+
liveness+ valence+ tempo+ duration_ms+danceability,train)
y   <- train$energy

fitmulti <- glmnet(x, y, alpha=0, nfolds = 10)
min2 = fitmulti$lambda.min
fitmulti <- glmnet(x, y, alpha=0, lambda = min2)

formula2 = energy ~ track_popularity + speechiness+ loudness+ key+ acousticness+instrumentalness+ liveness+ valence+ tempo+ instrumentalness+
liveness+ valence+ tempo+ duration_ms+danceability
newXmulti <- model.matrix(formula2, test)

prednewmr <- predict(fitmulti,newx=newXmulti,s=fitmulti$lambda.min, newdata=test)
rmsenewmr <- RMSE(prednewmr, test$energy)
c(rmsenewmr)
```


```{r}
print("poly regression lasso")

x   <- model.matrix(energy ~ poly(loudness,2),train)
y   <- train$energy

fitp <- glmnet(x, y, alpha=1, nfolds = 10)
minp = fitp$lambda.min
fitp <- glmnet(x, y, alpha=1, lambda = minp)

formula = energy ~ poly(loudness,2)
newX <- model.matrix(formula, test)

prednewp <- predict(fitp,newx=newX,s=fit$lambda.min, newdata=test)
rmsenewp <- RMSE(prednewp, test$energy)
c(rmsenewp)
```
```{r}
print("poly regression ridge")

x   <- model.matrix(energy ~ poly(loudness,2),train)
y   <- train$energy

fitp <- glmnet(x, y, alpha=0, nfolds = 10)
minp = fitp$lambda.min
fitp <- glmnet(x, y, alpha=0, lambda = minp)

formula = energy ~ poly(loudness,2)
newX <- model.matrix(formula, test)

prednewpr <- predict(fitp,newx=newX,s=fit$lambda.min, newdata=test)
rmsenewpr <- RMSE(prednewpr, test$energy)
c(rmsenewpr)
```
```{r}
c(rmsenew11,rmsenewlr,rmsenewm,rmsenewmr,rmsenewp,rmsenewpr)
```
The Multi regression lasso model yielded minimum test loss.
1. Multi regression lasso
2. Linear regression lasso
3. Multi regression ridge
4. Linear regression ridge
5. Poly regression ridge
6. Poly regression lasso

\pagebreak
Question 2: Text Classification
```{r}
library('glmnet')
health <- read.csv("mental_health.csv")[,-1]
set.seed(156) # Set seed is needed if we want 
# to get the same random numbers always
train_size <- floor(0.8 * nrow(health))
train_inds <- sample(1:nrow(health), size = train_size)
test_inds  <- setdiff(1:nrow(health), train_inds)

trainh <- health[ train_inds , ] 
testh  <- health[ test_inds , ]

cat('train size:', nrow(trainh), '\ntest size:', nrow(testh))

```

2.2 Fit models

Fit a logistic regression model without regularization on train set. Call this model fit.logreg.
```{r}

x   <- model.matrix(IsMentalHealthRelated ~ .,trainh)
y   <- trainh$IsMentalHealthRelated
fit.logreg <- glmnet(x,y,family="binomial", lambda = 0)
head(coef(fit)[,1], 10)

cv.fit  <- cv.glmnet(x,y,alpha=1, family="binomial", nfolds = 10)
cv.fit$lambda.min
```

Fit a logistic regression model with L1 regularization on train set:
```{r}

fit.l1 <- glmnet(x,y,alpha=1, family="binomial", lambda = cv.fit$lambda.min)
#head(coef(fit.l1)[,1], 10)

```

Fit a logistic regression model with L2 regularization on train set:
```{r}
cv.fit2  <- cv.glmnet(x,y,alpha=0, family="binomial", nfolds = 10)
cv.fit2$lambda.min

fit.l2 <- glmnet(x,y,alpha=0, family="binomial",lambda = cv.fit2$lambda.min)
#head(coef(fit.l1)[,1], 10)
```

2.3. Compare performances

```{r}


formula = health ~ IsMentalHealthRelated
newX <- model.matrix(IsMentalHealthRelated ~ .,testh)


probs  <- predict(fit.logreg,newX, type = 'response')
preds  <- ifelse(probs >= 0.5, "1", "0")
target <- testh$IsMentalHealthRelated
acc1 <- mean(preds == target)
acc1


probs  <- predict(fit.l1,newX, type = 'response')
preds  <- ifelse(probs >= 0.5, "1", "0")
target <- testh$IsMentalHealthRelated
acc2 <- mean(preds == target)
acc2

```
```{r}
probs  <- predict(fit.l2,newX, type = 'response')
preds  <- ifelse(probs >= 0.5, "1", "0")
target <- testh$IsMentalHealthRelated
acc3 <- mean(preds == target)
acc3
```
The accuracy is improving from without regularization to L1,then L2.

2.4. Interpret the models
```{r}
coef1 = coef(fit.l1)[,1]
sort(coef1)
```

```{r}
coef2 = coef(fit.l2)[,1]
sort(coef2)
```
Largest: term, counsel,university, anxiety
Lowest: fitness, workout, sugar

L1 tends to zero many coefficients while keeping the rest as they are based on what is showing in coef1. L2 tends to shrink all the coefficients and doesn't zero any as we cannot find zero values in coef2.

\pagebreak

Question 3: Subset Selection

```{r}
library("tidyverse")
library("leaps")
library("AmesHousing")
ames        <- AmesHousing::make_ames()
numericVars <- ames %>% summarise_all(is.numeric) %>% unlist()
ames        <- ames[, numericVars]
ames <- na.omit(ames)
ames <- ames[sample(5000), ]
head(ames)
names(ames)
```

<!-- Lot_Frontage + Lot_Area + Year_Built+ Year_Remod_Add + Mas_Vnr_Area + BsmtFin_SF_1 + BsmtFin_SF_2 + Bsmt_Unf_SF + Total_Bsmt_SF + First_Flr_SF + Second_Flr_SF + Low_Qual_Fin_SF + Gr_Liv_Area + Bsmt_Full_Bath + Bsmt_Half_Bath + Full_Bath + Half_Bath + Bedroom_AbvGr + Kitchen_AbvGr + TotRms_AbvGrd + Fireplaces + Garage_Cars + Garage_Area + Wood_Deck_SF + Open_Porch_SF + Enclosed_Porch + Three_season_porch + Screen_Porch + Pool_Area + Misc_Val + Mo_Sold + Year_Sold + Longitude + Latitude -->

#Forward Selection

```{r}
res <- regsubsets(Sale_Price ~ .,data=ames, method = "forward",  nvmax=35)

smm <- summary(res)
smm$rss

```

```{r}
smm <- summary(res)
y <- smm$rss
x <- c(1:length(y))
for (i in 1:length(y)) {
  x[i] <- sum(summary(res)$which[i,])
}

x_name <- "num_predictors"
y_name <- "RSS"
df <- data.frame(x,y)

ggplot(df, aes(x=x, y=y)) + 
  geom_point(alpha=.3, size=2)

coef(res,33)
which.min(y)
```
33 predictors were used in the best mode.



bic
```{r}
smm <- summary(res)
y <- smm$bic
x <- c(1:length(y))
for (i in 1:length(y)) {
  x[i] <- sum(summary(res)$which[i,])
}

x_name <- "num_predictors"
y_name <- "RSS"
df <- data.frame(x,y)

ggplot(df, aes(x=x, y=y)) + 
  geom_point(alpha=.3, size=2)

coef(res,33)
```
21 predictors yielded the minimum BIC.


#backward

```{r}

res <- regsubsets(Sale_Price ~ .,data=ames, method = "backward",  nvmax=35)

smm <- summary(res)
smm$rss

```


```{r}
smm <- summary(res)
y <- smm$rss

x <- c(1:length(y))
for (i in 1:length(y)) {
  x[i] <- sum(summary(res)$which[i,])
}

x_name <- "num_predictors"
y_name <- "RSS"
df <- data.frame(x,y)

ggplot(df, aes(x=x, y=y)) + 
  geom_point(alpha=.3, size=2)

coef(res,33)
which.min(y)

```
33 predictors were used in the best model.

```{r}
smm <- summary(res)
y <- smm$bic
x <- c(1:length(y))
for (i in 1:length(y)) {
  x[i] <- sum(summary(res)$which[i,])
}

x_name <- "num_predictors"
y_name <- "RSS"
df <- data.frame(x,y)

ggplot(df, aes(x=x, y=y)) + 
  geom_point(alpha=.3, size=2)

coef(res,33)
which.min(y)
```

21 predictors yielded the minimum BIC.

