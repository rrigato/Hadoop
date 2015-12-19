library(MASS); library(ISLR)
head(Smarket)
lda_model = lda(Direction~ Lag1 + Lag2, data = Smarket, subset = train)
lda_model; plot(lda_model)
lda_predict = predict(lda_model, Smarket.2005)
names(lda_predict)
lda_class = lda_predict$class
table(lda_class, Direction.2005)
mean(lda_class == Direction.2005)

#using a different probability threshold for the posterior
sum(lda_predict$posterior[,1] >=.9)