library(MASS); library(ISLR)
head(Smarket)

x_variable = 1
if (x_variable <0)
{
  print("Error: x_variable cannot be negative")
}


lda_model = lda(Direction~ Lag1 + Lag2, data = Smarket, subset = train)
lda_model; plot(lda_model)
lda_predict = predict(lda_model, Smarket.2005)
names(lda_predict)
lda_class = lda_predict$class
table(lda_class, Direction.2005)
mean(lda_class == Direction.2005)

#using a different probability threshold for the posterior
sum(lda_predict$posterior[,1] >=.9)
da_class = lda_predict$class
predictors = predict%x_variables)
table(lda_class, Direction.2005)
mean(lda_class != Direction.2005)
