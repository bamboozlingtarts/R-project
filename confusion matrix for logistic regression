df_ini = read.csv("iris.csv")
my_df = df_ini[df_ini$species != "virginica", ]

my_df$species_bin = ifelse(my_df$species == "setosa", 0, 1)

#split data 
set.seed(111) #to make the result repetitive 
RandomNumbers=runif(nrow(my_df))
my_df['Rn']=RandomNumbers

my_df=my_df[order(my_df$Rn),]
rownames(my_df)=1:nrow(my_df)

#test and train
iris_testset=my_df[1:as.integer(0.25*nrow(my_df)),]
iris_trainingset=my_df[(as.integer(0.25*nrow(my_df))+1):nrow(my_df),]

model_iris = glm("species_bin~sepal_length+sepal_width+petal_length+petal_width",
                 data=iris_trainingset,family="binomial",maxit =1000)
species_p = predict.glm(model_iris,iris_testset,type = "response")

predicted_species = c()
for(i in species_p){
  #this is classifying that above 0.5 is 1, and less than 0.5 is 0
  if(i<0.5){
    predicted_species=c(predicted_species,0)
  }else{
    predicted_species=c(predicted_species,1)
  }
}
confusionmatrix_iris=table(Predicted=predicted_species, 
                           True=iris_testset$species_bin) 
print(confusionmatrix_iris)

y = 1 / (1 + exp(-(7.199e+00 +
                     -1.066e+01 * my_df$sepal_length +
                     -7.672e+00 * my_df$sepal_width +
                     2.061e+01 * my_df$petal_length +
                     2.556e+01 * my_df$petal_width)))


#plotting y against sepal_length
plot(my_df$sepal_length,y, 
     xlab = "sepal_length",
     ylab = "Predicted Probability (sigmoid)",
     main = "Logistic Sigmoid Curve",
     pch = 19, col = "pink")

xs=seq(min(my_df$sepal_length),max(my_df$sepal_length),by=0.01)
ys=1 / (1 + exp(-(7.199e+00 +
                    -1.066e+01 * xs +
                    -7.672e+00 * mean(my_df$sepal_width) +
                    2.061e+01 * mean(my_df$petal_length) +
                    2.556e+01 * mean(my_df$petal_width))))
plot(xs,ys, col="skyblue")
