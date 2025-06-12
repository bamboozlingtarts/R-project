setwd("C:/Users/bibhu/OneDrive/Desktop/R")

df = read.csv("DataForJune10th.csv")
Y = colnames(df)[1]
X = colnames(df)[2]

#plot : looks like cubic to me
plot(df[[X]], df[[Y]],
     main="Scatter Plot with Regression Curves",
     xlab=X, ylab=Y, pch=18, col="pink")

formula_linear = as.formula(Y~X) #linear
#has to be in formula form, or it will compare strings
model_linear = lm(formula_linear, data=df) 
print(summary(model_linear))

formula_quadratic = as.formula(paste(Y, "~", X, "+ I(", X, "^2)")) 
#without I, x will interact with 2, with I, x is raised to the power of 2
model_quadratic = lm(formula_quadratic, data=df) 
print(summary(model_quadratic))

formula_cubic = as.formula(paste(Y, "~", X, "+ I(", X, "^2)", "+I(",X,"^3)"))
model_cubic = lm(formula_cubic, data=df) 
print(summary(model_cubic))

formula_4thorder = as.formula(paste(Y, "~", X, "+ I(", X, "^2)", "+I(",X,"^3)",
                                    "+I(",X,"^4)"))
model_4thorder = lm(formula_4thorder, data=df) 
print(summary(model_4thorder))

formula_5thorder = as.formula(paste(Y, "~", X, "+ I(", X, "^2)", "+I(",X,"^3)", 
                                    "+I(",X,"^4)", "+I(",X,"^5)"))
model_5thorder = lm(formula_5thorder, data=df) 
print(summary(model_5thorder))

formula_6thorder = as.formula(paste(Y, "~", X, "+ I(", X, "^2)", "+I(",X,"^3)",
                                    "+I(",X,"^4)", "+I(",X,"^5)", "+I(",X,"^6)"))
model_6thorder = lm(formula_6thorder, data=df) 
print(summary(model_6thorder))

formula_7thorder = as.formula(paste(Y, "~", X, "+ I(", X, "^2)", "+I(",X,"^3)",
                                    "+I(",X,"^5)", "+I(",X,"^5)", "+I(",X,"^6)",
                                    "+I(",X,"^7)"))
model_7thorder = lm(formula_7thorder, data=df) 
print(summary(model_7thorder))


cat("R squared (linear) = ",summary(model_linear)$r.squared, "\n")
cat("R squared (quadratic) = ",summary(model_quadratic)$r.squared, "\n")
cat("R squared (cubic) = ",summary(model_cubic)$r.squared, "\n")
cat("R squared (4thorder) = ",summary(model_4thorder)$r.squared, "\n")
cat("R squared (5thorder) = ",summary(model_5thorder)$r.squared, "\n")
cat("R squared (6thorder) = ",summary(model_6thorder)$r.squared, "\n")
cat("R squared (7thorder) = ",summary(model_7thorder)$r.squared, "\n")

#4th, 5th, and 6th order: Stepwise R
#trace prints out each step

# 4th order
print("--------------------------stepwise regression for 4th order-------------------")
stepwise_4th = step(model_4thorder, direction = "both") 
summary(stepwise_4th)

# 5th order
print("--------------------------stepwise regression for 5th order-------------------")
stepwise_5th = step(model_5thorder, direction = "both")
summary(stepwise_5th)

# 6th order
print("--------------------------stepwise regression for 6th order-------------------")
stepwise_6th = step(model_6thorder, direction = "both", trace = 1)
#6th order trace explains for 4th, 5th, and 6th so trace here only.
summary(stepwise_6th)

print(predict(model_4thorder, 
        newdata = setNames(data.frame(0), X), 
        interval = "confidence", 
        level = 0.90))

