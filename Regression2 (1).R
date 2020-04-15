###Class Example###
# Create a dataset to tell a store 
# x = sample(seq(0,1000,1), size = 1000, replace = TRUE)
# y = 10 + 2*x - 0.002*(x^2) + rnorm(1000,sd=20)
# Party = data.frame(Alcohol = x, Satisfaction = y)
# write.csv(Party, "Party.csv", row.names = FALSE)

# Load the data
Party = read.csv("Party.csv")

# Regression using only linear term
PartyReg1 = lm(Satisfaction ~ Alcohol, data = Party)
summary(PartyReg1)

plot(Party$Satisfaction~Party$Alcohol)


# Creating a squared term
Party$Alcohol_Sq = Party$Alcohol * Party$Alcohol

# Regression using only linear and quadratic term
PartyReg2 = lm(Satisfaction ~ Alcohol + Alcohol_Sq, data = Party)
summary(PartyReg2)

# Predict and plot
x = seq(0,1000,1)
q = Party2$coefficient[1] + Party2$coefficient[2]*x + Party2$coefficient[3]*x^2

plot(Party$Satisfaction~Party$Alcohol,xlab = "Alcohol", ylab = "Satisfaction")
par(new = TRUE)
lines(x,q, col="red",lwd=3)

# Measure of Fitness
# Simulate the data
x = rnorm(1000)
y1 = 5 + 2*x + rnorm(1000, sd = 0.2)
y2 = 5 + 2*x + rnorm(1000, sd = 2)

m1 = lm(y1~x)
summary(m1)
m2 = lm(y2~x)
summary(m2)

plot(y1 ~ x, ylim = c(-12,20))
abline(m1, col="red", lwd=3)

plot(y2 ~ x,ylim = c(-12,20))
abline(m2, col="red", lwd=3)


oj <- read.csv("oj.csv") 

reg5 = lm(sales ~ feat + price, data = oj)

coef(reg5)["(Intercept)"] + coef(reg5)["feat"]*0 + coef(reg5)["price"]*2.2

coef(reg5) # coef() is to get the coefficients

vInput = c(1, 0, 2.2)

sum( coef(reg5) * vInput )

vInput = c(1, 1, 5)
sum( coef(reg5) * vInput )


#################
#Measure of Fit
#################


oj <- read.csv("oj.csv") 
reg3 = lm(sales ~ price, data = oj)
summary(reg3)

reg5 = lm(sales ~ feat + price, data = oj)
summary(reg5)

oj$price_sq = oj$price * oj$price

reg7 = lm(sales ~ feat + price + price_sq, data = oj)

summary(reg7)

oj$price_3 = oj$price * oj$price * oj$price

reg8 = lm(sales ~ feat + price + price_sq + price_3, data = oj)

summary(reg8)

oj$price_4 = oj$price * oj$price * oj$price* oj$price

reg9 = lm(sales ~ feat + price + price_sq + price_3+ price_4, data = oj)

summary(reg9)


