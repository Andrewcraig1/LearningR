### Modelling the expert
## Example Quick question
odds<-exp(-1.5+(3*1)+(-0.5*5))
odds
# The Logit is just log(Odds), and looks like the linear regression equation.
# So the Logit is -1.5 + 3*1 - 0.5*5 = -1.
logit<-log(odds)
logit

# What is the value of P(y = 1) for this observation?
#Using the Logistic Response Function, we can compute that
# P(y = 1) = 1/(1 + e^(-Logit))
#= 1/(1 + e^(1)) = 0.2689414.
P<-1/(1+exp(-logit))
P



