############################################################
#HW7
#Ben Horner
#MATH 240
############################################################
############################################################
#Libraries
############################################################



############################################################
#Question 1: Poisson probability function
############################################################
pois.prob = function(x, lambda, type = "<="){
  '
  input -> x (value), lambda (avg rate or mean of events), type
  output -> Probability
  computes: P(X=x), P(X!=x), P(X<x), P(X<=x), P(X>x), or P(X>=x)
  default is P(X<=x)
  '
  if (type == "="){
    P = dpois(x, lambda)
  }
  if (type == "!="){
    P = 1 - dpois(x, lambda)
  }
  if (type == "<"){
    P = ppois(x-1, lambda)
  }
  if (type ==  "<="){
    P = ppois(x, lambda)
  }
  if (type == ">"){
    P = 1 - ppois(x, lambda)
  }
  if (type == ">="){
    P = 1 - ppois(x-1, lambda)
  }
  return(P)
}


############################################################
#Question 2: Poisson probability function
############################################################
beta.prob = function(x, alpha, beta, type = "<="){
  '
  input -> x(value),  alpha, beta, type
  output -> Probability
  computes: P(X=x), P(X!=x), P(X<x), P(X<=x), P(X>x), or P(X>=x)
  default is P(X<=x)
  '
  if (type == "="){
    P = 0 #distribution is continuous
  }
  if (type == "!="){
    P = 1
  }
  if (type == "<"){
    P = pbeta(x, alpha, beta)
  }
  if (type ==  "<="){
    P = pbeta(x, alpha, beta)
  }
  if (type == ">"){
    P = 1 - pbeta(x, alpha, beta)
  }
  if (type == ">="){
    P = 1 - pbeta(x, alpha, beta)
  }
  return(P)
}  