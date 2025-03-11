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

