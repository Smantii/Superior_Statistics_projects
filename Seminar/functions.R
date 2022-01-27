library(geometry)
sigmoid <- function(z){
  g <- 1/(1+exp(-z))
  return(g)
}

NLL<- function(x, y, w){
  m <- nrow(x)
  g <- sigmoid(x%*%w)
  f = -(1/m)*sum(y*log(g) + (1-y)*log(1 - g))
  return(f)
}

NLL_grad <- function(x, y, w){
  mu =sigmoid(x%*%w)
  m = nrow(x)
  return((1/m)*t(x)%*%(mu - y))
}

NLL_hess <- function(x, y, w){
  mu =sigmoid(x%*%w)
  m = nrow(x)
  S <-(1/m)*diag(mu*(1-mu))
  return(t(x)%*%S%*%x)
}

L2_reg <- function(lambda, w){
  return(-2*lambda*w)
}

L1_reg <- function(lambda, w){
  return(-2*lambda*w)
}

initializer <- function(n){
  return(runif(n, min = -0.2, max = 0.2))
} 

gradientDescent <- function(train_x, train_y, valid_x, valid_y, eta, alpha, lambda, Reg, epochs){
  error_tr = rep(0, epochs)
  error_vl = rep(0, epochs)
  w = initializer(ncol(x))
  w_prec  = w
  for(k in 1:epochs){
    w = w - eta*NLL_grad(train_x, train_y, w) + alpha*(w - w_prec) + Reg(lambda, w)
    w_prec = w
    error_tr[k] = NLL(train_x,train_y,w)
    error_vl[k] = NLL(valid_x, valid_y, w)
    print(sprintf("Training error: %f || Validation error: %f", error_tr[k], error_vl[k]))  
    }
    plot(error_tr, type = "l", col = "blue")
    lines(error_vl, col = "red")
  return(list("Coeff" = w, "Training" = error_tr, "Valid" = error_vl))
}

newton <- function(x,y,w, epochs){
  w = initializer(57)
  error = rep(0, epochs)
  for(k in 1:epochs){
    g = NLL_grad(x,y,w)
    h = NLL_hess(x,y,w)
    d = solve(h, -g)
    w = w + d
    error[k] = NLL(x,y,w)
    print(error[k])
  }
  plot(error)
}

crossValid <- function(train_x, train_y,eta, alpha, lambda, Reg, epochs, K){
  n = floor(nrow(train_x)/K)
  r = list("Coeff" = c(), "Training" = rep(500, nrow(train_x - n)), "Valid" = rep(500, n))
  tr_mean = 0
  vl_mean = 0
for(k in 1:K){
  print(sprintf("%f fold su %f iniziato", k, K))
  id_valid = c(((k-1)*n + 1) : (k*n))
  training_x = train_x[-id_valid,]
  training_y = train_y[-id_valid]
  valid_x = train_x[id_valid,]
  valid_y = train_y[id_valid]
  r = gradientDescent(training_x, training_y, valid_x, valid_y, eta, alpha, lambda, Reg, epochs)
  tr_mean = tr_mean + tail(r$Training, n=1)
  vl_mean = vl_mean + tail(r$Valid, n=1)
  print("Finito!!")
}
  return(list("Mean training error" = tr_mean/K, "Mean validation error" = vl_mean/K))
  
}



#Newton animation
library(animation)
newton.method(FUN = function(x) -3*x^2 - 4, 
              init = 10, 
              rg = c(-1, 10), 
              tol = 0.001, 
              interact = FALSE, 
              col.lp = c("blue", "red", "red"))








