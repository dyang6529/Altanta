sgd <- function(
    par,                       # parameter estimates
    X,                         # model matrix
    y,                         # target variable
    stepsize = 1e-2,           # the learning rate; suggest 1e-3 for non-adagrad methods
    type = 'adagrad',          # one of adagrad, rmsprop, adam or nadam
    average = FALSE,           # a variation of the approach
    ...                        # arguments to pass to an updating function, e.g. gamma in rmsprop
){
  
  # initialize
  beta = par
  names(beta) = colnames(X)
  betamat = matrix(0, nrow(X), ncol = length(beta))      # Collect all estimates
  v    = rep(0, length(beta))                    # gradient variance (sum of squares)
  m    = rep(0, length(beta))                    # average of gradients for n/adam
  eps  = 1e-8                                    # a smoothing term to avoid division by zero
  grad_old = rep(0, length(beta))
  
  update_ff <- function(type, ...) {
    
    # if stepsize_tau > 0, a check on the LR at early iterations
    adagrad <- function(grad, stepsize_tau = 0) {
      v <<- v + grad^2  
      
      stepsize/(stepsize_tau + sqrt(v + eps)) * grad
    }
    
    rmsprop <- function(grad, grad_old, gamma = .9) {
      v = gamma * grad_old^2 + (1 - gamma) * grad^2
      
      stepsize / sqrt(v + eps) * grad
    }
    
    adam <- function(grad, b1 = .9, b2 = .999) {
      m <<- b1 * m + (1 - b1) * grad
      v <<- b2 * v + (1 - b2) * grad^2
      
      if (type == 'adam')
        # dividing v and m by 1 - b*^i is the 'bias correction'
        stepsize/(sqrt(v / (1 - b2^i)) + eps) *  (m / (1 - b1^i))
      else 
        # nadam
        stepsize/(sqrt(v / (1 - b2^i)) + eps) *  (b1 * m  +  (1 - b1)/(1 - b1^i) * grad)
    }
    
    switch(
      type,
      adagrad = function(grad, ...) adagrad(grad, ...),
      rmsprop = function(grad, ...) rmsprop(grad, grad_old, ...),
      adam    = function(grad, ...) adam(grad, ...),
      nadam   = function(grad, ...) adam(grad, ...)
    )
  }
  
  update = update_ff(type, ...)
  
  for (i in 1:nrow(X)) {
    Xi   = X[i, , drop = FALSE]
    yi   = y[i]
    LP   = Xi %*% beta                           # matrix operations not necessary, 
    grad = t(Xi) %*% (LP - yi)                   # but makes consistent with standard gd func
    
    # update
    beta = beta - update(grad, ...)
    
    if (average & i > 1) {
      beta = beta - 1/i * (betamat[i - 1, ] - beta)   # a variation
    } 
    
    betamat[i,] = beta
    grad_old = grad
  }
  
  LP = X %*% beta
  lastloss = crossprod(LP - y)
  
  list(
    par = beta,                               # final estimates
    par_chain = betamat,                      # estimates at each iteration
    RMSE = sqrt(sum(lastloss)/nrow(X)),
    fitted = LP
  )
}