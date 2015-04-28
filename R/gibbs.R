library(assertthat)

rbern = function(p){
  rbinom(length(p), size = 1, prob = p)
}

crf_gibbs = function(eta, b, iterations, inverse_link = plogis, r = rbern){
  assert_that(
    isSymmetric(b), 
    nrow(b) == ncol(eta),
    all(diag(b) == 0)
  )
  
  y = inverse_link(eta)
  
  for(i in 1:iterations){
    for(species in 1:ncol(y)){
      y[ , species] = r(inverse_link(eta[ , species] + y %*% b[ , species]))
    }
  }
  
  y
}

nrow = 50
ncol = 7

b = matrix(-1, nrow = ncol(eta), ncol = ncol(eta))
diag(b) = 0

y = crf_gibbs(eta = eta + 2, b = b, iterations = 1000)

nc =-3
df = 1
curve(dt(x, df = df, ncp = nc, log = TRUE) - dt(nc, df = df, ncp = nc, log = TRUE), from = nc-5, to = nc+5)
