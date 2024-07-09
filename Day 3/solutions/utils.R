## some auxiliary functions

## simulate HPP in 1D
sim.HPP <- function (lambda, min, max) {
  m <- lambda * (max - min)
  N <- rpois(1, lambda = m)
  sort(runif(N, min = min, max = max))
}


## simulate NHPP with deterministic function in 1D
sim.NHPP <- function (lambda.fun, lambda_max , min, max ){
  sim.hpp = sim.HPP(lambda = lambda_max, min = min, max = max)
  acc.rej = rbinom(rep(1,length(sim.hpp)), rep(1,length(sim.hpp)),lambda.fun(sim.hpp)/lambda_max)
  sim.nhpp = sim.hpp[acc.rej==1]
  sim.nhpp
} 


## matern covariance function used to simulate a gaussian field
cMatern <- function(h, nu, kappa, prec) {
  ifelse(h > 0, (1/prec)*besselK(h * kappa, nu) * (h * kappa)^nu /
           (gamma(nu) * 2^(nu - 1)), 1)
}

## simulate LGCP in 1D
sim.LGCP = function(min, max){
  grid = 0.1
  x0=min; xf=max
  idx = seq(x0,xf,by=grid)
  n=length(idx)
  
  ## simulation
  mat_cov = cMatern(as.matrix(dist(idx)), nu= 2, kappa= 1, prec = 1)
  log_lambda = rmvnorm(1, mean = rep(1, n), sigma = mat_cov)
  exp_lambda = exp( log_lambda)
  
  ## simulate independent poisson process on grid (0.1)
  pp_grid = rpois(n-1, lambda = grid*exp_lambda )
  x = numeric()
  exp_lambda_x =numeric()
  
  for( i in 1:(n-1) ){
    # browser()
    if(pp_grid[i]>0 ){
      #print(pp_grid[i])
      m = runif(pp_grid[i], min = 0, max = grid)
    #  print(m)
      y = idx[i] + m
     # print(y)
      x = c(x,y)
      exp_lambda_x = c(exp_lambda_x, rep(exp_lambda[i], pp_grid[i]) )}
    
  }
  
  
  ## grid and data
  new_x = seq(x0 + grid/2 , xf  , by = grid )
  plot(new_x, exp_lambda[-n] ,xaxt = 'n',type='l', xlim=c(8,18), ylab='intensity', main='LGCP 1D', xlab='arrival times')
  axis(1,x, labels = F, lwd=1, col.ticks = 'red')
  
}


## dual mesh creation
book.mesh.dual <- function(mesh) { 
  if (mesh$manifold=='R2') {
    ce <- t(sapply(1:nrow(mesh$graph$tv), function(i)
      colMeans(mesh$loc[mesh$graph$tv[i, ], 1:2])))
    library(parallel)
    pls <- mclapply(1:mesh$n, function(i) {
      p <- unique(Reduce('rbind', lapply(1:3, function(k) {
        j <- which(mesh$graph$tv[,k]==i)
        if (length(j)>0) 
          return(rbind(ce[j, , drop=FALSE],
                       cbind(mesh$loc[mesh$graph$tv[j, k], 1] +
                               mesh$loc[mesh$graph$tv[j, c(2:3,1)[k]], 1], 
                             mesh$loc[mesh$graph$tv[j, k], 2] +
                               mesh$loc[mesh$graph$tv[j, c(2:3,1)[k]], 2])/2))
        else return(ce[j, , drop=FALSE])
      })))
      j1 <- which(mesh$segm$bnd$idx[,1]==i)
      j2 <- which(mesh$segm$bnd$idx[,2]==i)
      if ((length(j1)>0) | (length(j2)>0)) {
        p <- unique(rbind(mesh$loc[i, 1:2], p,
                          mesh$loc[mesh$segm$bnd$idx[j1, 1], 1:2]/2 +
                            mesh$loc[mesh$segm$bnd$idx[j1, 2], 1:2]/2, 
                          mesh$loc[mesh$segm$bnd$idx[j2, 1], 1:2]/2 +
                            mesh$loc[mesh$segm$bnd$idx[j2, 2], 1:2]/2))
        yy <- p[,2]-mean(p[,2])/2-mesh$loc[i, 2]/2
        xx <- p[,1]-mean(p[,1])/2-mesh$loc[i, 1]/2
      } else {
        yy <- p[,2]-mesh$loc[i, 2]
        xx <- p[,1]-mesh$loc[i, 1]
      }
      Polygon(p[order(atan2(yy,xx)), ])
    })
    return(SpatialPolygons(lapply(1:mesh$n, function(i)
      Polygons(list(pls[[i]]), i))))
  }
  else stop("It only works for R2!")
}

