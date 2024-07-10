## TODO 1 
names(gorillas.extra)
par(mfrow=c(3,2))
plot(gorillas.extra[[1]], main = names(gorillas.extra)[[1]] );points(gorillas,pch=20,cex=0.5, col='green')
plot(gorillas.extra[[2]], main = names(gorillas.extra)[[2]] );points(gorillas,pch=20,cex=0.5, col='green')
plot(gorillas.extra[[3]], main = names(gorillas.extra)[[3]] );points(gorillas,pch=20,cex=0.5, col='green')
plot(gorillas.extra[[4]], main = names(gorillas.extra)[[4]] );points(gorillas,pch=20,cex=0.5, col='green')
plot(gorillas.extra[[5]], main = names(gorillas.extra)[[5]] );points(gorillas,pch=20,cex=0.5, col='green')
plot(gorillas.extra[[7]], main = names(gorillas.extra)[[7]] );points(gorillas,pch=20,cex=0.5, col='green')

## TODO 2: discussed in class


## TODO 3: 
x = seq(-1,1,length.out = 10)
my.fun.sim = function(x){
  lambda = exp(1+x)
  sim = rpois(length(x), lambda)
  return(sim)
}
n0= 3
x.sim = replicate(n0,my.fun.sim(x)) 
{plot(x,x.sim[,1], ylab='sim',pch=19,cex=1,ylim=c(min(x.sim), max(x.sim)+1),type='b')
  for(i in 1:(n0-1)){
    #browser()
    lines(x,x.sim[,i+1],col=i+1,pch=19,cex=1,type='b')
  }}


## TODO 4: done in class 

## TODO 5: done in class 

## TODO 6: 

hpp_2d = rpoispp(lambda_2, win= win )
region_interest_2d = owin(c(1,2.5),c(1,2.5))

{plot(hpp_2d$window,main= 'PP in 2D')
  plot(region_interest_2d,add=T)
  points(hpp_2d$x, hpp_2d$y, col='red',pch=20,cex=1)
  points(hpp_2d$x[which(inside.owin(hpp_2d$x, hpp_2d$y, region_interest_2d))], 
         hpp_2d$y[which(inside.owin(hpp_2d$x, hpp_2d$y, region_interest_2d))],col='blue', pch=20,cex=1)
  axis(1)
  axis(2)
}

points_in_region = function(region){
  hpp_2d = rpoispp(lambda_2, win= win )
  length(which(inside.owin(hpp_2d$x, hpp_2d$y, region)))
}
npoints_hpp_2d_region =  replicate(n_sim, points_in_region(region_interest_2d))
hist(npoints_hpp_2d_region, main=' ')
abline(v= mean(npoints_hpp_2d_region),col='red')
abline(v= area(region_interest_2d)*lambda_2,col='blue')
legend(x=15, y = n_sim/5,
       legend=c('average # of points',  'expected # points'), fill=c('red', 'blue' ), horiz=F)
print(mean(npoints_hpp_2d_region))


## TODO 7: done in class

### Takeaway from  HPP ?

#1.  The intensity $\lambda$ is constant. 

#2.  If we need the expected number of point in a region, we multiply the intensity by the area of that region. 

#3. If we need a  probability of certain event in a region, we use the fact that the number of events in that region is a Poisson random variable.  


## TODO 8 
n_sim = 100

npoints.nhpp_1d =  replicate(n_sim, length(sim.NHPP(lambda_nhpp, lambda_nhpp(13), min= S_1[1], max= S_1[2])) )

{hist(npoints.nhpp_1d, main = ' ' ) 
  abline(v=mean(npoints.nhpp_1d),col='red') 
  abline(v=expected.nhpp_1d$value,col='blue' )
  legend(x=40, y = n_sim/5,
         legend=c('average # of points',  'expected # points'), fill=c('red', 'blue' ), horiz=F)}
print(mean(npoints.nhpp_1d))


## TODO 9  done in class
