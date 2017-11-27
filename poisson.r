sim.poisson=function(num,mean=600)
{
  dist=rpois(num,mean)
  dist.summary(dist)
  print (comp.ci(dist))
NULL
}
