emission.matrix<-function(states,observations)
{
  
  emission=matrix(0,nrow = length(unique(states))
                  ,ncol = length(unique(observations)))
  dt=data.frame(x=observations,y=states)
  k=unique(states)
  q=unique(observations)
  # loop to get the emission matrix
  for(i in 1:length(unique(states)))
    for(j in 1:length(unique(observations)))
    {
      emission[i,j]=nrow(dt[dt$x==q[j] & dt$y==k[i],])/nrow(dt[dt$y==k[i],])
    }
  return(emission)
  
}