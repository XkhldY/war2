seq.matrix<-function(states)
{
  dt=data.frame(1,2)
  # dt=data.frame(x1=NA,x2=NA)
  b=1
  j=1
  for(i in 1:length(states))
  {
    if(length(states[[i]])>1)
      for(j in 1:length(states[[i]])-1)
      {
        # dt[b,]=c(states[[i]][j],states[[i]][j+1])
        dt=as.data.frame(rbind(dt,c(states[[i]][j],states[[i]][j+1])))
        # rbind(dt, do.call(rbind, as.list(c(states[[i]][j],states[[i]][j+1])))
  #       dt[b,1]=states[[i]][j]
  #       dt[b,2]=states[[i]][j+1]
        # b=b+1
      }
  }   
  m=seq(1,nrow(dt),2)
  dt1=dt[!row.names(dt)%in%m, ]
  return(dt1)
}