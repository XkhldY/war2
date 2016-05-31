# transition.matrix<-function(states)
# {
#   
  for(q in 1:length(states))
    if(length(states[[q]])==1)
      states=states[-q]

  
  all=as.vector(unlist(states))
  
  transition=matrix(0,nrow = length(unique(all)),ncol = length(unique(all)))
  
  
  nams=unique(all)
  # loop to get the emission matrix
  for(i in 1:length(nams))
  {
    
    for(j in 1:length(nams))
    {
      allprop=length(all[all==nams[j]])
      prop=0
      for(k in 1:length(states))
      {
        sta=states[[k]]
        if(length(sta)>1)
        {
        for(m in 2:length(sta))
         { 
            if(sta[m]==nams[i] && sta[m-1]==nams[j])
            prop=prop+1
         }
        }
#         else
#         {
#           allprop=allprop-1
#         }
      }
      propapility= prop/allprop
      transition[i,j]=propapility
    }
  }
  
  
  # return(transition)
  
# }



