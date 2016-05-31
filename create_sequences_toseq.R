to.sequence<-function(table)
{
  i=1
  obs.lst <- list()
  k=1
  Sequences_test=NULL
  
  while(i<=nrow(table))
  {
    sequence=as.character(table[i,]$location)
    
    
    i=i+1
    while(table[i,]$prev_SpID!=-1 & i<=nrow(table))
    {
       sequence=c(sequence,table_user[i,]$location)
      # sequence=stringi::stri_join(sequence,as.character(table[i,]$location),sep=",")
      i=i+1
    }
    Sequences_test=as.data.frame(rbind(Sequences_test,sequence),stringsAsFactors=FALSE)
    
    obs.lst[[k]] <- sequence
    k=k+1
  }
   return (obs.lst)
 # return (Sequences_test)
}