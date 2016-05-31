
postpro=distm(cbind(StayPoints2$lon,StayPoints2$lat))
th=50
col=NULL
row=NULL
h=1

for(x in 1:length(postpro))
{
  if(postpro[x]>0 & postpro[x]<th)
  {
    row[h]=as.integer(x/(sqrt(length(postpro))))+1
    col[h]=x%%sqrt(length(postpro))
    if(col[h]==0)
    {
      col[h]=29
      row[h]=row[h]-1
    }
    StayPoints2[row[h],]=StayPoints2[col[h],]
    h=h+1
  }
  # print(col,row)
  
}
