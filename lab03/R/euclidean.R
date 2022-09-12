euclidean <-
function(a,b){
  if(a<b){
    exc=b
    b=a
    a=exc
  }
  while(b!=0){
    t=b
    b=a%%b
    a=t
  }
  return(a)
}
