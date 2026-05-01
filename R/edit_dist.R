edit.dist = 
function (str) {
	t1 = c("D","L","R","O","W")
	t2 = strsplit(toupper(str),split="")[[1]]
	if(!length(t2)==5) return("Error: 'str' must be a character string of length 5")
	n1 = 5
	n2 = 5
	d = array(NA,dim=c(6,6))
	d[,1] = 0:5
	d[1,] = 0:5
  	for (i in 2:6) 
    	for (j in 2:6)
      	d[i,j] = ifelse(t1[i-1] == t2[j-1],min(d[i-1,j]+1, d[i,j-1]+1,d[i-1,j-1]),min(d[i-1,j]+1, d[i,j-1]+1,d[i-1,j-1]+1))
	return(d[6,6])
}