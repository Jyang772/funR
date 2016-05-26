
#Create hilbert curve

#To create an animated .gif:
#saveGIF(trace.animate(h),interval=0.2,movie.name="curve.gif")

library(ggplot2)
library(reshape2)
library(animation)
library(dplyr)

#hilbert(m=matrix(1),n=1,r=2)
#n=2 == 4x4 grid
#positional matching
hilbert <- function(n,m=matrix(1),r=2,draw=TRUE,gif=FALSE) {
  for (i in 1:n)
  {
    tmp=cbind(t(m), m+nrow(m)^2)
    m=rbind(tmp, (2*nrow(m))^r-tmp[nrow(m):1,]+1)
  }
	h <- melt(m) %>% plyr::rename(c("Var1" = "x", "Var2" = "y", "value"="order")) %>% arrange(order)

	if(draw == TRUE) 
		draw.curve(h,nrow(h))
	if(gif==TRUE)
		#trace.animate(h) #redraw rate may cause epilepsy lol...
		saveGIF(trace.animate(h),interval=0.1,movie.name="curve.gif")
	h
 }


draw.curve <- function(a,cutoff) {
	print(ggplot(a[attr(a,"row.names")<=cutoff+1,],aes(x,y)) + geom_path() + xlim(1,max(a$x)) + ylim(1,max(a$y)))
}

trace.animate <- function(b) {
	lapply(seq(0,nrow(b)),function(i) {
		draw.curve(b,i)
	})
}

