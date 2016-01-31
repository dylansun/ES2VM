library(rgl)
scatterplot3d(profit)
x = matrix(0,nrow = 9, ncol = 9)
y = x

for(i in 1:9){
  x[i,] = i;
}
for(j in 1:9){
  y[,j] = j;
}

profit.r = as.vector(profit)
x.r = as.vector(x)
y.r = as.vector(y)

jet.colors <- colorRampPalette( c("blue", "green") ) 
pal <- jet.colors(100)
col.ind <- cut(profit.r,100) # colour indices of each point
persp3d(x,y,profit.r,col=pal[col.ind])
x = seq(0.1, 0.9, 0.1)
y = seq(0.1,0.9, 0.1)
