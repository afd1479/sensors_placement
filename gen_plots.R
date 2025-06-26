### This script plots the k-coverage and GDOP values 

library(ggplot2)
library(lattice, lib.loc = "/usr/lib/R/library")
library(RColorBrewer)
library(viridisLite)

get_kcov <- function(pt_i, sens, dc)
{
  B = (dc[[pt_i]])[sens,]
  B = B[!is.na(B[,1]),]
  
  if(length(dim(B))!=2) return(0)
  return(nrow(B))
}

kcov_vect <- function(sens,dc)
{
  kcov=mclapply(1:length(dc), FUN = function(pt_i) get_kcov(pt_i, sens, dc), mc.cores = 100, mc.allow.recursive = TRUE)
  kcov = unlist(kcov)
}


gen_pie <- function(g,n_parts=6)
{
  aa=hist(g, breaks=seq(0, g_inf , length.out = n_parts) , col="blue", xlab = "GDOP", main = "GDOP", 
          cex.lab=1.5, cex.axis=1.5, cex.sub=1.25)
  a=round(aa$breaks[-1])
  
  
  a1 = paste('GDOP < ', a[-length(a)],sep = '')
  labels= c(a1, paste('GDOP >= ', a[(length(a)-1)],   sep = ''))
  
  ll=labels[-c(1,length(labels))]
  bb=  paste(a[1:length(ll)], ' <=', sep = '')   
  labels[-c(1,length(labels))]=paste(bb,ll)
  # labels
  
  x=aa$counts
  
  pct <- round(x/sum(x)*100)
  # lbls <- paste(labels, pct) # add percents to labels
  lbls <- paste(pct,"%",sep='') # ad % to labels
  
  postscript("gdop_pie.eps", width = 450, height = 450)
  pie(x,labels = lbls, radius = 0.7, col=rainbow(length(lbls)),main="GDOP pie chart",cex.lab=1.5, cex.axis=1.5, cex.sub=1.25)
  legend("topright", labels, cex=1.0,fill=rainbow(length(x)))
  dev.off()
}


genplots <- function(kcov, g, m_l, m_p, m_h) 
{ 
  
  myh = hist(kcov,plot=FALSE,  breaks=seq(-0.05, 0.05+max(kcov), by=0.1))
  myh$density = myh$counts/sum(myh$counts)*100
  
  #postscript("k_Coverage_Percentage.eps", width = 480, height = 480)
  jpeg("k_Coverage_Percentage.jpg", width = 480, height = 480, quality = 100)
  plot(myh, xlab="k Coverage" , ylab="Percentage", ylim = c(0,5+ max(myh$density)), col="bisque", 
       freq=FALSE, main = "k Coverage Percentage",
       bty="l", cex.lab=1.5, cex.axis=1.5, cex.sub=1.25)
  dev.off()
  
  
  
  
  kcov_m = array(kcov, dim=c(m_l,m_p,m_h))
  
  sub = floor(seq(1, m_h, length.out = 3))    # show kcov at altitudes indexed by sub
  
  # print(sub)
  #postscript("k_Coverage_Heat.eps", width = 480, height = 480)
  jpeg("k_Coverage_Heat.jpg", width = 480, height = 480, quality = 100)
  par(mfrow=c(2,2),mar=c(5,5,5,5))
  image2D(z = kcov_m, subset = sub,
          #x = lambda, y = phi,
          x = phi, y = lambda,
          margin = c(1, 2), NAcol = "black", colkey = FALSE,
          xlab = "longitude", ylab = "latitude", contour = FALSE,
          cex.lab=1.5, cex.axis=1.5, cex.sub=1.25,
          main = paste("height ", round(h[sub],2), " m"),
          clim = c(0, max(kcov)), mfrow = c(2, 2))
  colkey(clim = c(0, max(kcov)), clab = c("k Coverage"))
  dev.off()
  
  
  # g = gdop_vect(sens)
  
  # Filled Density Plot
  #postscript("GDOP_Kernel_Density.eps", width = 480, height = 480)
  jpeg("GDOP_Kernel_Densit.jpg", width = 480, height = 480, quality = 100)
  hist(g, breaks=seq(0, g_inf , length.out = 10) , col="blue", xlab = "GDOP", main = "GDOP", 
       cex.lab=1.5, cex.axis=1.5, cex.sub=1.25)
  dev.off()
  
  g_m = array(g, dim=c(m_l,m_p,m_h))
  
  #postscript("GDOP_Heat.eps", width = 480, height = 480)
  jpeg("GDOP_Heat.jpg", width = 480, height = 480, quality = 100)
  par(mfrow=c(2,2),mar=c(5,4.5,5,4))
  image2D(z = g_m, subset = sub,
          #x = lambda, y = phi,
          x = phi, y = lambda,
          margin = c(1, 2), NAcol = "black", colkey = FALSE,
          xlab = "longitude", ylab = "latitude", contour = FALSE,
          cex.lab=1.5, cex.axis=1.5, cex.sub=1.25,
          main = paste("height ", round(h[sub],2), " m"),
          clim = c(0, g_inf), mfrow = c(2, 2))
  colkey(clim = c(0, g_inf), clab = c("GDOP"))
  dev.off()
  
  gen_pie(g)
}

###### select whoch list of sensors that are required to get the plot of. 
#sens=seq(1, 10, by=1) we should change the IDs of sensors list solution 
#sens=seq(1, 790, by=1)    # Set of already deployed sensors from OpenSky
## selected_dep
# s_selected=round(NSGA_result[2,4:18])  # Set of new sensors that are selected from the algorithm 
# s_selected=append(s_selected,s_dep)    # Set of new+deployed sensors 
# sens=as.numeric(s_selected)

# K-coverage and GDOP for sensors for the 1200 selected points from space 
sens=seq(1, 790, by=1) 
g = gdop_vect(sens,dc)
kcov = kcov_vect(sens,dc)
genplots(kcov, g, m_l, m_p, m_h) 


# K-coverage and GDOP for sensors for the 75 jamming attacks
#s_selected=as.numeric(round(NSGA_result[1,4:33]))  # if you would like to see how are the solution resistant to jamming attack.
#sens=s_selected
# g = gdop_vect(sens,dc_J)
# kcov = kcov_vect(sens,dc_J)
# # OR
# kcov=mclapply(1:length(dc_J), FUN = function(pt_i) get_kcov(pt_i, sens, dc_J), mc.cores = 100, mc.allow.recursive = TRUE)
# kcov = unlist(kcov)
# kcov_m = array(kcov, dim=c(m_l_J,m_p_J,3))
# genplots(kcov, g, m_l_J, m_p_J, m_h) 


# Plot the number of sensors that are effected by each jammer 
{
x <- seq(lambda_l, lambda_u, length.out = 75)
y <- seq(phi_l, phi_u, length.out = 75)

data <- data.frame(x=x,y=y,z=kcov)
data1 <- data.frame(x=x[1:25],y=y[1:25],z=kcov[1:25])
data2 <- data.frame(x=x[26:50],y=y[26:50],z=kcov[26:50])
data3 <- data.frame(x=x[51:75],y=y[51:75],z=kcov[51:75])

fig <- plot_ly() 
fig <- fig %>%  add_trace(data = data1,  x=data1$x, y=data1$y, z=data1$z, name = ' <b>100 m </b> ',type="scatter3d", mode="lines+markers" )  
fig <- fig %>% add_trace(data = data2,  x=data2$x, y=data2$y, z=data2$z, name = '<b>3000 m</b>', type="scatter3d", mode="lines+markers" )
fig <- fig %>% add_trace(data = data3,  x=data3$x, y=data3$y, z=data3$z, name = '<b>6000 m</b>', type="scatter3d", mode="lines+markers" )

fig <- fig %>%  layout(scene = list(xaxis = list(title = '<b>Longitude</b>')))
fig <- fig %>%  layout(scene = list(yaxis = list(title = '<b>Latitude</b>')))
fig <- fig %>%  layout(scene = list(zaxis = list(title = '<b>No. Sensors</b>')))
fig <- fig %>% layout(legend = list(x = 0.1, y = 0.9))
fig
}

#### To plot the k-coverage of Jammers 
# kcov=mclapply(1:length(dc_J), FUN = function(pt_i) get_kcov(pt_i, sens, dc_J), mc.cores = 100, mc.allow.recursive = TRUE)
# kcov = unlist(kcov)
# kcov_m = array(kcov, dim=c(m_l_J,m_p_J,3))
# x <- seq(lambda_l, lambda_u, length.out = 75)
# y <- seq(phi_l, phi_u, length.out = 75)
# data <-cbind(x,y,kcov)
# data <- expand.grid(X=x, Y=y)
# data$Z <- kcov
# coul <- viridis(100)
# #coul <- terrain.colors(100)
# 
# levelplot(Z ~ X*Y, data=data  ,xlab="Longitude",ylab= "Latitude",main="", col.regions = coul,region = TRUE)
# 
# kcov_m = array(kcov, dim=c(m_l_J,m_p_J,3))
# image2D(z = kcov_m, subset = sub,
#         x = lambda, y = phi, 
#         margin = c(1, 2), NAcol = "black", colkey = FALSE,
#         xlab = "longitude", ylab = "latitude", contour = FALSE,
#         cex.lab=1.5, cex.axis=1.5, cex.sub=1.25,
#         main = paste("height  m"),
#         clim = c(0, max(kcov)), mfrow = c(2, 2))
# colkey(clim = c(0, max(kcov)), clab = c("k Coverage"))
# 
# gen_pie(g)
# 
# dev.off()
# x <- seq(lambda_l, lambda_u, length.out = 75)
# y <- seq(phi_l, phi_u, length.out = 75)
# data <-cbind(x,y,kcov)
# data=(data[50:75,])
# 
# colnames(data) <- data.frame("x", "y", "z")
# #fig.n <- plot_ly(data=data[50:75,],x=~as.matrix(data[50:75,1]),y=~as.matrix(data[50:75,2]),z = ~as.matrix(data[50:75,3]))
# fig.n <- plot_ly(data=as.data.frame(data),x=~x,y=~y,z = ~as.matrix(data[1:25,3]))
# fig.n <- fig.n %>% add_surface()
# fig.n

