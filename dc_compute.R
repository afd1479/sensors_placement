
#module load R/3.4.1
#cd ../../scratch/bdt2
#R --no-restore
setwd('/home/afd8/Alaa/sensor-placement')
#setwd("/cloud/project")

# install.packages('ggplot2')


#R --no-restore

require(data.table)
require(dplyr)
require(plot3D)
require(geosphere)
require(parallel)

### Function to create a mesh network of the selected area and generate set of points inside it. 
gen_p <- function(m_l,m_p,m_h)
{
  #m_l = 4*25; m_p = 3*25; m_h = 4;
  # discretization along the lambda, phi and height axis repectively
  
  M = mesh(lambda, phi, h) # creates a rectangular full 2-D or 3-D grid
  p = cbind(M$x, M$y, M$z) # combine vectors, matrices and/or data frames by columns.
  return(p)
}


### Functions generate set of sensors uniformly  ## On-Ground
gen_s <- function(m_l,m_p)
{
  n_t = m_l; n_m = m_p; n = n_t * n_m      # >> N     e.g. n_t^2 > 10 N
  theta = seq(lambda_l, lambda_u, length.out = n_t)
  mu = seq(phi_l, phi_u, length.out = n_m)
  
  SS = mesh(theta, mu,0)
  s = cbind(SS$x, SS$y, SS$z)
  
  return(s)
}


### Function to convert GEO coordinates to ECEF ones
geo2ecef <- function(v) # convert coordinates from GEO to ECEF format
{
  
  Rea = 6378137
  e = 0.08181919
  lon = pi*v[1]/180
  lat = pi*v[2]/180
  Ne = Rea/sqrt(1-(e^2) * (sin(lat))^2)
  x = (Ne + v[3]) * cos(lat) * cos(lon)
  y = (Ne + v[3]) * cos(lat) * sin(lon)
  z = (Ne * (1-e^2) + v[3]) * sin(lat)
  return(as.numeric(c(x,y,z)))
  
}

### 3-Functions below to get directional cosins of the selected points "p"
comp_dc_f <- function(vis,sen,pt) 
{
  if(vis>0)
  {
    # Rotation matrix R
    R1 = c(-sin(pt[2])*cos(pt[1]), -sin(pt[2])*sin(pt[1]),cos(pt[2]))
    R2 = c(-sin(pt[1]), cos(pt[1]),0)
    R3 = c(-cos(pt[2])*cos(pt[1]), -cos(pt[2])*sin(pt[1]), -sin(pt[2]))
    
    R = rbind(R1,R2,R3) #### rotation matrixR
    # IF "s" is deployed or generated use seconed line, but IF "s" is sata use first line. 
    if (pt[3]>500)
    dc_tmp = R %*% (geo2ecef(as.numeric(s[sen,]))-geo2ecef(pt)) ## R·(si−p) ## error in this line. I solved it by using as.numeric
    else
    dc_tmp = R %*% ((as.numeric(s[sen,]))-geo2ecef(pt)) ## R·(si−p) ## error in this line. I solved it by using as.numeric
    
    #dc_tmp = R %*% (geo2ecef((s[sen,]))-geo2ecef(pt)) 
    dc_tmp = c(dc_tmp/sqrt(sum(dc_tmp^2)))   #vector vi pointing from the aircraft to the ith sensor
    
    # print(dc_tmp)
    return(t(dc_tmp)) #covariance matrix
  }
  return(t(c(NA,NA,NA)))
}

comp_dc2 <- function(j,pt)
{
  t(c(j,comp_dc_f(1,j,pt)))
}

dir_cos3 <- function(los_p, pt) 
{
  
  res = data.frame(sen=1:length(los_p),vis=los_p) #vis=whole sensors that are within LOS of that point
  res = res[order(res$vis),]
  
  n_ze = sum(res$vis==0)
  
  res0= cbind(res$sen[res$vis==0], matrix(NA, nrow=n_ze, ncol=3, byrow=TRUE)) #set null for all cols
  
  vec=setdiff(res$sen,res$sen[res$vis==0])
  if(length(vec)==0) return(res0[,2:4])
  a = mclapply(vec, FUN = function(j) comp_dc2(j,pt), mc.cores = detectCores()-1, mc.allow.recursive = FALSE)
  a = matrix(unlist(a), nrow = length(vec), ncol = 4, byrow = TRUE)
  
  res = rbind(res0,a)
  res = res[order(res[,1]),]
  
  res[,2:4]
}


# Function to load of sensors files. 
load_s <- function(filename)
{
  s=(fread(filename, header = T, sep = ',', select=c( "latitude","longitude","height"))
     %>% filter(!abs(latitude+longitude)<0.0001))
 
}

# Function to load of space_sensors files. 
load_space_s <- function(filename)
{
  
  space_s=(fread(filename, header = T, sep = ',', select=c("serial","time", "longitude","latitude","height"))
     %>% filter(!abs(latitude+longitude)<0.0001))
}


### The dimenstiosn of the selected area. 
nn = 2
m_l = 2*nn*5; m_p = 2*nn*5; m_h = 3; m = m_l*m_p*m_h  ## number of generated sensors. Here  "nn"=2 means we need 400 sensors.

# lambdal latitude , phi_l: long 
# lambda_l = 47.4;  lambda_u = 51.4;  lambda = seq(lambda_l, lambda_u, length.out = m_l)
# phi_l = 5.71;     phi_u = 9.71;     phi = seq(phi_l, phi_u, length.out = m_p)
#h_l is the highet of aircraft
h_l = 600;     h_u = 12000;    h = seq(h_l, h_u, length.out = m_h)


## If we want to generate set of sensors 
# s = gen_s(m_l,m_p)
# s=as.data.frame(s)
# s=s[,c(2,1,3)]
s = load_s('sensors.csv')
s_ground = load_s('sensors.csv')
s_space=load_space_s('satellite_positions_2.csv')
s_space=s_space[,3-4]
s_space[,3]=s_space[,3]*1000
s=rbind(s_space,s_ground)
#space_s[,5]=space_s[,5]

## Three steps for mix dep+new LOS
dep_s=load_s('sensors.csv')
dep_s=as.matrix(dep_s)
s=rbind(s,dep_s)
s_bind=rbind(s,space_s[,3:5])
s=space_s[,2:4]
s=s_bind
space_s=space_s[,2:4]

### Combined Ground and Space sensors.
s_ground = load_s('sensors.csv')
s_space=load_space_s('satellite_positions_2.csv')
s_space=s_space[,3-4]
s_space[,3]=s_space[,3]*1000
s_ground=s_ground[,c(2,1,3)]
s=rbind(s_space,s_ground)

s=s_ground;
s=s_space;

#sens=s2;
# sens=round(GA@solution)
# s_2=s
# s=s[sens,]
s_backup=s
# Define the lat and long of the area that we are targetting
lambda_l = summary(s$latitude)[4]-nn; lambda_u = summary(s$latitude)[4]+nn; lambda = seq(lambda_l, lambda_u, length.out = m_l)
phi_l = summary(s$longitude)[4]-nn;   phi_u = summary(s$longitude)[4]+nn;   phi = seq(phi_l, phi_u, length.out = m_p)



p = gen_p(m_l,m_p,m_h)


get_dc <- function(j)
{
  
  ## Get all sensors that are within LOS of each point. 
  ### I disapplied the second line beacuse the lat and log anr switched. 
  #los <- distm(p[j,1:2],s[,1:2], fun = distHaversine) ## Get Haversine distance 
  #####los <- distm(p[j,2:1],s[,2:1], fun = distHaversine) ## Get Haversine distance 
  #####los <- as.integer(p[j,3] >= 0.058875 * (los/1000)^2) ### This line has to be considered when we are targeting satalites systtem
  #####dir_cos3(los, p[j,])
  
  
  # Assuming p[j,] is currently selected element of p
  # And assuming los calculation needs to be updated for each sensor in s
  
  # Initialize an empty vector to store los values for each sensor
  los_values <- numeric(length(s[[1]]))
  
  # Loop over each sensor in s
  for(i in 1:length(s[[1]])) {
    # Calculate the Haversine distance for the current sensor
    
    
    # Apply the condition based on the height of the current sensor
    ##*if (s[i,3] >= 500000) {
    if (s[i,3] >= 5000) {# This means its a sat receiver # We should chnage this condition if we deal with more sensors that has alt <1000 m
      # If sensor height >= 6000, use adjusted condition
      # Calculate the Haversine distance for the current sensor
      # current_los <- distm(p[j,2:1], s[i,1:2], fun = distHaversine)
      s[i,3]=s[i,3]/1000   # Convert altitude to KM 
      point <- as.data.frame(matrix(p[j, ], nrow = 1))
      names(point) <- c("latitude", "longitude", "height")
      sensor=s[i,]
      #*current_los <- sqrt((p[j,1] - s[i,1])^2 + (p[j,2] - s[i,2])^2 + (p[j,3] - s[i,3])^2)
      #*horizon_distance <- sqrt(2  earth_radius * s[i,3] + s[i,3]). ## there is a star
      #*los_values[i] <- current_los <= horizon_distance
      los_values[i] <- is_covered(sensor,point)
      #los_values[i] <- as.integer(p[j,3] <= 0.058875 * (current_los/1000)^2)
    } else {
      # If sensor height < 6000, use original condition
      # Calculate the Haversine distance for the current sensor
      current_los <- distm(p[j,2:1], s[i,1:2], fun = distHaversine)
      los_values[i] <- as.integer(p[j,3] >= 0.058875 * (current_los/1000)^2)
    }
  }
  dir_cos3(los_values, p[j,c(2,1,3)])
  # Now los_values contains the los calculation for each sensor
  # Depending on your next steps, you might aggregate these or use them individually
  
  # Proceed with dir_cos3 or any other operation, adjusting as necessary for your use case
  
}


## First way to get dc values
dc=NULL
dc=mclapply(1:m, FUN = function(j) get_dc(j), mc.cores = detectCores()-1, mc.allow.recursive = TRUE)













### Second way to generate dc values.
gen_dc_mcore <- function(p,s)
{
  dc=NULL
  m=nrow(p)
  pb = txtProgressBar(min = 0, max = m, style = 3)
  for(j in 1:m)  #   nrow(p)
  {
    #los <- distm(p[j,1:2],s[,1:2], fun = distHaversine)
    ####los <- distm(p[j,2:1],s[,2:1], fun = distHaversine)
    ####los <- as.integer(p[j,3] >= 0.058875 * (los/1000)^2) # I think there is a problem in this line!
    ####dc[[j]]=dir_cos3(los, p[j,]) 
    
    # Initialize an empty vector to store los values for each sensor
    los_values <- numeric(length(s[[1]]))
    
    # Loop over each sensor in s
    for(i in 1:length(s[[1]])) {
      # Calculate the Haversine distance for the current sensor
     
      
      # Apply the condition based on the height of the current sensor
      if (s[i,3] >= 500000) {
        current_los <- distm(p[j,2:1], s[i,1:2], fun = distHaversine)
        # If sensor height >= 6000, use adjusted condition
        los_values[i] <- as.integer(p[j,3] <= 0.058875 * (current_los/1000)^2)
      } else {
        current_los <- distm(p[j,2:1], s[i,1:2], fun = distHaversine)
        # If sensor height < 6000, use original condition
        los_values[i] <- as.integer(p[j,3] >= 0.058875 * (current_los/1000)^2)
      }
    }
    
    
    dc[[j]]=dir_cos3(los_values, p[j,c(2,1,3)]) 
    
    setTxtProgressBar(pb,j)
  }
  return(dc)
}


dc=NULL
### I did not use this code below 
NN=30
n = floor(m/NN)

for(j in 0:n)  #n
{
  vec = (NN*j+1):(NN*(j+1))
  if(j==n) vec = (NN*j+1):m
  
  v <- foreach(i = vec, .export = c('p','s','geo2ecef','dir_cos3','comp_dc2','comp_dc_f'),
               .packages = c('geosphere', 'parallel')) %dopar% {
                 
                 #los <- distm(p[i,1:2],s[,1:2], fun = distHaversine)
                 ####los <- distm(p[i,2:1],s[,2:1], fun = distHaversine)
                 ####los <- as.integer(p[i,3] >= 0.058875 * (los/1000)^2)   #This line has been changed from>= to <=
                 ####dc = dir_cos3(los, p[i,])
                 
                 # Initialize an empty vector to store los values for each sensor
                 los_values <- numeric(length(s[[1]]))
                 
                 # Loop over each sensor in s
                 for(f in 1:length(s[[1]])) {
                   # Calculate the Haversine distance for the current sensor
                   #current_los <- distm(p[j,2:1], s[[i]][2:1], fun = distHaversine)
                   
                   # Apply the condition based on the height of the current sensor
                   if (s[f,3] >= 500000) {
                     current_los <- distm(p[i,2:1], s[f,1:2], fun = distHaversine)
                     # If sensor height >= 6000, use adjusted condition
                     los_values[f] <- as.integer(p[i,3] <= 0.058875 * (current_los/1000)^2)
                   } else {
                     # If sensor height < 6000, use original condition
                     current_los <- distm(p[i,2:1], s[f,1:2], fun = distHaversine)
                     los_values[f] <- as.integer(p[i,3] >= 0.058875 * (current_los/1000)^2)
                   }
                 }
                 dc = dir_cos3(los_values, p[i, c(2,1,3)])
               }
  
  for (k in vec) {
    dc[[k]]=v[[k-NN*j]]
  }
  #print(j)
}
#head(dc[[1]])
# str(v)

dc <- array(v, dim = c(3,nrow(s),m))
list.save(dc, 'dclistm_l100m_p75m_h4n_t100n_m75.json')
dc1=list.load('dclistm_l100m_p75m_h4n_t100n_m75.json')

str(v)



