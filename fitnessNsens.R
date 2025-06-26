
require(DEoptim)

require(install.packages("GA"))


g_inf = 75;
pareto = 0.2;
N_CORE = 20

get_gdop <- function(B)
{
  
  # B = dc[,sens,pt]
  # print(B)
  # Sys.sleep(5)
  # print(na.omit(t(B)))
  
  if(any(!is.na(B)))
  {
    # B=na.omit(t(B))
    B = B[!is.na(B[,1]),] ## Remove NAN values
    
    # print(length(dim(B)))
    
    if(length(dim(B))!=2) return(g_inf)
    
    l=length(B[,1])
    
    B = cbind(B, t(t(rep(1, l))))
    B=round(B,digits=4)
    B=B[!duplicated(B), ]
    
    # print(l)
    
    if(l<4) { return(g_inf)}
    
    else tryCatch(
      {
        g_value = sqrt(sum(diag(solve(t(B) %*% B))))
        
        return(min(g_value, g_inf))
      }, 
      error=function(e)
      {  },
      finally=
        {  }
    )
    
  }
  return(g_inf)
}

gdop_vect <- function(sens,dc)
{
  m = length(dc)
  # pts= t(t(c(1:m)))
  
  # gd = t(t(rep(g_inf,m)))
  # gd = apply(pts, 1, function(pt) gd[pt] = get_gdop(dc[,sens,pt])) 
  
  gd=mclapply(1:m, FUN = function(pt_i) get_gdop((dc[[pt_i]])[sens,]), mc.cores = N_CORE, mc.allow.recursive = FALSE)
  
  
  # gd = NULL
  # 
  # for(pt_i in 1:m)
  # {
  #   #print(pt_i)
  #   gd[[pt_i]] = get_gdop((dc[[pt_i]])[sens,])
  # }
  
  
  
  # print(head(gd,20))
  
  # gd[gd>g_inf]=g_inf
  
  # print(gd)
  return(as.numeric(unlist(gd)))
}

gdop_score <- function(sens,dc)
{
  m = length(dc)
  gd = gdop_vect(sens,dc)
  
  g_score = sqrt(sum((gd-g_hat)^2))/m   # Mean Squared Deviation (MSD)
  
  g_score_norm = (g_score)/(g_inf)
  return(g_score_norm)
  
}




fitnessNsen <- function(sens, dc, N=NULL, Nmax=NULL)
{
  #### 1. If we need to add the popsize to the already deployed ones
  #sens=rbind(sens,as.matrix(dep_s)) 
  #sens=append(sens,seq(1,421,1)) # if we need to test the existing sensors with new ones!
  #sens=append(sens,s_dep)
  
  
  # These two lines if we wish to test the genrated with the sat deployed.Generated 400 uniformally
   #sens=append(seq(401,480,1),sens)
   #N=N+80
   
   # These two lines if we wish to test the genrated with the sat deployed.Generated only land uniformally
   sens=append(seq(130,209,1),sens)
   N=N+80
  
  # These two lines if we wish to test deployed sat+ground abd generated. 
  #sens=append(seq(401,501,1),sens)
  #N=N+101
  
  sens = round(c(sens))
  sens = unique(sens)
  
  # print(length(sens))
  
  if(is.null(Nmax) & !is.null(N)) penalty = abs(1 - length(sens)/N)  # prefer solution with all distinct sensors
  else penalty = length(sens)/Nmax           # prefer solution with few sensors
  
  fitness = (1-pareto)*gdop_score(sens,dc) + pareto*0.5*penalty^2
  
  # print(fitness)
  return(fitness)
}










 




##########      GDOP Required     ################# 
g_hat = rep(0,m_l*m_p*m_h)    # uniform requirement I set it to zero


##########   End GDOP Required     ################# 

N = 100 ## number of sensors that are uniformaly selected 
sens=round(runif(N, 1, nrow(s)))
sens=round(runif(N, 1, 400))
# sens = 1: nrow(s)
# sens
sens = 1: 20
fitnessNsen(sens,dc, N)
fitnessNsen2(sens,dc, N)
# gdop_vect(sens,dc)



N_u = round(1.05*N)
N_u = N
GA <- ga(type = "real-valued",
         fitness = function(x) -fitnessNsen(x,dc,N),
         #fitness = function(x) -fitnessNsen(x,N,dc),
         lower = c(rep(1, N_u)), upper = c(rep(nrow(s), N_u)), popSize = 20,
         maxiter = 20
)
summary(GA)
GA <- ga(type = "real-valued",
         fitness = function(x) -fitnessNsen(x,dc,N),
         #fitness = function(x) -fitnessNsen(x,N,dc),
         lower = c(rep(1, N_u)), upper = c(rep(nrow(s), N_u)), popSize = 50, 
         maxiter = 20,optim = TRUE, parallel = TRUE
)

## For run only
# > plot(s$latitude,s$longitude)
# > solutionres1=s[c(9,14,10,15,11,7,5,16,19,13),]
# > points(solutionres1(,1),solutionres1(,2),col="green")
# Error in solutionres1(, 1) : could not find function "solutionres1"
# > points(solutionres1[,1],solutionres1[,2],col="green")
# Error in xy.coords(x, y) : 
#   (list) object cannot be coerced to type 'double'
# > View(solutionres1)
# > points(solutionres1$latitude[,1],solutionres1$longitude[,2],col="green")
# Error in solutionres1$latitude[, 1] : incorrect number of dimensions
# > points(solutionres1$latitude,solutionres1$longitude,col="green")
#End for run only

#solutionlist=round(GA@solution)

# solutionres=s[c(solutionlist),]
# gen_s
## Adjust the search closely outside the area
#  49 to 51, and 7.5 to 9.5 
# build s uniformly
# lambda_l = 49; lambda_u = 51
# phi_l = 7.5; phi_u = 9.5
# m_l= 3; m_p =4
# s = gen_s(m_l,m_p)
N_u = nrow(s)
fitnessNsen(sens,dc, Nmax=N_u)
outDEoptimNmin <- DEoptim(fn =  function(x) fitnessNsen(x, dc, Nmax=N_u), 
                          lower = c(rep(1, N_u)), upper = c(rep(N_u, N_u)), 
                          DEoptim.control(NP = 10*N_u, itermax = 20, F = 1.2, CR = 0.7,
                                          trace=FALSE  #,  parallelType=1, packages = c('parallel'), 
                                          # parVar = c('fitnessNsen','N','pareto','gdop_score','dc','gdop_vect','g_inf','g_hat')
                          ))
summary(outDEoptimNmin)






digits = max(5, getOption('digits') - 2)
sens_i_min = round(outDEoptimNmin$optim$bestmem, digits)
sens_i_min = unique(round(sens_i_min))

length(sens_i_min)


fitnessNsen(sens_i_min,N,dc)




# sens_i100
# ***** summary of DEoptim object *****
#   best member   :  63.49384 128.5276 557.6103 453.3143 13.56434 401.4577 200.4466 417.3831 55.90221 201.3992 404.8957 637.3574 235.2645 256.3328 217.7151 545.1717 368.9412 230.7477 20.67752 658.649 447.5775 355.8061 29.99352 162.7361 439.3486 629.0903 521.2226 433.6472 101.3088 122.6072 325.2613 71.84005
# best value    :  0.25343
# after         :  100 generations
# fn evaluated  :  202 times
# *************************************


# outDEoptimNi500
# ***** summary of DEoptim object *****
#   best member   :  67.05203 382.9098 299.7337 61.25604 431.5755 13.87737 573.2435 627.4416 222.2132 422.6404 123.0652 121.6049 498.5549 194.1194 369.64 121.8622 375.1637 257.5634 63.32945 487.2392 632.6934 356.4569 447.5008 358.2852 256.7573 46.82026 401.574 20.69248 304.7555 119.2857 177.284 639.8972
# best value    :  0.23218
# after         :  500 generations
# fn evaluated  :  1002 times
# *************************************


# > summary(outDEoptimNi1000)
# 
# ***** summary of DEoptim object *****
#   best member   :  301.4879 566.7397 33.1951 375.4702 404.9422 315.034 93.23397 497.779 21.29672 617.2794 282.1837 196.4406 447.7386 5.35994 304.5512 303.4848 17.00849 640.2042 357.6021 121.8619 700.8392 62.74945 486.6347 25.65649 400.7571 156.6227 130.2119 554.0915 401.7292 297.2249 591.7008 299.5623
# best value    :  0.2264
# after         :  1000 generations
# fn evaluated  :  2002 times
# *************************************




get_gdop_sub <- function(x)
{
  tryCatch( 
    {
      gval = sqrt(sum(diag(solve(t(B[x,]) %*% B[x,]))))
      # print(gval)
      return(gval)
    }, 
    error=function(e)
    { },
    finally=
      { }
  )
}


get_gdop_old <- function(B)
{
  # B = dc[,sens,pt]
  # print(B)
  # Sys.sleep(5)
  # print(na.omit(t(B)))
  
  if(any(!is.na(B)))
  {
    
    # print(B)
    
    # B=na.omit(t(B))
    B = B[!is.na(B[,1]),]
    
    l=length(B[,1])
    
    B = cbind(B, t(t(rep(1, l))))
    B=round(B,digits=4)
    B=B[!duplicated(B), ]
    
    # print(l)
    
    if(l<4) { return(g_inf)}
    
    if(l>4) {  #  NOT EXECUTED
      ##### subsets
      
      # print('Good') 
      
      subs = t(combn(1:l, 4))
      
      n_gs= length(subs[,1])
      
      # pos = t(t(1:n_gs))
      # print(pos)
      # pos_subs=cbind(pos,subs)
      
      # print(pos_subs)
      
      # print(B[subs[,1],])
      
      gs=mclapply(1:n_gs, FUN = function(i) get_gdop_sub(subs[i,]), mc.cores = 60, mc.allow.recursive = FALSE)
      
      # print(min(rapply(list(gs), min, how = "unlist")))
      # print(min(unlist(gs)))
      # return(min(rapply(list(gs), min, how = "unlist")))
      return(min(unlist(gs)))
    }
    else tryCatch(
      {
        g_value = sqrt(sum(diag(solve(t(B) %*% B))))
        
        return(g_value)
      }, 
      error=function(e)
      { },
      finally=
        { }
    )
    
  }
  return(g_inf)
}



# require(data.table)
# 
# filename = 'sensors_10.csv'
# sensors = fread(filename) #, select=2:5)
# 
# # hist(sensors$longitude)
# # hist(sensors$latitude)
# 
# sensors = sensors[ latitude < 50 & latitude > 49 & longitude < 8 & longitude > 7]  
# fwrite(sensors, file='sensors_21.csv',append = F)


#sens2=as.numeric(solutionlist)

####### My plotting area 
NSGA_var <- NSGA$objectives %>% 
  as.data.frame() %>% 
  bind_cols(as.data.frame(NSGA$parameters)) %>% 
  filter(NSGA$paretoFrontRank == 1) %>% 
  mutate_all(.funs = function(x){round(x,6)}) %>%
  distinct() %>%  
  rename("f1" = V1, "f2" = V2)


plot(unlist(NSGA$objectives[,1]))

# plots Pareto fronts for the hp/mpg values of mtcars
show_front <- function(pref) {
  plot(df[,1], df[,2])
  sky <- psel(df[,1:2], pref)
  plot_front(df, pref, col = rgb(0, 0, 1))
  points(sky[,1], sky[,2], lwd = 3)
}

# do this for all four combinations of Pareto compositions
show_front(low(df$V1)  * low(df$V2))

plot(NSGA_10$objectives, main="Fitness Function Pareto Front of 2 objs with 10 sensors", 
     xlab="Obj.Fun.1", ylab="Obj.Fun.2")

plot(NSGA_100$objectives, main="Fitness Function Pareto Front of 2 objs with 100 sensors", 
     xlab="Obj.Fun.1", ylab="Obj.Fun.2")

plot(NSGA_10$objectives, main="Fitness Function Pareto Front of 2 objs with 10 sensors", 
     xlab="Obj.Fun.1", ylab="Obj.Fun.2")

