#===========================================================================
#
#
# Analysis of simulated data - complete case analysis
#
#===========================================================================


# rm(list=ls())



#Load the packages
# install.packages("doSNOW", repos = 'http://R-Forge.R-project.org')
dynamic.require <- function( package ) {
  
  if ( eval( parse( text = paste( 'require(', package, ')' ) ) ) ) {
    
    return( 'done' )
    
  }
  
  install.packages( package )
  
  return( eval( parse( text = paste( 'require(', package, ')' ) ) ) )
  
}

dynamic.require( 'doParallel' )

dynamic.require( 'rjags' )

dynamic.require( 'random' )



#Design Matrix

K = 5   #Number of tests
D5 = matrix(NA, nrow = 2^K, ncol = K+1)

D5[,1] = 1
D5[,2] = rep(c(1,0), 2^K/2)
D5[,3] = rep(c(rep(1,2), rep(0,2)),2^K/4)
D5[,4] = rep(c(rep(1,4), rep(0,4)),2^K/8)
D5[,5] = rep(c(rep(1,8), rep(0,8)),2^K/16)
D5[,6] = rep(c(rep(1,16), rep(0,16)),2^K/32)



#load simulated data
# load(
  # file = "C:\\Users\\Keter\\OneDrive - ITG\\EDCTP\\PhD\\lca\\Vukuzazi\\revised analysis\\vb_sim_analysis\\sim_dat_Final1.RData")

# M=100
sim1_dat_cc1 <- foreach(m=c(1:M)+j, .inorder = FALSE) %do% {
  
  
  
  
  #Data for the model
  
  dat.tested = as.data.frame(sim_dat_Final1[,,m])
  #First create a variable with 1s 
  ones = rep(1, length.out = dim(dat.tested)[1])
  
  var.nm  <- c("Y1", "Y2", "Y4",  "Y50", "Y60")
  
  # dat.tested[,"Y50"] <- ifelse( dat.tested[,"Y50"] %in% 9, 0, dat.tested[,"Y50"])
  # dat.tested[,"Y60"] <- ifelse( dat.tested[,"Y60"] %in% 9, 0, dat.tested[,"Y60"])
  
  pattern1 <- aggregate( ones ~   Y1 + Y2  + Y4 + Y50 + Y60, data = dat.tested, 
                         subset = (dat.tested[,"Y50"] %in% c(0,1) & dat.tested[,"Y60"] %in% c(0,1) ),function(x)NROW(x))
  
  names(pattern1) = c(var.nm,"Freqobs")
  pattern1$ones = rep(1, dim(pattern1)[1])
  jags.data = pattern1[,c("ones",var.nm,"Freqobs")]
  
  
  #Prepare the collapsed data as a list; this is important for jags to run
  jags.sim.data_cc1 = list( N = sum(jags.data[,"Freqobs"]),
                           Freqobs = jags.data[,"Freqobs"],
                           y = jags.data[,c(1:(dim(jags.data)[2]-1))],
                           J = dim(jags.data)[1],
                           K = 5,
                           D = D5,
                           dim_D = dim(D5)[1])
  #Assign names to the objects in the list
  names(jags.sim.data_cc1) <- c("N","Freqobs","y","J","K","D","dim_D")
  
  
  
  
  return( jags.sim.data_cc1 )
}






# saveRDS(sim1_dat_cc1, file = "C:\\Users\\Keter\\OneDrive - ITG\\EDCTP\\PhD\\lca\\Vukuzazi\\revised analysis\\vb_sim_analysis\\sim1_dat_cc1.RData")


