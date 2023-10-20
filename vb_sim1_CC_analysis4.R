
#===========================================================================
#
#
# Analysis of simulated data - complete case analysis
#
#===========================================================================

# setwd("C:\\Users\\aketer\\OneDrive - ITG\\EDCTP\\PhD\\lca\\Vukuzazi\\revised analysis\\vb_sim_analysis")

#Point in the working directory
project.path = "/kyukon/scratch/gent/438/vsc43892/vb_sim_analysis"
# project.path = "C:\\Users\\aketer\\OneDrive - ITG\\EDCTP\\PhD\\lca\\Vukuzazi\\revised analysis\\vb_sim_analysis"


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





#Read the data
#Source the dataset
# source("data_for_sim0_sim1_CC.R")
# sim1_dat = readRDS(file.path(project.path = getwd(),"sim1_dat_cc1.RData"))
load(file.path(project.path = getwd(),"sim_dat_Final1.RData"))

M = 25
j = 75

source("data_for_sim1_CC.R")
# sim1_dat = readRDS(file.path(project.path = getwd(),"sim1_dat.RData"))




source("sim_Model_CC.R")




set.seed(79538)
jags.inits.1 <- function( ) {
  
  return( list( beta = rep(rnorm(0,10),3), alpha = rep(rnorm(0,10),3), a_prev = rep(rnorm(0,10),3),
                .RNG.name = 'lecuyer::RngStream',
                .RNG.seed = round( 1e+06 * runif( 1 ) ) ) )
  
}

# Multi-threaded run
# Find out how many cores are available (if you don't already know)
n = detectCores()
# n
# Find out how many cores are being used
# getDoParWorkers()
# n = M*3
cl <- makeCluster(n, retry = 5L, sleep = 30.0 ) # change to makeCluster( n ) to use n processes
registerDoParallel( cl )

# M=100

set.seed( 74123 )

time.1 <- system.time( #getDoParWorkers( )
  vb.sim1.CC.res76_100 <- foreach( m = 1:M, .inorder = FALSE, 
                                  .packages = c('R2jags','doParallel'), 
                                  .multicombine = TRUE) %dopar% {
                                    load.module( 'lecuyer' )
                                    print(paste("Fitting model",m, sep = ""))
                                    
                                    result = jags.parallel(data = sim1_dat_cc1[[m]], inits = jags.inits.1,
                                                           parameters.to.save = c("beta","alpha","a_prev","se","sp","pi"),
                                                           model.file = 'sim_model1_CC.txt', n.chain = 3, 
                                                           n.iter = 50000, n.burnin = 25000,
                                                           n.thin = 10, DIC = T, jags.module = "dic", n.cluster = 3)
                                    return( result )
                                  }
)
print(time.1)



stopCluster(cl)



saveRDS(vb.sim1.CC.res76_100, file = "/kyukon/data/gent/438/vsc43892/vb_sim_analysis/vb.sim1.CC.res76_100.RData")
# saveRDS(vb.sim1.CC.res1_100, file = "C:\\Users\\aketer\\OneDrive - ITG\\EDCTP\\PhD\\lca\\Vukuzazi\\revised analysis\\vb_sim_analysis\\vb.sim1.CC.res1_100.RData")


print( vb.sim1.CC.res76_100, intervals = c(0.5, 0.025, 0.975))
