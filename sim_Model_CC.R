cat("model{
  ## likelihood -----------------------------------------------------------------------------------------
  
 Freqobs[1:J] ~ dmulti(p[1:J],N)

 for(j in 1:J){
 

   p[j] <- pi[1]*prod(cp1[j, 1:K]^y[j,2:(K+1)] * (1-cp1[j, 1:K])^(1-y[j,2:(K+1)])) +
            (1-pi[1])*prod(cp0[j, 1:K]^y[j,2:(K+1)] * (1-cp0[j, 1:K])^(1-y[j,2:(K+1)]))


    
    #Modeling pr(y_j = + | y_j-1 ... y_1, D=+)
    cp1[j,1]  =  phi( inprod( y[j,c(1:1)], beta[1:1,1] ) )
    cp1[j,2]  =  phi( inprod( y[j,c(1:1)], beta[1:1,2] ) )
    cp1[j,3]  =  phi( inprod( y[j,c(1,3)], beta[1:2,3] ) )
    cp1[j,4]  =  phi( inprod( y[j,c(1:1)], beta[1:1,4] ) ) 
    cp1[j,5]  =  phi( inprod( y[j,c(1,5)], beta[1:2,5] ) )

    

    #Modeling pr(y_j = + | y_j-1 ... y_1, D=-)
    cp0[j,1]  =  phi( inprod( y[j,c(1:1)], alpha[1:1,1] ) )
    cp0[j,2]  =  phi( inprod( y[j,c(1:2)], alpha[1:2,2] ) )
    cp0[j,3]  =  phi( inprod( y[j,c(1:3)], alpha[1:3,3] ) ) 
    cp0[j,4]  =  phi( inprod( y[j,c(1:1)], alpha[1:1,4] ) )  
    cp0[j,5]  =  phi( inprod( y[j,c(1:1)], alpha[1:1,5] ) )

    
    


#prob of TB
pD[j] =  phi(  a_prev[1] )
    
    }

#Probability of disease by group
  pi[1] = phi( a_prev[1] )

  
 
# Estimation of sensitivity & specificity
 # se[1,1] = mean( cp1[,1] )
 # se[2,1] = mean( cp1[,2] )
 # se[3,1] = mean( cp1[,3] ) * se[2,1] +
 #           mean( cp1[,3] ) * (1 - se[2,1] )
 # se[4,1] = mean( cp1[,4] )
 # se[5,1] = mean( cp1[,5] ) * se[4,1] +
 #           mean( cp1[,5] ) * (1 - se[4,1] )
 # 
 # sp[1,1] = 1 - mean( cp0[,1] )
 # sp[2,1] = 1 - ( mean( cp0[,2] ) * ( 1 - sp[1,1] ) +
 #                 mean( cp0[,2] ) * sp[1,1] )
 # sp[3,1] = 1 - ( mean( cp0[,3] ) * ( 1 - sp[2,1] ) +
 #                 mean( cp0[,3] ) *  sp[2,1] )
 # sp[4,1] = 1 - mean( cp0[,4] )
 # sp[5,1] = 1 - mean( cp0[,5] )
 
 for(j in 1:dim_D){

    s[j,1]  =  phi( inprod( D[j,c(1)],           beta[1:1,1] ) )
    s[j,2]  =  phi( inprod( D[j,c(1)],           beta[1:1,2] ) )
    s[j,3]  =  phi( inprod( D[j,c(1,3)],         beta[1:2,3] ) )
    s[j,4]  =  phi( inprod( D[j,c(1)],           beta[1:1,4] ) )
    s[j,5]  =  phi( inprod( D[j,c(1,5)],         beta[1:2,5] ) )



      #Modeling pr(y_j = + | y_j-1 ... y_1, D=-)
    c[j,1]  =  phi( inprod( D[j,c(1)],             alpha[1:1,1] ) )
    c[j,2]  =  phi( inprod( D[j,c(1,2)],           alpha[1:2,2] ) )
    c[j,3]  =  phi( inprod( D[j,c(1,2,3)],         alpha[1:3,3] ) )
    c[j,4]  =  phi( inprod( D[j,c(1)],             alpha[1:1,4] ) )
    c[j,5]  =  phi( inprod( D[j,c(1)],             alpha[1:1,5] ) )

    
    
    
  for( k in 1:K){
   ps[j,k] = s[j,k]*D[j,(k+1)] + (1 - s[j,k])*(1 - D[j,(k+1)])
   pc[j,k] = c[j,k]*D[j,(k+1)] + (1 - c[j,k])*(1 - D[j,(k+1)])
    }
    
    jp1[j,1] = ps[j,1]
    jp1[j,2] = ps[j,2]
    jp1[j,3] = prod( ps[j,2:3] )
    jp1[j,4] = ps[j,4]
    jp1[j,5] = prod( ps[j,4:5] )

    
    
    jp0[j,1] = pc[j,1]
    jp0[j,2] = prod( pc[j,1:2] )
    jp0[j,3] = prod( pc[j,1:3] )
    jp0[j,4] = pc[j,4]
    jp0[j,5] = pc[j,5]


}

    se[1,1] = sum( jp1[1:1,1] )
    se[2,1] = sum( jp1[1:1,2] )
    se[3,1] = sum( jp1[c(1,3),3] )
    se[4,1] = sum( jp1[1:1,4] )
    se[5,1] = sum( jp1[c(1,9),5] )

    
    
    sp[1,1] = 1 - sum( jp0[1:1,1] )
    sp[2,1] = 1 - sum( jp0[1:2,2] )
    sp[3,1] = 1 - sum( jp0[1:4,3] )
    sp[4,1] = 1 - sum( jp0[1:1,4] )
    sp[5,1] = 1 - sum( jp0[1:1,5] )



  ## priors -------------------------------------------------------------
  
# TRUE POSITIVE CASES
  beta[1,1] ~ dnorm(0, 1)

  beta[1,2] ~ dnorm(0, 1)

  beta[1,3] ~ dnorm(0, 1)
  beta[2,3] ~ dnorm(0, 1)

  beta[1,4] ~ dnorm(0, 1)


  beta[1,5] ~ dnorm(0, 1)
  beta[2,5] ~ dnorm(0, 1)



    
#TRUE NEGATIVE CASES
  alpha[1,1] ~ dnorm(0, .1)

  alpha[1,2] ~ dnorm(0, .1)
  alpha[2,2] ~ dnorm(0, .1)

  
  alpha[1,3] ~ dnorm(0, .1)
  alpha[2,3] ~ dnorm(0, .1)
  alpha[3,3] ~ dnorm(0, .1)

  
  alpha[1,4] ~ dnorm(-3, 10)
  alpha[1,5] ~ dnorm(-3, 10)



  # Prior for prevalence
  a_prev[1] ~ dnorm( -3, 5 )


}", file = "sim_model1_CC.txt")
#=============================================================================