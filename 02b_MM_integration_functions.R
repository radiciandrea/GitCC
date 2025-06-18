# ODE system with log+1 ----

#from MM_integration_functions

# Idea:
#   X = variabile ecologica non genativa di interesse
#   
#   dX/dt = f(X) edo definita non negativa
#   
#   Y = log(X+1) trasformata log +1 
#   X = exp(Y)-1 di conseguenza
#   
#   dY/dt = f(exp(Y)-1)/exp(Y) nuovo sistema

#origin

# this has the log of the adults
# https://stackoverflow.com/questions/47401678/solving-odes-only-positive-solutions
# https://stackoverflow.com/questions/41648878/replacing-negative-values-in-a-model-system-of-odes-with-zero

dfLog1 <- function(t, x, parms) {
  
  # initial conditions and parameters
  with(parms, { 
    
    logE1 = x[(1+nIDs*0):(1*nIDs)]
    logJ1 = x[(1+nIDs*1):(2*nIDs)]
    logI1 = x[(1+nIDs*2):(3*nIDs)]
    logA1 = x[(1+nIDs*3):(4*nIDs)]
    logEd1 = x[(1+nIDs*4):(5*nIDs)]
    
    #tN = t[1]-t_s+1 # time of numerical integration to index matrix
    tN = t[1]
    tH = 24*(t - tN) #should put t and not t[1]
    tasMax = tasMax[max(1,tN-1),]*(tH<tSr[tN, ]) + tasMax[tN,]*(tH>tSr[tN, ])
    tasMin = tasMin[tN, ]*(tH<14) + tasMin[min(tN+1, length(tasMin))]*(tH>14)
    
    tempH = ((tasMax+tasMin)/2 + (tasMax-tasMin)/2*cos(pi*(tH+10)/(10+tSr[tN, ])))*(tH<tSr[tN, ])+
      ((tasMax+tasMin)/2 - (tasMax-tasMin)/2*cos(pi*(tH-tSr[tN, ])/(14-tSr[tN, ])))*(tH>tSr[tN, ])*(tH<14)+
      ((tasMax+tasMin)/2 + (tasMax-tasMin)/2*cos(pi*(tH-14)/(10+tSr[tN, ])))*(tH>14)
    
    delta_J = 1/(83.85 - 4.89*tempH + 0.08*tempH^2) #juvenile development rate (in SI: 82.42 - 4.87*tempH + 0.08*tempH^ 2)
    delta_I = 1/(50.1 - 3.574*tempH + 0.069*tempH^2) #first pre blood mean rate
    mu_E = -log(0.955 * exp(-0.5*((tempH-18.8)/21.53)^6)) # egg mortality rate
    mu_J = -log(0.977 * exp(-0.5*((tempH-21.8)/16.6)^6)) # juvenile mortality rate
    beta = (33.2*exp(-0.5*((tempH-70.3)/14.1)^2)*(38.8 - tempH)^1.5)*(tempH<= 38.8) #fertility rate
    
    # ODE definition 
    dlogE1 = beta*(1-omega[t_n, ])*(exp(logA1)-1) - (h[t_n, ]*delta_E + mu_E)*(exp(logE1)-1)
    dlogJ1 = h[t_n, ]*(delta_E*(exp(logE1)-1) + sigma[t_n, ]*gamma*(exp(logEd1)-1)) - (delta_J + mu_J + (exp(logJ1)-1)/K[t_n, ])*(exp(logJ1)-1)  
    dlogI1 = 0.5*delta_J*(exp(logJ1)-1) - (delta_I + mu_A[t_n, ])*(exp(logI1)-1)
    dlogA1 = delta_I*(exp(logI1)-1) - mu_A[t_n, ]*(exp(logA1)-1)
    dlogEd1 = beta*omega[t_n, ]*(exp(logA1)-1) -  h[t_n, ]*sigma[t_n, ]*(exp(logE1)-1)
    
    dlogE1 = dlogE1/logE1
    dlogJ1 = dlogJ1/logJ1
    dlogI1 = dlogI1/logI1
    dlogA1 = dlogA1/logA1
    dlogEd1 = dlogEd1/logEd1
    
    dlog1x <- c(dlogE1, dlogJ1, dlogI1, dlogA1, dlogEd1)
    
    return(list(dlog1x))})
}