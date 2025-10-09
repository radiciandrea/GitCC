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

dfLogSEInv <- function(t, x, parms) {
  
  # initial conditions and parameters
  with(parms, { 
    
    logE1 = x[(1+nIDs*0):(1*nIDs)]
    logJ1 = x[(1+nIDs*1):(2*nIDs)]
    logI1 = x[(1+nIDs*2):(3*nIDs)]
    logAS1 = x[(1+nIDs*3):(4*nIDs)]
    logEd1 = x[(1+nIDs*4):(5*nIDs)]
    logAE1 = x[(1+nIDs*5):(6*nIDs)]
    logAI1= x[(1+nIDs*6):(7*nIDs)]
    logSH1= x[(1+nIDs*7):(8*nIDs)]
    
    
    #tN = t[1]-t_s+1 # time of numerical integration to index matrix
    tN = t[1]
    tH = 24*(t - tN) #SHould put t and not t[1]
    tasMax = tasMax[min(365,tN-1),]*(tH<tSr[tN, ]) + tasMax[tN,]*(tH>tSr[tN, ])
    tasMin = tasMin[tN, ]*(tH<14) + tasMin[min(tN+1, length(tasMin))]*(tH>14)
    
    tempH = ((tasMax+tasMin)/2 + (tasMax-tasMin)/2*cos(pi*(tH+10)/(10+tSr[tN, ])))*(tH<tSr[tN, ])+
      ((tasMax+tasMin)/2 - (tasMax-tasMin)/2*cos(pi*(tH-tSr[tN, ])/(14-tSr[tN, ])))*(tH>tSr[tN, ])*(tH<14)+
      ((tasMax+tasMin)/2 + (tasMax-tasMin)/2*cos(pi*(tH-14)/(10+tSr[tN, ])))*(tH>14)
    
    deltaJ = 1/(83.85 - 4.89*tempH + 0.08*tempH^2) #juvenile development rate (in SI: 82.42 - 4.87*tempH + 0.08*tempH^ 2)
    deltaI = 1/(50.1 - 3.574*tempH + 0.069*tempH^2) #first pre blood mean rate
    muE = -log(0.955 * exp(-0.5*((tempH-18.8)/21.53)^6)) # egg mortality rate
    muJ = -log(0.977 * exp(-0.5*((tempH-21.8)/16.6)^6)) # juvenile mortality rate
    beta = (33.2*exp(-0.5*((tempH-70.3)/14.1)^2)*(38.8 - tempH)^1.5)*(tempH<= 38.8) #fertility rate
    
    # supporting variable (with check for positivity)
    E1 = pmax(0, exp(logE1)-1)
    J1 = pmax(0, exp(logJ1)-1)
    I1 = pmax(0, exp(logI1)-1)
    AS1 = pmax(0, exp(logAS1)-1)
    Ed1 = pmax(0, exp(logEd1)-1)
    AE1 = pmax(0, exp(logAE1)-1)
    AI1 = pmax(0, exp(logAI1)-1)
    SH1 = pmax(0, exp(logSH1)-1)
    
    # ODE definition 
    dlogE1 = -beta*(1-omega[tN, ])*AS1 + (h[tN, ]*deltaE + muE)*E1
    dlogJ1 = -h[tN, ]*(deltaE*E1 + sigma[tN, ]*gamma*Ed1) + (deltaJ + muJ + J1/K[tN, ])*J1  
    dlogI1 = -0.5*deltaJ*J1 + (deltaI + muA[tN, ])*I1
    dlogEd1 = -beta*omega[tN, ]*AS1 +  h[tN, ]*sigma[tN, ]*Ed1
    
    # # with SEI
    # dlogAS1 = -deltaI*I1  + muA[tN, ]*AS1
    # dlogAE1 = 0
    # dlogAI1 =  0
    # 
    # #and S loss 
    # dlogSH1 = 0
    
    # # with SEI
    dlogAS1 = -deltaI*I1  + (muA[tN, ] + A[tN, ]*phiA[tN, ]*bH2v*iCm[tN, ])*AS1
    dlogAE1 = -A[tN, ]*phiA[tN, ]*bH2v*iCm[tN, ]*AS1 + (muA[tN, ] - ni[tN, ])*AE1
    dlogAI1 =  -ni[tN, ]*AE1 + muA[tN, ]*AI1

    #and S loss
    dlogSH1 = +A[tN, ]*bv2H*deltaM*phiA[tN, ]*AI1*SH1/(SH0/10^4)
    
    # and complete transformation
    dlogE1 = dlogE1/(E1+1)
    dlogJ1 = dlogJ1/(J1+1)
    dlogI1 = dlogI1/(I1+1)
    dlogAS1 = dlogAS1/(AS1+1)
    dlogEd1 = dlogEd1/(Ed1+1)
    dlogAE1 = dlogAE1/(AE1+1)
    dlogAI1 = dlogAI1/(AI1+1)
    dlogSH1 = dlogSH1/(SH1+1)
    
    dlog1x <- c(dlogE1, dlogJ1, dlogI1, dlogAS1, dlogEd1, dlogAE1, dlogAI1, dlogSH1)
    
    # cat("UPDATE from deSolve\nTime:", tN, "\n")
    
    return(list(dlog1x))})
}
