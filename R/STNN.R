STNN<-function(data,lag, weight0, weight1,hs, h){
  jj<-nrow(data)
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)
  scaled1 <- as.data.frame(scale(data, center = mins,
                                 scale = maxs - mins))
  input<-scaled1[1:(jj-lag),]# input lag
  output<-scaled1[(lag+1):jj,] # output data
  kk1=as.matrix(weight0)
  kk2=as.matrix(weight1)
  zz=as.matrix(input)
  input1=zz %*%kk1
  input2=zz%*%kk2
  inputrevised=cbind(input1,input2)
  modelfit = nnet(inputrevised, output, size=hs)
  xtab <- predict(modelfit,inputrevised)
  pr.nn<-xtab*(maxs-mins)+mins # fitted values
  jj1<-nrow(inputrevised)
  jj2=ncol(output)
  prd <- matrix(NA, nrow= h, ncol=jj2)
  x_iind <- inputrevised[jj1,]
  for (i in 1:h){
    prd[i,] <- predict(modelfit,x_iind )
    x_iind <- prd[i,]
  }
  pr.nn1<-prd*(maxs-mins)+mins
  return(list("Model Summary"=modelfit, "Fitted values"=pr.nn,
              "Forecasted values"=pr.nn1))
}
