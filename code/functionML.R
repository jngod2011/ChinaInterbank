# The following function is used to compute the deviance of a model
# deviance loss function
# y should be 0/1
# phat are probabilities obtain by our algorithm 
# wht shrinks probs in phat towards .5 --- this helps avoid numerical problems don't use log(0)!
lossf = function(y,phat,wht=0.0000001) {
  if(is.factor(y)) y = as.numeric(y)-1
  phat = (1-wht)*phat + wht*.5
  py = ifelse(y==1, phat, 1-phat)
  return(-2*sum(log(py)))
}

# The following will get confucion matrix:
# deviance loss function
# y should be 0/1
# phat are probabilities obtain by our algorithm 
# thr is the cut off value - everything above thr is classified as 1
getConfusionMatrix = function(y,phat,thr=0.5) {
  if(is.factor(y)) y = as.numeric(y)-1
  yhat = ifelse(phat > thr, 1, 0)
  tb = table(predictions = yhat, 
             actual = y)  
  rownames(tb) = c("predict_0", "predict_1")
  return(tb)
}

# this function gives miss-classification rate:
# deviance loss function
# y should be 0/1
# phat are probabilities obtain by our algorithm 
# thr is the cut off value - everything above thr is classified as 1
lossMR = function(y,phat,thr=0.5) {
  if(is.factor(y)) y = as.numeric(y)-1
  yhat = ifelse(phat > thr, 1, 0)
  return(1 - mean(yhat == y))
}