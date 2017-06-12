#' Calculate Multiple R-Squared Effect Sizes of an Amelia Object (Multiple Imputation)
#' @param data A data frame with missing data, e.g. <my.data.frame>
#' @param num.imp A number representing how many imputations the embeded Amelia function will create, e.g. <20>
#' @param dv.list A list of characters to be treated as dependent variables in the Amelia object , e.g. <c("dv1")>, (this package currently only supports a single DV)
#' @param num.dv A number representing how many dependent variables are in your desired model, e.g. <1>, (currently restircted to 1, but it is included for future version in which multivariate regression will be supported)
#' @param iv.list A list of characters to be treated as independent variables in the Amelia object, e.g. <c("iv1","iv2","iv3"), (must match names of variables from the data.frame in the 'data' argument of this function)
#' @param num.iv A number representing how many independent variables in your desired model, e.g. <3>
#' @param num.obs A number representing how many observations are in the data set, e.g. <420>, (must match the number of rows in the data.frame in the 'data' argument of this function)
#' @param idvars A list of characters to be treated as id variables by the embedded Amelia function, e.g. <c("id","year")
#' @return Spits out a number that represents the Multiple R-Squared effect size of your imputed data set.
#' @export

mi.mult.r.sqr<-function(data=data,num.imp=num.imp,dv.list=c(NULL),num.dv=num.dv,iv.list=c(NULL),num.iv=num.iv,num.obs=num.obs,idvars=c(NULL))
{
  require(Amelia)
  #STEP 1: CREATE THE DESIRED MODEL WITH A MATRIX
  
  #1a: Create a matrix of the dv(s) being used
  dv.mat<-matrix(nrow=num.obs,ncol=num.dv,dimnames=list(c(),c(dv.list)))
  i<-x<-n<-1#list vars
  p<-q<-1#mat vars[p,q]
  used.dv<-c(NULL)#take note of the dv(s) being used
  while(x<=length(dv.list))
  {
    if(names(data)[i]==dv.list[x])
    {
      used.dv<-c(used.dv,i)
      while(n<attributes(dv.mat)$dim[1])
      {
        dv.mat[p,q]<-data[p,i]
        p<-p+1
        n<-n+1
      }
      i<-1
      x<-x+1
      n<-1
      p<-1
      q<-q+1
    }else{i<-i+1}
  }
  
  #1b: Create a matrix of the iv(s) being used
  iv.mat<-matrix(nrow=num.obs,ncol=num.iv,dimnames=list(c(),c(iv.list)))
  i<-x<-n<-1#list vars
  p<-q<-1#mat vars[p,q]
  used.iv<-c(NULL)#take note of the iv(s) being used
  
  while(x<=length(iv.list))
  {
    if(names(data)[i]==iv.list[x])
    {
      used.iv<-c(used.iv,i)
      while(n<attributes(iv.mat)$dim[1])
      {
        iv.mat[p,q]<-data[p,i]
        p<-p+1
        n<-n+1
      }
      i<-1
      x<-x+1
      n<-1
      p<-1
      q<-q+1
    }else{i<-i+1}
  }
  
  #1c: create the model formula
  model.formula<-dv.mat~iv.mat
  
  #STEP 2: STANDARD AMELIA MULTIPLE IMPUTATION PROCEDURE
  #2a: initialize
  m<-num.imp #tell Amelia how mnay imputations you want
  a.obj<-amelia(data, m=m,idvars=idvars) #create the amelia object
  b.out<-NULL #create a null object for b regression coefficients
  se.out<-NULL #create a null object for se of b
  n<-1
  
  #2b: impute
  for(n in 1:m)
  {
    model<-lm(model.formula,data=a.obj$imputations[n])
    b.out<-rbind(b.out, model$coef)
    se.out<-rbind(se.out, coef(summary(model))[,2])
  }
  
  #2c: meld the model, which uses Rubins rules
  model.meld<-mi.meld(q = b.out, se = se.out)
  
  #STEP 3: CREATE NEW ARRAYS WITH THE IMPUTED VALUES
  
  #3a: array for the dv(s)
  j<-k<-1#array vars [,j,k]
  ar.dv<-array(NaN,c(num.obs,num.dv,num.imp))
  
  while(k<=num.imp)
  {
    while(j<=num.dv)
    {
      ar.dv[,j,k]<-a.obj$imputations[[k]][,used.dv[j]]
      j<-j+1
    }
    k<-k+1
    j<-1
  }
  
  #3b: array for the iv(s)
  j<-k<-1#array vars [,j,k]
  ar.iv<-array(NaN,c(num.obs,num.iv,num.imp))
  
  while(k<=num.imp)
  {
    while(j<=num.iv)
    {
      ar.iv[,j,k]<-a.obj$imputations[[k]][,used.iv[j]]
      j<-j+1
    }
    k<-k+1
    j<-1
  }
  
  #STEP 4: CREATE ARRAY WITH MEAN OF IMPUTED VALUES ACROSS IMPUTATIONS
  #4a: array for dv(s)
  i<-j<-k<-1#array vars [i,j,k]
  mean.dv<-0
  means.vect.dv<-c(NULL)
  means.mat.dv<-matrix(NaN,nrow=num.obs,ncol=num.dv)
  
  while(j<=num.dv)
  {
    while(i<=num.obs)
    {
      while(k<=num.imp)
      {
        means.vect.dv<-c(means.vect.dv,ar.dv[i,j,k])
        k<-k+1
      }
      mean.dv<-mean(means.vect.dv)
      means.mat.dv[i,j]<-mean.dv
      k<-1
      i<-i+1
      means.vect.dv<-c(NULL)
    }
    k<-1
    i<-1
    j<-j+1
  }
  
  #4b: array for iv(s)
  i<-j<-k<-1#array vars [i,j,k]
  mean.iv<-0
  means.vect.iv<-c(NULL)
  means.mat.iv<-matrix(NaN,nrow=num.obs,ncol=num.iv)
  
  while(j<=num.iv)
  {
    while(i<=num.obs)
    {
      while(k<=num.imp)
      {
        means.vect.iv<-c(means.vect.iv,ar.iv[i,j,k])
        k<-k+1
      }
      mean.iv<-mean(means.vect.iv)
      means.mat.iv[i,j]<-mean.iv
      k<-1
      i<-i+1
      means.vect.iv<-c(NULL)
    }
    k<-1
    i<-1
    j<-j+1
  }
  
  #STEP 5: CALCUALTE Y-HAT
  i<-j<-1#mat vars [i,j]
  x.by.b.vect<-c(NULL)
  yhat.vect<-c(NULL)
  while(i<=num.obs)
  {
    while(j<=num.iv)
    {
      x.by.b<-(model.meld$q.mi[j+1]*means.mat.iv[i,j])
      x.by.b.vect<-c(x.by.b.vect,x.by.b)
      j<-j+1
    }
    yhat<-model.meld$q.mi[1]+(sum(x.by.b.vect))
    yhat.vect<-c(yhat.vect,yhat)
    x.by.b.vect<-c(NULL)
    j<-1
    i<-i+1
  }
  
  #STEP 6: CALCUALTE SUM-OF-SQUARES-Y AND SUM-OF-SQUARES-Y-HAT
  
  #6a: sum-of-squares y
  mean.y<-mean(means.mat.dv[,1])
  sqr.dev.y.vect<-c(NULL)
  n<-1
  
  while(n<=num.obs)
  {
    sqr.dev.y<-(means.mat.dv[n]-mean.y)^2
    sqr.dev.y.vect<-c(sqr.dev.y.vect,sqr.dev.y)
    n<-n+1
  }
  
  sos.y<-sum(sqr.dev.y.vect)
  
  #6b: SOS y-hat
  mean.yhat<-mean(yhat.vect)
  sqr.dev.yhat.vect<-c(NULL)
  n<-1
  
  while(n<=num.obs)
  {
    sqr.dev.yhat<-(yhat.vect[n]-mean.yhat)^2
    sqr.dev.yhat.vect<-c(sqr.dev.yhat.vect,sqr.dev.yhat)
    n<-n+1
  }
  
  sos.yhat<-sum(sqr.dev.yhat.vect)
  
  #STEP 7: CALCUALTE R^2
  mult.r.sqr.mi<-sos.yhat/sos.y
  #STEP 8: CALCUALTE VARIOUS MULTIPLE R-SQUARED ADJUSTMENTS
  adj.r.sqr.type<-c("UNDER CONSTRUCTION")
  #END: OUTPUT THE EFFECT SIZE MEASURE
  return(mult.r.sqr.mi)
}
