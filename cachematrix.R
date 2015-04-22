## Put comments here that give an overall description of what your
## functions do

##This function get a square matrix in input, and return its determinant.
##For square-matrix more than 2x2, it recursive call itself to calculate 
##detrminant ad inverted matrix
##The output is a list with detrminant and inverted matrix, in this order
invertMatrix<-function(myMatrix)
{
  x<-myMatrix
  cofactorsMatrix<-myMatrix
  cofactorsTransposedMatrix<-myMatrix
  myRow<-1
  determinant<-0
  base<--1
  exponent<-0
  sign<-0
  complement<-0
  cofactor<-0
  
  if (nrow(x) == 2)
  {
    return((x[1,1]*x[2,2]) - (x[1,2]*x[2,1]))
  }
  else
  {
    while (myRow<=nrow(x))
    {
      y<-matrix(x[row(x)!=myRow],nrow=nrow(x)-1,ncol=ncol(x))
      
      myColumn<-1
      
      while (myColumn<=ncol(x))
      {
        element<-x[myRow,myColumn]
        
        z<-matrix(y[col(y)!=myColumn],nrow=nrow(y),ncol=ncol(y)-1)
        
        exponent<-myRow+myColumn
        sign<-(base^exponent)
        
        complement<-invertMatrix(z)[[1]]
        cofactor<-sign * complement
        
        cofactorsMatrix[myRow,myColumn]<-cofactor
        
        if(myRow==1)
        {
          determinant<-determinant + (element * cofactor)
        }
        
        myColumn<-myColumn+1
      }
      
      myRow<-myRow+1
    }
  }
  
  i<-2
  vect<-c(cofactorsMatrix[,1])
  while(i<=ncol(cofactorsMatrix))
  {
    vect<-c(vect,cofactorsMatrix[,i])
    i<-i+1
  }
  
  cofactorsTransposedMatrix<-matrix(vect,nrow=ncol(cofactorsMatrix),ncol=nrow(cofactorsMatrix),byrow=TRUE)
  
  invertMatrix<-cofactorsTransposedMatrix * (1/determinant)
  
  result<-list(determinant,invertMatrix)
  
  return(result)
}

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(invertMatrix) m <<- invertMatrix
  
  getmatrix <- function() m
  
  list(set = set
       , get = get
       , setmatrix = setmatrix
       , getmatrix = getmatrix
  )
  
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  data <- x$get()
  if( !is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  m <- invertMatrix(data, ...)
  x$setmatrix(m)
  m[[2]]
}
