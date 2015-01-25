## Author :Sudhanshu Badoni
## The functions are used to compute the Matrix inversion of a invertible Matrix. 
## Test Inputs : cacheSolve(matrix(c(4,3,3,2),2,2)) , cacheSolve(matrix(1:4,2,2))
## cacheSolve(matrix(c(2,2,3,2),2,2))
##

## makeCacheMatrix function provides the list of function to set and get the inverse
## of invertible Matrix.

makeCacheMatrix<-function(y= matrix()){
  jl<-matrix();
  
  set<-function(y){
    j<<-y
  }
  
  get<-function(){
    if(exists("j")){
      j;
    }
    else{
      jl;
    }
  
  }
  setinvs<-function(y){
    si<<- solve(y)
  }
  
  getinvs<-function() si;
  
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}

## cacheSolve function returns the matrix that is inverse of mtz 


cacheSolve<-function(mtz=matrix()){
  makeCacheMatrix();
  gtMt<-makeCacheMatrix()$get(); 
  
  ## Check if the inverse of matrix is already generated.
  ##If not generate the inverse.
  
  if(identical(mtz,gtMt)==FALSE){
    print("New Matrix")
    makeCacheMatrix()$set(mtz);
    makeCacheMatrix()$setinvs(mtz);
  }
  
  makeCacheMatrix()$getinvs(); 
  
}
