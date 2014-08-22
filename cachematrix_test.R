source("./cachematrix.R")

util.matcomp <- function(x,y){
  identical(all.equal(x,y),TRUE)
}

#test <- list()

#returned_obj_has_accessor
test.1 <- function(){
  obj <- makeCacheMatrix()
  if(!is.list(obj)){
    return (FALSE)
  }
  all.equal(names(obj),c("get","set","getInv","setInv"))
}

# Id matrix
#test[[length(test)+1]] <- function(){
test.2 <- function(){
  obj1 <- makeCacheMatrix(diag(1))
  cacheSolve(obj1)
  if(obj1$get() != diag(1) || obj1$getInv() != diag(1)){
    return(FALSE)
  }
  obj2 <- makeCacheMatrix(diag(2))
  cacheSolve(obj2)
  if(obj2$get() != diag(2) || obj2$getInv() != diag(2)){
    return(FALSE)
  }
  
  obj3 <- makeCacheMatrix(diag(3))
  cacheSolve(obj3)
  if(obj3$get() != diag(3) || obj3$getInv() != diag(3)){
    return (FALSE)
  }
  
  obj4 <- makeCacheMatrix(diag(4))
  cacheSolve(obj4)
  if(obj4$get() != diag(4) || obj4$getInv() != diag(4)){
    return (FALSE)
  }
  
  obj5 <- makeCacheMatrix(diag(5))
  cacheSolve(obj5)
  if(obj5$get() != diag(5) || obj5$getInv() != diag(5)){
    return (FALSE)
  }
  
  obj6 <- makeCacheMatrix(diag(61))
  cacheSolve(obj6)
  if(obj6$get() != diag(61) || obj6$getInv() != diag(61)){
    return (FALSE)
  }
  return (TRUE)
}

# normal & inverse
#test[[length(test)+1]] <- function(){
test.3 <- function(){
  randM <- matrix(round(rnorm(25),10),5,5)
  obj <- makeCacheMatrix(randM)
  cacheSolve(obj)
  if(!util.matcomp(obj$get(), randM)){
    return (FALSE)
  }
  util.matcomp(obj$get() %*% obj$getInv(),diag(5))
}


# execute
for(i in 1:3){
  if(eval(parse(text=paste("test.",i,"()",sep="")))!= TRUE){
      print(paste("failed. No.",i))
  }
}

