## Part of this function creates a matrix and prepare it to make an inverse version of it in the other function

makeCacheMatrix <- function(x = matrix()) {
                    inv <- NULL
                    ValorDeMatrix<-function(y){
                            x<<- y
                            inv <<- NULL
                    }
                    
                    verMatrix <-function(){x}
                    hacerInverso <- function(inverse) {inv<<- inverse}
                    verInverso <- function(){inv}
                    list(ValorDeMatrix = ValorDeMatrix, verMatrix = verMatrix, hacerInverso = hacerInverso, verInverso = verInverso)
              
}


## This function returns a matrix that is the inverse of the original matrix ('x')

cacheSolve <- function(x, ...) {
                    inv <- x$verInverso()
                    if(!is.null(inv)){
                                message("the cache data is being processed") 
                    return(inv)
                    }
                    opr <- x$verMatrix()
                    inv <- solve(opr, ...)
                    x$hacerInverso(inv)
                    inv
}
