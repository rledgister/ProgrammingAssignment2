## These functions are written to maintain the inverse of a given matrix,
## allowing it to be generated once and then held in memory rather than recreated.

## This function memoizes a matrix object with simple functions to get or set its
## inverse. Here, in comparison to the example, I have used a little more
## descriptive naming for variables.

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(input){
        x <<- input
        invmat <<- NULL
    }
    get <- function () x
    setmat <- function (mat) invmat <<- mat
    getmat <- function () invmat
    list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## This function takes a memoized matrix and either returns its solved inverse
## or produces the inverse solution, while also storing that inverse in the
## given matrix object.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Reticulating Splines")
        return(inv)
    }
    dat <- x$get()
    x$setinv(solve(dat)) #Memoize this
    x$getinv()
}
