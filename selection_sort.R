# selection sort
swap <- function(array, index1, index2){
    temp <- array[index1]
    array[index1] <- array[index2]
    array[index2] <- temp
    return(array)
}

# using swap
selection_sort <- function(array){
    n <- length(array)
    for(i in 1:n){
        min_ind <- which.min(array[i:n]) + i - 1
        array <- swap(array, i, min_ind)
    }
    return(array)
}

# with out swap function, this is a faster solution
selection_sort2 <- function(array){
    n <- length(array)
    sorted <- vector(length = n)
    for(i in 1:n){
        min_ind <- which.min(array)
        sorted[i] <- array[min_ind]
        array <- array[-min_ind]
    }
    return(sorted)
}