# Insert sort
insert <- function(array, rightIndex, value){
    for(i in rightIndex:1){
        if(value <= array[i]){
            array[i+1] <- array[i]
        } else {
            array[i+1] <- value
            break
        }
        
        if(i == 1){
            array[i] <- value
        }
    }
    return(array)
}

insert_sort <- function(array){
    n <- length(array)
    for(i in 1:(n-1)){
        array <- insert(array, i, array[i+1])
    }
    return(array)
}