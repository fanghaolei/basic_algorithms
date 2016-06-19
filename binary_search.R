# Binary search
pool <- c(1:1000)
rand_num <- floor(runif(1, 1, 100))

binary_search <- function(array, num){
    min <- 1
    max <- length(array)
    guess <- floor((min+max)/2)
    if (array[guess] == num) {
        cat('Index:', guess, 'Value:', array[guess], 'is found!\n')
    } else if (array[guess] < num) {
        cat('Index:', guess, 'Value:', array[guess], 'is too small, continue...\n')
        min <- guess
        array <- array[min:max]
        guess <- binary_search(array, num)
    } else {
        cat('Index:', guess, 'Value:', array[guess], 'is too large, continue...\n')
        max <- guess
        array <- array[1:max]
        guess <- binary_search(array, num)
    }
}

binary_search(pool, rand_num)