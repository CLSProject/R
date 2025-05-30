show.movie <- function(obj) {
  # now let us write our method
    cat("The name of the movie is", obj$name,".\n")
    cat(obj$leadActor, "is the lead actor.\n")
}



cat("S3Class\n")
# create a list with required components
    movieList <- list(name      = "Iron man",
                      leadActor = "Robert Downey Jr")
    # print(movieList)
    # show.movie(movieList)
# give a name to your class
    class(movieList) <- "movie"
    # print(movieList)
    # show.movie(movieList)
