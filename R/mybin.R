#' myBinomial
#'
#'
#' @param iter How many iterations to run through
#' @param n Size of population
#' @param p Probability of a hit (p<=1)
#'
#' @returns A table of the probability of a cetrain amount of hits occuring
#' @export
#'
#'
#' @examples mybin(100,10,0.5)
mybin=function(iter=100,n=10, p=0.5){

  #Make a matrix size n
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  # A vector for every successful trial.
  succ=c()

  #runs for every value between 1 and iter
  for( i in 1:iter){
    #Fill each column with a sample of data
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate the sum of the sample, giving whether it was successful or not
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot based on the successes of the binom simulation
  graphics::barplot(succ.tab/(iter), col=grDevices::rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
