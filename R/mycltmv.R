#' myCLTmv
#'
#' @param n Size of each sample
#' @param iter The amount of samples to have
#' @param a The lower bound of the sampled uniform distribution
#' @param b The upper bound of the sampled uniform distribution
#'
#' @returns A vector containing the sample means represented by the histogram
#' @export
#'
#' @examples mycltmv(10, 10000, 0, 5)
mycltmv=function(n,iter,a=0,b=5){
  y=stats::runif(n*iter,a,b) # Creates a random sample of a uniform distribution from 0-5, with a sample size of n*iter
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)#Assigns the created sample to a matrix with dimensions n x iter, essentially giving us a matrix of iter number n-sized samples
  sm=apply(data,2,mean) # Applies the sum function to every sample in the data matrix, separating by column given the 2 parameter.
  h=graphics::hist(sm,plot=FALSE)
  graphics::hist(sm,col=grDevices::rainbow(length(h$mids)),freq=FALSE,main="Distribution of the mean of uniforms")
  graphics::curve(stats::dnorm(x,mean=(a+b)/2,sd=sqrt((b-a)^2/(12*n))),add=TRUE,lwd=2,col="Blue")
  sm
}
