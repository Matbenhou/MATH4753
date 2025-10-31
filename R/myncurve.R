#' myncurve
#'
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of the normal distribution
#' @param a X coordination you want to find P(Y<=a)
#'
#' @returns a curve of the normal distribution and the area whose probability you are finding
#' @export
#'
#' @examples myncurve(0,1,0)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +3*sigma))

  xcurve = seq(mu-3*sigma,a,length=1000)
  ycurve =dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Purple")

  prob=dnorm(a,mean=mu,sd=sigma)-dnorm(mu-3*sigma,mean=mu,sd=sigma)
  prob=round(prob,4)
  text(x = a, y = (dnorm(a,mean=mu,sd=sigma)+1)/3, paste("Area = ", prob, sep=""))

  list(mu = mu, sigma = sigma, prob = prob)
}
