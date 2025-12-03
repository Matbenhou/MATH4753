#' nTickets
#'
#' @param N = Maximum capacity of the plane
#' @param gamma = The probability that the plane is truly overbooked
#' @param prob = The probability of someone not showing up to the plane
#'
#' @returns A named list of the discrete and continuous optimal numbers for the overbooking problem, along with the parameters of the function.
#' @export
#'
#' @examples ntickets(N=400,gamma = 0.02, p = 0.95)
ntickets <- function(N = 200, gamma = 0.02, prob = 0.95){

  normNTickets <-function(x)
  {
    -1*(stats::pnorm(N+0.5, x*prob, sqrt(x*prob*(1-prob))) -1 + gamma)
  }


  lower = as.integer(N*0.95)
  upper = as.integer(N*1.1)

  vec = c(lower:upper)

  ## Discrete Case

  obj <- -1*(stats::pbinom(N, vec, prob) -1 + gamma)
  ab = abs(obj)


  minIndex = which.min(ab)

  str = paste("N =", N, ", p =", prob, ", gamma =", gamma )
  discsub = paste(str, ", optimal point =", vec[minIndex])

  plot(x = vec, y = obj, type = "b", pch = 21,
       xlab = "n", ylab = "Objective", main = "Objective vs n to find optimal ticket sales",
       sub = discsub)
  graphics::lines(x = c(lower,upper), y = c(0,0), col = "Blue") #Zero Line

  graphics::lines(x = c(vec[minIndex], vec[minIndex]), y = c(-1, 1), col = "Red")
  graphics::lines(x = c(lower, upper), y = c(obj[minIndex], obj[minIndex]), col = "Red")



  ## Continuous Case

  #pnorm(N, vec*prob, sqrt(vec*prob*(1-prob))) - 1 + gamma


  root = (stats::uniroot(normNTickets, interval = c(lower,upper)))$root

  consub = paste(str, ", root =", root)

  graphics::curve(normNTickets(x),
        from = lower, to = upper,
        xlab = "n", ylab = "Objective", main = "Objective vs n to find optimal ticket sales",
        sub = consub)

  graphics::lines(x = c(lower,upper), y = c(0,0), col = "Blue") #Zero Line

  graphics::lines(x = c(root,root), y = c(-1, 1), col = "Red")
  graphics::points(root,normNTickets(root), pch = 21, bg = "Red", cex = 1)

  returnlist = list(nd= vec[minIndex],nc = root,N = N, prob = prob, gamma = gamma)

  returnlist
}
