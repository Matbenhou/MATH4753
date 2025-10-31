test_that("multiplication works", {
  relist = myncurve(0,1,0)
  expect_equal(relist$mu[1], 0)
  expect_equal(relist$sigma[1], 1)

  prob = dnorm(0,mean=0,sd=1)-dnorm(0-3*1,mean=0,sd=1)
  prob = round(prob,4)
  expect_equal(relist$prob[1], prob)
})
