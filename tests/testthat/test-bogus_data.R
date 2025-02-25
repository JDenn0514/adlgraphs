testthat::test_that("check Distribution Channel", {
  bogus_dist <- data.frame(
    DistributionChannel = c("preview", "preview", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous"),
    x = rep(c(1,2), 5)
  )
  out <- remove_bogus(bogus_dist)
  out_dist <- out$DistributionChannel
  exp_dist <- c("anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous")
  testthat::expect_equal(out_dist, exp_dist)

  bogus_dist <- data.frame(
    distribution_channel = c("preview", "preview", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous"),
    x = rep(c(1,2), 5)
  )
  out <- remove_bogus(bogus_dist)
  out_dist <- out$distribution_channel
  exp_dist <- c("anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous", "anonymous")
  testthat::expect_equal(out_dist, exp_dist)
})

testthat::test_that("check Duration", {
  bogus <- data.frame(
    Duration = c(10, 20, 91, 97, 698, 460, 512, 435, 443, 598),
    x = rep(c(1,2), 5)
  )
  out <- remove_bogus(bogus, duration = "Duration")
  out <- out$Duration
  exp <- c(698, 460, 512, 435, 443, 598)
  testthat::expect_equal(out, exp)

  bogus <- data.frame(
    duration = c(10, 20, 91, 97, 698, 460, 512, 435, 443, 598),
    x = rep(c(1,2), 5)
  )
  out <- remove_bogus(bogus, duration = "duration")
  out <- out$duration
  exp <- c(698, 460, 512, 435, 443, 598)
  testthat::expect_equal(out, exp)
})

testthat::test_that("check age", {
  bogus <- data.frame(
    age = c(18, 19, 17, 25, NA, 45, 35, 80, 50, 70),
    x = rep(c(1,2), 5)
  )
  out <- remove_bogus(bogus)
  out <- out$age
  exp <- c(18, 19, 25, 45, 35, 80, 50, 70)
  testthat::expect_equal(out, exp)

  bogus <- data.frame(
    age = c(18, 19, 17, 25, NA, 45, 35, 80, 50, 70),
    x = rep(c(1,2), 5)
  )
  out <- remove_bogus(bogus)
  out <- out$age
  exp <- c(18, 19, 25, 45, 35, 80, 50, 70)
  testthat::expect_equal(out, exp)
})

testthat::test_that("check bots", {
  bogus <- data.frame(
    age = c(18, 19, 17, 25, NA, 45, 35, 80, 50, 70),
    x = rep(c(1,2), 5)
  )
  # check remove bogus
  out <- remove_bogus(bogus)
  out <- out$age
  exp <- c(18, 19, 25, 45, 35, 80, 50, 70)
  testthat::expect_equal(out, exp)

  bogus <- data.frame(
    age = c(18, 19, 17, 25, NA, 45, 35, 80, 50, 70),
    x = rep(c(1,2), 5)
  )
  out <- remove_bogus(bogus)
  out <- out$age
  exp <- c(18, 19, 25, 45, 35, 80, 50, 70)
  testthat::expect_equal(out, exp)
})

# write a test that checks for the class of finished


