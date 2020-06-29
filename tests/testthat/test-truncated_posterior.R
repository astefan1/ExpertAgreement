#### Sanity check for truncated posterior distributions ####

context("Truncated posterior")

test_that("Height of symmetric posterior is twice as high at zero if domain is half", {
  post.notrunc <- posterior_t_trunc(0, a=-Inf, b=Inf, t=0, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1)
  post.truncpos <- posterior_t_trunc(0, a=0, b=Inf, t=0, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1)
  post.truncneg <- posterior_t_trunc(0, a=-Inf, b=0, t=0, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1)
  expect_equal(post.truncpos, post.truncneg)
  expect_equal(post.notrunc*2, post.truncpos)
  expect_equal(post.notrunc*2, post.truncneg)

  post.notrunc.norm <- posterior_normal_trunc(0, a=-Inf, b=Inf, t=0, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1)
  post.truncpos.norm <- posterior_normal_trunc(0, a=0, b=Inf, t=0, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1)
  post.truncneg.norm <- posterior_normal_trunc(0, a=-Inf, b=0, t=0, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1)
  expect_equal(post.truncpos.norm, post.truncneg.norm)
  expect_equal(post.notrunc.norm*2, post.truncpos.norm)
  expect_equal(post.notrunc.norm*2, post.truncneg.norm)
}
)

test_that("Truncated posterior integrates to 1", {
  post.notrunc.t <- integrate(posterior_t_trunc, a=-Inf, b=Inf, t=1, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1, lower=-Inf, upper=Inf)$value
  post.truncpos.t <- integrate(posterior_t_trunc, a=0, b=Inf, t=1, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1, lower=0, upper=Inf)$value
  post.truncneg.t <- integrate(posterior_t_trunc, a=-Inf, b=0, t=1, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1, lower=-Inf, upper=0)$value
  expect_equal(post.notrunc.t, 1)
  expect_equal(post.truncpos.t, 1)
  expect_equal(post.truncneg.t, 1)

  post.notrunc.norm <- integrate(posterior_normal_trunc, a=-Inf, b=Inf, t=1.5, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1, lower=-Inf, upper=Inf)$value
  post.truncpos.norm <- integrate(posterior_normal_trunc, a=0, b=Inf, t=1.5, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1, lower=0, upper=Inf)$value
  post.truncneg.norm <- integrate(posterior_normal_trunc, a=-Inf, b=0, t=1.5, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1, lower=-Inf, upper=0)$value
  expect_equal(post.notrunc.norm, 1)
  expect_equal(post.truncpos.norm, 1)
  expect_equal(post.truncneg.norm, 1)
}
)

test_that("Bayes factors can be computed using Savage-Dickey", {
  post.notrunc.t <- posterior_t_trunc(0, a=-Inf, b=Inf, t=1, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1)
  post.truncpos.t <- posterior_t_trunc(0, a=0, b=Inf, t=1, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1)
  post.truncneg.t <- posterior_t_trunc(0, a=-Inf, b=0, t=1, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1)
  bf_t <- bf10_t(t=1, n1=30, n2=30, independentSamples=TRUE, prior.df = 1, prior.location = 0, prior.scale = 1)
  expect_equal(dt(0, df=1, ncp=0)/post.notrunc.t, bf_t$BF10)
  expect_equal(dt(0, df=1, ncp=0)*2/post.truncpos.t, bf_t$BFplus0)
  expect_equal(dt(0, df=1, ncp=0)*2/post.truncneg.t, bf_t$BFmin0)

  post.notrunc.norm <- posterior_normal_trunc(0, a=-Inf, b=Inf, t=1, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1)
  post.truncpos.norm <- posterior_normal_trunc(0, a=0, b=Inf, t=1, n1=30, n2=30, independentSamples=TRUE,prior.mean=0, prior.variance=1)
  post.truncneg.norm <- posterior_normal_trunc(0, a=-Inf, b=0, t=1, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1)
  bf_norm <- bf10_normal(t=1, n1=30, n2=30, independentSamples=TRUE, prior.mean=0, prior.variance=1)
  expect_equal(dnorm(0)/post.notrunc.norm, bf_norm$BF10)
  expect_equal(dnorm(0)*2/post.truncpos.norm, bf_norm$BFplus0)
  expect_equal(dnorm(0)*2/post.truncneg.norm, bf_norm$BFmin0)
}
)


