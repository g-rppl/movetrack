test_that("circular difference", {
  expect_equal(.circDiff(10, 30), 20)
  expect_equal(.circDiff(-10, -30), 20)
  expect_equal(.circDiff(-10, 10), 20)
  expect_equal(.circDiff(-170, 170), 20)
  expect_equal(.circDiff(10, -10), 20)
})

test_that("geographic distance", {
  expect_equal(.distGeo(5, 50, 10, 60), 1157.25334)
  expect_equal(.distGeo(-5, -10, 5, 10), 2486.6443)
})

test_that("destination point", {
  expect_equal(.destPoint(5, -5, 0, 1000), cbind(lon = 5, lat = 3.9831528))
  expect_equal(
    .destPoint(-1, 50, 90, 100),
    cbind(lon = 0.39736789, lat = 49.99160840)
  )
  expect_equal(
    .destPoint(5, 50, 42, 100),
    cbind(lon = 5.9482904, lat = 50.6637554)
  )
})
