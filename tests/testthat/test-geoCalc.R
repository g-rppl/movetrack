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
  expect_equal(
    .destPoint(5, -5, 0, 1000),
    cbind(lon = 5, lat = 3.98315),
    tolerance = 1e-6
  )
  expect_equal(
    .destPoint(-1, 50, 90, 100),
    cbind(lon = 0.3974, lat = 49.9916),
    tolerance = 1e-6
  )
  expect_equal(
    .destPoint(5, 50, 42, 100),
    cbind(lon = 5.9483, lat = 50.6638),
    tolerance = 1e-6
  )
})

test_that("lagged distances", {
  expect_equal(
    .distance(
      lon = c(5, 10, 15),
      lat = c(50, 60, 70)
    ),
    c(NA, 1157253.3, 1157253.2),
    tolerance = 0.01
  )
})
