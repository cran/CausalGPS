test_that("absoulte_weighted_corr_fun works as expected.", {

  # see test-check_covar_balance.R for more details about the test data.
  skip_on_cran()
  data.table::setDTthreads(1)
  data1 <- data.table::setDF(pseudo_pop_weight_test)
  val1 <- absolute_weighted_corr_fun(data1[,2],
            data1[,13],
            data1[,5:12]
            )
  expect_equal(val1$mean_absolute_corr, 0.085138, tolerance=0.0001)
  expect_equal(val1$median_absolute_corr, 0.06913371, tolerance=0.0001)
  expect_equal(val1$maximal_absolute_corr, 0.2178712, tolerance = 0.0001)


  # Use data that cause missing value in the results.
  data2 <- data1
  data2$region <- "East"
  data2$region <- as.factor(data2$region)
  expect_warning(absolute_weighted_corr_fun(data2[,2],
                                            data2[,6],
                                            data2[,7:length(data2)]
  ))
})
