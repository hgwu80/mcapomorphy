testthat::context('Testing data')
if (basename(getwd()) == 'testthat') setwd('../..')  # workspace is reset per file

testthat::test_that('sample_100', {
    testthat::expect_identical(length(sample_100), 100L);
    testthat::expect_true(sum(sample_100) == 5050);
});
