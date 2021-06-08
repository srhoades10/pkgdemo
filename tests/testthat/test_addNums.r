context("add numbers")

test_that("addNums does.. add nums", {
    
    expect_equal(addNums(5,6),11)
    expect_error(addNums(5, 'abc'))
})
