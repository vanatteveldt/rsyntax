context("reshape")

test_that("reshape nodes works", {
  library(testthat)
  
  tokens = as_tokenindex(tokens_corenlp)
  tokens = tokens[tokens$sentence == 1,]
  
  tq = tquery(label='parent', POS = 'VB*',
              children(label='child', relation='nsubj'))
  
  test = select_nodes(tokens, tq) %>%
    mutate_nodes('child', token = parent$relation)
  expect_equal(test$token[1], 'ROOT')
  
  test = select_nodes(tokens, tq) %>%
    mutate_nodes('child', token = parent$relation, subset = child$token == 'Mary')
  expect_equal(test$token[1], 'John')
  
  test = select_nodes(tokens, tq) %>%
    copy_nodes('child', 'new_child') %>%
    copy_nodes('child', 'new_child2') 
  expect_equal(test$token_id[1:3], c(1,1.1,1.2))  ## id should count up correctly (1, 1.1, 1.2)
  expect_equal(test$token[1:3], c('John','John','John'))
  
  test = select_nodes(tokens, tq) %>%
    copy_nodes('child', 'new_child', subset = child$token == 'Mary')
  expect_equal(test$token[2], 'says')
  
  
  test = select_nodes(tokens, tq) %>%
    copy_fill('parent', 'child')
  expect_equal(test$token_id[3:4], c(3,3.1))
  expect_equal(test$token[3:4], c('Mary','Mary'))
  expect_equal(test$token_id[7:8], c(5,5.1))
  expect_equal(test$token[7:8], c('great','great'))
  
  test = select_nodes(tokens, tq) %>%
    copy_fill('parent', 'child', subset_fill = token %in% c('Mary','great'))
  expect_true(sum(test$token == 'is') == 1) ## "is" should not be copied
  
  test = select_nodes(tokens, tq) %>%
    copy_nodes('parent', 'new_child', copy_fill = TRUE)
  expect_equal(test$token_id[9], 5.1)
  expect_equal(test$parent[9], 2.1)
  
  test = select_nodes(tokens, tq) %>%
    remove_nodes('parent')
  expect_true(nrow(test) == 1)
  
  test = select_nodes(tokens, tq) %>%
    remove_nodes('parent', with_fill = FALSE)
  expect_true(nrow(test) == 5)
  expect_true(sum(is.na(test$parent)) == 3) ## top layer fill of 'parent' now become roots
  
})
