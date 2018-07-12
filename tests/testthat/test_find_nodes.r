context("find nodes")

test_that("find_nodes works", {
  library(testthat)
  tokens = as_tokenindex(tokens_dutchquotes)
  
  dat = find_nodes(tokens, save='id', select = lemma == 'dat')
  expect_equal(nrow(dat), 1)
  expect_equal(dat$id, 45)
  
  vcs = find_nodes(tokens, save='parent', relation='vc')
  expect_equal(nrow(vcs), 3)
  expect_equal(sort(vcs$parent), c(7,45,54))
  
  body = find_nodes(tokens, save='id', relation="vc", 
                    children(save='b', relation='body'))
  expect_equal(nrow(body), 1)
  expect_equal(colnames(body), c('doc_id','.KEY', "id", "b"))
  expect_equal(body$b, 53)
  
  # can we get_children with children?
  children = find_nodes(tokens, 
                        children(save='child', relation="body",
                                 children(save='grandchild', relation='vc')))
  expect_equal(nrow(children), 1)
  expect_equal(colnames(children), c('doc_id',".KEY", "child", "grandchild"))
  
  nodes = find_nodes(tokens, save='test', relation='su',
                children(save='child'))

  # get parents
  parents = find_nodes(tokens, relation="vc", 
                       parents(save='parent', select = POS == 'verb'))

  
  expect_equal(nrow(parents), 3)
  expect_equal(parents$parent, c(6,44,53))
  
  # get parents, grandparents, children and grandchildren
  family = find_nodes(tokens, relation='vc',
                         parents(save='parent',
                                 parents(save='grandparent')),
                         children(save='child', relation='obj1',
                                  children(save='grandchild', relation='mod')))
  
  expect_equal(nrow(family), 1)
  expect_equal(colnames(family), c('doc_id','.KEY','parent','grandparent','child','grandchild'))
})
