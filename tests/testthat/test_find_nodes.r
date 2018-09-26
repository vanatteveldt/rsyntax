context("find nodes")

test_that("find_nodes works", {
  library(testthat)
  tokens = as_tokenindex(tokens_dutchquotes)
  
  nrow(find_nodes(tokens, lemma__N = "Rutte"))
  
  dat = find_nodes(tokens, save='id', lemma = 'dat')
  expect_equal(nrow(dat), 1)
  expect_equal(dat$id, 45)
  
  vcs = find_nodes(tokens, save='parent', relation='vc')
  expect_equal(nrow(vcs), 3)
  expect_equal(sort(vcs$parent), c(7,45,54))
  
  body = find_nodes(tokens, save='id', relation="vc", 
                    children(save='b', relation='body'))
  expect_equal(nrow(body), 1)
  expect_equal(colnames(body), c('doc_id','sentence','.ID', "id", "b"))
  expect_equal(body$b, 53)
  
  # can we get_children with children?
  children = find_nodes(tokens, 
                        children(save='child', relation="body",
                                 children(save='grandchild', relation='vc')))
  expect_equal(nrow(children), 1)
  expect_equal(colnames(children), c('doc_id','sentence',".ID", "child", "grandchild"))
  
  nodes = find_nodes(tokens, save='test', relation='su',
                children(save='child'))

  # get parents
  parents = find_nodes(tokens, relation="vc", 
                       parents(save='parent', POS = 'verb'))

  
  expect_equal(nrow(parents), 3)
  expect_equal(parents$parent, c(6,44,53))
  
  # get parents, grandparents, children and grandchildren
  family = find_nodes(tokens, relation='vc',
                         parents(save='parent',
                                 parents(save='grandparent')),
                         children(save='child', relation='obj1',
                                  children(save='grandchild', relation='mod')))
  
  expect_equal(nrow(family), 1)
  expect_equal(colnames(family), c('doc_id','sentence','.ID','parent','grandparent','child','grandchild'))
  
  # test using req for optional arguments
  test_req = tokens_corenlp
  nodes1 = find_nodes(test_req, POS = 'VB*', save='verb',
                     children(relation = 'nsubj', save='subject'),
                     children(relation = 'dobj', save='object', req=F))
  nodes2 = find_nodes(test_req, POS = 'VB*', save='verb',
                     children(relation = 'nsubj', save='subject'),
                     children(relation = 'dobj', save='object', req=T))
  nodes3 = find_nodes(test_req, POS = 'VB*', save='verb',
                      children(relation = 'nsubj', save='subject'))
  expect_equal(nrow(nodes1), nrow(nodes3))
  expect_true(nrow(nodes2) < nrow(nodes1))
  
  find_nodes(test_req, POS = 'VB*', save='verb',
                      children(relation = 'nsubj', save='subject'),
                      children(relation = 'dobj', save='object', req=F))
})
