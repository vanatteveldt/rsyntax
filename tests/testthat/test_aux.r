 context("Aux functions")

test_that("get_nodes works", {
  data(example_tokens_dutchquotes)
  tokens = tokens[tokens$sentence == 4,]
  dat = find_nodes(tokens, lemma="dat")
  expect_equal(nrow(dat), 1)
  expect_equal(dat$id, "t_45")
  
  vcs = find_nodes(tokens, relation="vc", columns="parent")
  expect_equal(nrow(vcs), 2)
  expect_equal(sort(vcs$parent), sort(c("t_44", "t_53")))
  
  body = find_nodes(tokens, relation="vc", children=list(list(relation="body", rename="b")))
  expect_equal(nrow(body), 1)
  expect_equal(colnames(body), c("id", "b"))
  expect_equal(as.character(body$b), "t_53")
  
  # test abbreviation for rename and relation
  body2 = find_nodes(tokens, relation="vc", children=list(b="body"))
  expect_equal(body, body2)
  
  # can we get_children with children?
  kinderen = get_children(tokens, "body", rename="kind", children=list(kleinkind="vc"))
  expect_equal(colnames(kinderen), c("id", "kind", "kleinkind"))
  
  # test grandchildren
  body = find_nodes(tokens, children=list(kind=list("body", children=list(kleinkind="vc"))))
  expect_equal(nrow(body), 1)
  expect_equal(colnames(body), c("id", "kind", "kleinkind"))
  
  # multiple matching children
  dets = find_nodes(tokens, children=list(mid=list(children=list(det="det"))))
  expect_equal(nrow(dets), 2)
  expect_equal(sort(dets$det), sort(c("t_46", "t_48")) )
  
  # get parents
  parents = find_nodes(tokens, relation="vc", parent = list(pos='verb', rename='parent'))
  expect_equal(nrow(parents), 2)
  expect_equal(colnames(parents), c('id', 'parent'))
  
  # get parents, grandparents, children and grandchildren
  family = find_nodes(tokens, relation="vc", 
             parent = list(pos='verb', rename='parent',
                           parent = list(relation='vc', rename='grandparent')),
             child = list(relation='obj1', rename='child', 
                          child = list(relation='mod', rename='grandchild')))
  expect_equal(nrow(family), 1)
  expect_equal(colnames(family), c('id','child','grandchild','parent','grandparent'))
})
