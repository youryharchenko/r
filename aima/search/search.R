library(datastructures)

new_action <- setClass(
  "action",
  slots = c(
    value = "list"
  )
)

new_state <- setClass(
  "state",
  slots = c(
    value = "list"
  )
)

new_problem <- setClass(
  "problem",
  slots = c(
    initial_state = "state",
    goal_state = "state"
  )
)

setGeneric("goal_test", valueClass = "logical", function(object, state) {
  standardGeneric("goal_test")
})

setMethod("goal_test",
  signature(object = "problem"),
  function(object, state) {
    object@goal_state == state
  }
)

new_node <- setClass(
  "node",
  slots = c(
    parent = "node",
    state = "state",
    action = "action",
    path_cost = "numeric"
  )
)

solution <- function(node) {

}

breadth_first_search <- function(problem) {

  node <- new_node(state = problem@initial_state, path_cost = 0)

  if(goal_test(problem, node@state)) return(solution(node))

  frontier <- queue()
  frontier <- insert(frontier, node)

  explored <- binomial_heap()

  while(TRUE) {

    if(size(frontier)) return('fail')

    node <- pop(frontier)

    insert(explored, node@state)

    for(action in actions(problem, node@state)) {
      child <- child_node(problem, node, action)
      if(match(explored, child@state, 0) == 0 || match(frontier, child, 0) == 0) {
        if(goal_test(problem, child@state)) return(solution(child))
        frontier <- insert(frontier, child)
      }
    }
  }
}
