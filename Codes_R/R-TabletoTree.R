##################################################################################################################################################################
#             This code stores the function:
#                 1. Gets the decision tree in table form
#                 2. Class decision tree needs to be provided as input
##################################################################################################################################################################

tree2table <- function(party_tree) {
  
  df_list <- list()
  var_names <-  attr( party_tree$terms, "term.labels")
  var_levels <- lapply( party_tree$data, levels)
  
  walk_the_tree <- function(node, rule_branch = NULL) {
    # depth-first walk on partynode structure (recursive function)
    # decision rules are extracted for every branch
    if(missing(rule_branch)) {
      rule_branch <- setNames(data.frame(t(replicate(length(var_names), NA))), var_names)
      rule_branch <- cbind(rule_branch, nodeId = NA)
      rule_branch <- cbind(rule_branch, predict = NA)
    }
    if(is.terminal(node)) {
      rule_branch[["nodeId"]] <- node$id
      rule_branch[["predict"]] <- predict_party(party_tree, node$id) 
      df_list[[as.character(node$id)]] <<- rule_branch
    } else {
      for(i in 1:length(node)) {
        rule_branch1 <- rule_branch
        val1 <- decision_rule(node,i)
        rule_branch1[[names(val1)[1]]] <- val1
        walk_the_tree(node[i], rule_branch1)
      }
    }
  }
  
  decision_rule <- function(node, i) {
    # returns split decision rule in data.frame with variable name an values
    var_name <- var_names[node$split$varid[[1]]]
    values_vec <- var_levels[[var_name]][ node$split$index == i]
    values_txt <- paste(values_vec, collapse = ", ")
    return( setNames(values_txt, var_name))
  }
  # compile data frame list
  walk_the_tree(party_tree$node)
  # merge all dataframes
  res_table <- Reduce(rbind, df_list)
  return(res_table)
}
