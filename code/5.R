input_lines = readLines('./inputs/5.txt')

blank_line_id = which(input_lines == '')

order_rules = input_lines[1:(blank_line_id - 1)]
update_lists = input_lines[(blank_line_id + 1):length(input_lines)]

order_rules_df = data.frame(id = 1:length(order_rules),
                            lesser = split_vectorised(order_rules, '\\|', 1),
                            greater = split_vectorised(order_rules, '\\|', 2))

update_lists_list = lapply(strsplit(update_lists, ','),
                           as.numeric)

validate_against_rules = function(update_list){
  compliance = TRUE
  for(rule_index in 1:nrow(order_rules_df)){
    lesser = order_rules_df$lesser[rule_index]
    greater = order_rules_df$greater[rule_index]
    if((lesser %in% update_list) & 
       (greater %in% update_list)){
      if(which(update_list == lesser) > which(update_list == greater)){
        compliance = FALSE
      }
    }
  }
  return(compliance)
}

rule_compliance = sapply(update_lists_list, validate_against_rules)

get_middle_item = function(update_list){
  update_list[(length(update_list) + 1)/2]
}

middle_items = sapply(update_lists_list, get_middle_item)

sum(middle_items[rule_compliance]) #4569

validate_against_rules_vector = function(update_list){
  compliance_vector = rep(TRUE, nrow(order_rules_df))
  for(rule_index in 1:nrow(order_rules_df)){
    lesser = order_rules_df$lesser[rule_index]
    greater = order_rules_df$greater[rule_index]
    if((lesser %in% update_list) & 
       (greater %in% update_list)){
      if(which(update_list == lesser) > which(update_list == greater)){
        compliance_vector[rule_index] = FALSE
      }
    }
  }
  compliance_vector
}

compliance_vectors = lapply(update_lists_list, validate_against_rules_vector)

sapply(compliance_vectors, function(vector){sum(!vector)})

# Do I have to approach, map order_rules_df as a digraph, for every connected component in the digraph 
# take the intersections with the list and put those in the right order

order_rules_digraph = igraph::graph_from_data_frame(order_rules_df[,c('lesser', 'greater')], directed = TRUE)

order_rules_components = components(order_rules_digraph)

length(order_rules_components$csize) #1

sorted_rules_digraph = topo_sort(order_rules_digraph) # Fails as not acyclic?

# Code from Stack Exchange (https://stackoverflow.com/a/55094319/11732165):

v1 = 2
v2 = 4
shortest_paths(order_rules_digraph, 4,2, mode="out")
shortest_paths(order_rules_digraph, 2,4, mode="out")

plot(layouted_graph)  

#66 -> 61 -> 69 -> 66

violated_rules = !reduce(compliance_vectors, function(vector1, vector2){vector1 & vector2})

all_vertices = unique(unlist(update_lists_list))

digraph_with_all_vertices = igraph::graph_from_data_frame(order_rules_df[,c('lesser', 'greater')], directed = TRUE)

make_list_compliant = function(list_to_comply){
  ordered_vertices = topo_sort(subgraph(digraph_with_all_vertices, as.character(list_to_comply)))
  compliant_list = as.numeric(as_ids(ordered_vertices))
  compliant_list
}

lists_in_compliance = lapply(update_lists_list, make_list_compliant)

middle_items_after_compliance = sapply(lists_in_compliance, get_middle_item)

sum(middle_items_after_compliance[!rule_compliance]) #6456
