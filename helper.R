
PlotSetup <- function(n, m, grid_size){

  set.seed(1234)

  customer_locations <- data.frame(
    id = 1:n,
    x = round(runif(n) * grid_size),
    y = round(runif(n) * grid_size)
  )


  warehouse_locations <- data.frame(
    id = 1:m,
    x = round(runif(m) * grid_size),
    y = round(runif(m) * grid_size)
  )
  fixedcost <- round(rnorm(m, mean = grid_size * 10, sd = grid_size * 5))


  transportcost <- function(i, j) {
    customer <- customer_locations[i, ]
    warehouse <- warehouse_locations[j, ]
    round(sqrt((customer$x - warehouse$x)^2 + (customer$y - warehouse$y)^2))
  }

  library(ggplot2)
  g <- ggplot(customer_locations, aes(x, y), size = 3) +
    geom_point() +
    geom_point(data = warehouse_locations, color = "red", alpha = 0.5, shape = 17, size = 5) +
    scale_x_continuous(limits = c(0, grid_size)) +
    scale_y_continuous(limits = c(0, grid_size)) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(), panel.grid = element_blank())

  g + ggtitle("Warehouse location problem",
              "Black dots are customers. Light red triangles show potential warehouse locations.")
  return(g)
}

SolvedPlot <- function(n, m, grid_size){

  set.seed(1234)

  g <- PlotSetup(n, m, grid_size)

  customer_locations <- data.frame(
    id = 1:n,
    x = round(runif(n) * grid_size),
    y = round(runif(n) * grid_size)
  )

  warehouse_locations <- data.frame(
    id = 1:m,
    x = round(runif(m) * grid_size),
    y = round(runif(m) * grid_size)
  )
  fixedcost <- round(rnorm(m, mean = grid_size * 10, sd = grid_size * 5))


  transportcost <- function(i, j) {
    customer <- customer_locations[i, ]
    warehouse <- warehouse_locations[j, ]
    round(sqrt((customer$x - warehouse$x)^2 + (customer$y - warehouse$y)^2))
  }


  model <- MIPModel() %>%
    # 1 iff i gets assigned to warehouse j
    add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%

    # 1 iff warehouse j is built
    add_variable(y[j], j = 1:m, type = "binary") %>%

    # maximize the preferences
    set_objective(sum_expr(transportcost(i, j) * x[i, j], i = 1:n, j = 1:m) +
                    sum_expr(fixedcost[j] * y[j], j = 1:m), "min") %>%

    # every customer needs to be assigned to a warehouse
    add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>%

    # if a customer is assigned to a warehouse, then this warehouse must be built
    add_constraint(x[i,j] <= y[j], i = 1:n, j = 1:m)
  model


  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))


  matching <- result %>%
    get_solution(x[i,j]) %>%
    filter(value > .9) %>%
    select(i, j)



  plot_assignment <- matching %>%
    inner_join(customer_locations, by = c("i" = "id")) %>%
    inner_join(warehouse_locations, by = c("j" = "id"))
  customer_count <- matching %>% group_by(j) %>% summarise(n = n()) %>% rename(id = j)
  plot_warehouses <- warehouse_locations %>%
    mutate(costs = fixedcost) %>%
    inner_join(customer_count, by = "id") %>%
    filter(id %in% unique(matching$j))


  p <- g + geom_segment(data = plot_assignment, aes(x = x.y, y = y.y, xend = x.x, yend = y.x)) +
    geom_point(data  = plot_warehouses, color = "red", size = 5, shape = 17) +
    ggrepel::geom_label_repel(data  = plot_warehouses,
                              aes(label = paste0("fixed costs:", costs, "; customers: ", n)),
                              size = 3, nudge_y = 20) +
    ggtitle(paste0("Cost optimal warehouse locations and customer assignment"),
            "Big red triangles show warehouses that will be built, light red are unused warehouse locations.
            Dots represent customers served by the respective warehouses.")

  return(p)
}




VrSetupPlot <- function(n, m, max_x, max_y){

  set.seed(1)
  cities <- data.frame(id = 1:n, x = c(max_x / 2, runif(n - 1, max = max_x)),
                       y = c(max_y / 2, runif(n - 1, max = max_y))) %>%
    mutate(is_depot = ifelse(id == 1, TRUE, FALSE))
  g <- ggplot(cities, aes(x, y)) +
    geom_point(aes(size = is_depot, color = is_depot, shape = is_depot), size=5) +
    scale_y_continuous(limits = c(0, max_y)) +
    scale_x_continuous(limits = c(0, max_x))

  return(g)
}

VrSolvedPlot <- function(n, m, max_x, max_y) {

  set.seed(1)
  cities <- data.frame(id = 1:n, x = c(max_x / 2, runif(n - 1, max = max_x)),
                       y = c(max_y / 2, runif(n - 1, max = max_y))) %>%
    mutate(is_depot = ifelse(id == 1, TRUE, FALSE))

  g <- VrSetupPlot(n, m, max_x, max_y)

  distance <- as.matrix(dist(select(cities, x, y), diag = TRUE, upper = TRUE))

  # the depot is always idx 1
  model <- MIPModel() %>%

    # we create a variable that is 1 iff we travel from city i to j by Salesman k
    add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:m, type = "binary") %>%

    # helper variable for the MTZ sub-tour constraints
    add_variable(u[i, k], i = 1:n, k = 1:m, lb = 1, ub = n) %>%

    # minimize travel distance and latest arrival
    set_objective(sum_expr(distance[i, j] * x[i, j, k], i = 1:n, j = 1:n, k = 1:m), "min") %>%

    # you cannot go to the same city
    add_constraint(x[i, i, k] == 0, i = 1:n, k = 1:m) %>%

    # each salesman needs to leave the depot
    add_constraint(sum_expr(x[1, j, k], j = 2:n) == 1, k = 1:m) %>%

    # each salesman needs to come back to the depot
    add_constraint(sum_expr(x[i, 1, k], i = 2:n) == 1, k = 1:m) %>%

    # if a salesman comes to a city he has to leave it as well
    add_constraint(sum_expr(x[j, i, k], j = 1:n) == sum_expr(x[i, j, k], j = 1:n), i = 2:n, k = 1:m) %>%


    # leave each city with only one salesman
    add_constraint(sum_expr(x[i, j, k], j = 1:n, k = 1:m) == 1, i = 2:n) %>%

    # arrive at each city with only one salesman
    add_constraint(sum_expr(x[i, j, k], i = 1:n, k = 1:m) == 1, j = 2:n) %>%

    # ensure no subtours (arc constraints)
    add_constraint(u[i, k] >= 2, i = 2:n, k = 1:m) %>%
    add_constraint(u[i, k] - u[j, k] + 1 <= (n - 1) * (1 - x[i, j, k]), i = 2:n, j = 2:n, k = 1:m)


  result <- solve_model(model, with_ROI(solver = "glpk"))

  solution <- get_solution(result, x[i, j, k]) %>%
    filter(value > 0)

  paths <- select(solution, i, j, k) %>%
    rename(from = i, to = j, salesman = k) %>%
    mutate(trip_id = row_number()) %>%
    tidyr::gather(property, idx_val, from:to) %>%
    mutate(idx_val = as.integer(idx_val)) %>%
    inner_join(cities, by = c("idx_val" = "id"))


  g <- ggplot(cities, aes(x, y)) +
    geom_point(aes(size = is_depot, shape = is_depot, color = is_depot), size=5) +
    geom_line(data = paths, aes(group = trip_id, color = factor(salesman))) +
    ggtitle(paste0("Optimal route with cost: $", round(objective_value(result), 2))) +
    scale_y_continuous(limits = c(0, max_y)) +
    scale_x_continuous(limits = c(0, max_x))

  return(g)
}





TspSetupPlot <- function(n, max_x, max_y){

  set.seed(123456)
  cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))

  g <- ggplot(cities, aes(x, y)) +
    geom_point()

  return(g)
}


TspSolvedPlot <- function(n, max_x, max_y){

  set.seed(123456)
  cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))

  distance <- as.matrix(dist(select(cities, x, y), diag = TRUE, upper = TRUE))

  model <- MIPModel() %>%

    # we create a variable that is 1 iff we travel from city i to j
    add_variable(x[i, j], i = 1:n, j = 1:n,
                 type = "integer", lb = 0, ub = 1) %>%

    # a helper variable for the MTZ formulation of the tsp
    add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%

    # minimize travel distance
    set_objective(sum_expr(distance[i, j] * x[i, j], i = 1:n, j = 1:n), "min") %>%

    # you cannot go to the same city
    set_bounds(x[i, i], ub = 0, i = 1:n) %>%

    # leave each city
    add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%

    # visit each city
    add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%

    # ensure no subtours (arc constraints)
    add_constraint(u[i] >= 2, i = 2:n) %>%
    add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)


  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))


  solution <- get_solution(result, x[i, j]) %>%
    filter(value > 0)


  paths <- select(solution, i, j) %>%
    rename(from = i, to = j) %>%
    mutate(trip_id = row_number()) %>%
    tidyr::gather(property, idx_val, from:to) %>%
    mutate(idx_val = as.integer(idx_val)) %>%
    inner_join(cities, by = c("idx_val" = "id"))



  g <- ggplot(cities, aes(x, y)) +
    geom_point() +
    geom_line(data = paths, aes(group = trip_id)) +
    ggtitle(paste0("Optimal route with cost:$ ", round(objective_value(result), 2)))

  return(g)
}



