mod_results_adjusted = function (model, mod = "1", group, N = NULL, weights = "prop", 
          by = NULL, at = NULL, subset = FALSE, upper = TRUE, ...) 
{
  if (any(grepl("-1|0", as.character(model$formula.mods)))) {
    warning("It is recommended that you fit the model with an intercept. Unanticipated errors can occur otherwise.")
  }
  if (missing(model)) {
    stop("Please specify the 'model' argument by providing rma.mv or rma model object. See ?mod_results")
  }
  if (all(class(model) %in% c("robust.rma", "rma.mv", "rma", 
                              "rma.uni")) == FALSE) {
    stop("Sorry, you need to fit a metafor model of class rma.mv, rma, or robust.rma")
  }
  if (missing(group)) {
    stop("Please specify the 'group' argument by providing the name of the grouping variable. See ?mod_results")
  }
  if (is.null(stats::formula(model))) {
    model$formula.mods <- ~1
  }
  if (model$test == "t") {
    df_mod = as.numeric(model$ddf[[1]])
  }
  else {
    df_mod = 1e+06
  }
  data <- model$data
  if (any(model$not.na == FALSE)) {
    data <- data[model$not.na, ]
  }
  if (is.character(data[[mod]]) | is.factor(data[[mod]]) | 
      is.null(data[[mod]])) {
    grid <- emmeans::qdrg(formula = stats::formula(model), 
                          at = at, data = data, coef = model$b, vcov = stats::vcov(model), 
                          df = model$k - 1)
    mm <- emmeans::emmeans(grid, specs = mod, df = df_mod, 
                           by = by, weights = weights, ...)
    mm_test = emmeans::test(mm)
    mm_pi <- pred_interval_esmeans(model, mm, mod = mod)
    # define what is in mod_table output:
    if (is.null(by)) {
      mod_table <- data.frame(name = firstup(as.character(mm_pi[,1]), upper = upper), estimate = mm_pi[, "emmean"], 
                              lowerCL = mm_pi[, "lower.CL"], upperCL = mm_pi[,"upper.CL"], lowerPR = mm_pi[, "lower.PI"],upperPR = mm_pi[, "upper.PI"],
                              p = mm_test[,"p.value"], statistics = mm_test[,5])
    }
    else {
      mod_table <- data.frame(name = firstup(as.character(mm_pi[, 
                                                                1]), upper = upper), condition = mm_pi[, 2], 
                              estimate = mm_pi[, "emmean"], lowerCL = mm_pi[, 
                                                                            "lower.CL"], upperCL = mm_pi[, "upper.CL"], 
                              lowerPR = mm_pi[, "lower.PI"], upperPR = mm_pi[, 
                                                                             "upper.PI"])
    }
    # define what's in data output:
    data2 <- get_data_raw_adjusted(model, mod, group, N, at = at, 
                          subset)
    mod_table$name <- factor(mod_table$name, levels = mod_table$name, 
                             labels = mod_table$name)
  }
  else {
    at2 <- list(mod = seq(min(data[, mod], na.rm = TRUE), 
                          max(data[, mod], na.rm = TRUE), length.out = 100))
    names(at2) <- mod
    grid <- emmeans::qdrg(formula = stats::formula(model), 
                          data = data, coef = model$b, vcov = stats::vcov(model), 
                          df = model$k - 1, at = c(at2, at))
    mm <- emmeans::emmeans(grid, specs = mod, by = c(mod, 
                                                     by), weights = weights, df = df_mod)
    mm_pi <- pred_interval_esmeans(model, mm, mod = mod)
    if (is.null(by)) {
      mod_table <- data.frame(moderator = mm_pi[, 1], 
                              estimate = mm_pi[, "emmean"], lowerCL = mm_pi[, 
                                                                            "lower.CL"], upperCL = mm_pi[, "upper.CL"], 
                              lowerPR = mm_pi[, "lower.PI"], upperPR = mm_pi[, 
                                                                             "upper.PI"])
    }
    else {
      mod_table <- data.frame(moderator = mm_pi[, 1], 
                              condition = mm_pi[, 2], estimate = mm_pi[, "emmean"], 
                              lowerCL = mm_pi[, "lower.CL"], upperCL = mm_pi[, 
                                                                             "upper.CL"], lowerPR = mm_pi[, "lower.PI"], 
                              upperPR = mm_pi[, "upper.PI"])
    }
    data2 <- get_data_raw_cont(model, mod, group, N, by = by)
  }
  #define output elements:
  output <- list(mod_table = mod_table, data = data2)
  class(output) <- c("orchard", "data.frame")
  return(output)
}
