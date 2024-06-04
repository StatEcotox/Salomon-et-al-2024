get_data_raw_adjusted = function (model, mod, group, N = NULL, at = NULL, subset = TRUE) 
{
  if (missing(group)) {
    stop("Please specify the 'group' argument by providing the name of the grouping variable. See ?mod_results")
  }
  data <- model$data
  if (any(model$not.na == FALSE)) {
    data <- data[model$not.na, ]
  }
  if (!is.null(at) & subset) {
    at_mod <- at[[mod]]
    position2 <- which(data[, mod] %in% at_mod)
    data <- data[position2, ]
    yi <- model$yi[position2]
    vi <- model$vi[position2]
    type <- attr(model$yi, "measure")
  }
  else {
    yi <- model$yi
    vi <- model$vi
    type <- attr(model$yi, "measure")
  }
  if (mod == "1") {
    moderator <- "Intrcpt"
  }
  else {
    moderator <- as.character(data[[mod]])
    moderator <- firstup(moderator)
  }
  stdy <- data[[group]]
  at.added <- data[,names(at)[2]]
  # add a column for the second subsetting factor used in the at argument
  data_reorg <- data.frame(yi, vi, moderator, stdy, type,  at.added)
  # subset data according to selected levels in at.added
  data_reorg = data_reorg[data_reorg$at.added == at[[2]],]
  row.names(data_reorg) <- 1:nrow(data_reorg)
  if (is.null(N) == FALSE) {
    data_reorg$N <- data[, N]
  }
  return(data_reorg)
}
