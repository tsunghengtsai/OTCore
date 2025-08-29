#' Summarize analysis results per defined group
#'
#' For each group defined based on variables in the metadata, summarize in
#' terms of number of transistors (`n`), number of flagged transistors
#' (`n_flagged`), mean and standard deviation of
#' maximum transconductance (`gm_max_mean` and `gm_max_sd`), and
#' shift of threshold voltage (`V_thresh_shift_mean` and `V_thresh_shift_sd`).
#'
#' @param x An `OTAnalysis` object.
#' @param by_var Character vector for variables in metadata to define groups
#' for summarization.
#'
#' @returns A data frame.
#' @export
OT_results <- function(x, by_var = ".id") {
  if (!identical(class(x), cls_ota())) {
    stop("Input should be an `OTAnalysis` object")
  }

  df <- as.data.frame(x, with = "OTResult", .id = TRUE)
  if (!all(hasName(df, by_var))) {
    stop("The variable(s) by which summarization is performed should be part of the metadata")
  }
  if (".id" %in% by_var) {
    warning("Summarization is performed for each transistor - not possible to assess uncertainty")
  }
  message(paste0("Summarizing by the variable(s) ", paste0(by_var, collapse = ", "), " ..."))

  var_sum <- c("flag", "gm_max", "V_thresh_shift")
  df_sum <- nest(df[, c(by_var, var_sum)], data = all_of(var_sum))
  df_sum$n <- sapply(df_sum$data, nrow)
  df_sum$n_flagged <- sapply(df_sum$data, function(x) sum(x$flag))
  df_sum$gm_max_mean <- sapply(df_sum$data, function(x) ifelse(all(x$flag), NA, mean(x$gm_max[!x$flag], na.rm = TRUE)))
  df_sum$gm_max_SD <- sapply(df_sum$data, function(x) sd(x$gm_max[!x$flag], na.rm = TRUE))
  df_sum$V_thresh_shift_mean <- sapply(df_sum$data, function(x) ifelse(all(x$flag), NA, mean(x$V_thresh_shift[!x$flag], na.rm = TRUE)))
  df_sum$V_thresh_shift_SD <- sapply(df_sum$data, function(x) sd(x$V_thresh_shift[!x$flag], na.rm = TRUE))

  df_sum[names(df_sum) != "data"]
}

#' Display flagged transistors and sweeps
#'
#' @param x An `OTAnalysis` object.
#'
#' @returns A data frame of sweep-level estimates for flagged instances.
#' @export
OT_flag_display <- function(x) {
  if (!identical(class(x), cls_ota())) {
    stop("Input should be an `OTAnalysis` object")
  }
  df <- as.data.frame(x, with = "estimate", .id = T)
  df[df$flag, ]
}

#' Modify (flag or unflag) transistors and sweeps
#'
#' @param x An `OTAnalysis` object.
#' @param mod A data frame of three columns: `.id`, `sweep`, `flag`.
#'
#' @returns An `OTAnalysis` object.
#' @export
OT_flag_modify <- function(x, mod) {
  if (!identical(class(x), cls_ota())) {
    stop("Input should be an `OTAnalysis` object")
  }
  if (!is.data.frame(mod)) {
    stop("Modification instructions should be provided as a data frame", call. = FALSE)
  }
  if (!all(hasName(mod, c(".id", "sweep", "flag")))) {
    stop("Modification instructions must have three columns: `.id`, `sweep`, `flag`", call. = FALSE)
  }
  if (!(nrow(mod) > 1)) {
    stop("No modification instructions provided", call. = FALSE)
  }

  ests <- x[["sweepResult"]][["estimate"]]  # a list
  id <- mod[[".id"]]
  s <- mod[["sweep"]]
  f <- mod[["flag"]]
  for (i in 1:nrow(mod)) {
    y <- x[["sweepResult"]][["estimate"]][[id[i]]]
    y$flag[y$sweep == s[i]] <- f[i]
    x[["sweepResult"]][["estimate"]][[id[i]]] <- y
    x[["OTResult"]][[id[i]]] <- summarize_sweeps(y)
  }

  validate_OTAnalysis(x)
}
