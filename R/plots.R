#' @export
OT_plot_curve <- function(
    data,
    x_var = "V_gate",
    y_var = "abs_I_drain",
    y2_var = NULL,
    group = NULL,
    sweep = "both",
    .id = NULL,
    x_lab = x_var,
    y_lab = y_var,
    y2_lab = y2_var,
    y_trans = "identity"
) {
  if (!identical(cls_ota(), class(data))) {
    stop("Input should be an `OTAnalysis` object")
  }
  if (!is.null(group) && !is.null(y2_var)) {
    stop("Can't have varying color for both `group` and `y2_var`")
  }

  # Variable names must be characters
  if (!(is.character(x_var) && is.character(y_var))) {
    stop("Input `x_var` and `y_var` must be a character")
  }
  if (!is.null(group) && !is.character(group)) {
    stop("Input `group` must be a character")
  }
  if (!is.null(y2_var) && !is.character(y2_var)) {
    stop("Input `y2_var` must be a character")
  }

  # Check existence of variables
  var_swp <- names(data[["sweepResult"]][["sweepData"]][[1]])
  if (!all(c(x_var, y_var, y2_var) %in% var_swp)) {
    stop("The `x_var`, `y_var` (and `y2_var`)  variables should be part of sweepData")
  }
  if (!is.null(group)) {
    if (!(group %in% var_meta2())) {
      stop("The `group` variable should be part of the metadata")
    }
  }
  sweep <- match.arg(sweep, c("both", "fwd", "bwd"))

  # Check validity of index
  if (!is.null(.id)) {
    if (!(length(.id) == 1 && (.id %in% seq_along(data[["IVData"]])))) {
      stop("`.id` must be an integer for the index of a selected transistor")
    }
  }

  # One transistor per plot
  if (is.null(group)) {
    if (is.null(y2_var)) {
      gp <- function(d) {
        ggplot(d, aes(x = .data[[x_var]], linetype = .data$sweep)) +
          geom_line(aes(y = .data[[y_var]]), color = "#0072B2") +
          scale_y_continuous(transform = y_trans) +
          scale_linetype_manual(values = c("bwd" = "dashed", "fwd" = "solid")) +
          guides(linetype = guide_legend(override.aes = list(color = "#000000"))) +
          labs(x = x_lab, y = y_lab) +
          theme_bw()
      }
    } else {
      gp <- function(d) {
        scaling <- max(d[[y_var]], na.rm = TRUE) / max(d[[y2_var]], na.rm = TRUE)
        ggplot(d, aes(x = .data[[x_var]], linetype = .data$sweep)) +
          geom_line(aes(y = .data[[y_var]]), color = "#0072B2") +
          geom_line(aes(y = .data[[y2_var]] * scaling), color = "#D55E00") +
          scale_y_continuous(
            transform = y_trans,
            sec.axis = sec_axis(~ ./scaling, name = y2_lab)
          ) +
          scale_linetype_manual(values = c("bwd" = "dashed", "fwd" = "solid")) +
          guides(linetype = guide_legend(override.aes = list(color = "#000000"))) +
          labs(x = x_lab, y = y_lab) +
          theme_bw()
      }
    }

    # Forward sweep, backward sweep, or both
    swp <- data[["sweepResult"]]$sweepData
    if (sweep %in% c("fwd", "bwd")) {
      swp <- lapply(swp, function (df) df[df$sweep == sweep, ])
    }

    # One or multiple plots (transistors)
    if (is.null(.id)) {
      return(lapply(swp, gp))
    } else {
      return(gp(swp[[.id]]))
    }
  }

  # Multiple transistors per plot
  swp_all <- as.data.frame(data, .id = TRUE)
  if (sweep %in% c("fwd", "bwd")) {
    swp_all <- swp_all[swp_all$sweep == sweep, ]
  }
  swp_all[[group]] <- factor(swp_all[[group]])
  gg <- ggplot(swp_all, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[group]])) +
    geom_line(aes(group = .data$.id)) +
    scale_y_continuous(transform = y_trans) +
    labs(x = x_lab, y = y_lab, color = group)

  if (sweep != "both") {
    return(gg + theme_bw())
  } else {
    return(gg + facet_wrap(~ sweep) + theme_bw())
  }
}
