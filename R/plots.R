#' Plot measurement curve
#'
#' @description
#' `OT_plot_curve()` returns a `ggplot` object or a list of `ggplot` objects
#' specifying measurement curve(s) for one or multiple organic transistors.
#'
#' @details
#' By default, the function plots the transfer characteristic curve for each
#' transistor, which is useful in most cases. Specifically, the `x_var` argument
#' is set to `"V_gate"` and the `y_var` argument is set to `"abs_I_drain"`.
#' The plot may include an optional second curve, defined based on another
#' variable for an additional y-axis on the right of the plot (the `y2_var`
#' argument). The same `x_var` will be used for the second curve.
#' Transconductance (`"gm"`) and gate current (`"I_gate"`) are common choices
#' for `y2_var`. Default is to create such curves for all the transistors, and
#' return a list of `ggplot` objects, one for each transistor. Alternatively,
#' user can choose to plot just one transistor, indexed by the `.id` argument.
#' In that case, a `ggplot` object will be returned by the function. Another
#' possibility is to have all the curves in one plot, colored based on a
#' specified grouping variable (argument `group`) that should be part of the
#' metadata. By default, both `y2_var` and `group` are set to `NULL`. It is not
#' allowed to specify both arguments at the same time.
#'
#' @param data An `OTAnalysis` object.
#' @param x_var A character.
#' @param y_var A character.
#' @param y2_var An optional character.
#' @param group An optional character.
#' @param sweep A character with three possible values (`"fwd"`, `"bwd"`, or
#' `"both"` (default)).
#' @param .id An optional integer.
#' @param x_lab A character or expression.
#' @param y_lab A character or expression.
#' @param y2_lab An optional character or expression.
#' @param y_trans A character.
#'
#' @returns A `ggplot` object or a list of `ggplot` objects.
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
