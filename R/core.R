#' Processing and analysis of `OTDataSet`
#'
#' Based on the input I-V data restored in an `OTDataSet` object, separate into
#' sweep-level I-V data (one for forward sweep, one for backward sweep),
#' process and analyze the sweep-level data for quality assessment and
#' parameter estimation, and summarize each transistor based on its sweep-level
#' estimates.
#'
#' @param x An `OTDataSet` object.
#' @param n_ave An integer defining number of points for moving average.
#' @param ignore_outlier A logical value specifying whether outliers should be
#' ignored. Default is `TRUE`.
#'
#' @returns An `OTAnalysis` object.
#' @export
OT_analyze <- function(x, n_ave = 5, ignore_outlier = TRUE) {
  if (class(x) != cls_otd()) {
    stop(paste0("Input should be an ", cls_otd(), "object"))
  }

  if (!is.numeric(n_ave)) {
    stop("Parameter for moving average (`n_ave`) must be an numeric value")
  }
  n_ave <- as.integer(n_ave)
  if (n_ave <= 0) {
    stop("Parameter for moving average (`n_ave`) must be positive")
  }

  iv <- x[["IVData"]]
  meta <- x[["metadata"]]

  # Channel dimension ratio (W*d/L)
  meta$dim_ratio <- meta$width * meta$thickness / meta$length

  req_iv <- var_iv()
  swps <- vector("list", length(iv))  # sweep data
  qcs <- vector("list", length(iv))   # QC
  ests <- vector("list", length(iv))  # estimate
  otres <- vector("list", length(iv)) # OT result
  for (i in seq_along(iv)) {
    df <- iv[[i]][, req_iv]
    df$abs_I_drain <- abs(df$I_drain)
    idx <- index_sweep(df$V_gate)
    if (is.null(idx)) {
      stop(paste0("I-V data indexed ", i, " is not sorted properly for V_gate"))
    }
    d_ratio <- meta$dim_ratio[i]
    swp <- vector("list", length(idx))
    qc <- vector("list", length(idx))
    est <- vector("list", length(idx))
    for (s in seq_along(idx)) {
      d <- data.frame(sweep = names(idx)[s], df[idx[[s]], ])
      qc[[s]] <- data.frame(sweep = names(idx)[s], assess_transfer_curve(d))
      d$gm <- abs(averaged_diff(d$V_gate, d$abs_I_drain, n = n_ave))
      e <- data.frame(
        sweep = names(idx)[s],
        flag = any(qc[[s]]$flag, na.rm = TRUE),
        characterize_transfer_curve(d)
      )
      # d$V_bias <- d$V_gate - e$V_thresh
      # e$mu_Cv_prod <- stats::coef(stats::lm(d$gm ~ I(d_ratio * abs(d$V_bias)) ))[[2]]
      swp[[s]] <- d
      est[[s]] <- e
    }
    swps[[i]] <- bind_rows(swp)
    qcs[[i]] <- bind_rows(qc)
    ests[[i]] <- bind_rows(est)
    otres[[i]] <- summarize_sweeps(ests[[i]], ignore_outlier)
  }
  sres <- list(sweepData = swps, QC = qcs, estimate = ests)

  new_OTAnalysis(
    IVData = iv,
    metadata = meta,
    sweepResult = sres,
    OTResult = otres,
    units = units(x)
  )
}

#' Assess quality of transfer curve
#'
#' For a transfer curve (sqrt(I_D) vs. V_G), calculate quality measures about
#' on-off ratio, on-off range, magnitude of drain current, and ratio of
#' average drain current to average gate current, and flag abnormal instances
#' based on pre-defined thresholds.
#'
#' @param x A data frame of I-V data for one sweep.
#' @param type A character (`"p"` or `"n"`)
#'
#' @returns A data frame with four rows and three columns: `term`, `value`,
#' `flag`. Four QC terms are considered: `on_off_ratio`, `on_off_rng`,
#' `ID_max`, and `ID_IG_ratio`.
#' @export
assess_transfer_curve <- function(x, type = "p") {
  id <- abs(x$I_drain)
  vg <- x$V_gate

  # Max/min drain current
  max_id <- max(id, na.rm = TRUE)
  min_id <- min(id, na.rm = TRUE)
  r_on_off <- max_id / min_id

  # Span of on-off range in gate voltage
  # [TODO]: address the difference between p- and n-type devices
  rng_vg <- max(vg) - min(vg)
  max_vg <- vg[which(id == max_id)]
  min_vg <- vg[which(id == min_id)]
  rng_on_off <- (max_vg - min_vg) / rng_vg
  rng_on_off <- ifelse(type == "p", -rng_on_off, rng_on_off)

  qc_term <- c("on_off_ratio", "on_off_rng", "ID_max", "ID_IG_ratio")
  qc_thresh <- c(-Inf, 0.25, 10^(-9), 10^2)  # Flagging threshold
  if (!hasName(x, "I_gate")) {
    warning("`I_gate` is missing - can't compute `ID_IG_ratio`")
    qc_val <- c(r_on_off, rng_on_off, max_id, NA)
    qc_flag <- (qc_val < qc_thresh)
  } else {
    r_id_ig <- mean(id, na.rm = TRUE) / mean(abs(x$I_gate), na.rm = TRUE)
    qc_val <- c(r_on_off, rng_on_off, max_id, r_id_ig)
    qc_flag <- (qc_val < qc_thresh)
  }

  data.frame(term = qc_term, value = qc_val, flag = qc_flag)
}

#' Characterize transfer curve
#'
#' Based on gate voltage, drain current, and transconductance, calculate
#' maximum transconductance, threshold voltage, slope of the transfer curve
#' (sqrt(I_D) vs. V_G), and area under the transfer curve.
#'
#' @param x A data frame of I-V data for one sweep.
#'
#' @returns A data frame with one row and four columns: `gm_max`, `V_thresh`,
#' `slope`, `auc`.
#' @export
characterize_transfer_curve <- function(x) {
  vg <- x$V_gate
  sqrt_id <- sqrt(abs(x$I_drain))
  gm <- x$gm

  # Max transconductance
  gm_max <- max(gm, na.rm = TRUE)

  # Threshold voltage, slope & AUC
  idx_max <- which(gm == gm_max)
  rng_lm <- c(max(idx_max - 2, 1), min(idx_max + 2, length(gm)))
  lm_fit <- lm(sqrt_id[rng_lm] ~ vg[rng_lm])
  m <- coef(lm_fit)[[2]]
  v_t <- (-coef(lm_fit)[[1]]) / coef(lm_fit)[[2]]
  auc <- sum(diff(vg) * (head(sqrt_id, -1) + tail(sqrt_id, -1))) / 2

  data.frame(gm_max = gm_max, V_thresh = v_t, slope = m, auc = auc)
}

#' Summarize a transistor with sweep-level estimates
#'
#' @param x A data frame.
#' @param ignore_outlier A logical value.
#'
#' @returns A data frame with one row and four columns: `flag`,
#' `V_thresh_shift`, `auc_shift`, `gm_max`.
#' @export
summarize_sweeps <- function(x, ignore_outlier = TRUE) {
  flag <- any(x$flag, na.rm = TRUE)  # TRUE if any sweep is flagged as an outlier
  if (ignore_outlier && flag) {
    if (sum(x$flag) == 2) {
      # Both sweeps are flagged
      return(
        data.frame(
          flag = flag,
          V_thresh_shift = NA,
          auc_shift = NA,
          gm_max = NA
        )
      )
    } else {
      # One is flagged
      return(
        data.frame(
          flag = flag,
          V_thresh_shift = NA,
          auc_shift = NA,
          gm_max = x$gm_max[!x$flag]
        )
      )
    }
  }

  # When there is no outlier, or ignore_outlier is FALSE
  data.frame(
    flag = flag,
    V_thresh_shift = diff(x$V_thresh),
    auc_shift = diff(x$auc),
    gm_max = mean(x$gm_max)
  )
}
