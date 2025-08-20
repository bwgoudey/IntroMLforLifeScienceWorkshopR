`%||%` <- function(x, y) if (!is.null(x)) x else y

#.is_tunable_wflow <- function(wf) {
#  ps <- tune::extract_parameter_set_dials(wf)
#  if (length(ps$object) > 0) return(TRUE)
#  tb <- try(tune::tunable(wf), silent = TRUE)
#  if (inherits(tb, "try-error")) return(FALSE)
#  any(isTRUE(tb$tunable))
#}
.is_tunable_wflow <- function(wf) {
  ps <- tune::extract_parameter_set_dials(wf)
  # if there are any tunable params, ps$object has length > 0
  isTRUE(length(ps$object) > 0)
}

recover_outer_rset <- function(nested) {
  if (!inherits(nested, "nested_cv"))
    stop("`nested_resamples` must inherit 'nested_cv'.")
  if (!("inner_resamples" %in% names(nested)))
    stop("No `inner_resamples` column found; is it nested?")
  outer <- nested
  outer$inner_resamples <- NULL
  class(outer) <- setdiff(class(outer), "nested_cv")
  if (!inherits(outer, "rset"))
    stop("Recovered object does not inherit 'rset'.")
  outer
}

# --- new mapper -----------------------------------------------------------
workflow_map_nested <- function(
  x,                                   # workflow_set
  fn = c("tune_nested","tune_grid","tune_bayes",
         "tune_race_anova","tune_race_win_loss","fit_resamples"),
  nested_resamples,                    # rsample::nested_cv(...) tibble
  grid = NULL,
  metrics = NULL,
  control_tune = control_grid(save_pred = FALSE),
  control_resamples = control_resamples(save_pred = FALSE),
  verbose = TRUE
) {
  stopifnot(inherits(x, "workflow_set"))
  fn <- match.arg(fn)

  if (fn == "tune_nested" && !inherits(nested_resamples, "nested_cv"))
    stop("Provide `nested_resamples` created by rsample::nested_cv().")

  # Recover a shared OUTER rset for untuned workflows (and for non-nested fns)
  outer_resamples <- recover_outer_rset(nested_resamples)

  wflows <- purrr::map(x$info, ~ .x$workflow[[1]])
  get_opt <- function(i, name, default = NULL) {
    opts <- x$option[[i]]
    if (!is.null(opts) && !is.null(opts[[name]])) opts[[name]] else default
  }

  results <- purrr::imap(wflows, function(wf, i) {
    id <- x$wflow_id[[i]]
    is_tuned <- .is_tunable_wflow(wf)

    grid_i     <- get_opt(i, "grid",     grid)
    metrics_i  <- get_opt(i, "metrics",  metrics)
    ctrl_tune  <- get_opt(i, "control",  control_tune)
    ctrl_outer <- get_opt(i, "control_resamples", control_resamples)

    if (verbose) {
      msg <- if (is_tuned && fn == "tune_nested")
        sprintf("[%s] tuning via tune_nested (inner+outer)", id)
      else if (is_tuned)
        sprintf("[%s] tuning via %s (outer only)", id, fn)
      else
        sprintf("[%s] fit_resamples (outer only)", id)
      message(msg)
    }

    if (is_tuned && fn == "tune_nested") {
      finetune::tune_nested(
        wf,
        resamples = nested_resamples,   # uses inner for tuning, outer for assess
        grid      = grid_i,
        metrics   = metrics_i,
        control   = ctrl_tune
      )
    } else if (is_tuned) {
      # other tuning fns run on the OUTER rset (no inner loop)
      do.call(match.fun(fn), args = list(
        object    = wf,
        resamples = outer_resamples,
        grid      = grid_i,
        metrics   = metrics_i,
        control   = ctrl_tune
      ))
    } else {
      fit_resamples(
        wf,
        resamples = outer_resamples,
        metrics   = metrics_i,
        control   = ctrl_outer
      )
    }
  })

  x$result <- results
  x
}