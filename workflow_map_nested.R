
`%||%` <- function(x, y) if (!is.null(x)) x else y

.is_tunable_wflow <- function(wf) {
  tb <- try(tune::tunable(wf), silent = TRUE)
  if (inherits(tb, "try-error")) return(FALSE)
  any(purrr::map_lgl(tb$call_info, tune::is_tune))
}

# --- new mapper -----------------------------------------------------------
workflow_map_nested <- function(
  x,                                   # a workflow_set
  fn = c("tune_nested", "tune_grid", "tune_bayes",
         "tune_race_anova", "tune_race_win_loss", "fit_resamples"),
  outer_resamples,                     # rset for outer loop (used by all)
  nested_resamples = NULL,             # nested_cv tibble (used when tuning)
  grid = NULL,                         # fallback grid
  metrics = NULL,                      # fallback metric_set
  control_tune = control_grid(save_pred = FALSE),
  control_resamples = control_resamples(save_pred = FALSE),
  verbose = TRUE
) {
  stopifnot(inherits(x, "workflow_set"))
  fn <- match.arg(fn)

  if (fn == "tune_nested" && is.null(nested_resamples)) {
    stop("When fn = 'tune_nested', you must supply nested_resamples (from rsample::nested_cv()).",
         call. = FALSE)
  }

  # pull each workflow out of the set
  wflows <- purrr::map(x$info, ~ .x$workflow[[1]])

  # row-wise options (like workflow_map)
  get_opt <- function(i, name, default = NULL) {
    opts <- x$options[[i]]
    if (!is.null(opts) && !is.null(opts[[name]])) opts[[name]] else default
  }

  results <- purrr::imap(wflows, function(wf, i) {
    id <- x$wflow_id[[i]]
    is_tuned <- .is_tunable_wflow(wf)

    # allow per-workflow overrides added via option_add()
    grid_i     <- get_opt(i, "grid",     grid)
    metrics_i  <- get_opt(i, "metrics",  metrics)
    ctrl_tune  <- get_opt(i, "control",  control_tune)       # for tuned paths
    ctrl_outer <- get_opt(i, "control_resamples", control_resamples)

    if (verbose) {
      msg <- if (is_tuned) sprintf("[%s] tuning via %s", id, fn) else sprintf("[%s] fit_resamples", id)
      message(msg)
    }

    if (is_tuned) {
      if (fn == "tune_nested") {
        finetune::tune_nested(
          wf,
          resamples = nested_resamples,
          grid      = grid_i,
          metrics   = metrics_i,
          control   = ctrl_tune
        )
      } else {
        # fall back to any other supported tuning fn on the OUTER resamples
        # (kept for completeness; you asked specifically about tune_nested)
        do.call(match.fun(fn), args = list(
          object    = wf,
          resamples = outer_resamples,
          grid      = grid_i,
          metrics   = metrics_i,
          control   = ctrl_tune
        ))
      }
    } else {
      fit_resamples(
        wf,
        resamples = outer_resamples,
        metrics   = metrics_i,
        control   = ctrl_outer
      )
    }
  })

  # return the same tibble with a new `result` column (like workflow_map does)
  x$result <- results
  x
}