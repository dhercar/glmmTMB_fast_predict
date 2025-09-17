predict_delta_glmmTMB_fast <- function(model, newdata, type = "response",
                                       conf_level = 0.95, chunk_size = 50000) {

  # general checks
  if (!inherits(model, "glmmTMB")) stop("model must be a glmmTMB object")
  if (!is.data.frame(newdata)) stop("newdata must be a data.frame")
  if (!type %in% c("response", "link")) stop("type must be 'response' or 'link'")
  if (!(conf_level > 0 && conf_level < 1)) stop("conf_level in (0,1)")

  fam <- tryCatch(family(model), error = function(e) NULL)
  invlink <- if (!is.null(fam) && is.function(fam$linkinv)) fam$linkinv else model$modelInfo$family$linkinv
  if (!is.function(invlink)) stop("cannot find inverse link function")

  beta <- fixef(model)$cond # fix. effects
  V <- as.matrix(vcov(model)$cond) # var cov matrix
  terms_obj <- delete.response(terms(model)) # vars
  xlev <- model$modelInfo$xlev 

  # predict for newdata
  predict_chunk <- function(nd) {
    X <- model.matrix(terms_obj, data = nd, xlev = xlev)
    # columns align with beta
    if (!all(names(beta) %in% colnames(X))) {
      miss <- setdiff(names(beta), colnames(X))
      stop("Missing columns in newdata: ", paste(miss, collapse = ", "))
    }
    X <- X[, names(beta), drop = FALSE]

    eta <- as.vector(X %*% beta)
    XV <- X %*% V     
    var_eta <- pmax(0, rowSums(XV * X))
    se_link <- sqrt(var_eta)

    z <- qnorm(1 - (1 - conf_level) / 2)
    p <- invlink(eta)
    linkname <- if (!is.null(fam)) fam$link else NA_character_

    deriv <- switch(as.character(linkname),
                    "logit"    = p * (1 - p),
                    "probit"   = dnorm(eta),
                    "identity" = rep(1, length(eta)),
                    "log"      = p,
                    ((invlink(eta + 1e-6) - invlink(eta - 1e-6)) / (2e-6))
    )

    se_response <- deriv * se_link

    if (type == "link") {
      out <- data.frame(
        fit_link = eta, se_link = se_link,
        conf_low_link = eta - z * se_link, conf_high_link = eta + z * se_link,
        fit = p, se_response = se_response,
        conf_low = p - z * se_response, conf_high = p + z * se_response,
        stringsAsFactors = FALSE
      )
      # clamp response CIs if probabilities
      out$conf_low <- pmax(out$conf_low, 0)
      out$conf_high <- pmin(out$conf_high, 1)
    } else {
      fit <- p
      conf_low <- fit - z * se_response
      conf_high <- fit + z * se_response
      conf_low <- pmax(conf_low, 0)
      conf_high <- pmin(conf_high, 1)
      out <- data.frame(
        fit = fit, se_link = se_link, se_response = se_response,
        conf_low = conf_low, conf_high = conf_high,
        conf_low_link = eta - z * se_link, conf_high_link = eta + z * se_link,
        stringsAsFactors = FALSE
      )
    }
    cbind(nd, out)
  }

  n <- nrow(newdata)
  if (is.null(chunk_size) || chunk_size >= n) {
    res <- predict_chunk(newdata)
  } else {
    idx <- seq_len(n)
    splits <- split(idx, ceiling(seq_along(idx) / chunk_size))
    out_list <- vector("list", length(splits))
    for (i in seq_along(splits)) {
      rows <- splits[[i]]
      out_list[[i]] <- predict_chunk(newdata[rows, , drop = FALSE])
    }
    res <- do.call(rbind, out_list)
    rownames(res) <- NULL
  }
  res
}

