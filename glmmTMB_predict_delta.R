predict_delta_glmmTMB <- function(model, newdata, 
                                  type = "response", 
                                  conf_level = 0.95) {
  # Input validation
  if (!inherits(model, "glmmTMB")) {
    stop("Model must be fitted with glmmTMB")
  }
  
  if (!is.data.frame(newdata)) {
    stop("newdata must be a data.frame")
  }
  
  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1")
  }
  
  if (!type %in% c("response", "link")) {
    stop("type must be 'response' or 'link'")
  }
  
  family_info <- model$modelInfo$family
  invlink <- family_info$linkinv
  
  # Get fixed effects and variance-covariance matrix
  fixed_effects <- fixef(model)$cond
  vcov_fixed <- as.matrix(vcov(model)$cond)

  terms_obj <- delete.response(terms(model))
  xlev <- model$modelInfo$xlev
  X <- model.matrix(terms_obj, data = newdata, xlev = xlev)
  missing_cols <- setdiff(names(fixed_effects), colnames(X))
  if (length(missing_cols) > 0) {
    stop("Some fixed effect terms not found in newdata: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  extra_cols <- setdiff(colnames(X), names(fixed_effects))
  if (length(extra_cols) > 0) {
    warning("Removing extra columns: ", 
            paste(extra_cols, collapse = ", "))
    X <- X[, names(fixed_effects), drop = FALSE]
  }
  
  X <- X[, names(fixed_effects), drop = FALSE]
  
  # Calculate marginal predictions 
  eta <- as.vector(X %*% fixed_effects)
  
  # Calculate standard errors using delta method
  var_eta <- diag(X %*% vcov_fixed %*% t(X))
  se_eta <- sqrt(var_eta)
  
  # Calculate confidence intervals on link scale
  z_score <- qnorm(1 - (1 - conf_level) / 2)
  eta_lower <- eta - z_score * se_eta
  eta_upper <- eta + z_score * se_eta
  
  # Transform to response scale if requested
  if (type == "response") {
    fit <- invlink(eta)
    conf_low <- invlink(eta_lower)
    conf_high <- invlink(eta_upper)
  } else {  # type == "link"
    fit <- eta
    conf_low <- eta_lower
    conf_high <- eta_upper
    se_response <- se_eta
  }
  
  # Prepare output
  result <- data.frame(
    fit = fit,
    conf_low = conf_low,
    conf_high = conf_high,
    conf_level = conf_level
  )
  
  # Add original newdata
  result <- cbind(newdata, result)
  
  return(result)
}


