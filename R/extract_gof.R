#' Extract goodness-of-fit statistics from a single model
#'
extract_gof <- function(model, fmt = '%.3f', fe_map = NULL, gof_map = NULL, ...) {
    if (is.null(gof_map)) {
        gof_map <- gtsummary::gof_map
    }
    # extract gof from model object
    gof <- broom::glance(model)

    # adding additional stats for felm models:
    if(class(model) == "felm") {
        n_obs <- summary(model)$N %>% format(big.mark = ",")
        gof$N <- as.character(n_obs)

        fe_levels <- map_int(model$fe, n_distinct) %>% map_chr(format, big.mark = ",")

        if(!is.null(fe_map)){
            names(fe_levels) <- fe_map[match(names(fe_levels), names(fe_map))]
            }

        gof <- cbind(gof, t(fe_levels))
    }

    # extract nobs if not available from glance
    # TODO: This should be fixed upstream
    if (!'n' %in% names(gof)) {
        gof$n <- tryCatch(stats::nobs(model), error = function(e) NULL)
    }

    # round numeric values and rename
    for (column in colnames(gof)) {
        # is gof in gof_map?
        idx <- match(column, gof_map$raw)
        if (!is.na(idx)) { # yes
            if (class(gof[[column]]) %in% c('numeric', 'integer')) {
                gof[[column]] <- rounding(gof[[column]], gof_map$fmt[idx])
            } else {
                gof[[column]] <- as.character(gof[[column]])
            }
            colnames(gof)[colnames(gof) == column] <- gof_map$clean[idx]
        } else { # no
            if (class(gof[[column]]) %in% c('numeric', 'integer')) {
                gof[[column]] <- rounding(gof[[column]], fmt)
            } else {
                gof[[column]] <- as.character(gof[[column]])
            }
        }
    }
    # reshape
    gof <- gof %>%
           tidyr::gather(term, value)
    # output
    return(gof)
}
