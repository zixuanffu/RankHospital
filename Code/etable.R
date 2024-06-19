etable <- function(
    ..., vcov = NULL, stage = 2, agg = NULL, se = NULL,
    ssc = NULL, cluster = NULL, .vcov = NULL, .vcov_args = NULL,
    digits = 4, digits.stats = 5, tex, fitstat = NULL, title = NULL,
    coefstat = "se", ci = 0.95, se.row = NULL, se.below = NULL,
    keep = NULL, drop = NULL, order = NULL, dict = TRUE, file = NULL,
    replace = FALSE, convergence = NULL, signif.code = NULL,
    label = NULL, float = NULL, headers = list("auto"), fixef_sizes = FALSE,
    fixef_sizes.simplify = TRUE, keepFactors = TRUE, family = NULL,
    powerBelow = -5, interaction.combine = NULL, interaction.order = NULL,
    i.equal = NULL, depvar = TRUE, style.tex = NULL, style.df = NULL,
    notes = NULL, group = NULL, extralines = NULL, fixef.group = NULL,
    placement = "htbp", drop.section = NULL, poly_dict = c(
        "",
        " square", " cube"
    ), postprocess.tex = NULL, postprocess.df = NULL,
    tpt = FALSE, arraystretch = NULL, adjustbox = NULL, fontsize = NULL,
    fit_format = "__var__", coef.just = NULL, tabular = "normal",
    highlight = NULL, coef.style = NULL, meta = NULL, meta.time = NULL,
    meta.author = NULL, meta.sys = NULL, meta.call = NULL, meta.comment = NULL,
    view = FALSE, export = NULL, markdown = NULL, page.width = "fit",
    div.class = "etable") {
    useSummary <- TRUE
    if (missnull(vcov) && missnull(se) && missnull(cluster) &&
        missing(.vcov) && missing(stage) && missnull(agg)) {
        useSummary <- FALSE
    }
    if (missing(depvar) && !missing(file)) {
        depvar <- TRUE
    }
    check_arg(tex, view, "logical scalar")
    if (missing(tex)) {
        if (!missing(file)) {
            tex <- TRUE
        } else {
            tex <- FALSE
        }
    }
    check_arg(float, "NULL logical scalar")
    if (missnull(float)) {
        if (!missing(title) || !missing(label)) {
            float <- TRUE
        } else {
            float <- FALSE
        }
    } else if (!float && (!missnull(title) || !missnull(label))) {
        what <- c("title", "label")[c(!missing(title), !missing(label))]
        warning("Since float = FALSE, the argument", enumerate_items(
            what,
            "s.is"
        ), " ignored.", immediate. = TRUE, call. = FALSE)
    }
    check_value(div.class, "character scalar")
    dots <- error_sender(list(...), "Some elements in '...' could not be evaluated: ")
    if (".up" %in% names(dots)) {
        .up <- 2
        dots[[".up"]] <- NULL
    } else {
        .up <- 1
    }
    if (length(dots) == 0) {
        stop("You must provide at least one element in '...'.")
    }
    if ("subtitles" %in% names(dots)) {
        if (is.null(getOption("fixest_etable_arg_subtitles"))) {
            warning("The argument 'subtitles' is deprecated. Please use 'headers' instead.")
            options(fixest_etable_arg_subtitles = TRUE)
        }
        headers <- dots$subtitles
        dots$subtitles <- NULL
    }
    if ("extraline" %in% names(dots)) {
        if (is.null(getOption("fixest_etable_arg_extraline"))) {
            warning("The argument 'extraline' is deprecated. Please use 'extralines' instead (note the last 's'!).")
            options(fixest_etable_arg_extraline = TRUE)
        }
        extralines <- dots$extraline
        dots$extraline <- NULL
    }
    if ("sdBelow" %in% names(dots)) {
        if (is.null(getOption("fixest_etable_arg_sdBelow"))) {
            warning("The argument 'sdBelow' is deprecated. Please use 'se.below' instead.")
            options(fixest_etable_arg_sdBelow = TRUE)
        }
        se.below <- dots$sdBelow
        dots$sdBelow <- NULL
    }
    if ("signifCode" %in% names(dots)) {
        if (is.null(getOption("fixest_etable_arg_signifCode"))) {
            warning("The argument 'signifCode' is deprecated. Please use 'signif.code' instead.")
            options(fixest_etable_arg_signifCode = TRUE)
        }
        signif.code <- dots$signifCode
        dots$signifCode <- NULL
    }
    if (.up == 2) {
        sysOrigin <- sys.parent()
        mc <- match.call(
            definition = sys.function(sysOrigin),
            call = sys.call(sysOrigin), expand.dots = FALSE
        )
        dots_call <- mc[["..."]]
    } else {
        mc <- match.call(expand.dots = FALSE)
        dots_call <- mc[["..."]]
    }
    vcov <- oldargs_to_vcov(se, cluster, vcov, .vcov)
    if (is_function_in_it(vcov) && missnull(.vcov_args)) {
        vcov_fun <- if (is.function(vcov)) {
            vcov
        } else {
            vcov[[1]]
        }
        .vcov_args <- catch_fun_args(vcov_fun, dots,
            exclude_args = "vcov",
            erase_args = TRUE
        )
        for (var in intersect(names(.vcov_args), names(dots))) dots[[var]] <- NULL
    }
    opts <- getOption("fixest_etable")
    args_global <- c(
        "postprocess.tex", "postprocess.df", "view",
        "markdown", "page.width"
    )
    for (arg in setdiff(args_global, names(mc))) {
        if (arg %in% names(opts)) {
            assign(arg, opts[[arg]])
        }
    }
    cache <- opts$view.cache
    check_arg(markdown, "NULL scalar(logical, character)")
    if (is.logical(markdown)) {
        if (isFALSE(markdown)) {
            markdown <- NULL
        }
    }
    tex_origin <- tex
    is_md <- !is.null(markdown)
    if (is_md) {
        if (!is_Rmarkdown()) {
            if ("markdown" %in% names(mc)) {
                warning("The argument 'markdown' only works when knitting Rmarkdown documents. It is currently ignored.")
            }
            markdown <- NULL
            is_md <- FALSE
        } else {
            tex <- TRUE
            export <- NULL
            view <- FALSE
            if (knitr::is_latex_output()) {
                is_md <- FALSE
            }
        }
    }
    is_export <- !is.null(export)
    do_df <- isFALSE(tex)
    do_tex <- tex || view || is_export
    check_arg(postprocess.tex, "NULL function arg(1,)")
    pp_tex <- postprocess.tex
    is_pp_tex <- !is.null(pp_tex)
    check_arg(postprocess.df, "NULL function arg(1,)")
    pp_df <- postprocess.df
    is_pp_df <- !is.null(pp_df)
    pp_tex_args <- pp_df_args <- list()
    if (!is.null(names(dots))) {
        if (is_pp_tex) {
            fm <- formalArgs(pp_tex)
            qui <- names(dots) %in% fm
            pp_tex_args <- dots[qui]
            dots <- dots[!qui]
        }
        if (is_pp_df) {
            fm <- formalArgs(pp_df)
            qui <- names(dots) %in% fm
            pp_df_args <- dots[qui]
            dots <- dots[!qui]
        }
    }
    if (length(dots) == 0) {
        stop("After cleaning the arguments to the postprocessing functions, there is no element left in '...'. Please provide at least one.")
    }
    for (i in seq_along(dots)) {
        if (!is_fixest_model(dots[[i]]) && !(is.list(dots[[i]]) &&
            is_fixest_model(dots[[i]][[1]]))) {
            msg <- ""
            if (!is.null(names(dots))) {
                msg <- paste0(" (named '", names(dots)[i], "')")
            }
            stop("The ", n_th(i), " element of '...'", msg, " is not valid: it should be a fixest object or a list of fixest objects, it is neither.")
        }
    }
    caption.number <- TRUE
    build_etable_list <- function(TEX) {
        results2formattedList(
            dots = dots, vcov = vcov, ssc = ssc,
            fitstat_all = fitstat, stage = stage, agg = agg,
            .vcov_args = .vcov_args, digits = digits, digits.stats = digits.stats,
            se.row = se.row, se.below = se.below, signif.code = signif.code,
            coefstat = coefstat, ci = ci, title = title, float = float,
            headers = headers, keepFactors = keepFactors, tex = TEX,
            useSummary = useSummary, dots_call = dots_call, powerBelow = powerBelow,
            dict = dict, interaction.combine = interaction.combine,
            interaction.order = interaction.order, i.equal = i.equal,
            convergence = convergence, family = family, keep = keep,
            drop = drop, file = file, order = order, label = label,
            fixef_sizes = fixef_sizes, fixef_sizes.simplify = fixef_sizes.simplify,
            depvar = depvar, style.tex = style.tex, style.df = style.df,
            replace = replace, notes = notes, group = group,
            extralines = extralines, fixef.group = fixef.group,
            placement = placement, drop.section = drop.section,
            poly_dict = poly_dict, tex_tag = TRUE, fit_format = fit_format,
            coef.just = coef.just, meta = meta, meta.time = meta.time,
            meta.author = meta.author, meta.sys = meta.sys, meta.call = meta.call,
            meta.comment = meta.comment, tpt = tpt, arraystretch = arraystretch,
            adjustbox = adjustbox, fontsize = fontsize, tabular = tabular,
            highlight = highlight, coef.style = coef.style, caption.number = caption.number,
            mc = mc, .up = .up + 1
        )
    }
    if (view || is_md || is_export) {
        build_ok <- check_build_available()
        if (!isTRUE(build_ok)) {
            args <- c("view", "export", "markdown")
            what <- args[args %in% names(mc)]
            if (length(what) > 0) {
                warni("The argument{$s, enum.bq, require?what} {build_ok} to work. So they are currently ignored.")
            }
            view <- is_md <- is_export <- FALSE
            markdown <- export <- NULL
            do_tex <- tex <- tex_origin
        }
    }
    if (do_tex) {
        if (is_md) {
            caption.number <- FALSE
        }
        info_tex <- build_etable_list(TRUE)
        res_tex <- etable_internal_latex(info_tex)
        n_models <- attr(res_tex, "n_models")
        attr(res_tex, "n_models") <- NULL
    }
    if (do_df) {
        info_df <- build_etable_list(FALSE)
        res_df <- etable_internal_df(info_df)
    }
    make_png <- function(x) NULL
    is_png <- view || is_md || is_export
    if (is_png) {
        make_png <- function(x) {
            build_tex_png(x,
                view = view,
                export = export, markdown = markdown, cache = cache,
                page.width = page.width
            )
        }
    }
    if (isFALSE(tex) && is_png) {
        ok <- TRUE
        if (is_pp_tex) {
            pp_tex_args_all <- list(res_tex)
            if (length(pp_tex_args) > 0) {
                pp_tex_args_all[names(pp_tex_args)] <- pp_tex_args
            }
            tex_output <- capture.output(res_tex <- do.call(
                pp_tex,
                pp_tex_args_all
            ))
            if (length(tex_output) > 0) {
                ok <- FALSE
                args <- c("view", "export")[c(view, is_export)]
                warni("The argument{$s, enum.bq, don't?args} work with the current postprocessing function for tex. Try to remove it first.")
            }
        }
        if (ok) {
            make_png(res_tex)
        }
    }
    is_file <- !missnull(file)
    if (is_file) {
        error_sender(
            sink(file = file, append = !replace), "Argument 'file': error when creating the document in ",
            file
        )
        on.exit(sink())
    }
    if (tex) {
        if (is_pp_tex) {
            pp_tex_args_all <- list(res_tex)
            if (length(pp_tex_args) > 0) {
                pp_tex_args_all[names(pp_tex_args)] <- pp_tex_args
            }
            res_tex <- do.call(pp_tex, pp_tex_args_all)
        }
        if (is.null(res_tex)) {
            if (is_png) {
                args <- c("view", "export", "markdown")[c(
                    view,
                    is_export, is_md
                )]
                warni("The argument{$s, enum.bq, don't?args} work with the current postprocessing function for tex. Try to remove it first.")
            }
            return(invisible(NULL))
        }
        res_tex <- res_tex[!res_tex %in% c("%start:tab\n", "%end:tab\n")]
        res_tex <- tex.nice(res_tex, n_models)
        path <- make_png(res_tex)
        if (is_md) {
            if (!knitr::is_latex_output()) {
                path <- path_to_relative(path)
                cat(sma("<div class = \"{div.class}\"><img src = \"{path}\"></div>\n"))
                return(invisible(NULL))
            }
        }
        if (is_file) {
            cat("\n")
            cat(res_tex, sep = "\n")
            cat("\n\n")
        }
        if (identical(class(res_tex), "character")) {
            class(res_tex) <- "etable_tex"
        }
        if (is_file) {
            return(invisible(res_tex))
        } else {
            return(res_tex)
        }
    } else {
        if (is_pp_df) {
            pp_df_args_all <- list(res_df)
            if (length(pp_df_args) > 0) {
                pp_df_args_all[names(pp_df_args)] <- pp_df_args
            }
            res_df <- do.call(pp_df, pp_df_args_all)
        }
        if (is.null(res_df)) {
            return(invisible(NULL))
        }
        if (is_file) {
            if (is.data.frame(res_df)) {
                print(res_df)
            } else {
                cat(res_df)
            }
            return(invisible(res_df))
        } else {
            if (is.data.frame(res_df)) {
                return(res_df)
            } else {
                cat(res_df)
                return(invisible(res_df))
            }
        }
    }
}
