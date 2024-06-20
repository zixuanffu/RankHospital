pgmm
function(
    formula, data, subset, na.action, effect = c(
        "twoways",
        "individual"
    ), model = c("onestep", "twosteps"), collapse = FALSE,
    lost.ts = NULL, transformation = c("d", "ld"), fsm = NULL,
    index = NULL, ...) {
    cl <- match.call(expand.dots = TRUE)
    effect <- match.arg(effect)
    model <- match.arg(model)
    transformation <- match.arg(transformation)
    namesV <- NULL
    if (inherits(formula, "dynformula") || length(Formula(formula))[2L] ==
        1L) {
        if (!inherits(formula, "dynformula")) {
            formula <- match.call(expand.dots = TRUE)
            m <- match(c(
                "formula", "lag.form", "diff.form",
                "log.form"
            ), names(formula), 0)
            formula <- formula[c(1L, m)]
            formula[[1L]] <- as.name("dynformula")
            formula <- cl$formula <- eval(formula, parent.frame())
        }
        response.name <- paste(deparse(formula[[2L]]))
        main.lags <- attr(formula, "lag")
        if (length(main.lags[[1L]]) == 1L && main.lags[[1L]] >
            1L) {
            main.lags[[1L]] <- c(1L, main.lags[[1L]])
        }
        main.lags[2:length(main.lags)] <- lapply(
            main.lags[2:length(main.lags)],
            function(x) {
                if (length(x) == 1L && x != 0) {
                    x <- c(0, x)
                }
                x
            }
        )
        main.form <- dynterms2formula(main.lags, response.name)
        dots <- list(...)
        gmm.inst <- dots$gmm.inst
        lag.gmm <- dots$lag.gmm
        instruments <- dots$instruments
        gmm.form <- dynformula(gmm.inst, lag.form = lag.gmm)
        gmm.lags <- attr(gmm.form, "lag")
        gmm.lags <- lapply(gmm.lags, function(x) min(x):max(x))
        gmm.form <- dynterms2formula(gmm.lags)
        formula <- as.Formula(main.form, gmm.form)
    }
    x <- formula
    if (!inherits(x, "Formula")) {
        x <- Formula(formula)
    }
    gmm.form <- formula(x, rhs = 2, lhs = 0)
    gmm.lags <- dynterms(gmm.form)
    cardW <- length(gmm.lags)
    if (is.null(names(collapse))) {
        if (length(collapse) == 1L) {
            collapse <- as.vector(rep(collapse, cardW), mode = "list")
        } else {
            if (length(collapse) != cardW) {
                stop("the 'collapse' vector has a wrong length")
            }
        }
        names(collapse) <- names(gmm.lags)
    } else {
        if (any(!(names(collapse) %in% names(gmm.lags)))) {
            stop("unknown names in the 'collapse' vector")
        } else {
            bcollapse <- as.vector(rep(FALSE, cardW), mode = "list")
            names(bcollapse) <- names(gmm.lags)
            bcollapse[names(collapse)] <- collapse
            collapse <- bcollapse
        }
    }
    main.form <- formula(x, rhs = 1, lhs = 1)
    main.lags <- dynterms(main.form)
    if (length(x)[2L] == 3L) {
        normal.instruments <- TRUE
        inst.form <- formula(x, rhs = 3, lhs = 0)
        inst.form <- update(main.form, inst.form)
        inst.form <- formula(Formula(inst.form), lhs = 0)
        inst.lags <- dynterms(inst.form)
    } else {
        iv <- names(main.lags)[!names(main.lags) %in% names(gmm.lags)]
        inst.lags <- main.lags[iv]
        if (length(inst.lags) > 0L) {
            normal.instruments <- TRUE
            inst.form <- dynterms2formula(inst.lags)
        } else {
            normal.instruments <- FALSE
            inst.form <- NULL
            inst.lags <- NULL
        }
    }
    if (!is.null(lost.ts)) {
        if (!is.numeric(lost.ts)) {
            stop("argument 'lost.ts' should be numeric")
        }
        lost.ts <- as.numeric(lost.ts)
        if (!(length(lost.ts) %in% c(1L, 2L))) {
            stop("argument 'lost.ts' should be of length 1 or 2")
        }
        TL1 <- lost.ts[1L]
        TL2 <- if (length(lost.ts) == 1L) {
            TL1 - 1
        } else {
            lost.ts[2L]
        }
    } else {
        gmm.minlag <- min(unlist(gmm.lags, use.names = FALSE))
        inst.maxlag <- if (!is.null(inst.lags)) {
            max(unlist(inst.lags, use.names = FALSE))
        } else {
            0
        }
        main.maxlag <- max(unlist(main.lags, use.names = FALSE))
        TL1 <- max(main.maxlag + 1, inst.maxlag + 1, gmm.minlag)
        TL2 <- max(main.maxlag, inst.maxlag, gmm.minlag - 1)
        TL1 <- max(main.maxlag + 1, gmm.minlag)
        TL2 <- max(main.maxlag, gmm.minlag - 1)
    }
    gmm.form <- as.formula(paste("~", paste(names(gmm.lags),
        collapse = "+"
    )))
    if (!is.null(inst.form)) {
        Form <- as.Formula(main.form, gmm.form, inst.form)
    } else {
        Form <- as.Formula(main.form, gmm.form)
    }
    mf <- match.call(expand.dots = FALSE)
    m <- match(
        c("formula", "data", "subset", "na.action", "index"),
        names(mf), 0L
    )
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    mf$formula <- Form
    mf$na.action <- "na.pass"
    mf$subset <- NULL
    data <- eval(mf, parent.frame())
    index <- index(data)
    pdim <- pdim(data)
    N <- pdim$nT$n
    T <- pdim$nT$T
    balanced <- pdim$balanced
    if (!balanced) {
        un.id <- sort(unique(index(data, "id")))
        un.time <- sort(unique(index(data, "time")))
        rownames(data) <- paste(index(data, "id"), index(
            data,
            "time"
        ), sep = ".")
        allRows <- as.character(t(outer(un.id, un.time, paste,
            sep = "."
        )))
        data <- data[allRows, ]
        rownames(data) <- allRows
        index <- data.frame(
            id = rep(un.id, each = length(un.time)),
            time = rep(un.time, length(un.id)), row.names = rownames(data)
        )
        class(index) <- c("pindex", "data.frame")
        attr(data, "index") <- index
    }
    attr(data, "formula") <- formula(main.form)
    yX <- extract.data(data)
    names.coef <- colnames(yX[[1L]])[-1L]
    Z <- if (normal.instruments) {
        attr(data, "formula") <- inst.form
        extract.data(data)
    } else {
        NULL
    }
    attr(data, "formula") <- gmm.form
    W <- extract.data(data, as.matrix = FALSE)
    W1 <- lapply(W, function(x) {
        u <- mapply(makegmm, x, gmm.lags, TL1, collapse, SIMPLIFY = FALSE)
        u <- matrix(unlist(u), nrow = nrow(u[[1L]]))
        u
    })
    yX1 <- lapply(yX, function(x) {
        xd <- diff(x)
        xd <- xd[-c(seq_len(TL1 - 1)), , drop = FALSE]
        xd
    })
    if (normal.instruments) {
        Z1 <- lapply(Z, function(x) {
            xd <- diff(x)
            xd <- xd[-c(seq_len(TL1 - 1)), , drop = FALSE]
            xd
        })
    }
    if (transformation == "ld") {
        W2 <- lapply(W, function(x) {
            u <- mapply(makeW2, x, collapse, SIMPLIFY = FALSE)
            nrow.ud <- if (TL2 == 1L) {
                T - 2
            } else {
                T - TL2
            }
            u <- matrix(unlist(u), nrow = nrow.ud)
            if (TL2 == 1) {
                u <- rbind(0, u)
            }
            u
        })
        yX2 <- lapply(yX, function(x) {
            x <- x[-c(0:TL2), , drop = FALSE]
            x
        })
        if (normal.instruments) {
            Z2 <- lapply(Z, function(x) {
                x <- x[-c(0:TL2), , drop = FALSE]
                x
            })
        }
    }
    if (effect == "twoways") {
        namesV <- levels(index(data, which = "time"))
        if (transformation == "d") {
            V1 <- td.model.diff <- diff(diag(1, T - TL1 + 1))[
                ,
                -1
            ]
            namesV <- namesV[-c(0:(TL1))]
        } else {
            td <- cbind(1, rbind(0, diag(1, T - 1)))
            V2 <- td[-c(seq_len(TL2)), -c(2:(2 + TL2 - 1))]
            V1 <- diff(V2)
            namesV <- c("(Intercept)", namesV[-c(0:TL2 + 1)])
        }
        for (i in seq_len(N)) {
            yX1[[i]] <- cbind(yX1[[i]], V1)
            if (transformation == "d") {
                W1[[i]] <- cbind(W1[[i]], V1)
            } else {
                W2[[i]] <- cbind(W2[[i]], V2)
                yX2[[i]] <- cbind(yX2[[i]], V2)
            }
        }
    }
    if (effect == "individual" && transformation == "ld") {
        namesV <- levels(index(data, which = "time"))
        namesV <- c("(Intercept)", namesV[-c(0:TL2 + 1)])
    }
    for (i in seq_len(N)) {
        narows <- apply(yX1[[i]], 1, function(z) anyNA(z))
        yX1[[i]][narows, ] <- 0
        W1[[i]][is.na(W1[[i]])] <- 0
        W1[[i]][narows, ] <- 0
        if (normal.instruments) {
            Z1[[i]][is.na(Z1[[i]])] <- 0
            Z1[[i]][narows, ] <- 0
        }
        if (transformation == "ld") {
            narows <- apply(yX2[[i]], 1, function(z) anyNA(z))
            yX2[[i]][narows, ] <- 0
            W2[[i]][is.na(W2[[i]])] <- 0
            W2[[i]][narows, ] <- 0
            if (normal.instruments) {
                Z2[[i]][is.na(Z2[[i]])] <- 0
                Z2[[i]][narows, ] <- 0
            }
        }
    }
    if (transformation == "ld") {
        for (i in seq_len(N)) {
            W1[[i]] <- bdiag(W1[[i]], W2[[i]])
            yX1[[i]] <- rbind(yX1[[i]], yX2[[i]])
            if (normal.instruments) {
                Z1[[i]] <- bdiag(Z1[[i]], Z2[[i]])
            }
        }
    }
    if (normal.instruments) {
        for (i in seq_len(N)) W1[[i]] <- cbind(W1[[i]], Z1[[i]])
    }
    W <- W1
    yX <- yX1
    if (transformation == "d") {
        A1 <- tcrossprod(diff(diag(1, T - TL1 + 1)))
    }
    if (transformation == "ld") {
        A1 <- FSM(T - TL2, "full")
    }
    WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
    Wy <- lapply(WX, function(x) x[, 1L])
    WX <- lapply(WX, function(x) x[, -1L, drop = FALSE])
    A1 <- lapply(W, function(x) {
        crossprod(
            t(crossprod(x, A1)),
            x
        )
    })
    A1 <- Reduce("+", A1)
    minevA1 <- min(eigen(A1)$values)
    eps <- 1e-09
    A1 <- if (minevA1 < eps) {
        warning("the first-step matrix is singular, a general inverse is used")
        ginv(A1)
    } else {
        solve(A1)
    }
    A1 <- A1 * length(W)
    WX <- Reduce("+", WX)
    Wy <- Reduce("+", Wy)
    t.CP.WX.A1 <- t(crossprod(WX, A1))
    B1 <- solve(crossprod(WX, t.CP.WX.A1))
    Y1 <- crossprod(t.CP.WX.A1, Wy)
    coefficients <- as.numeric(crossprod(B1, Y1))
    if (effect == "twoways") {
        names.coef <- c(names.coef, namesV)
    }
    names(coefficients) <- names.coef
    residuals <- lapply(yX, function(x) {
        as.vector(x[, 1L] - crossprod(t(x[,
            -1L,
            drop = FALSE
        ]), coefficients))
    })
    outresid <- lapply(residuals, function(x) outer(x, x))
    A2 <- mapply(function(x, y) {
        crossprod(
            t(crossprod(x, y)),
            x
        )
    }, W, outresid, SIMPLIFY = FALSE)
    A2 <- Reduce("+", A2)
    minevA2 <- min(eigen(A2)$values)
    A2 <- if (minevA2 < eps) {
        warning("the second-step matrix is singular, a general inverse is used")
        ginv(A2)
    } else {
        solve(A2)
    }
    if (model == "twosteps") {
        coef1s <- coefficients
        t.CP.WX.A2 <- t(crossprod(WX, A2))
        Y2 <- crossprod(t.CP.WX.A2, Wy)
        B2 <- solve(crossprod(WX, t.CP.WX.A2))
        coefficients <- as.numeric(crossprod(B2, Y2))
        names(coefficients) <- names.coef
        residuals <- lapply(yX, function(x) {
            nz <- rownames(x)
            z <- as.vector(x[, 1L] - crossprod(
                t(x[, -1L, drop = FALSE]),
                coefficients
            ))
            names(z) <- nz
            z
        })
        vcov <- B2
    } else {
        vcov <- B1
    }
    rownames(vcov) <- colnames(vcov) <- names.coef
    fitted.values <- mapply(function(x, y) x[, 1L] - y, yX, residuals)
    if (model == "twosteps") {
        coefficients <- list(coef1s, coefficients)
    }
    args <- list(
        model = model, effect = effect, transformation = transformation,
        namest = namesV
    )
    result <- list(
        coefficients = coefficients, residuals = residuals,
        vcov = vcov, fitted.values = fitted.values, model = yX,
        W = W, A1 = A1, A2 = A2, call = cl, args = args
    )
    result <- structure(result,
        class = c("pgmm", "panelmodel"),
        pdim = pdim
    )
    result
}
