#' Define a Structural Path (DSL)
#'
#' 구조방정식에서 잠재변수 간 회귀경로를 정의하기 위한 DSL 함수이다.
#'
#' @param from character vector. 독립 잠재변수(들).
#' @param to character. 종속 잠재변수(단일).
#'
#' @return list with elements \code{from}, \code{to}.
#' @export
#'
#' @examples
#' path(c("F1","F2"), "F3")
path <- function(from, to) {
  if (length(to) != 1)
    stop("path(): to 는 길이 1 이어야 합니다.")
  list(from = as.character(from),
       to   = as.character(to))
}


#' Validate Residual Covariance Specification
#'
#' 관측변수 간 잔차공분산 설정의 유효성을 검사한다.
#'
#' @param residual_cov list. c("y1","y2") 또는 c("y1","y2","0").
#' @param obs_vars character. 관측변수 이름 벡터.
#'
#' @return list with elements \code{pairs}, \code{fixed}.
#' @export
validate_residual_cov <- function(residual_cov, obs_vars) {

  if (is.null(residual_cov))
    return(list(pairs = NULL, fixed = NULL))

  if (!is.list(residual_cov))
    stop("residual_cov 는 list() 여야 합니다.")

  pairs <- list()
  fixed <- list()

  for (rc in residual_cov) {

    if (!is.character(rc) || !(length(rc) %in% c(2,3)))
      stop("residual_cov 형식 오류: c('y1','y2') 또는 c('y1','y2','0')")

    v1 <- rc[1]; v2 <- rc[2]

    if (!(v1 %in% obs_vars && v2 %in% obs_vars))
      stop("residual_cov 는 관측변수만 허용됩니다.")

    if (v1 == v2)
      stop("residual_cov 는 비대각만 허용됩니다.")

    if (length(rc) == 3) {
      val <- as.numeric(rc[3])
      if (is.na(val)) stop("고정값은 숫자여야 합니다.")
      fixed[[length(fixed)+1]] <- list(i=v1, j=v2, value=val)
    } else {
      pairs[[length(pairs)+1]] <- list(i=v1, j=v2)
    }
  }

  list(pairs=pairs, fixed=fixed)
}

#' Validate Variance Constraints
#'
#' 관측·잠재변수 분산 고정 또는 자유 설정을 검증한다.
#'
#' @param variance list. c("x","x") 또는 c("f","f","1").
#' @param obs_vars character. 관측변수 이름.
#' @param latent_vars character. 잠재변수 이름.
#'
#' @return list with elements \code{free}, \code{fixed}.
#' @export
validate_variance <- function(variance, obs_vars, latent_vars) {

  if (is.null(variance))
    return(list(free=NULL, fixed=NULL))

  free  <- list()
  fixed <- list()

  for (v in variance) {

    if (!is.character(v) || !(length(v) %in% c(2,3)))
      stop("variance 형식 오류")

    if (v[1] != v[2])
      stop("variance 는 대각만 허용됩니다.")

    if (!(v[1] %in% c(obs_vars, latent_vars)))
      stop("variance 변수는 관측 또는 잠재변수여야 합니다.")

    if (length(v) == 3) {
      val <- as.numeric(v[3])
      if (is.na(val)) stop("variance 고정값은 숫자여야 합니다.")
      fixed[[length(fixed)+1]] <- list(var=v[1], value=val)
    } else {
      free[[length(free)+1]] <- list(var=v[1])
    }
  }

  list(free=free, fixed=fixed)
}


#' Generate Data from Covariance or Correlation Matrix
#'
#' 공분산 또는 상관행렬을 이용하여 정규분포 자료를 생성한다.
#'
#' @param M matrix. 공분산 또는 상관행렬.
#' @param n_cov integer. 표본 수.
#' @param seed integer.
#'
#' @return data.frame.
#' @keywords internal
.make_data_from_matrix <- function(M, n_cov = 200, seed = 2025) {

  if (!requireNamespace("MASS", quietly = TRUE))
    stop("MASS 패키지가 필요합니다.")

  set.seed(seed)
  mu <- rep(0, nrow(M))
  dat <- MASS::mvrnorm(n_cov, mu=mu, Sigma=as.matrix(M))
  as.data.frame(dat)
}

#' Parse Structural Model Specification
#'
#' path DSL을 회귀 경로 테이블로 변환한다.
#'
#' @param structural list of path().
#' @param latent_vars character.
#'
#' @return data.frame with from, to.
#' @keywords internal
.parse_structural <- function(structural, latent_vars) {

  edges <- data.frame(from=character(), to=character())

  for (pth in structural) {
    if (!(pth$to %in% latent_vars))
      stop("to 는 잠재변수여야 합니다.")

    if (!all(pth$from %in% latent_vars))
      stop("from 은 잠재변수만 허용됩니다.")

    edges <- rbind(edges,
                   data.frame(from=pth$from,
                              to=rep(pth$to, length(pth$from))))
  }
  edges
}



#' Single-Step SEM Estimation (Educational Closed-form Engine)
#'
#' 행렬 기반 수기계산 방식으로 단일 SEM 모형을 1회 추정한다.
#' 본 함수는 bootstrap 과정에서 반복 호출되는 핵심 계산 엔진이다.
#'
#' @param data data.frame. 관측자료.
#' @param latent_list named list. 잠재변수와 지표 매핑 정보.
#' @param edges data.frame. 구조경로(from, to).
#' @param residual_cov list or NULL. 잔차공분산 설정.
#' @param variance list or NULL. 분산 고정 설정.
#'
#' @return list containing:
#' \itemize{
#'   \item S: 표본 공분산 행렬
#'   \item eta_hat: 잠재변수 점수
#'   \item Lambda, B, Phi, Theta, Sigma_hat
#'   \item fit: 적합도 지수
#'   \item parameters: lavaan 스타일 파라미터 테이블
#' }
#'
#' @details
#' 본 함수는 다음 절차를 따른다.
#' 1) unit-weight factor score 계산
#' 2) Lambda 적재량 수기계산
#' 3) OLS 기반 잠재회귀(B) 추정
#' 4) Psi, Phi 행렬 재구성
#' 5) Sigma_hat 계산 및 ML 적합도 근사
#'
#' @keywords internal
.jjstat_sem_fit_once <- function(data, latent_list, edges,
                                 residual_cov=NULL, variance=NULL) {

  data <- as.data.frame(data)
  n <- nrow(data)

  ov <- colnames(data)
  lv <- names(latent_list)

  p <- length(ov)
  k <- length(lv)

  ## S
  S <- cov(data)

  ## unit-weight factor scores
  Z <- scale(data)
  eta_hat <- as.data.frame(lapply(latent_list, function(v) rowMeans(Z[, v, drop=FALSE])))
  colnames(eta_hat) <- lv
  eta_mat <- as.matrix(eta_hat[, lv, drop=FALSE])

  ## Lambda (첫 적재 1 고정)
  Lambda <- matrix(0, p, k, dimnames=list(ov, lv))
  fixed_loading <- matrix(FALSE, p, k, dimnames=list(ov, lv))

  for (f in lv) {
    items <- latent_list[[f]]
    eta <- eta_hat[[f]]
    v_eta <- var(eta)

    Lambda[items[1], f] <- 1
    fixed_loading[items[1], f] <- TRUE

    if (length(items) >= 2) {
      for (it in items[-1]) {
        Lambda[it, f] <- cov(data[, it], eta) / v_eta
      }
    }
  }

  ## observed residuals
  X <- as.matrix(data[, ov, drop=FALSE])
  Ehat <- X - (Lambda %*% t(eta_mat)) |> t()
  colnames(Ehat) <- ov

  ## Theta diag
  Theta <- diag(pmax(apply(Ehat, 2, var), 1e-6))
  dimnames(Theta) <- list(ov, ov)

  ## residual_cov 적용
  rcV <- validate_residual_cov(residual_cov, ov)
  if (!is.null(rcV$pairs)) {
    for (rc in rcV$pairs) {
      i <- rc$i; j <- rc$j
      Theta[i,j] <- cov(Ehat[,i], Ehat[,j])
      Theta[j,i] <- Theta[i,j]
    }
  }
  if (!is.null(rcV$fixed)) {
    for (rc in rcV$fixed) {
      i <- rc$i; j <- rc$j
      Theta[i,j] <- rc$value
      Theta[j,i] <- Theta[i,j]
    }
  }

  ## B (다중회귀)
  B <- matrix(0, k, k, dimnames=list(lv, lv))
  if (nrow(edges) > 0) {
    for (to in unique(edges$to)) {
      rhs <- edges$from[edges$to == to]
      Xrhs <- as.matrix(eta_hat[, rhs, drop=FALSE])
      fit <- lm(eta_hat[[to]] ~ Xrhs)
      b <- coef(fit)[-1]
      B[to, rhs] <- as.numeric(b)
    }
  }

  ## Psi diag (차원 일치 수정 핵심)
  Psi <- diag(k)
  dimnames(Psi) <- list(lv, lv)

  for (j in lv) {
    bvec <- as.numeric(B[j, lv])
    pred <- drop(eta_mat %*% bvec)
    resj <- eta_hat[[j]] - pred
    Psi[j, j] <- var(resj)
  }

  ## Phi
  I <- diag(k); dimnames(I) <- list(lv, lv)
  inv <- solve(I - B)
  Phi <- inv %*% Psi %*% t(inv)

  ## variance override (fixed만 반영)
  varV <- validate_variance(variance, ov, lv)
  if (!is.null(varV$fixed)) {
    for (vv in varV$fixed) {
      vname <- vv$var
      if (vname %in% ov) Theta[vname, vname] <- vv$value
      if (vname %in% lv) Phi[vname, vname] <- vv$value
    }
  }

  ## Sigma_hat
  Sigma_hat <- Lambda %*% Phi %*% t(Lambda) + Theta

  ## fit (ML 근사)
  detS <- det(S)
  detSig <- det(Sigma_hat)

  if (!is.finite(detSig) || detSig <= 0 || !is.finite(detS) || detS <= 0) {
    fit <- list(chisq=NA, df=NA, pvalue=NA, rmsea=NA, cfi=NA, tli=NA, srmr=NA)
  } else {
    Fml <- log(detSig) + sum(diag(S %*% solve(Sigma_hat))) - log(detS) - p
    chisq <- (n - 1) * Fml

    free_loadings <- sum(sapply(latent_list, length)) - length(latent_list)
    free_B <- nrow(edges)
    free_theta_diag <- p
    free_residcov <- length(rcV$pairs)
    free_psi <- k
    q <- free_loadings + free_B + free_theta_diag + free_residcov + free_psi

    df <- p*(p+1)/2 - q
    if (df < 1) df <- NA

    pval <- if (is.na(df)) NA else 1 - pchisq(chisq, df)
    rmsea <- if (is.na(df)) NA else sqrt(max(chisq - df, 0) / (df * (n - 1)))

    Sigma_null <- diag(diag(S))
    Fnull <- log(det(Sigma_null)) + sum(diag(S %*% solve(Sigma_null))) - log(detS) - p
    chisq_null <- (n - 1) * Fnull
    df_null <- p*(p-1)/2

    cfi <- 1 - max(chisq - df, 0) / max(chisq_null - df_null, 0)
    tli <- (chisq_null/df_null - chisq/df) / (chisq_null/df_null - 1)

    Robs <- cov2cor(S)
    Rhat <- cov2cor(Sigma_hat)
    srmr <- sqrt(mean((Robs[lower.tri(Robs)] - Rhat[lower.tri(Rhat)])^2))

    fit <- list(chisq=chisq, df=df, pvalue=pval,
                rmsea=rmsea, cfi=cfi, tli=tli, srmr=srmr)
  }

  ## standardized pieces
  sd_x <- sqrt(diag(S))
  sd_eta <- sqrt(diag(Phi))

  ## parameters
  params <- data.frame(
    lhs=character(), op=character(), rhs=character(),
    est=numeric(), std.lv=numeric(), std.all=numeric(),
    fixed=logical(), stringsAsFactors = FALSE
  )

  ## loadings
  for (f in lv) {
    items <- latent_list[[f]]
    for (it in items) {
      est <- Lambda[it, f]
      isfix <- fixed_loading[it, f]
      stdlv <- est * sd_eta[f]
      stdall <- (est * sd_eta[f]) / sd_x[it]
      params <- rbind(params, data.frame(
        lhs=f, op="=~", rhs=it,
        est=est, std.lv=stdlv, std.all=stdall,
        fixed=isfix
      ))
    }
  }

  ## regressions
  if (nrow(edges) > 0) {
    for (r in seq_len(nrow(edges))) {
      to <- edges$to[r]; from <- edges$from[r]
      est <- B[to, from]
      stdlv <- est
      stdall <- est * (sd_eta[from] / sd_eta[to])
      params <- rbind(params, data.frame(
        lhs=to, op="~", rhs=from,
        est=est, std.lv=stdlv, std.all=stdall,
        fixed=FALSE
      ))
    }
  }

  ## residual covariances
  if (!is.null(rcV$pairs)) {
    for (rc in rcV$pairs) {
      params <- rbind(params, data.frame(
        lhs=paste0(".", rc$i), op="~~", rhs=paste0(".", rc$j),
        est=Theta[rc$i,rc$j], std.lv=Theta[rc$i,rc$j],
        std.all=NA_real_, fixed=FALSE
      ))
    }
  }
  if (!is.null(rcV$fixed)) {
    for (rc in rcV$fixed) {
      params <- rbind(params, data.frame(
        lhs=paste0(".", rc$i), op="~~", rhs=paste0(".", rc$j),
        est=Theta[rc$i,rc$j], std.lv=Theta[rc$i,rc$j],
        std.all=NA_real_, fixed=TRUE
      ))
    }
  }

  ## residual variances
  for (it in ov) {
    params <- rbind(params, data.frame(
      lhs=paste0(".", it), op="~~", rhs=paste0(".", it),
      est=Theta[it,it], std.lv=Theta[it,it],
      std.all=NA_real_, fixed=FALSE
    ))
  }

  ## latent variances
  for (f in lv) {
    params <- rbind(params, data.frame(
      lhs=f, op="~~", rhs=f,
      est=Phi[f,f], std.lv=Phi[f,f],
      std.all=1, fixed=FALSE
    ))
  }

  list(
    S=S,
    eta_hat=eta_hat,
    Lambda=Lambda,
    B=B,
    Phi=Phi,
    Theta=Theta,
    Sigma_hat=Sigma_hat,
    fit=fit,
    parameters=params
  )
}



#' Educational Structural Equation Modeling (Closed-form Engine)
#'
#' 행렬 기반 수기계산 방식으로 구조방정식 모형을 추정하는
#' 교육용 SEM 엔진이다. 잠재변수 점수(unit-weight),
#' OLS 회귀, 모형암시 공분산 행렬을 이용하여
#' ML 적합도를 근사하며, bootstrap 기반 표준오차와
#' RMSEA 신뢰구간을 제공한다.
#'
#' @param data data.frame 또는 matrix.
#'   \itemize{
#'     \item \code{data_format="data"}: 관측자료
#'     \item \code{data_format="cov"}: 공분산 행렬
#'     \item \code{data_format="cor"}: 상관행렬
#'   }
#' @param latent_list named list.
#'   잠재변수와 관측지표의 매핑 정보.
#' @param structural list of \code{path()}.
#'   잠재변수 간 구조경로 정의.
#' @param residual_cov list or NULL.
#'   관측변수 잔차공분산 설정.
#' @param variance list or NULL.
#'   관측·잠재변수 분산 고정 설정.
#' @param data_format character.
#'   "data", "cov", "cor" 중 하나.
#' @param n_cov integer.
#'   공분산/상관행렬 입력 시 생성할 표본 크기.
#' @param nboot integer.
#'   bootstrap 반복 횟수.
#' @param seed integer.
#'   난수 시드.
#' @param rmseaCI numeric.
#'   RMSEA bootstrap 신뢰수준 (예: 0.90).
#'
#' @return \code{jjstat_sem} 클래스 객체.
#' \itemize{
#'   \item call: 함수 호출 정보
#'   \item n: 표본 크기
#'   \item Lambda, B, Phi, Theta, Sigma_hat
#'   \item fit: 적합도 지수
#'   \item parameters: lavaan 스타일 파라미터 테이블
#'   \item bootstrap: bootstrap 결과
#' }
#'
#' @details
#' 본 함수는 lavaan, OpenMx와 달리
#' \strong{모든 계산 단계를 명시적으로 드러내는 교육용 구현}이다.
#' 수기계산 검증, SEM 이론 강의, 알고리즘 이해를 목적으로 한다.
#'
#' @examples
#' latent <- list(
#'   F1 = c("y1","y2","y3"),
#'   F2 = c("y4","y5","y6")
#' )
#'
#' res <- jjstat_SEM(
#'   data = HolzingerSwineford1939[,1:6],
#'   latent_list = latent,
#'   structural = list(path("F1","F2")),
#'   nboot = 50
#' )
#'
#' summary(res)
#'
#' @export
jjstat_SEM <- function(
    data,
    latent_list,
    structural,
    residual_cov = NULL,
    variance = NULL,
    data_format = c("data","cov","cor"),
    n_cov = 200,
    nboot = 200,
    seed = 2025,
    rmseaCI = 0.90
) {

  data_format <- match.arg(data_format)
  set.seed(seed)

  ## -------------------------------------------------
  ## 1. 데이터 준비
  ## -------------------------------------------------
  if (data_format == "data") {
    data <- as.data.frame(data)
  } else {
    M <- as.matrix(data)
    if (data_format == "cor") diag(M) <- 1
    data <- .make_data_from_matrix(M, n_cov = n_cov, seed = seed)
  }

  ## -------------------------------------------------
  ## 2. 구조모형 파싱
  ## -------------------------------------------------
  lv <- names(latent_list)
  edges <- .parse_structural(structural, lv)

  ## -------------------------------------------------
  ## 3. 기본 모형 적합
  ## -------------------------------------------------
  base <- .jjstat_sem_fit_once(
    data = data,
    latent_list = latent_list,
    edges = edges,
    residual_cov = residual_cov,
    variance = variance
  )

  ## -------------------------------------------------
  ## 4. Bootstrap
  ## -------------------------------------------------
  set.seed(seed)

  P <- nrow(base$parameters)
  boot_est <- matrix(NA_real_, nrow = nboot, ncol = P)

  boot_rmsea <- boot_cfi <- boot_tli <- boot_srmr <- boot_chisq <-
    rep(NA_real_, nboot)

  for (b in seq_len(nboot)) {

    idx <- sample.int(nrow(data), replace = TRUE)
    dat_b <- data[idx, , drop = FALSE]

    tmp <- tryCatch(
      .jjstat_sem_fit_once(
        data = dat_b,
        latent_list = latent_list,
        edges = edges,
        residual_cov = residual_cov,
        variance = variance
      ),
      error = function(e) NULL
    )

    if (is.null(tmp)) next

    boot_est[b, ]   <- tmp$parameters$est
    boot_rmsea[b]   <- tmp$fit$rmsea
    boot_cfi[b]     <- tmp$fit$cfi
    boot_tli[b]     <- tmp$fit$tli
    boot_srmr[b]    <- tmp$fit$srmr
    boot_chisq[b]   <- tmp$fit$chisq
  }

  ## -------------------------------------------------
  ## 5. Bootstrap 기반 추론
  ## -------------------------------------------------
  se <- apply(boot_est, 2, sd, na.rm = TRUE)
  se[base$parameters$fixed] <- NA_real_

  z <- base$parameters$est / se
  pval <- 2 * (1 - pnorm(abs(z)))

  z[is.na(se)] <- NA_real_
  pval[is.na(se)] <- NA_real_

  ## -------------------------------------------------
  ## 6. RMSEA 및 적합도 신뢰구간
  ## -------------------------------------------------
  alpha <- 1 - rmseaCI

  rmsea_ci <- stats::quantile(
    boot_rmsea,
    probs = c(alpha/2, 1 - alpha/2),
    na.rm = TRUE, names = FALSE
  )

  cfi_ci <- stats::quantile(boot_cfi,
                            probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE, names = FALSE)

  tli_ci <- stats::quantile(boot_tli,
                            probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE, names = FALSE)

  srmr_ci <- stats::quantile(boot_srmr,
                             probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE, names = FALSE)

  fit <- base$fit
  fit$rmsea.ci.level <- rmseaCI
  fit$rmsea.ci.lower <- rmsea_ci[1]
  fit$rmsea.ci.upper <- rmsea_ci[2]

  fit$cfi.ci.lower  <- cfi_ci[1]
  fit$cfi.ci.upper  <- cfi_ci[2]
  fit$tli.ci.lower  <- tli_ci[1]
  fit$tli.ci.upper  <- tli_ci[2]
  fit$srmr.ci.lower <- srmr_ci[1]
  fit$srmr.ci.upper <- srmr_ci[2]

  ## -------------------------------------------------
  ## 7. 파라미터 테이블 정리
  ## -------------------------------------------------
  parameters <- base$parameters
  parameters$se <- se
  parameters$z  <- z
  parameters$pvalue <- pval

  op_order <- c("=~" = 1, "~" = 2, "~~" = 3)
  parameters$.opord <- op_order[parameters$op]
  parameters <- parameters[
    order(parameters$.opord, parameters$lhs, parameters$rhs), ]
  parameters$.opord <- NULL

  ## -------------------------------------------------
  ## 8. 결과 객체 구성
  ## -------------------------------------------------
  res <- list(
    call        = match.call(),
    lavaan      = FALSE,
    data_format = data_format,
    n           = nrow(data),
    obs_vars    = colnames(data),
    latent_vars = names(latent_list),

    S           = base$S,
    eta_hat     = base$eta_hat,
    Lambda      = base$Lambda,
    B           = base$B,
    Phi         = base$Phi,
    Theta       = base$Theta,
    Sigma_hat   = base$Sigma_hat,

    fit         = fit,
    parameters  = parameters,

    bootstrap = list(
      nboot      = nboot,
      seed       = seed,
      boot_est   = boot_est,
      boot_rmsea = boot_rmsea,
      boot_cfi   = boot_cfi,
      boot_tli   = boot_tli,
      boot_srmr  = boot_srmr,
      rmseaCI   = rmseaCI
    )
  )

  class(res) <- "jjstat_sem"
  res
}



#' Summary Method for jjstat_sem Objects
#'
#' lavaan 스타일의 요약 출력을 제공하는
#' \code{jjstat_sem} 클래스의 summary 메서드이다.
#'
#' @param object object of class \code{jjstat_sem}.
#' @param fit.measures logical.
#'   모형 적합도 지수 출력 여부.
#' @param standardized logical.
#'   표준화 계수(std.lv, std.all) 출력 여부.
#' @param digits integer.
#'   출력 시 반올림 자릿수.
#' @param ... additional arguments (unused).
#'
#' @return Invisibly returns the original \code{jjstat_sem} object.
#'
#' @details
#' 본 함수는 lavaan의 \code{summary()} 출력 형식을 참고하여,
#' 교육용 SEM 엔진의 결과를 사람이 읽기 쉬운 형태로 요약한다.
#' 표준오차는 bootstrap 기반으로 계산된다.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- jjstat_SEM(
#'   data = HolzingerSwineford1939[,1:6],
#'   latent_list = list(
#'     F1 = c("x1","x2","x3"),
#'     F2 = c("x4","x5","x6")
#'   ),
#'   structural = list(path("F1","F2")),
#'   nboot = 100
#' )
#'
#' summary(res)
#' }
summary.jjstat_sem <- function(object,
                               fit.measures=TRUE,
                               standardized=TRUE,
                               digits=3,
                               ...) {

  cat("\n")
  cat("jjstat SEM ended normally\n\n")

  cat("  Estimator                                         ML (educational closed-form)\n")
  cat("  Optimization method                           (none; unit-weight + OLS + implied Sigma)\n")
  cat(sprintf("  Number of observations                            %d\n\n", object$n))

  if (fit.measures) {
    fm <- object$fit

    cat("Model Test User Model:\n\n")
    cat(sprintf("  Test statistic (Chi-square)                 %12.3f\n", fm$chisq))
    cat(sprintf("  Degrees of freedom                          %12s\n",
                ifelse(is.na(fm$df), "NA", as.character(round(fm$df)))))
    cat(sprintf("  P-value (Chi-square)                        %12s\n\n",
                ifelse(is.na(fm$pvalue), "NA",
                       format(round(fm$pvalue, 3), nsmall=3))))

    cat("User Model versus Baseline Model:\n\n")
    cat(sprintf("  Comparative Fit Index (CFI)                 %12.3f\n", fm$cfi))
    cat(sprintf("  Tucker-Lewis Index (TLI)                    %12.3f\n\n", fm$tli))

    cat("Root Mean Square Error of Approximation:\n\n")
    cat(sprintf("  RMSEA                                        %12.3f\n", fm$rmsea))
    cat(sprintf("  %d Percent confidence interval - lower       %12.3f\n",
                round(fm$rmsea.ci.level*100), fm$rmsea.ci.lower))
    cat(sprintf("  %d Percent confidence interval - upper       %12.3f\n\n",
                round(fm$rmsea.ci.level*100), fm$rmsea.ci.upper))

    cat("Standardized Root Mean Square Residual:\n\n")
    cat(sprintf("  SRMR                                         %12.3f\n\n", fm$srmr))
  }

  cat("Parameter Estimates:\n\n")
  cat("  Standard errors                             Bootstrap\n")
  cat("  Information                                 (educational)\n\n")

  par <- object$parameters

  keep <- c("lhs","op","rhs","est","se","z","pvalue")
  if (standardized) keep <- c(keep, "std.lv", "std.all")

  par_out <- par[, keep, drop=FALSE]

  ## pvalue formatting
  par_out$pvalue <- ifelse(is.na(par_out$pvalue), NA,
                           format.pval(par_out$pvalue,
                                       digits=3, eps=1e-3))

  ## rounding
  num_cols <- setdiff(names(par_out),
                      c("lhs","op","rhs","pvalue"))
  for (cc in num_cols) {
    par_out[[cc]] <- round(par_out[[cc]], digits)
  }

  print(par_out, row.names=FALSE)
  invisible(object)
}
