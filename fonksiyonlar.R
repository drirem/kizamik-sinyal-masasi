# Kızamık Sinyal Masası — ortak fonksiyonlar
# hesapla.R ve app.R bu dosyayı source() ile yükler.
# Algoritma tanımları dogrulama_surveillance.R (bildiri Tablo 3) ile aynıdır.

# ---- eksik paketleri otomatik kur ------------------------------------------------
gerekli <- c("shiny", "bslib", "surveillance", "MASS", "ggplot2", "DT", "dplyr")
eksik <- gerekli[!vapply(gerekli, requireNamespace, logical(1), quietly = TRUE)]
if (length(eksik) && R.version$os != "emscripten") {   # tarayıcıda (Shinylive) kurulum yapılmaz
  message("Eksik paketler kuruluyor: ", paste(eksik, collapse = ", "))
  install.packages(eksik, repos = "https://cloud.r-project.org")
}
rm(gerekli, eksik)

suppressPackageStartupMessages({
  library(surveillance)   # >= 1.20
  library(MASS)
})

T0 <- 265   # değerlendirme (izlem) 265. haftadan başlar; ilk 264 hafta taban dönemi

ALGOS <- c("EARS-C1", "EARS-C2", "EARS-C3", "Bayes", "Farrington", "FarringtonFlexible", "GLR-NB")
ETIKET <- c("EARS-C1" = "EARS C1", "EARS-C2" = "EARS C2", "EARS-C3" = "EARS C3", "Bayes" = "Bayes",
            "Farrington" = "Farrington", "FarringtonFlexible" = "Farrington Flexible", "GLR-NB" = "GLR-NB")
# En az 5 yıllık geçmiş isteyen algoritmalar
UZUN_SERI_GEREKEN <- c("Farrington", "FarringtonFlexible", "GLR-NB")

# ---- biçimlendirme -----------------------------------------------------------
fmt <- function(x, d = 1) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d, decimal.mark = ","))
pct <- function(x, d = 1) fmt(100 * x, d)

# ---- veri okuma --------------------------------------------------------------
# Her seri: sim, cases, ol (salgın etiketi), cl (küme etiketi), al (herhangi etiket), ob (salgına bağlı vaka)
seri_oku <- function(yol) {
  d <- read.csv(yol)
  d <- d[order(d$sim, d$week), ]
  lapply(split(d, d$sim), function(g) list(
    sim = g$sim[1], cases = as.integer(g$cases),
    ol = as.integer(g$outbreak_label), cl = as.integer(g$cluster_label),
    al = as.integer(g$any_label), ob = as.integer(g$outbreak)))
}

# ---- EARS, CDC tanımı ----------------------------------------------------------
ears_cdc <- function(y, baseline = 7, sd_min = 0.2) {
  n <- length(y)
  cstat <- function(lag) {                      # lag = 0 (C1) ya da 2 (C2)
    out <- rep(NA_real_, n)
    for (t in seq_len(n)) {
      idx <- (t - lag - baseline):(t - lag - 1)
      if (min(idx) < 1) next
      out[t] <- (y[t] - mean(y[idx])) / max(sd(y[idx]), sd_min)
    }
    out
  }
  C1 <- cstat(0); C2 <- cstat(2)
  ex <- pmax(0, C2 - 1)
  C3 <- ex + c(NA, head(ex, -1)) + c(NA, NA, head(ex, -2))
  list(C1 = as.integer(C1 > 3), C2 = as.integer(C2 > 3), C3 = as.integer(C3 > 2))
}

# ---- GLR-NB tabanı: yinelemeli |z| >= 2,58 dışlama ---------------------------
glr_taban <- function(y, n_taban = 260, z_esik = 2.58) {
  kalan <- rep(TRUE, n_taban); yy <- y[1:n_taban]
  repeat {
    z <- (yy - mean(yy[kalan])) / sd(yy[kalan])
    yeni <- kalan & abs(z) < z_esik
    if (identical(yeni, kalan)) break
    kalan <- yeni
  }
  b <- data.frame(y = yy, t = 1:n_taban)[kalan, ]
  m0 <- MASS::glm.nb(y ~ sin(2 * pi * t / 52) + cos(2 * pi * t / 52), data = b)
  tt <- seq_along(y)
  mu <- exp(coef(m0)[1] + coef(m0)[2] * sin(2 * pi * tt / 52) + coef(m0)[3] * cos(2 * pi * tt / 52))
  list(mu0 = as.numeric(mu), alpha = 1 / m0$theta)
}

# ---- tek algoritmayı tek seride çalıştır ---------------------------------------
# Dönüş: seriyle aynı uzunlukta 0/1 vektör (t0 öncesi 0)
algoritma_calistir <- function(a, cases, t0 = T0, alpha = 0.05, carl = 5) {
  y <- as.integer(cases); n <- length(y); rng <- t0:n
  out <- integer(n)
  st <- function() sts(observed = y, start = c(2000, 1), frequency = 52)
  al <- switch(a,
    "EARS-C1" = ears_cdc(y)$C1[rng],
    "EARS-C2" = ears_cdc(y)$C2[rng],
    "EARS-C3" = ears_cdc(y)$C3[rng],
    "Bayes" = alarms(suppressWarnings(bayes(st(), control = list(range = rng, b = 0, w = 6, actY = TRUE,
                                                               alpha = alpha))))[, 1],
    "Farrington" = alarms(suppressWarnings(farrington(st(), control = list(range = rng, b = 5, w = 3, alpha = alpha,
                                          reweight = TRUE, trend = TRUE, limit54 = c(5, 4)))))[, 1],
    "FarringtonFlexible" = alarms(suppressWarnings(farringtonFlexible(st(), control = list(range = rng, b = 5, w = 3,
                                   noPeriods = 10, pastWeeksNotIncluded = 26, weightsThreshold = 2.58,
                                   thresholdMethod = "nbPlugin", alpha = alpha, trend = TRUE, reweight = TRUE,
                                   limit54 = c(5, 4)))))[, 1],
    "GLR-NB" = {
      gb <- suppressWarnings(glr_taban(y, n_taban = min(260, t0 - 1)))
      alarms(glrnb(st(), control = list(range = rng, mu0 = gb$mu0[rng], alpha = gb$alpha, c.ARL = carl,
                                        dir = "inc", change = "intercept", ret = "value")))[, 1]
    },
    stop("Bilinmeyen algoritma: ", a))
  al <- as.integer(al); al[is.na(al)] <- 0L
  out[rng] <- al
  out
}

# ---- performans ölçütleri ------------------------------------------------------
# S: seri listesi, A: aynı sırada alarm vektörleri listesi, capa: zamanlılık çapası (salgına bağlı haftalık vaka)
kosu <- function(x) {                       # 1'lerden oluşan ardışık blokların başlangıç/bitişi
  r <- rle(as.integer(x)); son <- cumsum(r$lengths); bas <- son - r$lengths + 1
  k <- r$values == 1
  data.frame(bas = bas[k], son = son[k])
}

degerlendir <- function(S, A, t0 = T0, capa = 10) {
  TP <- FP <- TN <- FN <- 0; yil <- 0; alarmSay <- 0
  det <- c(); cum <- c(); clHit <- c(); clSans <- c()
  for (i in seq_along(S)) {
    s <- S[[i]]; a <- A[[i]]; n <- length(s$cases); w <- t0:n
    al <- s$al[w]; aa <- a[w] == 1
    TP <- TP + sum(aa & al == 1); FP <- FP + sum(aa & al == 0)
    TN <- TN + sum(!aa & al == 0); FN <- FN + sum(!aa & al == 1)
    yil <- yil + length(w) / 52; alarmSay <- alarmSay + sum(aa)
    fa_orani <- sum(aa & al == 0) / max(1, sum(al == 0))   # etiketsiz haftalarda alarm olasılığı

    # salgınlar: çapa haftasından itibaren 4 hafta içinde (çapa..çapa+3) alarm var mı?
    if (!is.null(s$ol) && !is.null(s$ob)) {
      o <- kosu(s$ol); o <- o[o$bas >= t0, , drop = FALSE]
      for (j in seq_len(nrow(o))) {
        hh <- o$bas[j]:o$son[j]
        ca <- hh[s$ob[hh] >= capa][1]
        if (is.na(ca)) next                              # çapaya ulaşmayan salgın sayılmaz
        ilk <- hh[a[hh] == 1][1]
        det <- c(det, !is.na(ilk) && ilk <= ca + 3)
        if (!is.na(ilk)) cum <- c(cum, sum(s$cases[o$bas[j]:ilk]))
      }
    }
    # küçük kümeler: küme süresince en az bir alarm (POD) ve şans beklentisi
    if (!is.null(s$cl)) {
      k <- kosu(s$cl); k <- k[k$bas >= t0, , drop = FALSE]
      for (j in seq_len(nrow(k))) {
        hh <- k$bas[j]:k$son[j]
        clHit <- c(clHit, any(a[hh] == 1))
        clSans <- c(clSans, 1 - (1 - fa_orani)^length(hh))
      }
    }
  }
  bol <- function(x, y) if (y > 0) x / y else NA_real_
  list(sens = bol(TP, TP + FN), spec = bol(TN, TN + FP), ppv = bol(TP, TP + FP), npv = bol(TN, TN + FN),
       faYear = bol(FP, yil), alarmSay = alarmSay,
       nOutbreak = length(det), det4 = if (length(det)) mean(det) else NA_real_,
       medCum = if (length(cum)) median(cum) else NA_real_,
       nCluster = length(clHit), clPOD = if (length(clHit)) mean(clHit) else NA_real_,
       clChance = if (length(clSans)) mean(clSans) else NA_real_)
}
