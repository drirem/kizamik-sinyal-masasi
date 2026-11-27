# Kızamık erken uyarı algoritmaları — R ile alarm üretimi (bildiri Tablo 3 tanımları)
# Girdi : kizamik_sim_100.csv (sim, week, cases, ...)  — bu betikle aynı klasörde
# Çıktı : alarmlar_R.csv (sim, week, 7 algoritma sütunu) — bildiri_sayi_kontrolu.Rmd bunu okur
#
# Gerekli paketler (bir kez):  install.packages(c("surveillance", "dplyr", "MASS"))
# RStudio'da: Session > Set Working Directory > To Source File Location, sonra Source.
#
# v2 (31.10.2026):
#  - EARS C1–C3: paketteki earsC yerine CDC tanımı (Tablo 3): taban 7 hf, C2/C3'te 2 hf boşluk,
#    SS alt sınırı 0,2; C1/C2 alarm: C > 3; C3 alarm: son 3 haftada Σ max(0, C2 − 1) > 2
#  - GLR-NB tabanı: ilk 260 haftada |z| ≥ 2,58 haftalar YİNELEMELİ dışlanır (tek geçiş değil)
#  - Bayes, Farrington, FarringtonFlexible değişmedi. alarmlar_R.csv zaten varsa bu üçü
#    yeniden hesaplanmaz, mevcut sütunlar kullanılır (HEPSINI_YENIDEN <- TRUE ile hepsi yeniden).

library(surveillance)   # >= 1.20
library(dplyr)

HEPSINI_YENIDEN <- FALSE

if (!file.exists("kizamik_sim_100.csv"))
  stop("kizamik_sim_100.csv bu klasörde yok: ", getwd(), call. = FALSE)

d   <- read.csv("kizamik_sim_100.csv")
T0  <- 265                         # değerlendirme 265. haftadan başlar
rng <- T0:1040
ALGOS <- c("EARS-C1", "EARS-C2", "EARS-C3", "Bayes", "Farrington", "FarringtonFlexible", "GLR-NB")

# ---- EARS, CDC tanımı -------------------------------------------------------
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
  list(mu0 = as.numeric(mu), alpha = 1 / m0$theta, n_dislanan = sum(!kalan), mu_ort = mean(mu[1:52]))
}

eski <- if (!HEPSINI_YENIDEN && file.exists("alarmlar_R.csv")) read.csv("alarmlar_R.csv", check.names = FALSE) else NULL
if (!is.null(eski)) message("Bayes/Farrington/FarringtonFlexible mevcut alarmlar_R.csv'den alınıyor.")

taban_ozet <- list()
run_one <- function(s) {
  g  <- d |> dplyr::filter(sim == s) |> dplyr::arrange(week)
  st <- sts(observed = g$cases, start = c(2000, 1), frequency = 52)

  e <- ears_cdc(g$cases)

  gb <- glr_taban(g$cases)
  taban_ozet[[as.character(s)]] <<- c(sim = s, dislanan = gb$n_dislanan, mu0_ort = gb$mu_ort)
  glr <- glrnb(st, control = list(range = rng, mu0 = gb$mu0[rng], alpha = gb$alpha,
                                  c.ARL = 5, dir = "inc", change = "intercept", ret = "value"))

  out <- data.frame(sim = s, week = rng,
                    `EARS-C1` = e$C1[rng], `EARS-C2` = e$C2[rng], `EARS-C3` = e$C3[rng],
                    `GLR-NB` = as.integer(alarms(glr)[, 1]), check.names = FALSE)

  if (is.null(eski)) {
    far  <- farrington(st, control = list(range = rng, b = 5, w = 3, alpha = 0.05,
                                          reweight = TRUE, trend = TRUE, limit54 = c(5, 4)))
    flex <- farringtonFlexible(st, control = list(range = rng, b = 5, w = 3, noPeriods = 10,
                                   pastWeeksNotIncluded = 26, weightsThreshold = 2.58,
                                   thresholdMethod = "nbPlugin", alpha = 0.05,
                                   trend = TRUE, reweight = TRUE, limit54 = c(5, 4)))
    bay  <- bayes(st, control = list(range = rng, b = 0, w = 6, actY = TRUE, alpha = 0.05))
    out$Bayes <- as.integer(alarms(bay)[, 1])
    out$Farrington <- as.integer(alarms(far)[, 1])
    out$FarringtonFlexible <- as.integer(alarms(flex)[, 1])
  } else {
    out <- out |> dplyr::left_join(eski |> dplyr::filter(sim == s) |> dplyr::select(sim, week, Bayes, Farrington, FarringtonFlexible),
                            by = c("sim", "week"))
  }
  out[c("sim", "week", ALGOS)]
}

sims <- sort(unique(d$sim))
res <- bind_rows(lapply(seq_along(sims), function(i) {
  message(sprintf("Seri %d / %d", i, length(sims)))
  suppressWarnings(run_one(sims[i]))
}))
write.csv(res, "alarmlar_R.csv", row.names = FALSE)

# ---- Kontrol ---------------------------------------------------------------
# Paneldeki duyarlılık/özgüllükten beklenen alarm oranı: Se*p + (1-Sp)*(1-p), p = 279,5/776
p <- 279.5 / 776
panel <- c(`EARS-C1` = .100, `EARS-C2` = .233, `EARS-C3` = .412, Bayes = .324,
           Farrington = .382, FarringtonFlexible = .464, `GLR-NB` = .578)
panel_sp <- c(.947, .946, .839, .904, .980, .950, .982)
beklenen <- panel * p + (1 - panel_sp) * (1 - p)

cat("Satır:", nrow(res), "(beklenen 77600)\n")
print(data.frame(NA_alarm      = sapply(res[ALGOS], function(x) sum(is.na(x))),
                 alarm_orani   = round(sapply(res[ALGOS], mean, na.rm = TRUE), 3),
                 panel_beklenen = round(beklenen, 3)))

tb <- do.call(rbind, taban_ozet)
cat("\nGLR-NB tabanı: dışlanan hafta (medyan) =", median(tb[, "dislanan"]),
    "| ilk yıl ortalama mu0 (medyan) =", round(median(tb[, "mu0_ort"]), 2), "(gerçek taban ~1,8)\n")
