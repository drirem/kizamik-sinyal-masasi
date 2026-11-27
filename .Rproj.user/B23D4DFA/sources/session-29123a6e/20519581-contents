# 100 simüle seride yedi algoritmayı BİR KEZ çalıştırır ve sonucu on_hesap.rds dosyasına kaydeder.
# Uygulama (app.R) bu dosyayı okur; böylece her açılışta yeniden hesaplanmaz.
# Süre: FarringtonFlexible her hafta ayrı model kurduğu için en yavaş adımdır (bilgisayara göre 10–40 dk).
# Çalıştırma: proje (.Rproj) açıkken konsola source("hesapla.R")

source("fonksiyonlar.R")

S <- seri_oku("kizamik_sim_100.csv")
alarmlar <- list()
for (a in ALGOS) {
  t1 <- Sys.time()
  alarmlar[[a]] <- lapply(seq_along(S), function(i) {
    if (i %% 20 == 0) message(sprintf("  %s: %d/%d seri", a, i, length(S)))
    algoritma_calistir(a, S[[i]]$cases, T0)
  })
  message(sprintf("%-20s tamamlandı (%.0f sn)", a, as.numeric(difftime(Sys.time(), t1, units = "secs"))))
}
saveRDS(list(S = S, alarmlar = alarmlar, t0 = T0), "on_hesap.rds")

# Özet tablo: panel ve Python (v1) sonuçlarıyla karşılaştırmak için
ozet <- do.call(rbind, lapply(ALGOS, function(a) {
  m <- degerlendir(S, alarmlar[[a]], T0, 10)
  data.frame(Algoritma = ETIKET[[a]], Duyarlilik = pct(m$sens), Ozgulluk = pct(m$spec), PPD = pct(m$ppv),
             NPD = pct(m$npv), YA_yil = fmt(m$faYear, 2), Tespit_4hf = pct(m$det4), Kum_vaka = fmt(m$medCum, 1),
             Kume_POD = sprintf("%s (%s)", pct(m$clPOD), pct(m$clChance)))
}))
print(ozet, row.names = FALSE)
write.csv(ozet, "performans_R.csv", row.names = FALSE)

# Hafta düzeyinde alarm dosyası (Python alarmlar.csv ile aynı yapı)
uzun <- do.call(rbind, lapply(seq_along(S), function(i) {
  w <- T0:length(S[[i]]$cases)
  cbind(data.frame(sim = S[[i]]$sim, week = w),
        as.data.frame(lapply(setNames(ALGOS, ALGOS), function(a) alarmlar[[a]][[i]][w]), check.names = FALSE))
}))
write.csv(uzun, "alarmlar_R.csv", row.names = FALSE)
message("Kaydedildi: on_hesap.rds, performans_R.csv, alarmlar_R.csv")
