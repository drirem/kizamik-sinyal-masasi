# Kızamık Sinyal Masası

Yedi erken uyarı algoritmasını (EARS C1–C3, Bayes, Farrington, FarringtonFlexible, GLR-NB) Türkiye kızamık verisine kalibre edilmiş 100 simüle seride karşılaştıran Shiny karar destek prototipi. Araştırma prototipidir; operasyonel sinyal kararı için kullanılmamalıdır.

## İlk kurulum (bir kez)
1. `kizella.Rproj` dosyasına çift tıkla (RStudio bu klasörde açılır).
2. Konsola paketleri kur:
   ```r
   install.packages(c("shiny", "bslib", "surveillance", "MASS", "ggplot2", "DT", "dplyr"))
   ```

## Uygulamayı açmak
`on_hesap.rds` klasörde hazır, yeniden hesaplamaya gerek yok:
```r
shiny::runApp()
```
(ya da `app.R` dosyasını açıp sağ üstteki **Run App** düğmesine bas)

## Dosyalar
| Dosya | Görevi |
|---|---|
| `fonksiyonlar.R` | Algoritmalar (EARS C1–C3, Bayes, Farrington, FarringtonFlexible, GLR-NB) ve performans ölçütleri |
| `hesapla.R` | 100 seride tüm algoritmaları çalıştırır → `on_hesap.rds`, `performans_R.csv`, `alarmlar_R.csv` (10–40 dk) |
| `app.R` | Shiny uygulaması |
| `dogrulama_surveillance (4).R` | Bildiri Tablo 3 bağımsız kontrol betiği |
| `kizamik_sim_100.csv` | 100 simüle seri × 1040 hafta |

Algoritma ayarı değişirse önce `source("hesapla.R")`, sonra `shiny::runApp()`.
