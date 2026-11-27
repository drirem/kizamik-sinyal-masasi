# Kızamık Sinyal Masası — Shiny sürümü
# Gerekli paketler: install.packages(c("shiny", "bslib", "surveillance", "MASS", "ggplot2", "DT"))
# Önce bir kez: source("hesapla.R")  → on_hesap.rds oluşur
# Sonra: shiny::runApp()
source("fonksiyonlar.R")   # eksik paketleri de kurar
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (!file.exists("on_hesap.rds")) stop("on_hesap.rds bulunamadı. Önce source(\"hesapla.R\") çalıştırın.")
PRE <- readRDS("on_hesap.rds")
SERI_SEC <- setNames(as.character(seq_along(PRE$S)), paste("Seri", sapply(PRE$S, `[[`, "sim")))
VARSAYILAN_ALG <- c("EARS-C1", "EARS-C3", "Farrington", "FarringtonFlexible", "GLR-NB")
KISA <- c("EARS-C1" = "C1", "EARS-C2" = "C2", "EARS-C3" = "C3", "Bayes" = "Bayes",
          "Farrington" = "Farr.", "FarringtonFlexible" = "F.Flex", "GLR-NB" = "GLR-NB")
RENK <- c(bar = "#6f8593", salgin = "#f3dcb4", kume = "#ddd8f2", dogru = "#c2253b", yanlis = "#d98a1c")

tema <- bs_theme(version = 5, primary = "#0f6e78", base_font = font_google("Public Sans"),
                 heading_font = font_google("Archivo"), code_font = font_google("IBM Plex Mono"))

durum_karti <- function(baslik, alt, sinyal, hafta) {
  div(class = "border rounded p-2", style = "min-width:150px;flex:1",
      div(strong(baslik)), div(class = "text-muted small", alt),
      span(class = paste("badge", if (sinyal) "text-bg-danger" else "text-bg-success"),
           if (sinyal) paste0("SİNYAL · ", hafta, ". hf") else "SAKİN"))
}

ui <- page_navbar(
  title = "Kızamık Sinyal Masası", theme = tema, id = "sekme",
  header = div(class = "container-fluid text-muted small pt-2",
               "Yedi erken uyarı algoritmasını yan yana çalıştıran karar destek prototipi · araştırma prototipi, simüle veri"),

  nav_panel("İzlem", value = "izlem", layout_sidebar(
    sidebar = sidebar(width = 270,
      selectInput("seri", "Seri", choices = SERI_SEC),
      sliderInput("yil", "Gösterilen dönem (yıl)", min = 1, max = 20, value = c(5, 20), step = 1, sep = ""),
      checkboxGroupInput("alg", "Algoritmalar", choices = setNames(ALGOS, ETIKET[ALGOS]), selected = VARSAYILAN_ALG),
      p(class = "small text-muted", "Grafikte bir haftanın üzerine gelin: vaka sayısı, etiket ve sinyal veren algoritmalar altta görünür.")),
    card(card_header(textOutput("grafik_baslik", inline = TRUE)),
         uiOutput("grafik_ui"),
         div(class = "small text-muted", style = "min-height:2.6em", htmlOutput("hover_bilgi")),
         div(class = "small",
             span(style = paste0("color:", RENK["bar"]), "■"), " Haftalık vaka (karekök ölçek)  ",
             span(style = paste0("color:", RENK["salgin"]), "■"), " Salgın dönemi  ",
             span(style = paste0("color:", RENK["kume"]), "■"), " Küçük küme  ",
             span(style = paste0("color:", RENK["dogru"]), "■"), " Doğru alarm  ",
             span(style = paste0("color:", RENK["yanlis"]), "■"), " Yanlış alarm")),
    h5(textOutput("durum_baslik", inline = TRUE)),
    p(class = "small text-muted", textOutput("durum_alt", inline = TRUE)),
    uiOutput("durum"),
    h5("Bu serideki performans", class = "mt-3"),
    tableOutput("seri_tablo")
  )),

  nav_panel("Performans", value = "performans", layout_sidebar(
    sidebar = sidebar(width = 270,
      selectInput("capa", "Zamanlılık çapası (salgına bağlı haftalık vaka ≥)", choices = c(5, 10, 20), selected = 10),
      p(class = "small text-muted", "Duyarlılık, özgüllük, PPD ve NPD hafta düzeyindedir. Zamanlılık, salgına bağlı vakanın ilk kez çapa eşiğine ulaştığı haftadan ölçülür. Küme POD'da parantez içi değer şans beklentisidir.")),
    h5(textOutput("perf_alt", inline = TRUE)),
    DTOutput("perf_tablo"),
    card(class = "mt-3", card_header("Duyarlılık ve yanlış alarm yükü (nokta büyüklüğü: PPD)"),
         plotOutput("dagilim", height = 360))
  )),

  nav_panel("Veri yükle", value = "yukle", layout_sidebar(
    sidebar = sidebar(width = 270,
      fileInput("dosya", "CSV dosyası", accept = c(".csv", "text/csv"), buttonLabel = "Seç…", placeholder = "dosya seçilmedi"),
      actionButton("ornek", "Örnek seriyle dene", class = "btn-outline-primary"),
      numericInput("t0", "Değerlendirme başlangıcı (hafta)", value = T0, min = 10),
      numericInput("alpha", "Farrington α", value = 0.05, min = 0.001, max = 0.2, step = 0.01),
      numericInput("carl", "GLR-NB eşiği (c.ARL)", value = 5, min = 1, max = 20, step = 0.5),
      actionButton("calistir", "Algoritmaları çalıştır", class = "btn-primary")),
    h5("Kendi haftalık serinizi analiz edin"),
    p(class = "small", "Gereken sütunlar: ", code("week"), " ve ", code("cases"), ". Bir ", code("label"),
      " sütunu (salgın haftası 1, değilse 0) eklerseniz duyarlılık, özgüllük ve PPD de hesaplanır. ",
      "Farrington ailesi ve GLR-NB için en az 5 yıllık (265 hafta) geçmiş gerekir; daha kısa serilerde yalnızca EARS ve Bayes çalışır."),
    pre("week,cases,label\n1,2,0\n2,0,0\n3,4,0\n…\n268,15,1"),
    uiOutput("yukle_mesaj"),
    tableOutput("yukle_tablo"),
    uiOutput("yukle_git")
  )),

  nav_panel("Yöntem", value = "yontem", div(class = "container-fluid", style = "max-width:900px", markdown("
### Veri
Türkiye kızamık verisine (WHO, 2012–2026) kalibre edilmiş TSIR modeliyle üretilen 100 seri, her biri 20 yıl × 52 hafta.
İlk 264 hafta taban dönemi; değerlendirme 265.–1040. haftalar.

### Algoritmalar ve ayarlar
| Algoritma | Ayarlar |
|---|---|
| EARS C1/C2/C3 | CDC tanımı; 7 haftalık taban, C2 ve C3'te 2 hafta boşluk; C > 3, C3 için Σmax(0, C2−1) > 2 |
| Bayes | `surveillance::bayes`, son 6 hafta, α = 0,05 |
| Farrington | `surveillance::farrington`, b = 5, w = 3, α = 0,05, yeniden ağırlıklandırma, trend, limit54 |
| FarringtonFlexible | `surveillance::farringtonFlexible`, 10 mevsim dilimi, son 26 hafta dışlanır, ağırlık eşiği 2,58, nbPlugin |
| GLR-NB | `surveillance::glrnb`; μ₀ ilk 5 yılda harmonik NB-GLM (|z| ≥ 2,58 haftalar yinelemeli dışlanır), c.ARL = 5 |

### Ölçütler
Duyarlılık, özgüllük, PPD, NPD (hafta düzeyi; altın standart salgın ya da küme etiketi) · yanlış alarm/yıl ·
4 hafta içinde tespit (çapa: salgına bağlı haftalık vaka ≥ 10) · tespitte kümülatif vaka (medyan) · küme POD ve şans beklentisi.

### Sınırlılıklar
Simüle veriyle değerlendirildi; gerçek bildirim verisiyle doğrulanmadı. Operasyonel sinyal kararı için kullanılmamalıdır.
")))
)

server <- function(input, output, session) {
  yuklenen <- reactiveVal(NULL)   # list(ad, s, alarmlar, t0)
  bekleyen <- reactiveVal(NULL)   # okunmuş ama çalıştırılmamış seri

  # ---- seçili seri ----
  cur <- reactive({
    if (identical(input$seri, "up")) { req(yuklenen()); return(yuklenen()) }
    i <- as.integer(input$seri)
    list(ad = names(SERI_SEC)[i], s = PRE$S[[i]], t0 = PRE$t0,
         alarmlar = lapply(setNames(ALGOS, ALGOS), function(a) PRE$alarmlar[[a]][[i]]))
  })
  observeEvent(cur(), {
    ny <- ceiling(length(cur()$s$cases) / 52)
    updateSliderInput(session, "yil", max = ny, value = c(max(1, floor(cur()$t0 / 52)), ny))
  })
  pencere <- reactive({
    n <- length(cur()$s$cases)
    c((input$yil[1] - 1) * 52 + 1, min(n, input$yil[2] * 52))
  })
  gorunen_alg <- reactive(intersect(input$alg, names(cur()$alarmlar)))

  output$grafik_baslik <- renderText(cur()$ad)
  output$grafik_ui <- renderUI(plotOutput("grafik", height = 330 + 24 * length(gorunen_alg()),
                                          hover = hoverOpts("grafik_hover", delay = 80, delayType = "throttle")))

  # vaka çubukları karekök ölçekte; alarm şeritleri aynı grafikte sıfırın altında (x ekseni hizalı kalsın diye)
  output$grafik <- renderPlot({
    s <- cur()$s; w <- pencere(); hafta <- w[1]:w[2]; algs <- gorunen_alg()
    y <- sqrt(s$cases[hafta]); ymax <- max(1, y); h <- ymax * 0.075
    etik <- rep("", length(hafta))
    if (!is.null(s$cl)) etik[s$cl[hafta] == 1] <- "kume"
    if (!is.null(s$ol)) etik[s$ol[hafta] == 1] <- "salgin"
    else if (!is.null(s$al)) etik[s$al[hafta] == 1] <- "salgin"   # yüklenen veride tek etiket sütunu
    bant <- data.frame(hafta = hafta, etik = etik)[etik != "", ]
    cubuk <- data.frame(hafta = hafta, y = y)
    serit <- do.call(rbind, lapply(seq_along(algs), function(k) {
      a <- cur()$alarmlar[[algs[k]]][hafta]
      if (!any(a == 1)) return(NULL)
      hh <- hafta[a == 1]
      tur <- if (is.null(s$al)) "dogru" else ifelse(s$al[hh] == 1, "dogru", "yanlis")
      data.frame(hafta = hh, y = -h * (k + 0.2), tur = tur)
    }))
    kirilim <- c(0, 5, 25, 100, 250, 500, 1000, 2000); kirilim <- kirilim[sqrt(kirilim) <= ymax]
    ybr <- c(sqrt(kirilim), -h * (seq_along(algs) + 0.2))
    ylab <- c(kirilim, KISA[algs])
    yil_bas <- seq(ceiling((w[1] - 1) / 52) * 52 + 1, w[2], by = 52)
    p <- ggplot() +
      geom_rect(data = bant, aes(xmin = hafta - 0.5, xmax = hafta + 0.5, ymin = 0, ymax = ymax * 1.03, fill = etik),
                show.legend = FALSE) +
      geom_col(data = cubuk, aes(x = hafta, y = y), fill = RENK["bar"], width = 1) +
      geom_hline(yintercept = -h * (seq_along(algs) + 0.2), colour = "grey88", linewidth = 0.3)
    if (!is.null(serit)) p <- p + geom_tile(data = serit, aes(x = hafta, y = y, fill = tur), width = 1, height = h * 0.7,
                                            show.legend = FALSE)
    if (cur()$t0 > w[1] && cur()$t0 < w[2])
      p <- p + geom_vline(xintercept = cur()$t0 - 0.5, linetype = "dashed", colour = "grey45") +
        annotate("text", x = cur()$t0 + 2, y = ymax, label = "izlem başlangıcı", hjust = 0, vjust = 1, size = 3.2, colour = "grey35")
    p + scale_fill_manual(values = c(salgin = unname(RENK["salgin"]), kume = unname(RENK["kume"]),
                                     dogru = unname(RENK["dogru"]), yanlis = unname(RENK["yanlis"]))) +
      scale_y_continuous(breaks = ybr, labels = ylab, expand = expansion(mult = c(0.01, 0.02))) +
      scale_x_continuous(breaks = yil_bas + 26, labels = paste0((yil_bas - 1) / 52 + 1, ". yıl"),
                         expand = c(0, 0)) +
      coord_cartesian(xlim = c(w[1] - 0.5, w[2] + 0.5)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
  }, res = 96)

  output$hover_bilgi <- renderUI({
    hv <- input$grafik_hover; if (is.null(hv)) return(NULL)
    s <- cur()$s; t <- round(hv$x); w <- pencere()
    if (t < w[1] || t > w[2]) return(NULL)
    lab <- if (!is.null(s$ol) && s$ol[t] == 1) "salgın" else if (!is.null(s$cl) && s$cl[t] == 1) "küme" else
      if (!is.null(s$al)) (if (s$al[t] == 1) "etiketli" else "sakin") else "etiket yok"
    sin <- ETIKET[names(Filter(function(a) a[t] == 1, cur()$alarmlar))]
    HTML(sprintf("<b>%d. hafta</b> (%d. yıl, %d. hf) · Vaka: <b>%d</b> · %s · %s", t, (t - 1) %/% 52 + 1,
                 (t - 1) %% 52 + 1, s$cases[t], lab, if (length(sin)) paste("Sinyal:", paste(sin, collapse = ", ")) else "Sinyal yok"))
  })

  # ---- dönem sonu durumu ----
  output$durum_baslik <- renderText(sprintf("Dönem sonu durumu: %d. hafta", pencere()[2]))
  output$durum_alt <- renderText({
    e <- pencere()[2]
    sprintf("Son 4 haftada (%d.–%d. hafta) sinyal veren algoritmalar. Haftalık vaka: %s",
            e - 3, e, paste(cur()$s$cases[(e - 3):e], collapse = ", "))
  })
  output$durum <- renderUI({
    e <- pencere()[2]
    div(class = "d-flex flex-wrap gap-2", lapply(names(cur()$alarmlar), function(a) {
      A <- cur()$alarmlar[[a]]; son <- which(A[max(1, e - 3):e] == 1)
      durum_karti(ETIKET[[a]], sprintf("son 52 hf: %d alarm", sum(A[max(1, e - 51):e])),
                  length(son) > 0, if (length(son)) max(1, e - 3) - 1 + max(son) else NA)
    }))
  })

  output$seri_tablo <- renderTable({
    s <- cur()$s
    validate(need(!is.null(s$al), "Bu seride etiket sütunu yok; yalnızca alarmlar gösterilir."))
    do.call(rbind, lapply(names(cur()$alarmlar), function(a) {
      m <- degerlendir(list(s), list(cur()$alarmlar[[a]]), cur()$t0, 10)
      data.frame(Algoritma = ETIKET[[a]], `Duyarlılık %` = pct(m$sens), `Özgüllük %` = pct(m$spec),
                 `PPD %` = pct(m$ppv), `Yanlış alarm/yıl` = fmt(m$faYear, 2), `Alarm sayısı` = m$alarmSay, check.names = FALSE)
    }))
  }, align = "lrrrrr", striped = TRUE)

  # ---- performans ----
  perf <- reactive({
    capa <- as.numeric(input$capa)
    lapply(setNames(ALGOS, ALGOS), function(a) degerlendir(PRE$S, PRE$alarmlar[[a]], PRE$t0, capa))
  }) |> bindCache(input$capa)

  output$perf_alt <- renderText({
    m <- perf()[[1]]
    sprintf("%d seri × %d hafta değerlendirme · %d salgın (çapa ≥%s vaka) · %d küme",
            length(PRE$S), length(PRE$S[[1]]$cases) - PRE$t0 + 1, m$nOutbreak, input$capa, m$nCluster)
  })
  output$perf_tablo <- renderDT({
    P <- perf()
    d <- data.frame(Algoritma = unname(ETIKET[ALGOS]),
      `Duyarlılık %` = sapply(P, function(m) round(100 * m$sens, 1)),
      `Özgüllük %` = sapply(P, function(m) round(100 * m$spec, 1)),
      `PPD %` = sapply(P, function(m) round(100 * m$ppv, 1)),
      `NPD %` = sapply(P, function(m) round(100 * m$npv, 1)),
      `Yanlış alarm/yıl` = sapply(P, function(m) round(m$faYear, 2)),
      `4 hf içinde tespit %` = sapply(P, function(m) round(100 * m$det4, 1)),
      `Tespitte kümülatif vaka` = sapply(P, function(m) m$medCum),
      `Küme POD %` = sapply(P, function(m) round(100 * m$clPOD, 1)),
      `Küme POD şans %` = sapply(P, function(m) round(100 * m$clChance, 1)),
      check.names = FALSE, row.names = NULL)
    datatable(d, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = TRUE,
              language = list(decimal = ",")), class = "compact stripe") |>
      formatRound(2:10, digits = 1, mark = ".", dec.mark = ",") |>
      formatRound(6, digits = 2, mark = ".", dec.mark = ",")
  })
  output$dagilim <- renderPlot({
    P <- perf()
    d <- data.frame(alg = unname(ETIKET[ALGOS]), ya = sapply(P, `[[`, "faYear"), sens = sapply(P, `[[`, "sens"),
                    ppv = sapply(P, `[[`, "ppv"), model = ALGOS %in% UZUN_SERI_GEREKEN)
    ggplot(d, aes(ya, sens)) +
      geom_point(aes(size = ppv, colour = model), alpha = 0.85, show.legend = FALSE) +
      geom_text(aes(label = alg), hjust = -0.18, size = 3.6) +
      scale_colour_manual(values = c(`TRUE` = "#0f6e78", `FALSE` = "#8a98a2")) +
      scale_size(range = c(3, 9)) +
      scale_x_continuous("Yanlış alarm / yıl", limits = c(0, 6.5), breaks = 0:6) +
      scale_y_continuous("Duyarlılık", labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 0.7)) +
      theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank())
  }, res = 96)

  # ---- veri yükleme ----
  mesaj <- function(txt, hata = FALSE)
    output$yukle_mesaj <- renderUI(div(class = paste("alert", if (hata) "alert-danger" else "alert-info", "py-2"), txt))

  csv_oku <- function(yol, ad) {
    ilk <- readLines(yol, n = 1, warn = FALSE)
    sep <- if (grepl(";", ilk) && !grepl(",", ilk)) ";" else ","
    d <- read.csv(yol, sep = sep, dec = if (sep == ";") "," else ".", check.names = FALSE)
    nm <- tolower(trimws(names(d)))
    iw <- match(TRUE, nm %in% c("week", "hafta")); ic <- match(TRUE, nm %in% c("cases", "vaka", "count", "n"))
    il <- match(TRUE, nm %in% c("label", "etiket", "any_label", "outbreak"))
    if (is.na(ic)) stop("\"cases\" (ya da \"vaka\") sütunu bulunamadı. İlk satır sütun adlarını içermeli.")
    if (!is.na(iw)) d <- d[order(d[[iw]]), ]
    vaka <- pmax(0L, as.integer(round(as.numeric(d[[ic]]))))
    vaka[is.na(vaka)] <- 0L
    list(ad = ad, s = list(cases = vaka, al = if (!is.na(il)) as.integer(d[[il]] != 0) else NULL))
  }
  bekleyen_ayarla <- function(b) {
    n <- length(b$s$cases); bekleyen(b)
    if (n < 265) updateNumericInput(session, "t0", value = min(input$t0, max(10, n - 1)))
    mesaj(sprintf("%s: %d hafta okundu%s. %s", b$ad, n, if (is.null(b$s$al)) ", etiket sütunu yok" else ", etiket sütunu var",
                  if (n < 265) "5 yıldan kısa: yalnızca EARS ve Bayes çalışacak." else "Tüm algoritmalar çalışabilir."))
  }
  observeEvent(input$dosya, {
    tryCatch(bekleyen_ayarla(csv_oku(input$dosya$datapath, input$dosya$name)),
             error = function(e) { bekleyen(NULL); mesaj(conditionMessage(e), TRUE) })
  })
  observeEvent(input$ornek, {
    s <- PRE$S[[7]]
    bekleyen_ayarla(list(ad = paste("Örnek: Seri", s$sim), s = list(cases = s$cases, al = s$al)))
    updateNumericInput(session, "t0", value = T0)
  })
  observeEvent(input$calistir, {
    b <- bekleyen()
    if (is.null(b)) { mesaj("Önce bir CSV dosyası seçin ya da örnek seriyi yükleyin.", TRUE); return() }
    n <- length(b$s$cases); t0 <- min(max(10, round(input$t0)), n)
    algs <- if (n >= 265 && t0 >= 265) ALGOS else setdiff(ALGOS, UZUN_SERI_GEREKEN)
    sonuc <- list()
    withProgress(message = "Algoritmalar çalışıyor", value = 0, {
      for (a in algs) {
        incProgress(0, detail = ETIKET[[a]])
        sonuc[[a]] <- tryCatch(algoritma_calistir(a, b$s$cases, t0, alpha = input$alpha, carl = input$carl),
                               error = function(e) { showNotification(sprintf("%s çalışmadı: %s", ETIKET[[a]], conditionMessage(e)),
                                                                      type = "warning"); NULL })
        incProgress(1 / length(algs))
      }
    })
    sonuc <- Filter(Negate(is.null), sonuc)
    yuklenen(list(ad = b$ad, s = b$s, t0 = t0, alarmlar = sonuc))
    mesaj(sprintf("Tamamlandı: %d algoritma, %d haftalık izlem dönemi.", length(sonuc), n - t0 + 1))
    updateSelectInput(session, "seri", choices = c("Yüklenen veri" = "up", SERI_SEC), selected = "up")
  })
  output$yukle_tablo <- renderTable({
    u <- yuklenen(); req(u)
    e <- length(u$s$cases)
    do.call(rbind, lapply(names(u$alarmlar), function(a) {
      A <- u$alarmlar[[a]]
      r <- data.frame(Algoritma = ETIKET[[a]], `Alarm sayısı` = sum(A[u$t0:e]),
                      `Son 4 hafta` = if (any(A[(e - 3):e] == 1)) "SİNYAL" else "SAKİN", check.names = FALSE)
      if (!is.null(u$s$al)) {
        m <- degerlendir(list(list(cases = u$s$cases, al = u$s$al)), list(A), u$t0)
        r <- cbind(r, data.frame(`Duyarlılık %` = pct(m$sens), `Özgüllük %` = pct(m$spec), `PPD %` = pct(m$ppv),
                                 `Yanlış alarm/yıl` = fmt(m$faYear, 2), check.names = FALSE))
      }
      r
    }))
  }, striped = TRUE)
  output$yukle_git <- renderUI({ req(yuklenen()); actionButton("git_izlem", "İzlem sekmesinde aç", class = "btn-primary") })
  observeEvent(input$git_izlem, nav_select("sekme", "izlem"))
}

shinyApp(ui, server)
