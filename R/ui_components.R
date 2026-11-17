# Reusable UI building blocks and dependency-free SVG/HTML charts. All styling is in www/styles.css.
library(shiny)

js_set <- function(id, value) sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", id, value)
nav_btn <- function(id, value, label, on = FALSE, cls = "") tags$button(type = "button", class = paste(cls, if (on) "on"), onclick = js_set(id, value), label)
grade_class <- function(g) switch(g, "Ready" = "ready", "Almost ready" = "almost", "Needs work" = "work", "not")

score_ring <- function(score, size = 150, label = NULL) {
  col <- if (score >= 85) "var(--ok)" else if (score >= 70) "var(--warn)" else "var(--bad)"
  r <- 52; circ <- 2 * pi * r; off <- circ * (1 - score / 100)
  HTML(sprintf('<svg class="ring" width="%d" height="%d" viewBox="0 0 120 120" role="img" aria-label="Score %d out of 100"><circle class="trk" cx="60" cy="60" r="%d" fill="none" stroke-width="11"/>
    <circle cx="60" cy="60" r="%d" fill="none" style="stroke:%s" stroke-width="11" stroke-linecap="round" stroke-dasharray="%.1f" stroke-dashoffset="%.1f" transform="rotate(-90 60 60)"/>
    <text class="num" x="60" y="66" text-anchor="middle" font-size="34">%d</text>%s</svg>', size, size, score, r, r, col, circ, off, score,
    if (!is.null(label)) sprintf('<text class="sub" x="60" y="84" text-anchor="middle" font-size="9">%s</text>', label) else ""))
}

stat_box <- function(label, value, tone = "") div(class = paste("stat", tone), div(class = "v", value), div(class = "l", label))
msg_ui <- function(m) if (is.null(m)) NULL else div(class = paste("msg", m$type), role = "alert", m$text)

fmt_cell <- function(x) {
  if (inherits(x, "Date")) return(as.character(x))
  if (is.numeric(x)) return(ifelse(is.na(x), "", format(round(x, 3), trim = TRUE, scientific = FALSE)))
  x <- as.character(x); x[is.na(x)] <- ""; x
}
tbl_ui <- function(df, n = 12) {
  if (is.null(df) || !nrow(df)) return(div(class = "empty", "Nothing to show."))
  d <- utils::head(df, n)
  div(class = "scroll", tags$table(class = "t", tags$thead(tags$tr(lapply(names(d), tags$th))),
    tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(lapply(d, function(col) tags$td(fmt_cell(col[i]))))))))
}

# Interactive table: search, sort, pages, per-column filters, copy / CSV / Excel export.
dt_table <- function(df, page = 10, filter = TRUE, select = FALSE, buttons = TRUE) {
  df <- as.data.frame(df); if (!nrow(df)) df <- data.frame(Message = "No records yet.")
  DT::datatable(df, rownames = FALSE, filter = if (filter) "top" else "none", extensions = if (buttons) "Buttons" else character(),
    selection = if (select) list(mode = "single") else "none", class = "compact stripe",
    options = list(pageLength = page, lengthMenu = c(5, 10, 25, 50, 100), scrollX = TRUE, autoWidth = FALSE, dom = if (buttons) "Bfrtip" else "frtip",
                   buttons = if (buttons) list("copy", "csv", "excel") else NULL, language = list(search = "Search:", emptyTable = "No records yet.")))
}

ICON <- list(
  analyze = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 20V10M10 20V4M16 20v-7M22 20H2"/></svg>',
  predict = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M5 12h4l2-6 3 12 2-6h3"/></svg>',
  drift   = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 17l5-5 4 4 8-9M15 7h5v5"/></svg>',
  history = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="9"/><path d="M12 7v5l3 2"/></svg>',
  admin   = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 3l8 3v6c0 5-3.5 8-8 9-4.5-1-8-4-8-9V6z"/></svg>',
  settings= '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="3"/><path d="M19 12a7 7 0 0 0-.1-1.2l2-1.5-2-3.4-2.3.9a7 7 0 0 0-2-1.2L14 3h-4l-.6 2.6a7 7 0 0 0-2 1.2l-2.3-.9-2 3.4 2 1.5a7 7 0 0 0 0 2.4l-2 1.5 2 3.4 2.3-.9a7 7 0 0 0 2 1.2L10 21h4l.6-2.6a7 7 0 0 0 2-1.2l2.3.9 2-3.4-2-1.5c.1-.4.1-.8.1-1.2z"/></svg>',
  help    = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="9"/><path d="M9.5 9a2.5 2.5 0 1 1 3.5 2.3c-.7.4-1 1-1 1.7M12 17h.01"/></svg>',
  more    = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><circle cx="5" cy="12" r="1"/><circle cx="12" cy="12" r="1"/><circle cx="19" cy="12" r="1"/></svg>',
  install = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 4v11M7.5 10.5L12 15l4.5-4.5"/><path d="M4 18.5h16"/></svg>',
  upload  = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 16V4M7 9l5-5 5 5"/><path d="M4 15v3a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-3"/></svg>')

GOOGLE_G <- HTML('<svg width="20" height="20" viewBox="0 0 48 48" aria-hidden="true"><path fill="#EA4335" d="M24 9.5c3.5 0 6.6 1.2 9.1 3.6l6.8-6.8C35.8 2.4 30.3 0 24 0 14.6 0 6.5 5.4 2.6 13.2l7.9 6.1C12.4 13.6 17.7 9.5 24 9.5z"/><path fill="#4285F4" d="M46.5 24.5c0-1.6-.1-3.1-.4-4.5H24v9h12.7c-.6 3-2.3 5.5-4.8 7.2l7.5 5.8c4.4-4.1 7.1-10.1 7.1-17.5z"/><path fill="#FBBC05" d="M10.5 28.7A14.5 14.5 0 0 1 9.5 24c0-1.6.3-3.2.8-4.7l-7.9-6.1A24 24 0 0 0 0 24c0 3.9.9 7.5 2.6 10.8l7.9-6.1z"/><path fill="#34A853" d="M24 48c6.5 0 11.9-2.1 15.9-5.8l-7.5-5.8c-2.1 1.4-4.9 2.3-8.4 2.3-6.3 0-11.6-4.1-13.5-9.8l-7.9 6.1C6.5 42.6 14.6 48 24 48z"/></svg>')

SAMPLES <- list(
  jaipur_house_prices = list(title = "Jaipur house prices", target = "price_lakh", desc = "Regression. Prices stored as text with the rupee sign, mixed-case localities, an impossible area."),
  campus_placement    = list(title = "Campus placement", target = "placed", desc = "Classification. Contains student emails and mobile numbers (privacy), missing scores, outliers."),
  loan_default        = list(title = "Loan default risk", target = "defaulted", desc = "Rare defaults, income as 'Rs. 45,000', and a column that secretly leaks the answer."),
  telecom_churn       = list(title = "Telecom churn", target = "churn", desc = "Which customers leave? Blank charges and repeated rows."),
  heart_screening     = list(title = "Heart screening", target = "heart_disease", desc = "Question marks for missing values and impossible zero cholesterol."),
  upi_transactions    = list(title = "UPI fraud check", target = "is_fraud", desc = "Dates in two formats and very rare fraud cases.")
)

# ---------------- charts (inline SVG / HTML, theme aware) ----------------
PALETTE <- c("#0F766E", "#D97706", "#B42318", "#52606D", "#8A5A2B", "#157F3B")

svg_roc <- function(rocs, aucs = NULL) {
  if (!length(rocs)) return(NULL)
  w <- 360; h <- 300; pad <- 36; X <- function(v) pad + v * (w - pad - 10); Y <- function(v) h - pad - v * (h - pad - 12)
  lines <- vapply(seq_along(rocs), function(i) { d <- rocs[[i]]; pts <- paste(sprintf("%.1f,%.1f", X(d$fpr), Y(d$tpr)), collapse = " ")
    sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="2"/>', pts, PALETTE[(i - 1) %% length(PALETTE) + 1]) }, "")
  leg <- vapply(seq_along(rocs), function(i) sprintf('<span><i style="background:%s"></i>%s%s</span>', PALETTE[(i - 1) %% length(PALETTE) + 1], htmltools::htmlEscape(names(rocs)[i]),
                if (!is.null(aucs) && !is.na(aucs[names(rocs)[i]])) sprintf(" (AUC %.2f)", aucs[names(rocs)[i]]) else ""), "")
  HTML(sprintf('<svg class="svgchart" viewBox="0 0 %d %d" role="img" aria-label="ROC curves"><line class="ax" x1="%d" y1="%d" x2="%d" y2="%d"/><line class="ax" x1="%d" y1="%d" x2="%d" y2="%d"/>
    <line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#94A3B8" stroke-dasharray="4 4"/>%s<text x="%d" y="%d" text-anchor="middle">False positive rate</text><text x="10" y="%d" transform="rotate(-90 10 %d)" text-anchor="middle">True positive rate</text></svg><div class="legend">%s</div>',
    w, h, pad, h - pad, w - 10, h - pad, pad, h - pad, pad, 12, X(0), Y(0), X(1), Y(1), paste(lines, collapse = ""), w / 2, h - 8, h / 2, h / 2, paste(leg, collapse = "")))
}

svg_scatter <- function(x, y, group = NULL, flag = NULL, w = 520, h = 320) {
  if (is.null(flag)) flag <- rep(FALSE, length(x))
  pad <- 30; rx <- range(x); ry <- range(y); sx <- function(v) pad + (v - rx[1]) / max(1e-9, diff(rx)) * (w - 2 * pad); sy <- function(v) h - pad - (v - ry[1]) / max(1e-9, diff(ry)) * (h - 2 * pad)
  gi <- if (is.null(group)) rep(1L, length(x)) else as.integer(factor(group)); col <- PALETTE[(gi - 1) %% length(PALETTE) + 1]
  pts <- sprintf('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s" fill-opacity="%s"%s/>', sx(x), sy(y), ifelse(flag, "4.2", "2.8"), col, ifelse(flag, "1", "0.6"), ifelse(flag, ' stroke="#DC2626" stroke-width="1.6"', ""))
  HTML(sprintf('<svg class="svgchart" viewBox="0 0 %d %d" role="img"><line class="ax" x1="%d" y1="%d" x2="%d" y2="%d"/><line class="ax" x1="%d" y1="%d" x2="%d" y2="%d"/>%s<text x="%d" y="%d" text-anchor="middle">Component 1</text><text x="10" y="%d" transform="rotate(-90 10 %d)" text-anchor="middle">Component 2</text></svg>',
    w, h, pad, h - pad, w - 6, h - pad, pad, h - pad, pad, 6, paste(pts, collapse = ""), w / 2, h - 8, h / 2, h / 2))
}

svg_trend <- function(d) {
  w <- 380; h <- 190; pad <- 24; n <- nrow(d); mx <- max(1, d$ok + d$bad); bw <- (w - 2 * pad) / n
  bars <- vapply(seq_len(n), function(i) { x <- pad + (i - 1) * bw; hok <- d$ok[i] / mx * (h - 2 * pad); hbad <- d$bad[i] / mx * (h - 2 * pad)
    sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="2" fill="#059669"><title>%s: %d successful</title></rect><rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="2" fill="#DC2626"><title>%s: %d failed</title></rect>',
      x + bw * .12, h - pad - hok, bw * .36, hok, d$day[i], d$ok[i], x + bw * .52, h - pad - hbad, bw * .36, hbad, d$day[i], d$bad[i]) }, "")
  lab <- vapply(seq(1, n, length.out = min(n, 5)) |> round() |> unique(), function(i) sprintf('<text x="%.1f" y="%d" text-anchor="middle">%s</text>', pad + (i - .5) * bw, h - 8, substr(d$day[i], 6, 10)), "")
  HTML(sprintf('<svg class="svgchart" viewBox="0 0 %d %d" role="img" aria-label="Sign-ins per day"><line class="ax" x1="%d" y1="%d" x2="%d" y2="%d"/>%s%s<text x="%d" y="14">max %d / day</text></svg><div class="legend"><span><i style="background:#059669"></i>Successful</span><span><i style="background:#DC2626"></i>Failed</span></div>',
    w, h, pad, h - pad, w - pad, h - pad, paste(bars, collapse = ""), paste(lab, collapse = ""), pad, mx))
}

heat_cell <- function(v, lo = -1, hi = 1) {
  a <- min(1, abs(v) / max(abs(c(lo, hi)))); col <- if (v >= 0) "52,165,131" else "232,105,92"
  sprintf("background:rgba(%s,%.2f)", col, 0.12 + 0.7 * a)
}
heatmap_ui <- function(cm) {
  nm <- colnames(cm); short <- ifelse(nchar(nm) > 11, paste0(substr(nm, 1, 10), "…"), nm)
  div(class = "scroll", tags$table(class = "heat", tags$thead(tags$tr(tags$th(""), lapply(short, tags$th))),
    tags$tbody(lapply(seq_len(nrow(cm)), function(i) tags$tr(tags$th(short[i]), lapply(seq_len(ncol(cm)), function(j) tags$td(style = heat_cell(cm[i, j]), title = paste(nm[i], "vs", nm[j]), sprintf("%.2f", cm[i, j]))))))))
}
cm_ui <- function(cm) {
  mx <- max(1, max(cm)); lv <- colnames(cm)
  tags$table(class = "cm", tags$thead(tags$tr(tags$th(""), lapply(lv, function(l) tags$th(paste("Pred:", l))))),
    tags$tbody(lapply(seq_len(nrow(cm)), function(i) tags$tr(tags$th(paste("Actual:", rownames(cm)[i])),
      lapply(seq_len(ncol(cm)), function(j) tags$td(style = sprintf("background:rgba(%s,%.2f)", if (i == j) "52,165,131" else "232,105,92", 0.08 + 0.7 * cm[i, j] / mx), cm[i, j]))))))
}
hbars <- function(labels, values, vmax = NULL, fmt = function(v) format(round(v, 3)), tone = "") {
  vmax <- vmax %||% max(values, 1e-9)
  lapply(seq_along(labels), function(i) div(class = "cat", span(labels[i]), div(class = "bar", tags$i(class = tone, style = sprintf("width:%d%%", max(1, round(100 * max(0, values[i]) / vmax))))), span(class = "muted small", fmt(values[i]))))
}
