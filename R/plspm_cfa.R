#' plspm CFA
#'
#' @param plsres_boot bootstrap plspm data
#' @param type all, cfa1, cfa2, item, CRAVE, fl1981, htmt, f2. total_effect
#' @param axis_x 1.2 setting efffecbar text size
#'
#' @param web Logical; default TRUE opens a standalone HTML report with all
#'   results, regardless of type. FALSE returns the selected R result.
#'
#' @return With web = FALSE, the result selected by type. With web = TRUE,
#'   invisibly returns all results with the report path in attribute web_file.
#'   The report supports table copying and HTML, Markdown and PNG downloads.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(satisfaction)
#' sat_path = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY"))
#' )
#' sat_path
#'
#' # blokcs
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#'
#' sat_blocks1
#'
#' # apply plspm
#' # satpls = plspm(satisfaction,
#' #                path_matrix = sat_path,
#' #                blocks = sat_blocks1,
#' #                scaled = FALSE)
#'
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
#' ##
#' satpls_boot%>%plspm_cfa(web = TRUE)
#' result <- plspm_cfa(satpls_boot, type = "CRAVE", web = FALSE)
#' satpls_boot%>%plspm_cfa("res", web = FALSE)
#'
#' satpls_boot%>%plspm_cfa(type="cfa1", web = FALSE)
#' satpls_boot%>%plspm_cfa(type="cfa2", web = FALSE)
#' satpls_boot%>%plspm_cfa(type="cor", web = FALSE)
#' satpls_boot%>%plspm_cfa(type="model", web = FALSE)
#' satpls_boot%>%plspm_cfa(type="inner_model", web = FALSE) %>%plspm_inner_model_sig()
#' satpls_boot%>%plspm_cfa(type="outer_model", web = FALSE)
#' satpls_boot%>%plspm_cfa(type="effect_bar", web = FALSE)
#' }
#'
#'
plspm_cfa = function(plsres_boot, type="all", axis_x=1.2, web=TRUE){
  if (!is.logical(web) || length(web) != 1L || is.na(web)) {
    stop("web must be TRUE or FALSE.")
  }
  if (web && !requireNamespace("htmltools", quietly = TRUE)) {
    stop("Install htmltools to use plspm_cfa(web = TRUE).")
  }
  if (is.null(plsres_boot$boot) || !is.list(plsres_boot$boot)) {
    stop("A bootstrapped plspm result is required (boot.val = TRUE).")
  }
  # summary
  res_boot = lapply(plsres_boot$boot,
                    function(x){ x%>%
                        row2col("relationships")%>%
                        Round(3)%>%
                        add_t_sig(3,4,5,T,ns="")%>%
                        unite_ci() %>%
                        rename(요인적재량= Original,
                               평균계수= Mean.Boot,
                               표준오차= Std.Error,
                               관계 = relationships )
                      #  dplyr::select(-Original)
                    })


  plsres_boot = plsres_boot

  weight = res_boot$weight
  loading = res_boot$loading
  path = res_boot$path
  rsq= res_boot$rsq
  total.efs = res_boot$total.efs

  # indicator = plspm_loadings(res_boot$boot$loading)
  item = res_boot$loadings %>%
    dplyr::rename(신뢰구간=`95%CI`)%>%
    dplyr::select(-평균계수)%>%
    tidyr::separate(관계,c("잠재변수","측정변수"), sep="-")

  CR_AVE = plspm_CRAVE(plsres_boot)

  CR_AVE_partial = plspm_CRAVE(plsres_boot)%>%
    dplyr::select(-Cronbach)%>%
    rename(잠재변수= Latent, CR=`CR(DG.rho)`)

  item_combine = full_join(item, CR_AVE_partial, by="잠재변수")%>%
    dplyr::select(-신뢰구간)%>%
    nice_table(exclude=c("측정변수","표준오차","요인적재량","t"))%>%
    dall()

  fl = plspm_fl(plsres_boot)
  #
  htmt = plspm_htmt(plsres_boot$data, plspm_extract_blocks(plsres_boot$model))
  #
  total_effect = plsres_boot$effect %>%cut_print_all()
  #
  effectssize = plspm_f2(plsres_boot)
  #
  #effect join
  total_effect_sig = full_join(total_effect,
                               res_boot$total.efs%>%
                                 dplyr::select(-요인적재량, -평균계수, -표준오차)%>%
                                 rename(relationships=관계),
                               by="relationships")


  # ###plot

  ##CFA
  loadings_plot = plspm_loadings_plot(plsres_boot)%>%
    suppressWarnings()
  corssloadings_plot = plspm_crossloadings_plot(plsres_boot, T)
  #
  if (web) {
    path_image <- .plspm_cfa_image(function() plspm_path_coefs_plot(plsres_boot))
    paths_plot <- path_image$value
  } else {
    paths_plot <- suppressWarnings(plspm_path_coefs_plot(plsres_boot))
  }


  if (web) {
    effect_image <- .plspm_cfa_image(function() {
      plspm_effectbar(plsres_boot, axis_x=axis_x, col=c("gray30", "gray70"))
    })
    effect_bar <- effect_image$value
  } else {
    effect_bar <- plspm_effectbar(plsres_boot, axis_x=axis_x, col=c("gray30", "gray70"))
  }



  all = list(
    item = item,
    CR_AVE = CR_AVE,
    item = item_combine,
    fl_1981 = fl,
    htmt= htmt,
    path = path%>%dplyr::select(-평균계수,-`95%CI`),
    total_effect_sig=total_effect_sig,
    f2= effectssize,
    loadings_plot=loadings_plot
  )

  if (web) {
    tables <- list(
      "01 Indicator loadings" = item,
      "02 Reliability / CR / AVE" = CR_AVE,
      "03 Item loadings / CR / AVE" = item_combine,
      "04 Fornell-Larcker" = fl,
      "05 HTMT" = htmt,
      "06 Path coefficients" = all$path,
      "07 Total effects" = total_effect_sig,
      "08 Effect size (f2)" = effectssize,
      "09 Inner summary / R2" = plsres_boot$inner_summary,
      "10 Crossloadings" = plsres_boot$crossloadings,
      "11 Goodness of fit" = data.frame(GoF = plsres_boot$gof)
    )
    cross_image <- .plspm_cfa_image(function() corssloadings_plot)
    images <- list("Crossloadings" = cross_image, "Path diagram" = path_image,
                   "Direct / indirect effects" = effect_image)
    for (i in seq_along(images)) {
      images[[i]]$filename <- paste0("plspm-cfa-plot-", i, ".png")
    }
    blocks <- plspm_extract_blocks(plsres_boot$model)
    model_text <- paste(capture.output(print(plsres_boot$model$IDM)), collapse = "\n")
    model_text <- paste(model_text, "", "Measurement blocks:",
      paste(vapply(names(blocks), function(name) {
        paste0(name, " =~ ", paste(blocks[[name]], collapse = " + "))
      }, character(1)), collapse = "\n"), sep = "\n")
    report <- .plspm_cfa_web_report(tables, model_text, loadings_plot,
      plsres_boot$outer_model$name, report_title = "PLS-PM CFA / SEM Results",
      report_prefix = "plspm-cfa", extra_images = images)
    names(all)[3] <- "item_CR_AVE"
    all$crossloadings_plot <- corssloadings_plot
    all$paths_plot <- paths_plot
    all$effect_bar <- effect_bar
    all$inner_summary <- plsres_boot$inner_summary
    all$gof <- plsres_boot$gof
    attr(all, "web_file") <- report
    utils::browseURL(report)
    return(invisible(all))
  }

  switch(type,
         all = all,
         pass = plsres_boot,
         res = summary(plsres_boot),
         reshape2 =  res_boot,
         boot =  res_boot,
         item = item,
         cfa1 = item_combine,
         cfa2 = list(fornell_locker=fl, HTMT=htmt),
         CRAVE = CR_AVE,
         fl1981 = fl,
         htmt = htmt,
         cor = plsres_boot$score%>%cor %>%lowerMat("",1)%>%dall(),
         # paths = res_boot$boot$pahts,
         total_effect=total_effect,
         f2=effectssize,
         loadings_plot=loadings_plot,
         crossloadngs_plot= corssloadings_plot,
         crossloadings_plot= corssloadings_plot,
         paths_plot = paths_plot,
         effect_bar = effect_bar,
         inner_summary = plsres_boot$inner_summary,
         unidim = plsres_boot$unidim,
         model = plsres_boot$model,
         data = plsres_boot$data,

         weight = res_boot$weight,
         loading = res_boot$loading,
         path = res_boot$path,
         rsq= res_boot$rsq,
         total.efs = res_boot$total.efs,

         gof= plsres_boot$gof,
         crossloadings = plsres_boot$crossloadings,
         inner_model = plsres_boot$inner_model,
         outer_model = plsres_boot$outer_model,
         outer_mpath_coefsodel = plsres_boot$path_coefs,
         scores = plsres_boot$scores,
         manifests = plsres_boot$manifests,
  )
  #
}

# Capture both ggplot returns and functions which draw directly (qgraph/effectbar).
.plspm_cfa_image <- function(draw) {
  file <- tempfile("plspm-cfa-", fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 1800, height = 1100, res = 150, bg = "white")
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  tryCatch({
    value <- draw()
    if (inherits(value, "ggplot")) print(value)
    grDevices::dev.off(device)
    list(value = value, uri = knitr::image_uri(file), error = NULL)
  }, error = function(e) list(value = NULL, uri = NULL, error = conditionMessage(e)))
}

# Kept in this file so source("R/plspm_cfa.R") works with older installed
# jjstat versions and without sourcing cfa2.R or its internal helpers.
.plspm_cfa_web_report <- function(tables, model, plot, variable_order, digits = 3,
                              file = tempfile("plspm-cfa-report-", fileext = ".html"),
                              table_notes = list(),
                              report_title = "PLS-PM CFA / SEM Results", report_prefix = "plspm-cfa",
                              extra_images = list()) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Install htmltools to use plspm_cfa(web = TRUE).")
  }
  esc <- function(x) as.character(htmltools::htmlEscape(enc2utf8(as.character(x))))
  code_block <- function(x) paste0("<pre><code>", esc(x), "</code></pre>")
  # A fenced block longer than any backtick sequence in user-supplied syntax.
  md_code <- function(x) {
    runs <- regmatches(x, gregexpr("`+", x))[[1]]
    fence <- paste(rep("`", max(c(2L, nchar(runs))) + 1L), collapse = "")
    paste(fence, x, fence, sep = "\n")
  }
  model_text <- paste(as.character(model), collapse = "\n")
  code_card <- function(title, text, id) paste0(
    '<section class="code-card"><div class="table-heading"><h2>', esc(title), '</h2>',
    '<div class="table-actions"><button type="button" class="copy-code" data-copy-target="',
    id, '">코드 복사</button><span class="copy-status" role="status" aria-live="polite"></span>',
    '</div></div><pre><code id="', id, '">', esc(text), '</code></pre></section>')
  image_card <- function(title, uri, filename, alt) paste0(
    '<section class="image-card"><div class="table-heading"><h2>', esc(title), '</h2>',
    '<a class="download-image" href="', uri, '" download="', filename, '">PNG 다운로드</a>',
    '</div><img class="report-plot" alt="', esc(alt), '" src="', uri, '"></section>')
  sections <- code_card("Model", model_text, "model-code")
  markdown <- c(paste0("# ", report_title), "", "## Model", "", md_code(model_text))
  for (i in seq_along(tables)) {
    title <- names(tables)[i]
    data <- tables[[i]]
    if (is.null(data)) {
      sections <- c(sections, paste0("<section><h2>", esc(title),
                    "</h2><p>Not computed / unavailable.</p></section>"))
      markdown <- c(markdown, "", paste0("## ", title), "", "Not computed / unavailable.")
      next
    }
    # knitr escapes cell HTML. Markdown needs explicit escaping of pipes and newlines.
    md_data <- as.data.frame(data, stringsAsFactors = FALSE)
    md_escape <- function(x) {
      x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
      x <- gsub("<", "&lt;", x, fixed = TRUE)
      x <- gsub(">", "&gt;", x, fixed = TRUE)
      x <- gsub("|", "&#124;", x, fixed = TRUE)
      gsub("[\r\n]+", "<br>", x)
    }
    for (col in names(md_data)) {
      if (!is.numeric(md_data[[col]])) md_data[[col]] <- md_escape(md_data[[col]])
    }
    names(md_data) <- md_escape(names(md_data))
    rownames(md_data) <- md_escape(rownames(md_data))
    html_table <- paste(knitr::kable(data, format = "html", digits = digits,
                                     escape = TRUE), collapse = "\n")
    md_table <- paste(knitr::kable(md_data, format = "pipe", digits = digits), collapse = "\n")
    notes <- table_notes[[title]]
    note_html <- if (length(notes)) paste0('<div class="table-note" role="note">',
                         paste(esc(notes), collapse = "<br>"), '</div>') else ""
    sections <- c(sections, paste0(
      '<section class="table-card" aria-labelledby="table-title-', i, '">',
      '<div class="table-heading"><h2 id="table-title-', i, '">', esc(title), '</h2>',
      '<div class="table-actions"><button type="button" class="copy-table" aria-label="',
      esc(paste(title, "표 복사")), '">표 복사</button>',
      '<span class="copy-status" role="status" aria-live="polite"></span></div></div>',
      '<div class="table-scroll" tabindex="0">', html_table, '</div>', note_html, '</section>'))
    markdown <- c(markdown, "", paste0("## ", title), "", md_table)
    if (length(notes)) markdown <- c(markdown, "", paste0("> ", notes))
  }
  png_file <- tempfile("cfa2-loadings-", fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  ggplot2::ggsave(png_file, plot = plot, width = max(9, length(unique(variable_order)) * .55),
                 height = 5.5, dpi = 150, bg = "white", limitsize = FALSE)
  image_uri <- knitr::image_uri(png_file)
  sections <- c(sections, image_card("Factor loadings", image_uri,
                                      paste0(report_prefix, "-factor-loadings.png"), "Standardized factor loadings"),
                  paste0('<section><h2>Variable order</h2>',
                         code_block(paste(variable_order, collapse = ", ")), '</section>'))
  markdown <- c(markdown, "", "## Factor loadings", "",
                paste0("![Standardized factor loadings](", image_uri, ")"),
                "", "## Variable order", "", md_code(paste(variable_order, collapse = ", ")))
  for (title in names(extra_images)) {
    current <- extra_images[[title]]
    if (!is.null(current$uri)) {
      sections <- c(sections, image_card(title, current$uri, current$filename, title))
      markdown <- c(markdown, "", paste0("## ", title), "",
                    paste0("![", title, "](", current$uri, ")"))
    } else {
      sections <- c(sections, paste0("<section><h2>", esc(title), "</h2><p>",
                                      esc(current$error), "</p></section>"))
      markdown <- c(markdown, "", paste0("## ", title), "", current$error)
    }
  }
  css <- r"---(
:root{font-family:system-ui,-apple-system,'Segoe UI',sans-serif;color:#1e293b;background:#f1f5f9}
*{box-sizing:border-box}body{margin:0}header{position:sticky;top:0;z-index:5;background:#fff;border-bottom:1px solid #dbe2ec;padding:18px max(24px,calc((100% - 1200px)/2));display:flex;gap:16px;align-items:center;justify-content:space-between;flex-wrap:wrap}
h1{font-size:23px;margin:0 0 5px}header p{margin:0;color:#64748b;font-size:14px}main{max-width:1248px;margin:auto;padding:24px}section{background:#fff;border:1px solid #dbe2ec;border-radius:12px;padding:22px;margin-bottom:20px;min-width:0}
h2{font-size:18px;margin:0 0 16px}.downloads,.table-actions{display:flex;align-items:center;gap:10px;flex-wrap:wrap}button,.download-image{font:inherit;text-decoration:none;display:inline-block;cursor:pointer;border:1px solid #cbd5e1;border-radius:7px;padding:9px 14px;background:#fff;color:#1e293b}button:hover,.download-image:hover{background:#eff6ff;border-color:#2563eb}button:focus-visible,.download-image:focus-visible,[tabindex]:focus-visible{outline:3px solid #93c5fd;outline-offset:3px}#download-html{background:#1d4ed8;border-color:#1d4ed8;color:#fff}
.table-heading{display:flex;justify-content:space-between;gap:16px;align-items:baseline;flex-wrap:wrap}.table-actions{min-height:40px}.copy-table{opacity:0;transition:opacity .15s}.table-card:hover .copy-table,.table-card:focus-within .copy-table{opacity:1}.copy-status{font-size:13px;color:#1d4ed8}.table-note{margin-top:14px;padding:12px 14px;border-left:4px solid #f59e0b;background:#fffbeb;font-size:13px;line-height:1.7;overflow-wrap:anywhere}.table-scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:10px 12px;border-bottom:1px solid #e2e8f0;white-space:nowrap}thead{background:#f8fafc}th{font-weight:600}tbody tr:hover{background:#f8fafc}pre{padding:16px;background:#f8fafc;white-space:pre-wrap;overflow-wrap:anywhere;border-radius:7px;font-size:14px;line-height:1.6}.report-plot{display:block;width:100%;height:auto}.image-card .table-heading{margin-bottom:16px}.code-card pre{max-height:360px;overflow:auto}.diagram-code{margin-bottom:20px}.diagram-code summary{cursor:pointer;padding:12px;font-weight:600}.diagram-code section{margin-top:8px}footer{padding:0 24px 24px;text-align:center;color:#64748b;font-size:13px}[hidden]{display:none!important}
@media(hover:none){.copy-table{opacity:1}}@media(max-width:600px){main{padding:12px}section{padding:14px}header{padding:14px}.table-heading h2{width:100%;margin-bottom:0}}
@media print{header{position:static}.downloads,.table-actions,.download-image,footer{display:none}main{padding:0}section{border:0;break-inside:avoid}.table-scroll{overflow:visible}table{font-size:10px}th,td{padding:5px;white-space:normal}}
)---"
  js <- r"---(
(() => {
  'use strict';
  function download(name, content, type) {
    const url = URL.createObjectURL(new Blob([content], {type}));
    const link = document.createElement('a');
    link.href = url; link.download = name; document.body.appendChild(link);
    link.click(); link.remove(); setTimeout(() => URL.revokeObjectURL(url), 1000);
  }
  document.getElementById('download-html').addEventListener('click', () => {
    download('cfa2-results.html', '<!doctype html>\n' + document.documentElement.outerHTML, 'text/html;charset=utf-8');
  });
  document.getElementById('download-md').addEventListener('click', () => {
    download('cfa2-results.md', document.getElementById('markdown-source').value, 'text/markdown;charset=utf-8');
  });
  function fallbackCopy(table, text, html) {
    const selection = window.getSelection();
    const saved = [];
    for (let i = 0; i < selection.rangeCount; i++) saved.push(selection.getRangeAt(i).cloneRange());
    const range = document.createRange(); range.selectNodeContents(table);
    const copy = event => {
      if (event.clipboardData) {
        event.clipboardData.setData('text/plain', text);
        if (html) event.clipboardData.setData('text/html', html); event.preventDefault();
      }
    };
    document.addEventListener('copy', copy);
    selection.removeAllRanges(); selection.addRange(range);
    try { if (!document.execCommand('copy')) throw new Error('Copy unavailable'); }
    finally {
      document.removeEventListener('copy', copy); selection.removeAllRanges();
      saved.forEach(item => selection.addRange(item));
    }
  }
  document.querySelectorAll('.copy-code').forEach(button => {
    button.addEventListener('click', async () => {
      const code = document.getElementById(button.getAttribute('data-copy-target'));
      const status = button.closest('.code-card').querySelector('.copy-status');
      const text = code.textContent;
      try {
        let copied = false;
        if (navigator.clipboard && navigator.clipboard.writeText) {
          try { await navigator.clipboard.writeText(text); copied = true; } catch (_) {}
        }
        if (!copied) fallbackCopy(code, text, null);
        status.textContent = '코드 복사 완료';
      } catch (_) { status.textContent = '코드를 복사할 수 없습니다. 코드를 선택해 복사해 주세요.'; }
    });
  });
  document.querySelectorAll('.copy-table').forEach(button => {
    button.addEventListener('click', async () => {
      const card = button.closest('.table-card'); const table = card.querySelector('table');
      const status = card.querySelector('.copy-status');
      const text = Array.from(table.rows, row => Array.from(row.cells, cell =>
        cell.innerText.replace(/[\t\r\n]+/g, ' ').trim()).join('\t')).join('\n');
      const html = '<html><head><meta charset="utf-8"></head><body>' + table.outerHTML + '</body></html>';
      try {
        let copied = false;
        if (navigator.clipboard && navigator.clipboard.write && window.ClipboardItem) {
          try {
            await navigator.clipboard.write([new ClipboardItem({
              'text/html': new Blob([html], {type:'text/html'}),
              'text/plain': new Blob([text], {type:'text/plain'})
            })]); copied = true;
          } catch (_) { /* Local-file browsers may require the selection fallback. */ }
        }
        if (!copied) fallbackCopy(table, text, html);
        status.textContent = '복사 완료';
      } catch (_) { status.textContent = '복사할 수 없습니다. 표를 선택해 복사해 주세요.'; }
    });
  });
})();
)---"
  js <- gsub("cfa2-results.", paste0(report_prefix, "-results."), js, fixed = TRUE)
  html <- paste0('<!doctype html>\n<html lang="ko"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>', esc(report_title), '</title><style>', css, '</style></head><body>',
    '<header><div><h1>', esc(report_title), '</h1><p>표에 마우스를 올리면 복사 버튼이 나타납니다.</p></div>',
    '<div class="downloads"><button type="button" id="download-html">HTML 다운로드</button>',
    '<button type="button" id="download-md">Markdown (.md) 다운로드</button></div></header><main>',
    paste(sections, collapse = "\n"), '</main>',
    '<footer>jjstat · ', esc(report_title), '</footer>',
    '<textarea id="markdown-source" hidden aria-hidden="true">',
    esc(paste(markdown, collapse = "\n")), '</textarea>',
    '<script>', js, '</script></body></html>')
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}
