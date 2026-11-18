# R AI clients | Ollama + LM Studio + Google AI Studio + Lemonade + LG EXAONE
# Recommended packages: install.packages(c('jsonlite','processx','commonmark','rstudioapi','later'))
# Standalone script: source('ai_clients.R', encoding='UTF-8')

.ai_require <- function() {
  for (p in c('jsonlite', 'processx')) {
    if (!requireNamespace(p, quietly = TRUE))
      stop('\ud328\ud0a4\uc9c0\uac00 \ud544\uc694\ud569\ub2c8\ub2e4: install.packages("', p, '")', call. = FALSE)
  }
}

.ai_curl <- function() {
  bin <- Sys.which(if (.Platform$OS.type == 'windows') 'curl.exe' else 'curl')
  if (!nzchar(bin)) stop('curl \uc2e4\ud589 \ud30c\uc77c\uc744 PATH\uc5d0\uc11c \ucc3e\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4.', call. = FALSE)
  unname(bin)
}

.ai_text <- function(x, max_rows = 100L, max_chars = 30000L) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) {
    n <- nrow(x)
    small <- utils::head(x, as.integer(max_rows))
    v <- paste(utils::capture.output(utils::write.table(
      small, file = '', sep = '\t', row.names = FALSE,
      col.names = TRUE, quote = FALSE, na = 'NA'
    )), collapse = '\n')
    if (n > nrow(small)) v <- paste0(v, '\n[\uc804\uccb4 ', n, '\ud589 \uc911 ', nrow(small), '\ud589\ub9cc \uc804\uc1a1\ub428]')
  } else if (is.character(x) && length(x) == 1L) {
    v <- x
  } else if (is.matrix(x)) {
    v <- paste(utils::capture.output(print(x)), collapse = '\n')
  } else {
    v <- paste(utils::capture.output(print(x)), collapse = '\n')
  }
  if (nchar(v, type = 'chars') > max_chars)
    v <- paste0(substr(v, 1L, max_chars), '\n[\uc785\ub825 \uae38\uc774 \uc81c\ud55c\uc73c\ub85c \uc77c\ubd80 \uc0dd\ub7b5\ub428]')
  v
}

.ai_system <- paste(
  '\ub2f9\uc2e0\uc740 \ud55c\uad6d\uc5b4\ub85c \ub2f5\ud558\ub294 \ud1b5\uacc4\ud559 \ubc0f \ud559\uc220 \uc5f0\uad6c \uc870\uad50\uc774\ub2e4.',
  '\uc81c\uacf5\ub41c \ucd9c\ub825 \uacb0\uacfc\ub97c \uadfc\uac70\ub85c \ud574\uc11d\ud558\uace0, \ub370\uc774\ud130\uc5d0 \uc5c6\ub294 \uc218\uce58\ub098 \uac80\uc815 \uacb0\uacfc\ub97c \ub9cc\ub4e4\uc5b4\ub0b4\uc9c0 \uc54a\ub294\ub2e4.',
  'p=0.00\uc740 \ubc18\uc62c\ub9bc \uac12\uc774\ubbc0\ub85c p=0\uc774\ub77c\uace0 \ub2e8\uc815\ud558\uc9c0 \uc54a\ub294\ub2e4.',
  '\uc6d0\uc790\ub8cc\uac00 \uc5c6\uc73c\uba74 \ud6a8\uacfc\ud06c\uae30, \uc2e0\ub8b0\uad6c\uac04, \uac00\uc815\uac80\uc815 \uacb0\uacfc\ub97c \uc784\uc758\ub85c \uacc4\uc0b0\ud558\uc9c0 \uc54a\ub294\ub2e4.',
  '\ub450 \uc9d1\ub2e8\ubcc4 \ud589\uc5d0 \ub3d9\uc77c\ud55c \uac80\uc815 \ud1b5\uacc4\ub7c9\uc774 \ubc18\ubcf5\ub418\uba74 \ud55c \uac1c\uc758 \uac80\uc815\uc73c\ub85c \ud574\uc11d\ud55c\ub2e4.',
  't \uac12 \ubd80\ud638\uc758 \ubc29\ud5a5\uc740 \uc9d1\ub2e8 \uc21c\uc11c\uac00 \uba85\ud655\ud558\uc9c0 \uc54a\uc73c\uba74 \ub2e8\uc815\ud558\uc9c0 \uc54a\ub294\ub2e4.',
  '\ucd94\ub860\uc758 \ud55c\uacc4\uc640 \ubcf4\uace0 \uac00\ub2a5\ud55c \uacb0\uacfc\ub97c \uad6c\ubd84\ud55c\ub2e4.'
)

.ai_prompt <- function(data, prompt, max_rows, max_chars) {
  if (is.null(prompt) && is.character(data) && length(data) == 1L) {
    prompt <- data
    data <- NULL
  }
  if (!is.character(prompt) || length(prompt) != 1L || is.na(prompt) ||
      !nzchar(trimws(prompt))) {
    stop("\uc9c8\ubb38\uc744 \uc785\ub825\ud558\uc2ed\uc2dc\uc624. \uc608: lmstudio('\uc548\ub155\ud558\uc138\uc694') \ub610\ub294 df %>% lmstudio('\ud574\uc11d\ud574\uc918')", call. = FALSE)
  }
  dt <- .ai_text(data, max_rows, max_chars)
  user <- if (is.null(dt)) prompt else
    paste0('[\uc694\uccad]\n', prompt, '\n\n[\ubd84\uc11d \ub370\uc774\ud130]\n', dt)
  list(user = user, raw_prompt = prompt)
}

# Resolve assets when sourced, so changing the working directory remains safe.
.ai_math_assets <- local({
  files <- vapply(sys.frames(), function(f) {
    if (is.null(f$ofile)) '' else as.character(f$ofile)[1L]
  }, character(1))
  files <- files[nzchar(files)]
  base <- if (length(files)) dirname(normalizePath(utils::tail(files, 1L),
    winslash = '/', mustWork = FALSE)) else getwd()
  file.path(base, 'assets', 'katex')
})

.ai_math_dir <- function() {
  env <- environment(.ai_math_dir)
  if (isNamespace(env))
    return(system.file('assets', 'katex', package = getNamespaceName(env), mustWork = TRUE))
  if (dir.exists(.ai_math_assets)) return(.ai_math_assets)
  normalizePath(file.path(.ai_math_assets, '..', '..', '..', 'inst', 'assets', 'katex'),
                winslash = '/', mustWork = TRUE)
}

.ai_html_escape <- function(x) {
  for (pair in list(c('&', '&amp;'), c('<', '&lt;'), c('>', '&gt;'),
                    c('"', '&quot;'), c("'", '&#39;')))
    x <- gsub(pair[1L], pair[2L], x, fixed = TRUE)
  x
}

# Accept paired ** delimiters regardless of adjacent Korean text or punctuation.
# Code fences/spans, indented code, HTML tags and escaped stars retain their source.
.ai_relaxed_bold <- function(x) {
  protected <- "(?m:^[ ]{0,3}(?<fence>`{3,}|~{3,})[^\\n]*\\n[\\s\\S]*?^[ ]{0,3}\\k<fence>[~`]*[ \\t]*(?:\\n|$))|(?m:^[ ]{0,3}(?:`{3,}|~{3,})[^\\n]*(?:\\n[\\s\\S]*)?\\z)|(?m:^(?: {4}|\\t)[^\\n]*(?:\\n|$))|(?<ticks>`+)[^`]*?(?:`(?!\\k<ticks>)[^`]*)*?\\k<ticks>(?!`)|<[^>\\n]+>|\\\\."
  pattern <- paste0("(?:",protected,")(*SKIP)(*F)|(?<![\\\\*])\\*\\*(?!\\*)[^\\r\\n]+?\\*\\*(?!\\*)")
  matches <- gregexpr(pattern,x,perl=TRUE)[[1L]]
  if(matches[1L]<0L)return(list(text=x,replacements=character()))
  sizes <- attr(matches,'match.length');parts <- character();cursor <- 1L;replacements <- character()
  marker <- 'AISTRONGPLACEHOLDER';while(grepl(marker,x,fixed=TRUE))marker<-paste0(marker,'X')
  for(i in seq_along(matches)) {
    start <- matches[i];end <- start+sizes[i]-1L
    if(start>cursor)parts<-c(parts,substr(x,cursor,start-1L))
    inside <- substr(x,start+2L,end-2L)
    if(nzchar(trimws(inside))) {
      id<-paste0(marker,length(replacements)+1L,'END')
      rendered<-commonmark::markdown_html(inside,extensions='strikethrough')
      rendered<-sub('^<p>','',rendered);rendered<-sub('</p>\\n$','',rendered)
      replacements[id]<-paste0('<strong>',rendered,'</strong>');parts<-c(parts,id)
    }else parts<-c(parts,substr(x,start,end))
    cursor <- end+1L
  }
  if(cursor<=nchar(x))parts<-c(parts,substr(x,cursor,nchar(x)))
  list(text=paste0(parts,collapse=''),replacements=replacements)
}

# Protect TeX before commonmark consumes backslashes, underscores and table pipes.
# Code fences, code spans, indented code and HTML tags take precedence over math.
.ai_markdown_html <- function(x) {
  if (!requireNamespace('commonmark', quietly = TRUE))
    stop('Viewer Markdown \ub80c\ub354\ub9c1\uc5d0 commonmark \ud328\ud0a4\uc9c0\uac00 \ud544\uc694\ud569\ub2c8\ub2e4: install.packages("commonmark")', call. = FALSE)
  pattern <- paste0(
    '(?m:^[ ]{0,3}(`{3,}|~{3,})[^\\n]*\\n[\\s\\S]*?^[ ]{0,3}\\1[~`]*[ \\t]*(?:\\n|$))',
    '|(?m:^[ ]{0,3}(?:`{3,}|~{3,})[^\\n]*(?:\\n[\\s\\S]*)?\\z)',
    '|(?m:^(?: {4}|\\t)[^\\n]*(?:\\n|$))',
    '|(`+)[^`]*?(?:`(?!\\2)[^`]*)*?\\2(?!`)',
    '|<[^>\\n]+>',
    '|\\\\\\\\|\\\\\\$',
    '|\\$\\$(?:[^$]|\\$(?!\\$))+?\\$\\$',
    '|\\\\\\[[\\s\\S]*?\\\\\\]',
    '|\\\\\\([^\\n]*?\\\\\\)',
    '|(?<![\\w\\\\$])\\$(?![\\s$])(?:\\\\[^\\n]|[^$\\n\\\\])*?(?<!\\s)\\$(?![\\w$])'
  )
  matches <- gregexpr(pattern, x, perl = TRUE)[[1L]]
  replacements <- character()
  if (matches[1L] != -1L) {
    lengths <- attr(matches, 'match.length')
    parts <- character()
    cursor <- 1L
    # Use a marker absent from the response to avoid replacing user text.
    marker <- 'AIMATHPLACEHOLDER'
    while (grepl(marker, x, fixed = TRUE)) marker <- paste0(marker, 'X')
    for (i in seq_along(matches)) {
      token <- substr(x, matches[i], matches[i] + lengths[i] - 1L)
      display <- startsWith(token, '$$') || startsWith(token, '\\[')
      math <- display || startsWith(token, '\\(') ||
        (startsWith(token, '$') && !startsWith(token, '$$'))
      if (matches[i] > cursor) parts <- c(parts, substr(x, cursor, matches[i] - 1L))
      if (math) {
        width <- if (startsWith(token, '$') && !display) 1L else 2L
        tex <- substr(token, width + 1L, nchar(token) - width)
        id <- paste0(marker, length(replacements) + 1L, 'END')
        replacements[id] <- paste0('<span class="ai-math" data-display="',
          if (display) 'true' else 'false', '" data-tex="', .ai_html_escape(tex),
          '">', .ai_html_escape(token), '</span>')
        parts <- c(parts, id)
      } else parts <- c(parts, token)
      cursor <- matches[i] + lengths[i]
    }
    if (cursor <= nchar(x)) parts <- c(parts, substr(x, cursor, nchar(x)))
    x <- paste0(parts, collapse = '')
  }
  bold <- .ai_relaxed_bold(x)
  html <- commonmark::markdown_html(bold$text, extensions = c('table', 'strikethrough'))
  for(id in names(bold$replacements))html<-gsub(id,bold$replacements[[id]],html,fixed=TRUE)
  for (id in names(replacements)) html <- gsub(id, replacements[[id]], html, fixed = TRUE)
  html
}

# Live Markdown Viewer. HTML is isolated in a sandboxed iframe (scripts disabled).
.ai_viewer <- function(title, question) {
  if (!requireNamespace('rstudioapi', quietly = TRUE) ||
      !rstudioapi::isAvailable())
    stop('viewer=TRUE\ub294 RStudio\uc5d0\uc11c \uc2e4\ud589\ud574\uc57c \ud569\ub2c8\ub2e4. install.packages("rstudioapi")\ub97c \ud655\uc778\ud558\uc2ed\uc2dc\uc624.', call. = FALSE)
  if (!requireNamespace('commonmark', quietly = TRUE))
    stop('Markdown \ub80c\ub354\ub9c1\uc5d0 commonmark\uac00 \ud544\uc694\ud569\ub2c8\ub2e4: install.packages("commonmark")', call. = FALSE)
  dir <- tempfile('r_ai_viewer_')
  dir.create(dir, recursive = TRUE)
  .ai_math_assets <- .ai_math_dir()
  required <- file.path(.ai_math_assets, c('katex.min.js', 'katex.min.css', 'fonts'))
  if (!all(file.exists(required)))
    stop('\uc218\uc2dd \ub80c\ub354\ub9c1 \ud30c\uc77c\uc774 \uc5c6\uc2b5\ub2c8\ub2e4. ai_clients.R\uacfc \ud568\uaed8 assets/katex \ud3f4\ub354\ub97c \ubcf4\uad00\ud558\uace0 source()\ub85c \ub2e4\uc2dc \ubd88\ub7ec\uc624\uc2ed\uc2dc\uc624.', call. = FALSE)
  if (!file.copy(.ai_math_assets, dir, recursive = TRUE))
    stop('Viewer \uc218\uc2dd \ub80c\ub354\ub9c1 \ud30c\uc77c\uc744 \ubcf5\uc0ac\ud558\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4.', call. = FALSE)
  index <- file.path(dir, 'index.html')
  state <- file.path(dir, 'state.js')
  ui <- .ai_ui_dir()
  if(!file.copy(ui,dir,recursive=TRUE)) stop('Cannot copy Viewer UI assets.')
  file.copy(file.path(ui,'index.html'),index,overwrite=TRUE)
  writeLines('window.APP_CONFIG={mode:"viewer"};',file.path(dir,'config.js'))
  accumulated <- ''
  rendered <- ''; rendered_text <- NULL
  control <- .ai_control()
  console_url <- .ai_console_bridge()
  status <- '\uc694\uccad \uc900\ube44 \uc911'
  last_write <- 0
  save <- function(force = FALSE) {
    now <- as.numeric(Sys.time())
    if (!force && now - last_write < 0.1) return(invisible(NULL))
    last_write <<- now
    if(!identical(rendered_text,accumulated)) {
      rendered <<- .ai_markdown_html(accumulated); rendered_text <<- accumulated
    }
    js <- paste0('window.__ai_snapshot = ',
                 as.character(jsonlite::toJSON(list(question = question,
                   markdown = accumulated, html = rendered, status = status, console_url = console_url, stop_url = control$url, can_stop = control$is_open()), auto_unbox = TRUE)), ';')
    tmp <- tempfile('snapshot_', tmpdir = dir, fileext = '.js')
    writeLines(js, tmp, useBytes = TRUE)
    if (file.exists(state)) unlink(state)
    if (!file.rename(tmp, state)) {
      file.copy(tmp, state, overwrite = TRUE)
      unlink(tmp)
    }
    invisible(NULL)
  }
  save(force = TRUE)
  rstudioapi::viewer(index)
  push <- function(text) {
    first <- !nzchar(accumulated)
    accumulated <<- paste0(accumulated, text)
    status <<- '\uc0dd\uc131 \uc911...'
    save(force = first)
    invisible(NULL)
  }
  set_status <- function(value) {
    status <<- as.character(value)
    save(force = TRUE)
    invisible(NULL)
  }
  shutdown <- function() {control$close();save(force=TRUE);invisible(NULL)}
  list(push = push, status = set_status, path = index,poll=control$poll,
       is_stopped=control$stopped,close=shutdown)
}

.ai_popup <- function(title) {
  if (!capabilities('tcltk') || !requireNamespace('tcltk', quietly = TRUE))
    stop('\ucc3d \uae30\ub2a5\uc740 Tcl/Tk \uc9c0\uc6d0 R \ud658\uacbd\uc5d0\uc11c\ub9cc \uc774\uc6a9 \uac00\ub2a5\ud569\ub2c8\ub2e4.', call. = FALSE)
  tk <- asNamespace('tcltk')
  top <- tk$tktoplevel()
  tk$tkwm.title(top, title)
  tk$tkwm.geometry(top, '980x680')
  frame <- tk$tkframe(top)
  sc <- tk$tkscrollbar(frame)
  box <- tk$tktext(frame, wrap = 'word', background = '#141b25',
                   foreground = '#eef3fa', insertbackground = 'white',
                   yscrollcommand = function(...) tk$tkset(sc, ...))
  tk$tkconfigure(sc, command = function(...) tk$tkyview(box, ...))
  tk$tkpack(sc, side = 'right', fill = 'y')
  tk$tkpack(box, side = 'left', fill = 'both', expand = TRUE)
  tk$tkpack(frame, fill = 'both', expand = TRUE)
  tk$tkconfigure(box, state = 'disabled')
  push <- function(s) {
    try({
      tk$tkconfigure(box, state = 'normal')
      tk$tkinsert(box, 'end', s)
      tk$tksee(box, 'end')
      tk$tkconfigure(box, state = 'disabled')
      tk$tcl('update')
    }, silent = TRUE)
    invisible(NULL)
  }
  list(top = top, push = push)
}

.ai_parse <- function(event, provider, stream, show, finish = function(reason) NULL, usage = function(info) NULL, activity = function(stage) NULL) {
  if (stream && provider != 'ollama') {
    if (!startsWith(event, 'data:')) return(FALSE)
    event <- trimws(substring(event, 6L))
    if (identical(event, '[DONE]')) return(TRUE)
  }
  if (!nzchar(trimws(event))) return(FALSE)
  obj <- tryCatch(jsonlite::fromJSON(event, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(obj)) return(FALSE)
  if (!is.null(obj$error)) {
    msg <- if (is.list(obj$error)) obj$error$message else as.character(obj$error)
    if (is.null(msg)) msg <- 'API \uc624\ub958\uac00 \ubc1c\uc0dd\ud588\uc2b5\ub2c8\ub2e4.'
    stop(provider, ': ', msg, call. = FALSE)
  }
  usage(.ai_usage(obj,provider))
  reasoning <- FALSE
  has_text <- function(x) is.character(x) && any(nzchar(x))
  if (identical(provider, 'ollama')) {
    reasoning <- has_text(obj$message$thinking)
    content <- obj$message$content
    done <- isTRUE(obj$done)
    if (done) finish(if (is.null(obj$done_reason)) 'stop' else obj$done_reason)
  } else if (provider %in% c('lmstudio', 'lemonade', 'exaone')) {
    ch <- if (length(obj$choices)) obj$choices[[1L]] else NULL
    message <- if (stream) ch$delta else ch$message
    reasoning <- has_text(message$reasoning_content) || has_text(message$reasoning)
    content <- message$content
    done <- !is.null(ch$finish_reason)
    if (done) finish(ch$finish_reason)
  } else {
    parts <- if (length(obj$candidates)) obj$candidates[[1L]]$content$parts else NULL
    reasoning <- any(vapply(parts,function(p) isTRUE(p$thought),logical(1)))
    parts <- Filter(function(p) !isTRUE(p$thought),parts)
    content <- if (length(parts)) {
      paste(vapply(parts, function(p) if (is.character(p$text)) p$text else '',
                   character(1)), collapse = '')
    } else NULL
    done <- length(obj$candidates) > 0L && !is.null(obj$candidates[[1L]]$finishReason)
    if (done) finish(obj$candidates[[1L]]$finishReason)
  }
  activity(if(has_text(content)) 'content' else if(reasoning) 'reasoning' else 'event')
  if (is.character(content) && length(content)) show(paste(content, collapse = ''))
  isTRUE(done)
}


# Normalize provider counters without estimating tokens from character counts.
.ai_usage <- function(obj, provider) {
  number <- function(...) {
    for (x in list(...)) if (is.numeric(x) && length(x)==1L && is.finite(x) && x>=0) return(as.numeric(x))
    NULL
  }
  if (provider=='ollama') {
    seconds <- number(obj$eval_duration)
    if (!is.null(seconds) && seconds>0) seconds <- seconds/1e9 else seconds <- NULL
    result <- list(input_tokens=number(obj$prompt_eval_count),output_tokens=number(obj$eval_count),
                   generation_seconds=seconds)
  } else if (provider=='aistudio') {
    u <- obj$usageMetadata
    result <- list(input_tokens=number(u$promptTokenCount),output_tokens=number(u$candidatesTokenCount),
                   total_tokens=number(u$totalTokenCount),thinking_tokens=number(u$thoughtsTokenCount))
  } else {
    u <- obj$usage; timing <- obj$timings; stats <- obj$stats
    seconds <- number(timing$predicted_ms)
    if (!is.null(seconds) && seconds>0) seconds <- seconds/1000 else seconds <- NULL
    result <- list(input_tokens=number(u$prompt_tokens,u$input_tokens),
      output_tokens=number(u$completion_tokens,u$output_tokens),total_tokens=number(u$total_tokens),
      server_rate=number(timing$predicted_per_second,stats$tokens_per_second),generation_seconds=seconds,
      thinking_tokens=number(u$reasoning_tokens,u$completion_tokens_details$reasoning_tokens))
  }
  result[!vapply(result,is.null,logical(1))]
}
.ai_finalize_metrics <- function(metrics,elapsed) {
  metrics$elapsed_seconds <- elapsed
  if (!is.null(metrics$server_rate)) {
    metrics$tokens_per_second <- metrics$server_rate; metrics$rate_source <- 'server'
  } else if (!is.null(metrics$output_tokens) && !is.null(metrics$generation_seconds) && metrics$generation_seconds>0) {
    metrics$tokens_per_second <- metrics$output_tokens/metrics$generation_seconds; metrics$rate_source <- 'server'
  } else if (!is.null(metrics$output_tokens) && elapsed>0) {
    metrics$tokens_per_second <- metrics$output_tokens/elapsed; metrics$rate_source <- 'request_average'
  }
  metrics
}
.ai_metric_text <- function(metrics,elapsed=0,final=FALSE) {
  if(is.null(metrics)) metrics <- list()
  missing <- if(final) '\ubbf8\uc81c\uacf5' else '--'
  count <- function(x) if(is.null(x)) missing else paste0(formatC(x,format='f',digits=0,big.mark=','),' tok')
  m <- .ai_finalize_metrics(metrics,elapsed)
  rate <- if(is.null(m$tokens_per_second)) paste0(missing,' tok/s') else
    paste0(sprintf('%.1f tok/s',m$tokens_per_second),if(m$rate_source=='request_average') ' (\uc694\uccad \ud3c9\uade0)' else '')
  output <- count(m$output_tokens)
  if(is.null(m$output_tokens) && !is.null(m$output_chunks) && m$output_chunks>0) {
    output <- paste0('\u2248',formatC(m$output_chunks,format='f',digits=0,big.mark=','),' tok (\ucd94\uc815)')
    interval <- elapsed - m$first_chunk_seconds
    if(m$output_chunks>1 && is.finite(interval) && interval>=0.1)
      rate <- paste0('\u2248',sprintf('%.1f tok/s',(m$output_chunks-1)/interval),' (\uc218\uc2e0 \ucd94\uc815)')
  }
  paste0('Context = ',count(m$input_tokens),' | Output = ',output,' | ',rate)
}
# Completion length is unknown: percentages below 100 are explicitly estimates.
.ai_console_progress <- function(enabled = TRUE, metrics = function() list()) {
  started <- proc.time()[['elapsed']]
  last_draw <- -Inf
  percent <- 0L
  closed <- FALSE
  previous_width <- 0L;last_stage <- NULL
  elapsed_time <- function() max(0,proc.time()[['elapsed']] - started)
  update <- function(stage = '\uc11c\ubc84 \uc751\ub2f5 \ub300\uae30', receiving = FALSE, finish = FALSE,
                     failed = FALSE, force = FALSE) {
    if (!isTRUE(enabled) || closed) return(invisible(NULL))
    elapsed <- elapsed_time()
    if (!force && !finish && !failed && elapsed - last_draw < 0.5) return(invisible(NULL))
    estimate <- if (receiving) 35 + 60 * (1 - exp(-elapsed / 90)) else 5 + 25 * (1 - exp(-elapsed / 30))
    percent <<- if (finish) 100L else max(percent, min(95L, as.integer(estimate)))
    filled <- as.integer(percent / 5)
    bar <- paste0(strrep('=', filled), strrep(' ', 20L - filled))
    label <- if (finish) '\uc644\ub8cc' else if (failed) stage else paste0(stage, ' \u00b7 \ucd94\uc815')
    line1 <- sprintf('[AI] [%s] %3d%% | %s | %.1f\ucd08',bar,percent,label,elapsed)
    line2 <- paste0('| ',.ai_metric_text(metrics(),elapsed,final=finish || failed))
    redraw_both <- interactive() && isatty(stdout()) && Sys.getenv('RSTUDIO')!='1'
    if(previous_width>0L && redraw_both) {
      cat('\r\033[2K\033[1A\r\033[2K',line1,'\n',line2,sep='')
    } else if(previous_width==0L || finish || failed || !identical(last_stage,label)) {
      if(previous_width>0L) cat('\r',strrep(' ',previous_width),'\r',sep='')
      cat(line1,'\n',line2,sep='')
    } else cat('\r',line2,strrep(' ',max(0L,previous_width-nchar(line2,type='width'))),sep='')
    previous_width <<- nchar(line2,type='width');last_stage <<- label
    utils::flush.console()
    last_draw <<- elapsed
    if (finish || failed) { cat('\n'); closed <<- TRUE }
    invisible(NULL)
  }
  list(update = update, elapsed = elapsed_time)
}

.ai_send_once <- function(provider, data = NULL, prompt = NULL, model = NULL,
                     base_url = NULL, api_key = NULL,
                     stream = TRUE, window = FALSE, viewer = TRUE, echo = NULL,
                     on_chunk = NULL, system_prompt = .ai_system,
                     temperature = 0.2, max_tokens = 2048L,
                     max_rows = 100L, max_chars = 30000L,
                     timeout = 600L, retries = 3L, retry_wait = 2,
                     progress = isTRUE(viewer), history = NULL, .view = NULL, include_usage = TRUE, on_status = NULL, reasoning_effort = NULL) {
  .ai_require()
  if(!is.null(on_status) && !is.function(on_status)) stop('on_status must be a function.')
  if(provider=='exaone') {
    if(is.null(api_key) || !nzchar(trimws(api_key)))
      stop('EXAONE API key required: use exaone_setting(api_key=..., open=FALSE) or EXAONE_API_KEY.',call.=FALSE)
    if(!is.null(reasoning_effort)) reasoning_effort <- match.arg(reasoning_effort,c('low','high','max'))
  }
  if (is.null(echo)) echo <- !isTRUE(progress)
  if (!is.null(on_chunk) && !is.function(on_chunk)) stop('on_chunk\ub294 \ud568\uc218\uc5ec\uc57c \ud569\ub2c8\ub2e4.')
  query <- .ai_prompt(data, prompt, max_rows, max_chars)
  if (is.null(model) || !nzchar(model)) stop('\ubaa8\ub378 ID\uac00 \ud544\uc694\ud569\ub2c8\ub2e4.')
  if (identical(provider, 'aistudio')) {
    # API 키의 시작 문자/구분 기호는 검증하지 않는다.
    # 키의 실제 유효성 및 권한은 Google API 서버가 판단한다.
    if (is.null(api_key) || (is.character(api_key) && length(api_key) == 1L &&
                             !is.na(api_key) && !nzchar(trimws(api_key))))
      api_key <- Sys.getenv('GEMINI_API_KEY', '')
    if (!is.character(api_key) || length(api_key) != 1L ||
        is.na(api_key) || !nzchar(trimws(api_key)))
      stop('Google AI Studio API \ud0a4\ub97c \uc804\ub2ec\ud558\uac70\ub098 GEMINI_API_KEY \ud658\uacbd\ubcc0\uc218\uc5d0 \uc124\uc815\ud558\uc2ed\uc2dc\uc624.', call. = FALSE)
    # HTTP 헤더 및 curl 설정 파일의 줄바꿈 삽입만 차단한다.
    if (grepl('[[:cntrl:]]', api_key))
      stop('API \ud0a4\uc5d0 \uc904\ubc14\uafc8 \ub610\ub294 \uc81c\uc5b4\ubb38\uc790\uac00 \ud3ec\ud568\ub418\uc5b4 \uc788\uc2b5\ub2c8\ub2e4.', call. = FALSE)
    api_key <- trimws(api_key)
  }
  if (provider %in% c('lmstudio', 'lemonade', 'exaone')) {
    if (is.null(base_url)) base_url <- if(provider=='exaone') 'https://api.exaone.ai/v1' else 'http://127.0.0.1:5678'
    url <- .ai_url(provider, base_url, 'chat/completions')
    payload <- list(model = model,
                    messages = c(list(list(role = 'system', content = system_prompt)),
                                 if (is.null(history)) list(list(role = 'user', content = query$user)) else history),
                    stream = isTRUE(stream), temperature = temperature,
                    max_tokens = as.integer(max_tokens))
    if(provider=='exaone' && !is.null(reasoning_effort)) payload$reasoning_effort <- reasoning_effort
    if (isTRUE(stream) && isTRUE(include_usage)) payload$stream_options <- list(include_usage=TRUE)
  } else if (identical(provider, 'ollama')) {
    if (is.null(base_url)) base_url <- 'http://127.0.0.1:11434'
    url <- paste0(sub('/+$', '', base_url), '/api/chat')
    payload <- list(model = model,
                    messages = c(list(list(role = 'system', content = system_prompt)),
                                 if (is.null(history)) list(list(role = 'user', content = query$user)) else history),
                    stream = isTRUE(stream),
                    options = list(temperature = temperature, num_predict = as.integer(max_tokens)))
  } else if (identical(provider, 'aistudio')) {
    if (is.null(base_url)) base_url <- 'https://generativelanguage.googleapis.com/v1beta'
    url <- paste0(sub('/+$', '', base_url), '/models/', model, ':',
                  if (isTRUE(stream)) 'streamGenerateContent?alt=sse' else 'generateContent')
    payload <- list(
      systemInstruction = list(parts = list(list(text = system_prompt))),
      contents = lapply(if (is.null(history)) list(list(role = 'user', content = query$user)) else history,
        function(message) list(role = if (message$role == 'assistant') 'model' else 'user',
          parts = list(list(text = message$content)))),
      generationConfig = list(temperature = temperature,
                              maxOutputTokens = as.integer(max_tokens)))
  } else stop('\uc9c0\uc6d0\ud558\uc9c0 \uc54a\ub294 AI \uc81c\uacf5\uc790\uc785\ub2c8\ub2e4.')

  body_file <- tempfile(fileext = '.json')
  conf_file <- tempfile(fileext = '.conf')
  header_file <- tempfile(fileext = '.headers')
  on.exit(unlink(c(body_file, conf_file, header_file)), add = TRUE)
  jsonlite::write_json(payload, body_file, auto_unbox = TRUE, null = 'null')
  # API key must never be placed on the command line, printed, or written to an R project file.
  # curl reads a short-lived config file; it is deleted when the call finishes.
  extra_args <- character()
  if (!is.null(api_key) && nzchar(api_key)) {
    if (grepl('[[:cntrl:]]', api_key)) stop('API \ud0a4\uc5d0 \uc81c\uc5b4\ubb38\uc790\uac00 \ud3ec\ud568\ub418\uc5b4 \uc788\uc2b5\ub2c8\ub2e4.')
    # encodeString()이 따옴표와 역슬래시를 escape하여 curl config 주입을 방지한다.
    header_line <- paste0('header = ',
                          encodeString(paste0(if (provider == 'aistudio') 'x-goog-api-key: ' else 'Authorization: Bearer ', api_key), quote = '"'))
    writeLines(header_line, conf_file, useBytes = TRUE)
    try(Sys.chmod(conf_file, mode = '0600'), silent = TRUE)
    extra_args <- c('--config', conf_file)
  }
  args <- c('--silent', '--show-error', '--no-buffer', '--fail-with-body',
            '--connect-timeout', '15', '--max-time', as.character(as.integer(timeout)),
            extra_args, '--dump-header', header_file, '-X', 'POST', url,
            '-H', 'Content-Type: application/json',
            '--data-binary', paste0('@', body_file))
  pop <- if (isTRUE(window)) .ai_popup(paste0(toupper(provider), ' | AI \uc2e4\uc2dc\uac04 \uc751\ub2f5')) else NULL
  if (!is.null(pop)) pop$push(paste0('\uc9c8\ubb38: ', query$raw_prompt, '\n\n[\uc0dd\uc131 \uc694\uccad \uc911...]\n'))
  view <- if (!is.null(.view)) .view else if (isTRUE(viewer)) .ai_viewer(paste0(toupper(provider), ' | RStudio Viewer'), query$raw_prompt) else NULL
  notify <- function(text) {
    if(!is.null(view)) view$status(text)
    if(is.function(on_status)) on_status(text)
    invisible(NULL)
  }
  if(is.null(.view) && !is.null(view) && is.function(view$close)) on.exit(view$close(),add=TRUE)
  if (!is.null(view) || is.function(on_status)) notify('\uc11c\ubc84 \uc751\ub2f5 \ub300\uae30 \uc911...')
  metrics <- list()
  usage <- function(info) { metrics <<- utils::modifyList(metrics,info) }
  console_stream <- isTRUE(stream) && isTRUE(echo)
  meter <- .ai_console_progress(isTRUE(progress) && !console_stream,metrics=function() metrics)
  completed <- FALSE
  cancelled <- FALSE
  on.exit(if (!completed) meter$update(if(cancelled) '\uc0ac\uc6a9\uc790 \uc911\uc9c0' else '\uc694\uccad \uc911\ub2e8/\uc2e4\ud328', failed = TRUE, force = TRUE), add = TRUE)
  meter$update('\uc694\uccad \uc900\ube44', force = TRUE)
  activity_state <- 'waiting'
  stage_label <- function() switch(activity_state,
    reasoning='\ubaa8\ub378 \ucd94\ub860 \uc911 (\ubcf8\ubb38 \ub300\uae30)',
    content='\uc751\ub2f5 \uc0dd\uc131 \uc911',
    event='\uc751\ub2f5 \uc218\uc2e0 \uc911 (\ubcf8\ubb38 \ub300\uae30)',
    '\uc11c\ubc84 \uc751\ub2f5 \ub300\uae30')
  activity <- function(stage) {
    if(isTRUE(stream) && stage %in% c('reasoning','content')) {
      if(is.null(metrics$first_chunk_seconds)) metrics$first_chunk_seconds <<- meter$elapsed()
      metrics$output_chunks <<- if(is.null(metrics$output_chunks)) 1L else metrics$output_chunks+1L
    }
    previous <- activity_state
    if(stage %in% c('reasoning','content') || activity_state=='waiting') activity_state <<- stage
    if(previous != activity_state) {
      meter$update(stage_label(),receiving=TRUE,force=TRUE)
      if(!is.null(view) || is.function(on_status)) notify(stage_label())
    }
  }
  finish_reason <- NULL
  terminal <- FALSE
  finish <- function(reason) { finish_reason <<- reason }
  full <- ''
  show <- function(s) {
    if (!is.character(s) || length(s) != 1L || !nzchar(s)) return(invisible(NULL))
    full <<- paste0(full, s)
    meter$update("\uc751\ub2f5 \uc0dd\uc131 \uc911", receiving = TRUE)
    if (isTRUE(echo) && (!isTRUE(progress) || console_stream)) { cat(s); utils::flush.console() }
    if (is.function(on_chunk)) on_chunk(s)
    if (!is.null(pop)) pop$push(s)
    if (!is.null(view)) view$push(s)
    invisible(NULL)
  }
  # Google의 일시적 429/5xx 응답만 재시도한다. 출력이 시작되었다면
  # 중복 텍스트 생성을 막기 위해 자동 재시도하지 않는다.
  retries <- as.integer(retries)
  if (is.na(retries) || retries < 0L || retries > 10L)
    stop('retries\ub294 0~10 \uc0ac\uc774 \uc815\uc218\uc5ec\uc57c \ud569\ub2c8\ub2e4.', call. = FALSE)
  if (!is.numeric(retry_wait) || length(retry_wait) != 1L ||
      !is.finite(retry_wait) || retry_wait < 0)
    stop('retry_wait\ub294 0 \uc774\uc0c1 \uc22b\uc790\uc5ec\uc57c \ud569\ub2c8\ub2e4.', call. = FALSE)
  for (attempt in seq_len(retries + 1L)) {
    writeLines(character(), header_file)
    stderr <- ''
    carry <- ''
    raw <- ''
    error_body <- ''
    got_event <- FALSE
    consume_line <- function(line) {
      line <- sub('\r$', '', line)
      if (!nzchar(line)) return(invisible(NULL))
      if (isTRUE(stream)) {
        if (provider != 'ollama' && !startsWith(line, 'data:')) return(invisible(NULL))
        got_event <<- TRUE
        if (.ai_parse(line, provider, TRUE, show, finish, usage, activity)) terminal <<- TRUE
      }
      invisible(NULL)
    }
    ingest <- function(s) {
      if (!length(s) || !nzchar(s)) return(invisible(NULL))
      # 실패 응답의 JSON 본문도 보존하여 curl 종료 코드 대신 실제 오류를 표시한다.
      if (nchar(error_body, type = 'bytes') < 30000L)
        error_body <<- paste0(error_body, s)
      if (!isTRUE(stream)) {
        raw <<- paste0(raw, s)
        return(invisible(NULL))
      }
      carry <<- paste0(carry, s)
      repeat {
        k <- regexpr('\n', carry, fixed = TRUE)[1L]
        if (k < 0L) break
        ln <- if (k > 1L) substr(carry, 1L, k - 1L) else ''
        carry <<- substring(carry, k + 1L)
        consume_line(ln)
      }
      invisible(NULL)
    }
    proc <- processx::process$new(.ai_curl(), args = args, stdout = '|', stderr = '|',
                                  cleanup = TRUE, windows_hide_window = TRUE)
    last_gui_update <- 0
    last_view_update <- 0
    tryCatch({
      repeat {
        if(!is.null(view) && is.function(view$poll)) view$poll()
        requested <- (!is.null(view) && is.function(view$is_stopped) && view$is_stopped()) ||
          (isTRUE(.ai_state()$busy) && isTRUE(.ai_state()$cancel_requested))
        if(requested) {
          cancelled <- TRUE
          stop(structure(list(message='Response stopped by user.',call=NULL),class=c('ai_cancelled','error','condition')))
        }
        state <- proc$poll_io(25L)
        if(isTRUE(viewer) || !is.null(view)) Sys.sleep(0.001)
        meter$update(stage_label(), receiving = activity_state != 'waiting')
        if((!is.null(view) || is.function(on_status)) && meter$elapsed()-last_view_update>=0.5) {
          notify(paste(stage_label(),.ai_metric_text(metrics,meter$elapsed()),sep='\n'))
          last_view_update <- meter$elapsed()
        }
        # Keep Tcl/Tk responsive even BEFORE the first AI token arrives.
        if ((isTRUE(window) || isTRUE(getOption('r_ai_gui_active', FALSE))) &&
            capabilities('tcltk') && (as.numeric(Sys.time()) - last_gui_update > 0.2)) {
          last_gui_update <- as.numeric(Sys.time())
          try(tcltk::tcl('update'), silent = TRUE)
        }
        if (state[['output']] %in% c('ready', 'closed')) {
          t <- proc$read_output()
          if (length(t)) ingest(paste(t, collapse = ''))
        }
        if (state[['error']] %in% c('ready', 'closed')) {
          t <- proc$read_error()
          if (length(t)) stderr <- paste0(stderr, paste(t, collapse = ''))
        }
        if (!proc$is_alive()) {
          t <- proc$read_all_output()
          if (length(t)) ingest(paste(t, collapse = ''))
          t <- proc$read_all_error()
          if (length(t)) stderr <- paste0(stderr, paste(t, collapse = ''))
          break
        }
      }
    }, error = function(e) {
      if(inherits(e,'ai_cancelled')) stop(e)
      msg <- conditionMessage(e)
      if (!is.null(api_key) && nzchar(api_key)) msg <- gsub(api_key, '[REDACTED]', msg, fixed = TRUE)
      e <- simpleError(msg)
      if (!is.null(pop)) pop$push(paste0('\n[\uc624\ub958] ', conditionMessage(e), '\n'))
      if (!is.null(view)) { view$push(paste0('\n[\uc624\ub958] ', conditionMessage(e))); notify('\uc694\uccad \uc2e4\ud328') }
      stop(e)
    }, finally = {
      if (proc$is_alive()) proc$kill()
    })
    if (isTRUE(stream) && nzchar(carry)) consume_line(carry)
    status <- proc$get_exit_status()
    headers <- tryCatch(readLines(header_file, warn = FALSE), error = function(e) character())
    statuses <- grep('^HTTP/[0-9.]+[[:space:]]+[0-9]{3}', headers, value = TRUE)
    http_code <- if (length(statuses)) {
      as.integer(sub('^HTTP/[0-9.]+[[:space:]]+([0-9]{3}).*$', '\\1', utils::tail(statuses, 1L)))
    } else NA_integer_
    if (identical(status, 0L)) break

    # JSON 에러 메시지는 사용자에게 표시하되 API 키가 섞이면 마스킹한다.
    api_message <- ''
    if (nzchar(error_body)) {
      err_obj <- tryCatch(jsonlite::fromJSON(error_body, simplifyVector = FALSE),
                          error = function(e) NULL)
      if (!is.null(err_obj$error$message)) api_message <- err_obj$error$message
    }
    if (!nzchar(api_message)) api_message <- if (nzchar(error_body)) error_body else stderr
    if (!is.null(api_key) && nzchar(api_key))
      api_message <- gsub(api_key, '[REDACTED]', api_message, fixed = TRUE)

    can_retry <- identical(provider, 'aistudio') &&
      !is.na(http_code) && http_code %in% c(429L, 500L, 502L, 503L, 504L) &&
      !nzchar(full) && attempt <= retries
    if (can_retry) {
      delay <- min(30, retry_wait * 2^(attempt - 1L))
      notice <- sprintf('\n[Gemini HTTP %s: %s\ucd08 \ud6c4 \uc7ac\uc2dc\ub3c4 (%d/%d)]\n',
                        http_code, format(delay), attempt, retries)
      meter$update(paste0("\uc7ac\uc2dc\ub3c4 \ub300\uae30 (", attempt, "/", retries, ")"), force = TRUE)
      if (isTRUE(echo) && !isTRUE(progress)) cat(notice)
      if (!is.null(pop)) pop$push(notice)
      if (!is.null(view) || is.function(on_status)) notify(paste0('HTTP ', http_code, ': ', delay, '\ucd08 \ud6c4 \uc7ac\uc2dc\ub3c4 ', attempt, '/', retries))
      if (is.function(on_chunk)) on_chunk(notice)
      Sys.sleep(delay)
      next
    }
    suggestion <- if (identical(http_code, 503L))
      ' Google \uc11c\ubc84 \uc77c\uc2dc \uacfc\ubd80\ud558 \uac00\ub2a5\uc131: \uc7a0\uc2dc \ud6c4 \uc7ac\uc2dc\ub3c4\ud558\uac70\ub098 \ub2e4\ub978 Gemini \ubaa8\ub378\uc744 \uc120\ud0dd\ud558\uc2ed\uc2dc\uc624.'
    else if (identical(http_code, 429L))
      ' \uc694\uccad \uc81c\ud55c \ub610\ub294 \ud560\ub2f9\ub7c9 \ucd08\uacfc \uac00\ub2a5\uc131: Google AI Studio\uc758 \uc0ac\uc6a9\ub7c9\uacfc \ud55c\ub3c4\ub97c \ud655\uc778\ud558\uc2ed\uc2dc\uc624.'
    else ''
    if (!is.null(pop)) pop$push(paste0('\n[\uc624\ub958] ', substr(api_message, 1L, 1200L), '\n'))
    if (!is.null(view)) { view$push(paste0('\n[\uc624\ub958] ', substr(api_message, 1L, 1200L))); notify('\uc694\uccad \uc2e4\ud328') }
    stop(provider, ' HTTP \uc694\uccad \uc2e4\ud328 (HTTP ',
         if (is.na(http_code)) '\ubbf8\ud655\uc778' else http_code,
         ', curl \uc885\ub8cc ', status, '): ', substr(api_message, 1L, 1800L),
         suggestion, call. = FALSE)
  }
  if (!isTRUE(stream)) {
    .ai_parse(raw, provider, FALSE, show, finish, usage, activity)
    terminal <- TRUE
  }
  if (isTRUE(stream) && !terminal)
    stop('\uc751\ub2f5 \uc5f0\uacb0\uc774 \uc644\ub8cc \uc2e0\ud638 \uc5c6\uc774 \ub05d\ub0ac\uc2b5\ub2c8\ub2e4. ai_continue()\ub85c \uc774\uc5b4 \uc4f8 \uc218 \uc788\uc2b5\ub2c8\ub2e4.', call. = FALSE)
  if (!nzchar(full)) {
    if(activity_state=='reasoning') stop('\ubaa8\ub378\uc774 \ucd94\ub860\ub9cc \uc0dd\uc131\ud558\uace0 \ubcf8\ubb38\uc744 \ubc18\ud658\ud558\uc9c0 \uc54a\uc558\uc2b5\ub2c8\ub2e4. max_tokens\ub97c \ub298\ub9ac\uac70\ub098 \ubaa8\ub378\uc758 \ucd94\ub860 \uc124\uc815\uc744 \ud655\uc778\ud558\uc2ed\uc2dc\uc624.',call.=FALSE)
    note <- if (got_event) '\uc751\ub2f5 \uc774\ubca4\ud2b8\uc5d0\uc11c \ud14d\uc2a4\ud2b8\ub97c \ucc3e\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4.' else 'AI \uc751\ub2f5\uc744 \uc218\uc2e0\ud558\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4.'
    if (!is.null(pop)) pop$push(paste0('\n[\uc624\ub958] ', note, '\n'))
    if (!is.null(view)) { view$push(paste0('\n[\uc624\ub958] ', note)); notify('\ud14d\uc2a4\ud2b8 \uc5c6\uc74c') }
    stop(provider, ': ', note, ' \ubaa8\ub378/\ucd9c\ub825 \ud1a0\ud070/\uc548\uc804 \uc124\uc815\uc744 \ud655\uc778\ud558\uc2ed\uc2dc\uc624.', call. = FALSE)
  }
  metrics <- .ai_finalize_metrics(metrics,meter$elapsed())
  meter$update('\uc644\ub8cc', finish = TRUE, force = TRUE)
  completed <- TRUE
  if (isTRUE(echo) && isTRUE(progress) && !console_stream) cat(full)
  if (isTRUE(echo)) cat('\n')
  if (!is.null(pop)) pop$push('\n\n[\uc644\ub8cc]')
  if (!is.null(view) || is.function(on_status)) notify(paste0('\uc644\ub8cc | ',.ai_metric_text(metrics,metrics$elapsed_seconds,TRUE)))
  invisible(structure(full, finish_reason = finish_reason, metrics = metrics))
}

