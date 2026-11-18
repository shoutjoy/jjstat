Sys.setlocale('LC_CTYPE', 'Korean_Korea.utf8')
# Local mock HTTP checks; does not call a real model or require RStudio.
for (file in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE)) source(file,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
.ai_require()
server_code <- paste0(
  'const http=require("http");',
  'const s=http.createServer((req,res)=>{let body="";req.on("data",c=>body+=c);',
  'req.on("end",()=>{const p=JSON.parse(body);',
  'if(p.model==="error"){res.writeHead(500);res.end(JSON.stringify({error:{message:"mock failure"}}));return}',
  'if(p.stream){res.writeHead(200,{"Content-Type":"text/event-stream"});',
  'if(p.model!=="empty")res.write("data: "+JSON.stringify({choices:[{delta:{content:"mock answer"}}]})+"\\n\\n");',
  'setTimeout(()=>res.end("data: [DONE]\\n\\n"),650);',
  '}else{res.writeHead(200,{"Content-Type":"application/json"});',
  'res.end(JSON.stringify({choices:[{message:{content:"mock answer"}}]}))}',
  '})});s.listen(0,"127.0.0.1",()=>console.log(s.address().port));'
)
server_file <- tempfile(fileext = '.js')
writeLines(server_code, server_file)
server <- processx::process$new('node', server_file, stdout = '|', stderr = '|')
tryCatch({
  port <- character()
  for (i in seq_len(100L)) {
    server$poll_io(100L)
    port <- server$read_output_lines()
    if (length(port)) break
  }
  if (length(port) != 1L) stop(paste(server$read_all_error(), collapse = "\n"))
  url <- paste0('http://127.0.0.1:', port)
  chunks <- character()
  .ai_viewer <- function(...) list(
    status = function(...) invisible(NULL),
    push = function(text) { chunks <<- c(chunks, text); invisible(NULL) })
  out <- capture.output(answer <- lmstudio('test', model = 'ok', base_url = url,
    viewer = TRUE, on_chunk = function(text) stopifnot(identical(text, 'mock answer'))))
  stopifnot(identical(answer, 'mock answer'), identical(chunks, 'mock answer'),
    any(grepl('100%', out, fixed = TRUE)), any(grepl('추정', out, fixed = TRUE)),
    !any(grepl('mock answer', out, fixed = TRUE)))
  out <- capture.output(answer <- lmstudio('test', model = 'ok', base_url = url,
    viewer = TRUE, progress = FALSE))
  stopifnot(any(grepl('mock answer', out, fixed = TRUE)), !any(grepl('[AI]', out, fixed = TRUE)))
  out <- capture.output(answer <- lmstudio('test', model = 'ok', base_url = url,
    stream = FALSE, progress = TRUE, echo = TRUE))
  stopifnot(any(grepl('100%', out, fixed = TRUE)), any(grepl('mock answer', out, fixed = TRUE)))
  for (model in c('error', 'empty')) {
    failure <- NULL
    out <- capture.output(tryCatch(lmstudio('test', model = model, base_url = url,
      progress = TRUE), error = function(e) failure <<- conditionMessage(e)))
    stopifnot(!is.null(failure), !any(grepl('100%', out, fixed = TRUE)),
      any(grepl('중단/실패', out, fixed = TRUE)))
  }
  out <- capture.output({ meter <- .ai_console_progress(FALSE); meter$update(finish = TRUE) })
  stopifnot(length(out) == 0L)
  cat('PASS: streamed Viewer progress, callbacks, progress opt-out, nonstream echo, HTTP failure, empty response, disabled meter\n')
}, finally = { if (server$is_alive()) server$kill(); unlink(server_file) })