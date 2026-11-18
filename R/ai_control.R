
# A tiny local stop endpoint serviced by the existing nonblocking receive loop.
# It never reads/writes user files and accepts only a per-viewer random URL token.
.ai_control <- function(console_handler=NULL) {
  listener <- NULL
  first <- 35000L + as.integer((as.numeric(Sys.time())*1000+Sys.getpid()) %% 20000)
  for(port in first+0:30) {
    listener <- tryCatch(serverSocket(port),error=function(e) NULL)
    if(!is.null(listener)) break
  }
  if(is.null(listener)) stop('Cannot open the local response-stop endpoint.')
  token <- paste0(basename(tempfile()),basename(tempfile()))
  path <- paste0('/stop/',token);console_path <- paste0('/console/',token)
  stopped <- FALSE; closed <- FALSE; clients <- list()
  close_client <- function(client) try(close(client$con),silent=TRUE)
  poll <- function() {
    if(closed) return(invisible(NULL))
    for(i in seq_len(4L)) {
      if(!socketSelect(list(listener),timeout=0)) break
      con <- socketAccept(listener,blocking=FALSE,open='r+b',timeout=0.01)
      if(length(clients)>=8L) {try(close(con),silent=TRUE);next}
      clients[[length(clients)+1L]] <<- list(con=con,data=raw(),opened=proc.time()[['elapsed']])
    }
    keep <- list()
    for(client in clients) {
      if(proc.time()[['elapsed']]-client$opened>if(is.function(console_handler))15 else 2) {close_client(client);next}
      if(socketSelect(list(client$con),timeout=0)) {
        bytes <- tryCatch(readBin(client$con,'raw',65536L),error=function(e) raw())
        client$data <- c(client$data,bytes)
        text <- tryCatch(rawToChar(client$data),error=function(e) NULL)
        if(is.null(text)) {close_client(client);next}
        if(length(client$data)>1048576L) {close_client(client);next}
        if(grepl('\r\n\r\n',text,fixed=TRUE)) {
          split <- regexpr('\r\n\r\n',text,fixed=TRUE,useBytes=TRUE)[1L]
          header <- rawToChar(client$data[seq_len(split-1L)])
          lines <- strsplit(header,'\r\n',fixed=TRUE)[[1L]];line <- lines[1L]
          length_line <- grep('^Content-Length:',lines,value=TRUE,ignore.case=TRUE)
          count <- if(length(length_line))suppressWarnings(as.numeric(trimws(sub('^[^:]*:','',length_line[1L])))) else 0
          if(!is.finite(count)||count<0||count>1000000) {close_client(client);next}
          start <- split+4L
          if(length(client$data)<start-1L+count){keep[[length(keep)+1L]]<-client;next}
          valid <- line %in% paste0('POST ',path,c(' HTTP/1.1',' HTTP/1.0'))
          console <- is.function(console_handler)&&line %in% paste0('POST ',console_path,c(' HTTP/1.1',' HTTP/1.0'))
          preflight <- is.function(console_handler)&&line %in% paste0('OPTIONS ',console_path,c(' HTTP/1.1',' HTTP/1.0'))
          if(valid) stopped <<- TRUE
          status <- if(valid||console||preflight)'200 OK' else '404 Not Found'
          body <- if(valid)'{"stopped":true}' else if(preflight)'{}' else '{"error":"not found"}'
          if(console) {
            body <- tryCatch({
              if(count==0L)stop('Empty code.')
              payload <- jsonlite::fromJSON(rawToChar(client$data[start:(start+count-1L)]))
              if(!is.character(payload$code)||length(payload$code)!=1L||is.na(payload$code)||!nzchar(payload$code))stop('Empty code.')
              console_handler(payload$code)
              '{"inserted":true,"executed":false}'
            },error=function(e){status<<-'400 Bad Request';as.character(jsonlite::toJSON(list(error=conditionMessage(e)),auto_unbox=TRUE))})
          }
          response <- paste0('HTTP/1.1 ',status,
            '\r\nContent-Type: application/json; charset=utf-8\r\nAccess-Control-Allow-Origin: *',
            '\r\nAccess-Control-Allow-Methods: POST, OPTIONS\r\nAccess-Control-Allow-Headers: Content-Type\r\nCache-Control: no-store',
            '\r\nConnection: close\r\nContent-Length: ',nchar(body,type='bytes'),'\r\n\r\n',body)
          try(writeBin(charToRaw(enc2utf8(response)),client$con),silent=TRUE)
          close_client(client);next
        }
      }
      keep[[length(keep)+1L]] <- client
    }
    clients <<- keep
    invisible(NULL)
  }
  shutdown <- function() {
    if(!closed) {closed <<- TRUE;for(client in clients)close_client(client);try(close(listener),silent=TRUE)}
    invisible(NULL)
  }
  list(console_url=paste0('http://127.0.0.1:',port,console_path),url=paste0('http://127.0.0.1:',port,path),poll=poll,stopped=function()stopped,
       close=shutdown,is_open=function()!closed)
}
#' Stop the active AI response while retaining received text
#' @details The Viewer stop button and RStudio Esc/Stop are available during a call.
#' This function is useful in callbacks or a Tcl/Tk control; it cannot run from a busy console.
#' @return Whether an active request was marked for cancellation, invisibly.
#' @export
ai_stop <- function() {
  state <- .ai_state()
  if(!isTRUE(state$busy)) return(invisible(FALSE))
  state$cancel_requested <- TRUE
  invisible(TRUE)
}

# A persistent main-session bridge: only inserts code; it never evaluates model output.
.ai_console_bridge <- function() {
  if(!requireNamespace('rstudioapi',quietly=TRUE)||!rstudioapi::isAvailable()||!requireNamespace('later',quietly=TRUE))return(NULL)
  st <- .ai_state()
  if(!is.null(st$console_bridge)&&st$console_bridge$is_open())return(st$console_bridge$console_url)
  bridge <- .ai_control(console_handler=function(code)rstudioapi::sendToConsole(code,execute=FALSE,echo=TRUE,focus=TRUE))
  st$console_bridge <- bridge
  tick <- function(){
    if(!bridge$is_open())return(invisible(NULL))
    try(bridge$poll(),silent=TRUE)
    later::later(tick,0.1)
    invisible(NULL)
  }
  later::later(tick,0.1)
  bridge$console_url
}
