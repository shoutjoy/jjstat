.ai_ui_dir <- function() {
  env <- environment(.ai_ui_dir)
  if(isNamespace(env)) return(system.file('assets','ui',package=getNamespaceName(env),mustWork=TRUE))
  normalizePath(file.path(dirname(.ai_math_dir()),'ui'),winslash='/',mustWork=TRUE)
}
.ai_json_read <- function(file,default=NULL) tryCatch(jsonlite::fromJSON(file,simplifyVector=FALSE),error=function(e)default)
.ai_json_write <- function(value,file) {
  tmp <- tempfile('json_',tmpdir=dirname(file))
  jsonlite::write_json(value,tmp,auto_unbox=TRUE,null='null',na='null',digits=NA)
  if(file.exists(file)) unlink(file)
  if(!file.rename(tmp,file)) {file.copy(tmp,file,overwrite=TRUE);unlink(tmp)}
  invisible(NULL)
}
.ai_history_dir <- function() file.path(tools::R_user_dir('jjstat','data'),'ai-history')
.ai_record_id <- function() paste0(format(Sys.time(),'%Y%m%dT%H%M%S'),'_',basename(tempfile('record_')))
.ai_history_write <- function(record,folder=.ai_history_dir()) {
  dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  safe <- record[intersect(names(record),c('id','question','markdown','html','provider','model','created','updated','finish_reason','status'))]
  if(is.null(safe$id))safe$id <- .ai_record_id()
  if(!grepl('^[A-Za-z0-9_-]+$',safe$id))stop('Invalid history identifier.')
  .ai_json_write(safe,file.path(folder,paste0(safe$id,'.json')))
  invisible(safe$id)
}
.ai_history_records <- function(folder=.ai_history_dir()) {
  files <- list.files(folder,pattern='[.]json$',full.names=TRUE)
  records <- Filter(function(x)is.list(x)&&!is.null(x$id)&&!is.null(x$question),lapply(files,.ai_json_read))
  if(length(records))records <- records[order(vapply(records,function(x)if(is.null(x$updated))'' else x$updated,character(1)),decreasing=TRUE)]
  records
}
#' List locally saved AI questions and responses
#' @param limit Maximum number of recent records.
#' @return A data frame with record ID, question, provider, model and time.
#' @export
ai_history <- function(limit=100L) {
  rows <- utils::head(.ai_history_records(),as.integer(limit))
  if(!length(rows))return(data.frame(id=character(),question=character(),provider=character(),model=character(),updated=character()))
  do.call(rbind,lapply(rows,function(x)data.frame(id=x$id,question=x$question,provider=x$provider,model=x$model,updated=x$updated,stringsAsFactors=FALSE)))
}
.ai_runtime_info <- function() {
  env <- environment(.ai_web_window)
  if(isNamespace(env))return(list(package=getNamespaceName(env),libs=.libPaths(),assets=.ai_math_dir()))
  root <- dirname(dirname(.ai_math_assets))
  if(file.exists(file.path(root,'ai_web.R')))return(list(modules=normalizePath(root,winslash='/'),libs=.libPaths(),assets=.ai_math_dir()))
  script <- file.path(root,'ai_clients.R')
  if(file.exists(script))return(list(script=normalizePath(script,winslash='/'),libs=.libPaths(),assets=.ai_math_dir()))
  modules <- file.path(dirname(root),'R')
  if(!dir.exists(modules))stop('Cannot locate AI source modules.')
  list(modules=normalizePath(modules,winslash='/'),libs=.libPaths(),assets=.ai_math_dir())
}
.ai_web_worker <- function(boot) {
  .ai_require();if(!requireNamespace('commonmark',quietly=TRUE))stop('Install commonmark to render Markdown.')
  box <- boot$box;statefile <- file.path(box,'state.json');jobfile <- file.path(box,'job.json');lock <- file.path(box,'working')
  .ai_save_config(boot$provider,boot$config$base_url,boot$config$model,boot$config$api_key,FALSE)
  record <- .ai_json_read(statefile,list());last_write <- 0;rendered_text <- NULL
  publish <- function(force=FALSE) {
    now <- proc.time()[['elapsed']]
    if(!force && now-last_write<0.1)return(invisible(NULL))
    last_write <<- now
    if(!identical(rendered_text,record$markdown)){record$html <<- .ai_markdown_html(record$markdown);rendered_text <<- record$markdown}
    record$revision <<- paste0(Sys.time(),'_',nchar(record$markdown))
    .ai_json_write(record,statefile)
  }
  if(nzchar(record$markdown)) {
    seed <- .ai_state();query <- .ai_prompt(boot$data,record$question,100L,30000L)
    seed$last <- list(provider=boot$provider,model=boot$config$model,base_url=boot$config$base_url,
      question=record$question,text=record$markdown,finish_reason='restored',
      history=list(list(role='user',content=query$user),list(role='assistant',content=record$markdown)),
      options=boot$options)
  }
  writeLines('ready',file.path(box,'worker-ready'))
  repeat {
    if(file.exists(file.path(box,'quit')))break
    job <- .ai_json_read(jobfile)
    if(is.null(job)){Sys.sleep(0.025);next}
    file.create(lock)
    record <- list(id=job$id,job_id=job$id,question=job$question,markdown='',html='',provider=boot$provider,
      model=job$model,console_url=boot$console_url,created=format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6%z'),updated='',status='\uc11c\ubc84 \uc751\ub2f5 \ub300\uae30 \uc911...',running=TRUE,finish_reason=NULL)
    if(identical(job$kind,'continue'))record$markdown <- ai_last_response()
    publish(TRUE);unlink(jobfile)
    cancel <- function() {
      if(file.exists(file.path(box,paste0('stop_',job$id)))||file.exists(file.path(box,'quit')))ai_stop()
    }
    args <- boot$options
    args$viewer <- FALSE;args$window <- FALSE;args$echo <- FALSE;args$progress <- FALSE
    args$on_chunk <- function(text){record$markdown <<- paste0(record$markdown,text);cancel();publish()}
    args$on_status <- function(text){record$status <<- text;cancel();publish()}
    tryCatch({
      if(identical(job$kind,'continue')) {
        args$prompt <- job$question
        answer <- do.call(ai_continue,args)
      } else {
        args$provider <- boot$provider;args$data <- boot$data;args$prompt <- job$question
        args$model <- if(nzchar(job$model))job$model else NULL
        args$base_url <- boot$config$base_url;args$api_key <- boot$config$api_key
        answer <- do.call(.ai_chat,args)
      }
      record$markdown <- as.character(answer)
      last <- .ai_state()$last;record$model <- last$model;record$finish_reason <- last$finish_reason
      record$status <- if(identical(last$finish_reason,'cancelled'))'\uc911\uc9c0\ub428 \u00b7 \ubc1b\uc740 \ub0b4\uc6a9\uc740 \uc800\uc7a5\ub418\uc5c8\uc2b5\ub2c8\ub2e4.' else if(.ai_truncated(last$finish_reason))'\ud1a0\ud070 \uc81c\ud55c \u00b7 \uc774\uc5b4\uc11c \uc791\uc131\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.' else '\uc644\ub8cc'
      if(!is.null(last$metrics))record$status <- paste(record$status,.ai_metric_text(last$metrics,last$metrics$elapsed_seconds,TRUE),sep='\n')
    },error=function(e){record$status <<- paste0('\uc624\ub958: ',conditionMessage(e));record$finish_reason <<- 'error'})
    record$running <- FALSE;record$updated <- format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6%z')
    record$html <- .ai_markdown_html(record$markdown)
    .ai_history_write(record,boot$history)
    publish(TRUE)
    unlink(lock)
  }
}
.ai_web_http <- function(boot) {
  requireNamespace('jsonlite',quietly=TRUE)
  listener <- NULL
  for(port in (35000L+as.integer(Sys.getpid()%%15000L))+0:100) {
    listener <- tryCatch(serverSocket(port),error=function(e)NULL)
    if(!is.null(listener))break
  }
  if(is.null(listener))stop('No free local port.')
  on.exit(close(listener),add=TRUE)
  prefix <- paste0('/',boot$token,'/');origin <- paste0('http://127.0.0.1:',port)
  .ai_json_write(list(url=paste0(origin,prefix,'index.html')),file.path(boot$box,'server-ready.json'))
  last_contact <- proc.time()[['elapsed']];last_state <- .ai_json_read(file.path(boot$box,'state.json'),list())
  reply <- function(con,status,body,type='application/json; charset=utf-8') {
    if(!is.raw(body))body <- charToRaw(enc2utf8(body))
    header <- paste0('HTTP/1.1 ',status,'\r\nContent-Type: ',type,'\r\nContent-Length: ',length(body),
      '\r\nCache-Control: no-store\r\nConnection: close\r\nX-Content-Type-Options: nosniff\r\n\r\n')
    writeBin(c(charToRaw(header),body),con)
  }
  json <- function(con,value,status='200 OK')reply(con,status,jsonlite::toJSON(value,auto_unbox=TRUE,null='null'))
  snapshot <- function() {
    last_state <<- .ai_json_read(file.path(boot$box,'state.json'),last_state)
    if(file.exists(file.path(boot$box,'job.json'))||file.exists(file.path(boot$box,'working')))last_state$running <<- TRUE
    last_state
  }
  repeat {
    if(file.exists(file.path(boot$box,'quit')))break
    if(proc.time()[['elapsed']]-last_contact>300){writeLines('quit',file.path(boot$box,'quit'));break}
    if(!socketSelect(list(listener),timeout=0.25))next
    con <- socketAccept(listener,blocking=TRUE,open='r+b',timeout=2)
    tryCatch({
      line <- readLines(con,n=1L,warn=FALSE);headers <- character()
      if(!length(line))stop('Empty request')
      repeat {h<-readLines(con,n=1L,warn=FALSE);if(!length(h)||!nzchar(h))break;headers<-c(headers,h);if(sum(nchar(headers))>8192L)stop('Headers too large')}
      parts <- strsplit(line,' ',fixed=TRUE)[[1L]];method<-parts[1L];path<-sub('[?].*$','',parts[2L])
      if(!startsWith(path,prefix)){json(con,list(error='Not found'),'404 Not Found');next}
      last_contact <- proc.time()[['elapsed']]
      rel <- substring(path,nchar(prefix)+1L)
      if(grepl('(^|/)\\.\\.(/|$)|[\\\\:]',utils::URLdecode(rel)))stop('Invalid path')
      length_header <- grep('^Content-Length:',headers,value=TRUE,ignore.case=TRUE)
      count <- if(length(length_header))as.numeric(trimws(sub('^[^:]*:','',length_header[1L]))) else 0
      if(!is.finite(count)||count<0||count>1024*1024)stop('Invalid body size')
      payload <- if(count>0)jsonlite::fromJSON(rawToChar(readBin(con,'raw',count)),simplifyVector=FALSE) else list()
      if(method=='POST') {
        given_origin <- grep('^Origin:',headers,value=TRUE,ignore.case=TRUE)
        if(length(given_origin)&&trimws(sub('^[^:]*:','',given_origin[1L]))!=origin)stop('Origin not allowed')
        if(rel %in% c('api/ask','api/continue')) {
          s <- snapshot()
          if(isTRUE(s$running)){json(con,list(error='\ud604\uc7ac \uc751\ub2f5\uc744 \uc644\ub8cc\ud558\uac70\ub098 \uc911\uc9c0\ud55c \ub4a4 \uc2e4\ud589\ud558\uc2ed\uc2dc\uc624.'),'409 Conflict');next}
          if(!is.character(payload$question)||length(payload$question)!=1L||!nzchar(trimws(payload$question)))stop('\uc9c8\ubb38\uc744 \uc785\ub825\ud558\uc2ed\uc2dc\uc624.')
          if(nchar(payload$question)>30000L)stop('Question is too long.')
          if(!is.null(payload$model)&&(!is.character(payload$model)||length(payload$model)!=1L||is.na(payload$model)))stop('Invalid model.')
          if(rel=='api/continue'&&!nzchar(s$markdown))stop('No previous response to continue.')
          id <- .ai_record_id()
          .ai_json_write(list(id=id,question=payload$question,model=if(is.null(payload$model))'' else payload$model,
            kind=if(rel=='api/continue')'continue' else 'ask'),file.path(boot$box,'job.json'))
          json(con,list(id=id,accepted=TRUE),'202 Accepted')
        } else if(rel=='api/stop') {
          s<-snapshot();pending<-.ai_json_read(file.path(boot$box,'job.json'))
          id<-if(!is.null(pending$id))pending$id else s$job_id
          if(!is.null(id))writeLines('stop',file.path(boot$box,paste0('stop_',id)))
          json(con,list(stopped=TRUE))
        } else if(rel=='api/close'){writeLines('quit',file.path(boot$box,'quit'));json(con,list(closed=TRUE))}
        else json(con,list(error='Not found'),'404 Not Found')
      } else if(method=='GET'&&rel=='api/state')json(con,snapshot())
      else if(method=='GET'&&rel=='api/history') {
        records<-utils::head(.ai_history_records(boot$history),1000L)
        json(con,list(records=lapply(records,function(x)x[intersect(names(x),c('id','question','provider','model','created','updated','finish_reason'))])))
      } else if(method=='GET'&&startsWith(rel,'api/history/')) {
        id<-substring(rel,nchar('api/history/')+1L)
        if(!grepl('^[A-Za-z0-9_-]+$',id))stop('Invalid history ID')
        record<-.ai_json_read(file.path(boot$history,paste0(id,'.json')))
        if(is.null(record))json(con,list(error='\uae30\ub85d\uc744 \ucc3e\uc744 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4.'),'404 Not Found') else {if(is.character(record$markdown)&&length(record$markdown)==1L)record$html<-.ai_markdown_html(record$markdown);json(con,record)}
      } else if(method=='GET'&&rel=='config.js') {
        s <- snapshot()
        reply(con,'200 OK',paste0('window.APP_CONFIG=',jsonlite::toJSON(list(mode='window',provider=s$provider,model=s$model,prompt=s$question,console_url=boot$console_url),auto_unbox=TRUE),';'),'text/javascript; charset=utf-8')
      } else if(method=='GET') {
        source<-if(rel=='index.html')file.path(boot$ui,'index.html') else if(startsWith(rel,'ui/'))file.path(boot$ui,substring(rel,4L)) else if(startsWith(rel,'katex/'))file.path(boot$assets,substring(rel,7L)) else ''
        if(!nzchar(source)||!file.exists(source)||dir.exists(source))json(con,list(error='Not found'),'404 Not Found') else {
          ext<-tolower(tools::file_ext(source));type<-switch(ext,html='text/html; charset=utf-8',js='text/javascript; charset=utf-8',css='text/css; charset=utf-8',woff2='font/woff2',woff='font/woff',ttf='font/ttf','application/octet-stream')
          reply(con,'200 OK',readBin(source,'raw',file.info(source)$size),type)
        }
      } else json(con,list(error='Method not allowed'),'405 Method Not Allowed')
    },error=function(e)try(json(con,list(error=conditionMessage(e)),'400 Bad Request'),silent=TRUE),finally=try(close(con),silent=TRUE))
  }
}
.ai_web_window <- function(provider,data=NULL,prompt='\uc774 \ubd84\uc11d\uacb0\uacfc\ub97c \uc124\uba85\ud574\uc918',model=NULL,api_key=NULL,
                           initial_response=NULL,launch=TRUE,history_dir=.ai_history_dir(),...) {
  .ai_require();provider<-.ai_provider(provider);opts<-list(...)
  if(!requireNamespace('commonmark',quietly=TRUE))stop('Install commonmark to render Markdown.')
  if(!is.null(initial_response)&&(!is.character(initial_response)||length(initial_response)!=1L||is.na(initial_response)))stop('initial_response must be a single string.')
  if(!is.character(prompt)||length(prompt)!=1L||is.na(prompt))stop('prompt must be a single string.')
  if(is.function(opts$on_chunk)||is.function(opts$on_status))stop('R callbacks are supported with engine="tk".')
  cfg<-.ai_config(provider)
  if(!is.null(opts$base_url))cfg$base_url<-opts$base_url
  if(!is.null(model))cfg$model<-model
  if(!is.null(api_key))cfg$api_key<-api_key
  cfg$base_url<-.ai_base(cfg$base_url)
  opts[intersect(names(opts),c('data','prompt','model','api_key','base_url','provider','viewer','window','echo','progress','on_chunk','on_status'))]<-NULL
  opts<-utils::modifyList(list(stream=TRUE,max_tokens=4096L,auto_continue=TRUE,max_continuations=3L),opts)
  box<-tempfile('jjstat_ai_web_');dir.create(box)
  runtime<-.ai_runtime_info();runtime$box<-box;runtime$ui<-.ai_ui_dir();runtime$history<-history_dir;runtime$console_url<-.ai_console_bridge()
  runtime$token<-paste0(basename(tempfile()),basename(tempfile()))
  state<-list(id=NULL,job_id=NULL,question=prompt,markdown=if(is.null(initial_response))'' else initial_response,
    html=if(is.null(initial_response))'' else .ai_markdown_html(initial_response),provider=provider,model=cfg$model,
    console_url=runtime$console_url,status='\uc900\ube44 \u00b7 \uc9c8\ubb38 \uc2e4\ud589 \ubc84\ud2bc\uc744 \ub20c\ub7ec \uc694\uccad\ud558\uc2ed\uc2dc\uc624.',running=FALSE,revision='initial')
  .ai_json_write(state,file.path(box,'state.json'))
  helper<-file.path(box,'helper.R')
  writeLines(c("if(.Platform$OS.type=='windows')try(Sys.setlocale('LC_CTYPE','Korean_Korea.utf8'),silent=TRUE)",
    "path<-commandArgs(TRUE)[1L];boot<-readRDS(path);unlink(path)",".libPaths(boot$libs)",
    "if(!is.null(boot$package)){ns<-asNamespace(boot$package)}else{ns<-new.env(parent=globalenv());if(!is.null(boot$script))source(boot$script,local=ns,encoding='UTF-8')else for(file in list.files(boot$modules,pattern='[.]R$',full.names=TRUE))source(file,local=ns,encoding='UTF-8');ns$.ai_math_assets<-boot$assets}",
    "get(boot$role,envir=ns)(boot)"),helper,useBytes=TRUE)
  worker<-runtime;worker$role<-'.ai_web_worker';worker$data<-data;worker$config<-cfg;worker$options<-opts;worker$provider<-provider
  http<-runtime;http$role<-'.ai_web_http'
  workerfile<-file.path(box,'worker.rds');httpfile<-file.path(box,'server.rds')
  saveRDS(worker,workerfile);saveRDS(http,httpfile);try(Sys.chmod(c(workerfile,httpfile),'0600'),silent=TRUE)
  rscript<-file.path(R.home('bin'),if(.Platform$OS.type=='windows')'Rscript.exe' else 'Rscript')
  workers<-lapply(c(workerfile,httpfile),function(file)processx::process$new(rscript,c('--vanilla',helper,file),
    stdout=file.path(box,paste0(basename(file),'.out')),stderr=file.path(box,paste0(basename(file),'.err')),windows_hide_window=TRUE,cleanup=TRUE))
  deadline<-proc.time()[['elapsed']]+20
  repeat {
    ready<-.ai_json_read(file.path(box,'server-ready.json'))
    if(!is.null(ready$url)&&file.exists(file.path(box,'worker-ready')))break
    if(any(!vapply(workers,function(p)p$is_alive(),logical(1)))||proc.time()[['elapsed']]>deadline) {
      for(p in workers)if(p$is_alive())p$kill()
      stop('AI window startup failed. Details: ',box)
    }
    Sys.sleep(0.025)
  }
  st<-.ai_state();st$web_windows<-c(Filter(function(x)any(vapply(x,function(p)p$is_alive(),logical(1))),st$web_windows),list(workers))
  url<-ready$url
  if(isTRUE(launch)) {
    edge<-c(Sys.which('msedge.exe'),file.path(Sys.getenv('ProgramFiles(x86)'),'Microsoft','Edge','Application','msedge.exe'),file.path(Sys.getenv('ProgramFiles'),'Microsoft','Edge','Application','msedge.exe'))
    edge<-edge[nzchar(edge)&file.exists(edge)]
    if(.Platform$OS.type=='windows'&&length(edge))processx::process$new(edge[1L],c(paste0('--app=',url),'--window-size=1200,920'),cleanup=FALSE,windows_hide_window=TRUE)
    else utils::browseURL(url)
  }
  shutdown<-function(){writeLines('quit',file.path(box,'quit'));invisible(NULL)}
  invisible(structure(list(url=url,close=shutdown,path=box,history_dir=history_dir),class='jjstat_ai_window'))
}
