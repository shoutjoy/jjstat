# Settings and credentials live in session options, never in package objects.
.ai_state <- function() {
  state <- getOption('jjstat.ai.state')
  if (!is.environment(state)) {
    state <- new.env(parent = emptyenv())
    state$config <- list(); state$keys <- list(); state$last <- NULL; state$busy <- FALSE
    file <- .ai_config_file()
    if (file.exists(file)) state$config <- tryCatch(readRDS(file), error = function(e) list())
    options(jjstat.ai.state = state)
  }
  state
}
.ai_config_file <- function() file.path(tools::R_user_dir('jjstat', 'config'), 'ai-settings.rds')
.ai_defaults <- function(provider) switch(provider,
  ollama = list(base_url = 'http://127.0.0.1:11434', model = ''),
  lmstudio = list(base_url = 'http://127.0.0.1:5678', model = ''),
  lemonade = list(base_url = 'http://127.0.0.1:13305/api/v1', model = ''),
  exaone = list(base_url = 'https://api.exaone.ai/v1', model = 'k-exaone_v2.5'),
  aistudio = list(base_url = 'https://generativelanguage.googleapis.com/v1beta', model = ''))
.ai_provider <- function(provider) match.arg(provider, c('ollama','lmstudio','aistudio','lemonade','exaone'))
.ai_config <- function(provider) {
  provider <- .ai_provider(provider); state <- .ai_state()
  stored <- state$config[[provider]]
  cfg <- utils::modifyList(.ai_defaults(provider), if (is.null(stored)) list() else stored)
  key <- state$keys[[provider]]
  if (is.null(key)) key <- Sys.getenv(switch(provider, aistudio = 'GEMINI_API_KEY',
    exaone = 'EXAONE_API_KEY', lmstudio = 'LM_API_TOKEN', lemonade = 'LEMONADE_API_KEY', ollama = 'OLLAMA_API_KEY'), '')
  cfg$api_key <- key
  cfg
}
.ai_base <- function(base_url, port = NULL) {
  if (!is.character(base_url) || length(base_url) != 1L || is.na(base_url) ||
      !grepl('^https?://[^/?#@[:space:]]+', base_url) || grepl('[?#@[:cntrl:]]', base_url))
    stop('\uc11c\ubc84 \uc8fc\uc18c\ub294 \ud0a4\ub098 \ucffc\ub9ac \uc5c6\uc774 http:// \ub610\ub294 https://\ub85c \uc785\ub825\ud558\uc2ed\uc2dc\uc624.', call. = FALSE)
  base_url <- sub('/+$', '', trimws(base_url))
  if (!is.null(port)) {
    value <- suppressWarnings(as.numeric(port))
    if (length(value) != 1L || !is.finite(value) || value != floor(value) || value < 1 || value > 65535)
      stop('\ud3ec\ud2b8\ub294 1~65535 \uc0ac\uc774 \uc815\uc218\uc5ec\uc57c \ud569\ub2c8\ub2e4.', call. = FALSE)
    authority <- sub('^(https?://[^/]+).*$', '\\1', base_url)
    path <- substring(base_url, nchar(authority) + 1L)
    authority <- sub(':[0-9]+$', '', authority)
    base_url <- paste0(authority, ':', as.integer(value), path)
  }
  base_url
}
.ai_root <- function(base_url) sub('/(?:api/)?v[0-9]+$|/api$', '', sub('/+$','',base_url), perl = TRUE)
.ai_url <- function(provider, base_url, endpoint) {
  base_url <- .ai_base(base_url)
  if (provider == 'ollama') return(paste0(sub('/api$','',base_url), '/api/', endpoint))
  if (provider == 'aistudio') return(paste0(base_url, '/', endpoint))
  if (!grepl('/(?:api/)?v[0-9]+$', base_url, perl = TRUE))
    base_url <- paste0(base_url, if (provider == 'lemonade') '/api/v1' else '/v1')
  paste0(base_url, '/', endpoint)
}
.ai_save_config <- function(provider, base_url, model, api_key, save = FALSE) {
  provider <- .ai_provider(provider); state <- .ai_state()
  base_url <- .ai_base(base_url)
  if (!is.character(model) || length(model) != 1L || is.na(model)) stop('\ubaa8\ub378 ID\ub97c \uc785\ub825\ud558\uc2ed\uc2dc\uc624.')
  if (!is.character(api_key) || length(api_key) != 1L || is.na(api_key) || grepl('[[:cntrl:]]',api_key))
    stop('API \ud0a4 \ud615\uc2dd\uc774 \uc798\ubabb\ub418\uc5c8\uc2b5\ub2c8\ub2e4.')
  state$config[[provider]] <- list(base_url = base_url, model = trimws(model))
  state$keys[[provider]] <- trimws(api_key)
  if (isTRUE(save)) {
    file <- .ai_config_file(); dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    # Only addresses and model IDs are serialized. Keys remain session-only.
    safe <- lapply(state$config, function(x) x[intersect(names(x),c('base_url','model'))])
    saveRDS(safe, file)
  }
  invisible(NULL)
}
#' Inspect AI provider settings without revealing credentials
#' @param provider Provider name.
#' @return A list containing address, model, and whether an API key is set.
#' @export
ai_settings <- function(provider = 'lmstudio') {
  cfg <- .ai_config(provider); cfg$api_key_set <- nzchar(cfg$api_key); cfg$api_key <- NULL; cfg
}
.ai_http <- function(provider, url, payload = NULL, api_key = '', timeout = 30) {
  .ai_require(); .ai_base(sub('[?].*$', '', url))
  files <- c(tempfile(fileext='.json'),tempfile(fileext='.conf'),tempfile(fileext='.body'))
  on.exit(unlink(files), add=TRUE)
  args <- c('--silent','--show-error','--connect-timeout','10','--max-time',as.character(timeout),
    '--output',files[3L],'--write-out','%{http_code}')
  if (nzchar(api_key)) {
    if (grepl('[[:cntrl:]]',api_key)) stop('API \ud0a4\uc5d0 \uc81c\uc5b4\ubb38\uc790\uac00 \uc788\uc2b5\ub2c8\ub2e4.')
    header <- paste0(if(provider == 'aistudio') 'x-goog-api-key: ' else 'Authorization: Bearer ',api_key)
    writeLines(paste0('header = ',encodeString(header,quote='"')),files[2L],useBytes=TRUE)
    try(Sys.chmod(files[2L],'0600'),silent=TRUE)
    args <- c(args,'--config',files[2L])
  }
  if (!is.null(payload)) {
    jsonlite::write_json(payload,files[1L],auto_unbox=TRUE,null='null')
    args <- c(args,'-H','Content-Type: application/json','--data-binary',paste0('@',files[1L]))
  }
  proc <- processx::process$new(.ai_curl(),c(args,url),stdout='|',stderr='|',windows_hide_window=TRUE)
  on.exit(if(proc$is_alive()) proc$kill(),add=TRUE)
  repeat {
    proc$poll_io(100L)
    if (isTRUE(getOption('r_ai_gui_active',FALSE)) && capabilities('tcltk'))
      try(tcltk::tcl('update'),silent=TRUE)
    if (!proc$is_alive()) break
  }
  status <- suppressWarnings(as.integer(trimws(proc$read_all_output())))
  body <- if(file.exists(files[3L])) paste(readLines(files[3L],warn=FALSE,encoding='UTF-8'),collapse='\n') else ''
  obj <- tryCatch(jsonlite::fromJSON(body,simplifyVector=FALSE),error=function(e) NULL)
  if (proc$get_exit_status() != 0L || is.na(status) || status >= 400L || is.null(obj) || !is.null(obj$error) ||
      (!is.null(obj$status) && is.character(obj$status) && obj$status %in% c('error','failed','failure'))) {
    detail <- if(is.list(obj$error)) obj$error$message else if(is.character(obj$error)) obj$error else ''
    if (is.null(detail) || !nzchar(detail)) detail <- '\uc8fc\uc18c\u00b7\ud3ec\ud2b8\u00b7\uc11c\ubc84 \uc2e4\ud589 \uc0c1\ud0dc\u00b7API \ud0a4\ub97c \ud655\uc778\ud558\uc2ed\uc2dc\uc624.'
    if(nzchar(api_key)) detail <- gsub(api_key,'[REDACTED]',detail,fixed=TRUE)
    cond <- structure(list(message=paste0(provider,' HTTP ',status,': ',detail),call=NULL,status=status),
                      class=c('ai_http_error','error','condition'))
    stop(cond)
  }
  obj
}
#' List available text-generation models
#' @param provider Provider name.
#' @param base_url Server address; defaults to saved settings.
#' @param api_key Optional session-only API key.
#' @param details Return identifiers and load-state columns.
#' @return Character model IDs, or a data frame when details is TRUE.
#' @export
ai_models <- function(provider = 'lmstudio', base_url = NULL, api_key = NULL, details = FALSE) {
  provider <- .ai_provider(provider); cfg <- .ai_config(provider)
  if(is.null(base_url)) base_url <- cfg$base_url
  if(is.null(api_key)) api_key <- cfg$api_key
  # The official EXAONE docs do not define a model-list endpoint.
  # This is a documented catalog, not an authenticated availability check.
  if(provider == 'exaone') {
    result <- data.frame(id='k-exaone_v2.5',loaded=NA,stringsAsFactors=FALSE)
    return(if(isTRUE(details)) result else result$id)
  }
  items <- list(); native <- FALSE
  if(provider == 'aistudio') {
    if(!nzchar(api_key)) stop('API \ud0a4\ub97c \uc785\ub825\ud558\uc2ed\uc2dc\uc624.')
    token <- NULL
    repeat {
      url <- .ai_url(provider,base_url,'models?pageSize=1000')
      if(!is.null(token)) url <- paste0(url,'&pageToken=',utils::URLencode(token,reserved=TRUE))
      obj <- .ai_http(provider,url,api_key=api_key)
      items <- c(items,obj$models); token <- obj$nextPageToken
      if(is.null(token) || !nzchar(token)) break
    }
    items <- Filter(function(x) 'generateContent' %in% unlist(x$supportedGenerationMethods),items)
  } else if(provider == 'lmstudio') {
    url <- paste0(.ai_root(base_url),'/api/v1/models')
    obj <- tryCatch(.ai_http(provider,url,api_key=api_key), ai_http_error=function(e) {
      if(!e$status %in% c(404L,405L)) stop(e)
      .ai_http(provider,.ai_url(provider,base_url,'models'),api_key=api_key)
    })
    native <- !is.null(obj$models); items <- if(native) obj$models else obj$data
  } else {
    obj <- .ai_http(provider,.ai_url(provider,base_url,if(provider=='ollama') 'tags' else 'models'),api_key=api_key)
    items <- if(provider=='ollama') obj$models else obj$data
  }
  items <- Filter(function(x) {
    type <- if(is.null(x$type)) '' else x$type
    labels <- tolower(unlist(x$labels))
    !type %in% c('embedding','embeddings','image','audio','decision') &&
      !any(labels %in% c('image','image-generation','diffusion','embedding','audio','tts','stt'))
  },items)
  rows <- lapply(items,function(x) {
    id <- if(provider=='aistudio') sub('^models/','',x$name) else if(provider=='ollama') x$name else if(native) x$key else x$id
    loaded <- if(native) length(x$loaded_instances)>0L else if(!is.null(x$state)) x$state=='loaded' else NA
    data.frame(id=as.character(id),loaded=loaded,stringsAsFactors=FALSE)
  })
  result <- if(length(rows)) do.call(rbind,rows) else data.frame(id=character(),loaded=logical())
  if(isTRUE(details)) result else result$id
}
#' Load a selected model into a local server
#' @param provider Provider name.
#' @param model Model identifier.
#' @param base_url Server address.
#' @param api_key Optional API key.
#' @param timeout Model loading timeout in seconds.
#' @return Server response invisibly. AI Studio returns availability, not a local load.
#' @export
ai_load_model <- function(provider, model, base_url=NULL, api_key=NULL, timeout=300) {
  provider <- .ai_provider(provider); cfg <- .ai_config(provider)
  if(is.null(base_url)) base_url <- cfg$base_url
  if(is.null(api_key)) api_key <- cfg$api_key
  if(!is.character(model) || length(model)!=1L || !nzchar(model)) stop('\ubaa8\ub378\uc744 \uc120\ud0dd\ud558\uc2ed\uc2dc\uc624.')
  if(provider=='exaone') {
    answer <- ai_test_connection(provider,model,base_url,api_key)
    return(invisible(list(status='available',model=model,response=answer)))
  }
  if(provider=='aistudio') {
    if(!model %in% ai_models(provider,base_url,api_key)) stop('\ud574\ub2f9 \ud0a4\ub85c \uc0ac\uc6a9\ud560 \uc218 \uc5c6\ub294 \ubaa8\ub378\uc785\ub2c8\ub2e4.')
    return(invisible(list(status='available',model=model)))
  }
  if(provider=='ollama') {url <- .ai_url(provider,base_url,'generate'); payload <- list(model=model,stream=FALSE)}
  else if(provider=='lemonade') {url <- .ai_url(provider,base_url,'load'); payload <- list(model_name=model)}
  else {url <- paste0(.ai_root(base_url),'/api/v1/models/load'); payload <- list(model=model)}
  invisible(.ai_http(provider,url,payload,api_key,timeout))
}
#' Test a selected model with a short generation request
#' @param provider Provider name.
#' @param model Selected model identifier.
#' @param base_url Server address.
#' @param api_key Optional API key.
#' @return Generated test text invisibly.
#' @export
ai_test_connection <- function(provider,model=NULL,base_url=NULL,api_key=NULL) {
  provider <- .ai_provider(provider); cfg <- .ai_config(provider)
  if(is.null(model) || !nzchar(model)) model <- cfg$model
  if(is.null(base_url)) base_url <- cfg$base_url
  if(is.null(api_key)) api_key <- cfg$api_key
  if(!nzchar(model)) stop('\ubaa8\ub378 \ubaa9\ub85d\uc744 \uc870\ud68c\ud55c \ub4a4 \ubaa8\ub378\uc744 \uc120\ud0dd\ud558\uc2ed\uc2dc\uc624.')
  extra <- if(provider=='exaone') list(reasoning_effort='low') else list()
  text <- do.call(.ai_send_once,c(list(provider=provider,prompt='Reply with OK.',model=model,base_url=base_url,api_key=api_key,
    viewer=FALSE,stream=FALSE,echo=FALSE,progress=FALSE,max_tokens=if(provider=='exaone')2048L else 128L,timeout=90L,retries=0L),extra))
  invisible(as.character(text))
}
.ai_setting <- function(provider,base_url=NULL,port=NULL,model=NULL,api_key=NULL,open=TRUE,save=FALSE) {
  provider <- .ai_provider(provider); cfg <- .ai_config(provider)
  if(!is.null(base_url)) cfg$base_url <- base_url
  cfg$base_url <- .ai_base(cfg$base_url,port)
  if(!is.null(model)) cfg$model <- model
  if(!is.null(api_key)) cfg$api_key <- api_key
  if(!isTRUE(open)) {
    .ai_save_config(provider,cfg$base_url,cfg$model,cfg$api_key,save)
    return(invisible(ai_settings(provider)))
  }
  if(!capabilities('tcltk') || !requireNamespace('tcltk',quietly=TRUE)) stop('Tcl/Tk\ub97c \uc9c0\uc6d0\ud558\ub294 \ub370\uc2a4\ud06c\ud1b1 R\uc5d0\uc11c \uc2e4\ud589\ud558\uc2ed\uc2dc\uc624.')
  tk <- asNamespace('tcltk'); top <- tk$tktoplevel()
  tk$tkwm.title(top,paste0(toupper(provider),' \uc5f0\uacb0 \uc124\uc815'))
  tk$tkwm.geometry(top,'740x390')
  urlvar <- tk$tclVar(cfg$base_url); keyvar <- tk$tclVar(cfg$api_key); modelvar <- tk$tclVar(cfg$model)
  cloud <- provider %in% c('aistudio','exaone')
  porttext <- if(cloud) '' else sub('^.*:([0-9]+)(/.*)?$','\\1',cfg$base_url)
  if(!grepl('^[0-9]+$',porttext)) porttext <- ''
  portvar <- tk$tclVar(porttext); persist <- tk$tclVar(if(save) '1' else '0')
  status <- tk$tclVar('\uc8fc\uc18c\u00b7\ud3ec\ud2b8 \uc785\ub825 \u2192 \ubaa8\ub378 \ubaa9\ub85d \uc870\ud68c \u2192 \ubaa8\ub378 \uc120\ud0dd \u2192 \ub85c\ub4dc / \uc751\ub2f5 \ud14c\uc2a4\ud2b8 \u2192 \uc800\uc7a5')
  row <- function(label,widget) {
    tk$tkgrid(tk$tklabel(top,text=label),widget,sticky='ew',padx=10,pady=5)
  }
  row('\uc11c\ubc84 \uc8fc\uc18c',tk$tkentry(top,textvariable=urlvar,width=62))
  if(!cloud) row('\ud3ec\ud2b8',tk$tkentry(top,textvariable=portvar,width=10))
  row(if(cloud) 'API \ud0a4' else 'API \ud1a0\ud070 (\uc120\ud0dd)',tk$tkentry(top,textvariable=keyvar,show='*',width=50))
  models <- tk$ttkcombobox(top,textvariable=modelvar,width=55)
  row('\ubaa8\ub378 \uc120\ud0dd / \uc9c1\uc811 \uc785\ub825',models)
  tk$tkgrid.columnconfigure(top,1,weight=1)
  note <- if(cloud) '\ud074\ub77c\uc6b0\ub4dc \ubaa8\ub378\uc740 \ub85c\uceec \ub85c\ub529 \ub300\uc2e0 \uc0ac\uc6a9 \uac00\ub2a5 \uc5ec\ubd80\uc640 \uc2e4\uc81c \uc751\ub2f5\uc744 \ud655\uc778\ud569\ub2c8\ub2e4.' else
    '\ub85c\ub529\uc5d0 \uc2dc\uac04\uc774 \uac78\ub9b4 \uc218 \uc788\uc2b5\ub2c8\ub2e4. Lemonade\ub294 \ubbf8\uc124\uce58 \ubaa8\ub378\uc744 \ub2e4\uc6b4\ub85c\ub4dc\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.'
  tk$tkgrid(tk$tklabel(top,text=note,wraplength=690,justify='left'),columnspan=2,sticky='w',padx=10,pady=5)
  tk$tkgrid(tk$tkcheckbutton(top,text='\uc8fc\uc18c\u00b7\ubaa8\ub378\uc744 \ub2e4\uc74c R \uc138\uc158\uc5d0\ub3c4 \uc800\uc7a5 (\ud0a4\ub294 \ud604\uc7ac \uc138\uc158\uc5d0\ub9cc \ubcf4\uad00)',variable=persist),columnspan=2,sticky='w',padx=10)
  bar <- tk$tkframe(top); tk$tkgrid(bar,columnspan=2,sticky='ew',padx=10,pady=8)
  tk$tkgrid(tk$tklabel(top,textvariable=status,wraplength=690,justify='left'),columnspan=2,sticky='w',padx=10,pady=8)
  busy <- FALSE; buttons <- list()
  values <- function() list(base_url=.ai_base(as.character(tk$tclvalue(urlvar)),
    if(cloud || !nzchar(as.character(tk$tclvalue(portvar)))) NULL else as.character(tk$tclvalue(portvar))),
    model=trimws(as.character(tk$tclvalue(modelvar))),api_key=as.character(tk$tclvalue(keyvar)))
  action <- function(label,work) function() {
    if(busy) return(invisible(NULL))
    if(isTRUE(.ai_state()$busy)) {
      tcltk::tclvalue(status) <- 'AI \uc751\ub2f5 \uc0dd\uc131\uc774 \ub05d\ub09c \ub4a4 \uc124\uc815\uc744 \ud14c\uc2a4\ud2b8\ud558\uc2ed\uc2dc\uc624.'
      return(invisible(NULL))
    }
    busy <<- TRUE; old <- getOption('r_ai_gui_active'); options(r_ai_gui_active=TRUE)
    for(b in buttons) tk$tkconfigure(b,state='disabled')
    tcltk::tclvalue(status) <- paste0(label,' \uc911...'); tk$tcl('update')
    on.exit({busy <<- FALSE; options(r_ai_gui_active=old)
      for(b in buttons) try(tk$tkconfigure(b,state='normal'),silent=TRUE)},add=TRUE)
    tryCatch(work(values()),error=function(e) { try(tcltk::tclvalue(status) <- conditionMessage(e),silent=TRUE) })
    invisible(NULL)
  }
  addbutton <- function(text,work) {
    b <- tk$tkbutton(bar,text=text,command=action(text,work)); buttons[[length(buttons)+1L]] <<- b
    tk$tkpack(b,side='left',padx=3)
  }
  addbutton('\uc5f0\uacb0 / \ubaa8\ub378 \ubaa9\ub85d',function(v) {
    ids <- ai_models(provider,v$base_url,v$api_key)
    if(provider=='exaone') ai_test_connection(provider,v$model,v$base_url,v$api_key)
    tk$tkconfigure(models,values=ids)
    if(length(ids) && !v$model %in% ids) tcltk::tclvalue(modelvar) <- ids[[1L]]
    tcltk::tclvalue(status) <- paste0('\uc5f0\uacb0 \uc131\uacf5 \u00b7 \uc0ac\uc6a9 \uac00\ub2a5\ud55c \ud14d\uc2a4\ud2b8 \ubaa8\ub378 ',length(ids),'\uac1c')
  })
  addbutton(if(cloud) '\ubaa8\ub378 \uc0ac\uc6a9 \ud655\uc778' else '\uc120\ud0dd \ubaa8\ub378 \ub85c\ub4dc',function(v) {
    result <- ai_load_model(provider,v$model,v$base_url,v$api_key)
    if(provider=='lmstudio' && !is.null(result$instance_id)) tcltk::tclvalue(modelvar) <- result$instance_id
    tcltk::tclvalue(status) <- if(cloud) '\ubaa8\ub378 \uc0ac\uc6a9 \uac00\ub2a5 \u00b7 \uc751\ub2f5 \ud14c\uc2a4\ud2b8\ub85c \uc2e4\uc81c \uc0dd\uc131\uc744 \ud655\uc778\ud558\uc2ed\uc2dc\uc624.' else '\ubaa8\ub378 \ub85c\ub529 \uc131\uacf5 \u00b7 \uc751\ub2f5 \ud14c\uc2a4\ud2b8\ub85c \uc2e4\uc81c \uc0dd\uc131\uc744 \ud655\uc778\ud558\uc2ed\uc2dc\uc624.'
  })
  addbutton('\uc9e7\uc740 \uc751\ub2f5 \ud14c\uc2a4\ud2b8',function(v) {
    answer <- ai_test_connection(provider,v$model,v$base_url,v$api_key)
    tcltk::tclvalue(status) <- paste0('\uc751\ub2f5 \uc131\uacf5: ',substr(answer,1L,180L))
  })
  addbutton('\uc124\uc815 \uc800\uc7a5',function(v) {
    .ai_save_config(provider,v$base_url,v$model,v$api_key,as.character(tk$tclvalue(persist))=='1')
    tcltk::tclvalue(status) <- '\uc124\uc815\uc744 \uc801\uc6a9\ud588\uc2b5\ub2c8\ub2e4. API \ud0a4\ub294 \ud604\uc7ac R \uc138\uc158\uc5d0\ub9cc \ubcf4\uad00\ud569\ub2c8\ub2e4.'
  })
  tk$tkwm.protocol(top,'WM_DELETE_WINDOW',function() {
    if(!busy) tk$tkdestroy(top) else tcltk::tclvalue(status) <- '\uc791\uc5c5\uc774 \ub05d\ub09c \ub4a4 \ucc3d\uc744 \ub2eb\uc744 \uc218 \uc788\uc2b5\ub2c8\ub2e4.'
  })
  invisible(top)
}
