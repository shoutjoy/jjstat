#' Open an independent AI question window with Markdown and PV tabs
#' @param provider AI provider name.
#' @param data Optional analysis data, including piped data.
#' @param prompt Initial question. Requests start only when the run button is pressed.
#' @param model Optional model ID; defaults to saved settings.
#' @param api_key Optional API key; defaults to saved settings.
#' @param initial_response Optional Markdown restored without an AI request.
#' @param engine Web application (default) or legacy Tcl/Tk text window.
#' @param launch Open the web application window. FALSE returns a local URL for embedding.
#' @param ... Request options such as base_url, stream and max_tokens.
#' @return Invisibly, a web-window handle with url and close(), or a Tk toplevel.
#' @export
ai_window <- function(provider=c('ollama','lmstudio','aistudio','lemonade','exaone'),data=NULL,
                      prompt='\uc774 \ubd84\uc11d\uacb0\uacfc\ub97c \uc124\uba85\ud574\uc918',model=NULL,api_key=NULL,
                      initial_response=NULL,engine=c('web','tk'),launch=TRUE,...) {
  provider <- match.arg(provider);engine <- match.arg(engine)
  if(engine=='tk') return(.ai_tk_window(provider,data,prompt,model,api_key,initial_response,...))
  .ai_web_window(provider,data,prompt,model,api_key,initial_response,launch,...)
}

 .ai_tk_window <- function(provider=c('ollama','lmstudio','aistudio','lemonade','exaone'),
                      data=NULL,prompt='\uc774 \ubd84\uc11d\uacb0\uacfc\ub97c \uc124\uba85\ud574\uc918',model=NULL,api_key=NULL,initial_response=NULL,...) {
  provider <- match.arg(provider)
  force(data);force(prompt);force(initial_response);opts <- list(...)
  if(!is.null(initial_response) && (!is.character(initial_response) || length(initial_response)!=1L || is.na(initial_response)))
    stop('initial_response must be a single character string.')
  if(!capabilities('tcltk') || !requireNamespace('tcltk',quietly=TRUE))
    stop('Tcl/Tk\ub97c \uc9c0\uc6d0\ud558\ub294 \ub370\uc2a4\ud06c\ud1b1 R\uc5d0\uc11c \uc2e4\ud589\ud558\uc2ed\uc2dc\uc624.')
  cfg <- .ai_config(provider)
  if(is.null(model)) model <- cfg$model
  if(is.null(api_key)) api_key <- cfg$api_key
  external_chunk <- opts$on_chunk;external_status <- opts$on_status
  if(!is.null(external_chunk) && !is.function(external_chunk)) stop('on_chunk\ub294 \ud568\uc218\uc5ec\uc57c \ud569\ub2c8\ub2e4.')
  if(!is.null(external_status) && !is.function(external_status)) stop('on_status\ub294 \ud568\uc218\uc5ec\uc57c \ud569\ub2c8\ub2e4.')
  top <- tcltk::tktoplevel()
  tcltk::tkwm.title(top,paste0(toupper(provider),' | R AI \uc9c8\ubb38\ucc3d [Live transcript]'))
  tcltk::tkwm.geometry(top,'980x760')
  settings_bar <- tcltk::tkframe(top)
  model_var <- tcltk::tclVar(model);key_var <- tcltk::tclVar(api_key)
  tcltk::tkpack(tcltk::tklabel(settings_bar,text='\ubaa8\ub378 ID'),side='left')
  model_entry <- tcltk::tkentry(settings_bar,textvariable=model_var,width=45)
  tcltk::tkpack(model_entry,side='left',fill='x',expand=TRUE)
  key_entry <- NULL
  if(provider %in% c('aistudio','exaone')) {
    tcltk::tkpack(tcltk::tklabel(settings_bar,text=' API \ud0a4'),side='left')
    key_entry <- tcltk::tkentry(settings_bar,textvariable=key_var,show='*',width=24)
    tcltk::tkpack(key_entry,side='left')
  }
  tcltk::tkpack(settings_bar,fill='x')
  query_bar <- tcltk::tkframe(top)
  question_var <- tcltk::tclVar(prompt)
  question <- tcltk::tkentry(query_bar,textvariable=question_var,width=80)
  tcltk::tkpack(question,side='left',fill='x',expand=TRUE)
  tcltk::tkpack(query_bar,fill='x')
  status_var <- tcltk::tclVar('\uc900\ube44 \u00b7 \uc9c8\ubb38 \uc2e4\ud589 \ubc84\ud2bc\uc744 \ub20c\ub7ec \uc694\uccad\ud558\uc2ed\uc2dc\uc624.')
  tcltk::tkpack(tcltk::tklabel(top,textvariable=status_var,anchor='w',justify='left',wraplength=940),fill='x')
  area <- tcltk::tkframe(top);scroll <- tcltk::tkscrollbar(area)
  output <- tcltk::tktext(area,wrap='word',background='#141b25',foreground='#eef3fa',
    insertbackground='white',state='disabled',yscrollcommand=function(...)tcltk::tkset(scroll,...))
  tcltk::tkconfigure(scroll,command=function(...)tcltk::tkyview(output,...))
  tcltk::tkpack(scroll,side='right',fill='y');tcltk::tkpack(output,side='left',fill='both',expand=TRUE)
  tcltk::tkpack(area,fill='both',expand=TRUE)
  transcript_var <- tcltk::tclVar('')
  transcript <- ''
  writer_name <- paste0('::jjstat_write_',gsub('[^A-Za-z0-9]','',basename(tempfile())))
  writer_body <- paste(
    'if {![winfo exists $widget]} {return}',
    'set value [set $variable]',
    '$widget configure -state normal',
    'set current [$widget get 1.0 end-1c]',
    'if {[string first $current $value] == 0} {$widget insert end [string range $value [string length $current] end]} else {$widget delete 1.0 end; $widget insert 1.0 $value}',
    '$widget configure -state disabled',
    '$widget see end',
    'update idletasks',sep='; ')
  tcltk::tcl('proc',writer_name,'widget variable index operation',writer_body)
  writer_command <- tcltk::tcl('list',writer_name,output)
  tcltk::tcl('trace','add','variable',transcript_var,'write',writer_command)
  busy <- FALSE;closed <- FALSE;last_paint <- -Inf
  paint <- function(force=FALSE) {
    if(closed) return(invisible(NULL))
    now <- proc.time()[['elapsed']]
    if(force || now-last_paint>=0.03) {
      tcltk::tksee(output,'end');tcltk::tcl('update','idletasks');last_paint <<- now
    }
    invisible(NULL)
  }
  status <- function(text) {
    if(closed) return(invisible(NULL))
    tcltk::tclvalue(status_var) <- enc2utf8(paste(text,collapse='
'))
    paint(TRUE)
  }
  write_transcript <- function(value,force=FALSE) {
    if(closed) return(invisible(NULL))
    transcript <<- enc2utf8(value)
    tcltk::tclvalue(transcript_var) <- transcript
    actual <- tcltk::tclvalue(tcltk::tcl(output,'get','1.0','end-1c'))
    if(!identical(enc2utf8(actual),transcript))
      stop('The question-window transcript was not written to the text widget.')
    paint(force)
    invisible(NULL)
  }
  push <- function(text,force=FALSE) {
    write_transcript(paste0(transcript,enc2utf8(paste(text,collapse=''))),force)
  }
  set_busy <- function(value) {
    busy <<- value
    if(closed) return(invisible(NULL))
    tcltk::tkconfigure(run_button,state=if(value)'disabled' else 'normal')
    tcltk::tkconfigure(stop_button,state=if(value)'normal' else 'disabled')
    for(w in list(question,model_entry,key_entry)) if(!is.null(w))
      tcltk::tkconfigure(w,state=if(value)'disabled' else 'normal')
  }
  ask <- function(...) {
    if(busy || closed) return(invisible(NULL))
    q <- tcltk::tclvalue(question_var)
    if(!nzchar(trimws(q))) {status('\uc9c8\ubb38\uc744 \uc785\ub825\ud558\uc2ed\uc2dc\uc624.');return(invisible(NULL))}
    m <- trimws(tcltk::tclvalue(model_var))
    old_active <- getOption('r_ai_gui_active',FALSE)
    options(r_ai_gui_active=TRUE)
    set_busy(TRUE)
    on.exit({set_busy(FALSE);options(r_ai_gui_active=old_active)},add=TRUE)
    tryCatch({
      status('\uc694\uccad \uc900\ube44 \uc911...')
      push(paste0('

[\uc9c8\ubb38] ',q,'
',strrep('-',58L),'
'),TRUE)
      response_prefix <- transcript
      first <- TRUE
      receive <- function(text) {
        push(text,force=first);first <<- FALSE
        if(is.function(external_chunk)) external_chunk(text)
      }
      receive_status <- function(text) {
        status(text)
        if(is.function(external_status)) external_status(text)
      }
      args <- opts
      args$data <- data;args$prompt <- q;args$model <- if(nzchar(m))m else NULL
      args$api_key <- tcltk::tclvalue(key_var)
      args$viewer <- FALSE;args$window <- FALSE;args$echo <- FALSE
      args$on_chunk <- receive;args$on_status <- receive_status
      if(is.null(args$stream)) args$stream <- TRUE
      args$provider <- provider
      answer <- do.call(.ai_chat,args)
      # Reconcile with the returned answer even if a live callback did not run.
      if(is.character(answer) && length(answer)==1L)
        write_transcript(paste0(response_prefix,answer),TRUE)
      if(identical(.ai_state()$last$finish_reason,'cancelled')) {
        push('
[\uc911\uc9c0\ub428 \u00b7 \ubc1b\uc740 \ubcf8\ubb38 \ubcf4\uc874]
',TRUE);status('\uc911\uc9c0\ub428 \u00b7 \ubc1b\uc740 \ub0b4\uc6a9\uc740 ai_last_response()\ub85c \ud655\uc778\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.')
      } else push('
[\uc644\ub8cc]
',TRUE)
      invisible(answer)
    },interrupt=function(e) {
      if(!closed) {push('
[\uc911\uc9c0\ub428]
',TRUE);status('\uc0ac\uc6a9\uc790\uac00 \uc694\uccad\uc744 \uc911\uc9c0\ud588\uc2b5\ub2c8\ub2e4.')}
      invisible(NULL)
    },error=function(e) {
      message <- conditionMessage(e)
      if(!closed) {
        status(paste0('\uc624\ub958: ',message))
        tryCatch(push(paste0('
[\uc624\ub958] ',message,'
'),TRUE),error=function(display_error)
          warning('\uc9c8\ubb38\ucc3d \ucd9c\ub825 \uc624\ub958: ',conditionMessage(display_error),call.=FALSE))
      }
      invisible(NULL)
    })
    invisible(NULL)
  }
  run_button <- tcltk::tkbutton(query_bar,text='\uc9c8\ubb38 \uc2e4\ud589',command=ask)
  stop_button <- tcltk::tkbutton(query_bar,text='\uc751\ub2f5 \uc911\uc9c0',state='disabled',command=function(){
    if(busy) {ai_stop();status('\uc751\ub2f5\uc744 \uc911\uc9c0\ud558\ub294 \uc911...')}
  })
  tcltk::tkpack(stop_button,side='right');tcltk::tkpack(run_button,side='right')
  tcltk::tkbind(question,'<Return>',ask)
  tcltk::tkwm.protocol(top,'WM_DELETE_WINDOW',function(){
    if(busy) ai_stop()
    closed <<- TRUE
    try(tcltk::tcl('trace','remove','variable',transcript_var,'write',writer_command),silent=TRUE)
    try(tcltk::tcl('rename',writer_name,''),silent=TRUE)
    tcltk::tkdestroy(top)
  })
  push('\uc9c8\ubb38\uc744 \uc785\ub825\ud55c \ub4a4 [\uc9c8\ubb38 \uc2e4\ud589]\uc744 \ub204\ub974\uc2ed\uc2dc\uc624.
',TRUE)
  if(!is.null(initial_response) && nzchar(initial_response)) {
    push(paste0('\n[\uc774\uc804 \uc751\ub2f5 \ubcf5\uc6d0]\n',initial_response,'\n'),TRUE)
    status('\uc774\uc804 \uc751\ub2f5\uc744 \ubcf5\uc6d0\ud588\uc2b5\ub2c8\ub2e4. \ubaa8\ub378\uc5d0 \ub2e4\uc2dc \uc694\uccad\ud558\uc9c0 \uc54a\uc558\uc2b5\ub2c8\ub2e4.')
  }
  invisible(top)
}


#' Ollama 전용 질문창
#' @param data 선택적 분석 결과
#' @param ... ai_window 옵션
#' @export
ollama_window <- function(data = NULL, ...) ai_window('ollama', data = data, ...)

#' LM Studio 전용 질문창
#' @param data 선택적 분석 결과
#' @param ... ai_window 옵션
#' @export
lmstudio_window <- function(data = NULL, ...) ai_window('lmstudio', data = data, ...)

#' Google AI Studio 전용 질문창 (API 키 마스킹 입력)
#' @param data 선택적 분석 결과
#' @param ... ai_window 옵션
#' @export
aistudio_window <- function(data = NULL, ...) ai_window('aistudio', data = data, ...)


#' RStudio Viewer에서 실시간 출력하는 통합 함수
#' @param data 데이터프레임 또는 분석 결과
#' @param prompt 사용자 질문
#' @param provider ollama / lmstudio / aistudio / lemonade / exaone
#' @param ... 해당 제공자 함수에 전달할 추가 인수
#' @return AI 답변 문자열 (invisible)
#' @export
ai_viewer <- function(data = NULL, prompt = NULL,
                      provider = c('lmstudio', 'ollama', 'aistudio', 'lemonade', 'exaone'), ...) {
  provider <- match.arg(provider)
  f <- switch(provider, lmstudio = lmstudio, ollama = ollama, aistudio = aistudio, lemonade = lemonade, exaone = exaone)
  f(data = data, prompt = prompt, viewer = TRUE, ...)
}

#' Lemonade text question window
#' @param data Optional analysis data.
#' @param ... Options passed to ai_window.
#' @export
lemonade_window <- function(data=NULL,...) ai_window('lemonade',data=data,...)

#' Open an LG EXAONE question window
#' @inheritParams ai_window
#' @param reasoning_effort Reasoning intensity: low (default), high or max.
#' @param temperature Sampling temperature (default 1).
#' @param max_tokens Output budget including reasoning (default 16384).
#' @return A window handle invisibly.
#' @export
exaone_window <- function(data=NULL,reasoning_effort='low',temperature=1,max_tokens=16384L,...) {
  ai_window('exaone',data=data,reasoning_effort=reasoning_effort,
    temperature=temperature,max_tokens=max_tokens,...)
}
