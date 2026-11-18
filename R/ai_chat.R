.ai_continue_prompt <- paste(
  '\uc55e\uc120 \ub2f5\ubcc0\uc758 \ub9c8\uc9c0\ub9c9 \ubb38\uc790 \ubc14\ub85c \ub2e4\uc74c\ubd80\ud130 \uc774\uc5b4\uc11c \uc791\uc131\ud558\ub77c.',
  '\uc774\ubbf8 \uc791\uc131\ud55c \uc81c\ubaa9\u00b7\ud45c\u00b7\ubb38\uc7a5\uc740 \ubc18\ubcf5\ud558\uc9c0 \ub9d0\uace0, \ub2eb\ud788\uc9c0 \uc54a\uc740 \ubb38\uc7a5\u00b7Markdown \ud45c\u00b7\ucf54\ub4dc\ube14\ub85d\uc744 \uc774\uc5b4\uc11c \uc644\uc131\ud558\ub77c.',
  '\uc6d0\ub798 \uc9c8\ubb38\uacfc \ubd84\uc11d \ub370\uc774\ud130 \ubc0f \ubb38\uccb4\ub97c \uc720\uc9c0\ud558\uace0, \uc774\uc5b4\uc4f0\uae30 \uc548\ub0b4 \ubb38\uad6c \uc5c6\uc774 \ubcf8\ubb38\ub9cc \ucd9c\ub825\ud558\ub77c.'
)
.ai_truncated <- function(reason) !is.null(reason) && tolower(reason) %in%
  c('length','max_tokens','max_token','max_output_tokens')
.ai_chat <- function(provider,data=NULL,prompt=NULL,model=NULL,base_url=NULL,api_key=NULL,
                     viewer=TRUE,window=FALSE,stream=TRUE,echo=NULL,on_chunk=NULL,max_tokens=4096L,
                     auto_continue=TRUE,max_continuations=3L,system_prompt=.ai_system,
                     .session=NULL,...) {
  provider <- .ai_provider(provider); state <- .ai_state()
  if(!is.logical(stream) || length(stream)!=1L || is.na(stream)) stop('stream must be TRUE or FALSE.')
  if(isTRUE(state$busy)) stop('\ub2e4\ub978 AI \uc694\uccad\uc774 \uc9c4\ud589 \uc911\uc785\ub2c8\ub2e4. \uc644\ub8cc \ud6c4 \ub2e4\uc2dc \uc2e4\ud589\ud558\uc2ed\uc2dc\uc624.')
  if(length(max_continuations)!=1L || !is.numeric(max_continuations) || is.na(max_continuations) ||
     max_continuations!=floor(max_continuations) || max_continuations<0 || max_continuations>20)
    stop('max_continuations\ub294 0~20 \uc0ac\uc774 \uc815\uc218\uc5ec\uc57c \ud569\ub2c8\ub2e4.')
  if(length(max_tokens)!=1L || !is.numeric(max_tokens) || !is.finite(max_tokens) || max_tokens<1 || max_tokens!=floor(max_tokens))
    stop('max_tokens\ub294 \uc591\uc758 \uc815\uc218\uc5ec\uc57c \ud569\ub2c8\ub2e4.')
  cfg <- .ai_config(provider)
  if(is.null(base_url)) base_url <- cfg$base_url
  base_url <- .ai_base(base_url)
  if(is.null(api_key)) api_key <- cfg$api_key
  if(!is.character(api_key) || length(api_key)!=1L || is.na(api_key) || grepl('[[:cntrl:]]',api_key)) stop('API \ud0a4 \ud615\uc2dd\uc774 \uc798\ubabb\ub418\uc5c8\uc2b5\ub2c8\ub2e4.')
  if(provider=='exaone' && !nzchar(trimws(api_key)))
    stop('EXAONE API key required: use exaone_setting(api_key=..., open=FALSE) or EXAONE_API_KEY.',call.=FALSE)
  if(is.null(model) || !nzchar(model)) model <- cfg$model
  if(!nzchar(model)) {
    ids <- ai_models(provider,base_url,api_key)
    if(!length(ids)) stop('\uc0ac\uc6a9 \uac00\ub2a5\ud55c \ud14d\uc2a4\ud2b8 \ubaa8\ub378\uc774 \uc5c6\uc2b5\ub2c8\ub2e4. \uc124\uc815 \ucc3d\uc5d0\uc11c \ubaa8\ub378\uc744 \ud655\uc778\ud558\uc2ed\uc2dc\uc624.')
    model <- ids[[1L]]
    state$config[[provider]] <- list(base_url=base_url,model=model)
  }
  if(provider == 'aistudio') model <- sub('^models/', '', model)
  # Keep explicit credentials in options for continuation, not in the transcript.
  state$keys[[provider]] <- api_key
  dots <- list(...)
  query <- .ai_prompt(data,prompt,if(is.null(dots$max_rows)) 100L else dots$max_rows,
    if(is.null(dots$max_chars)) 30000L else dots$max_chars)
  if(is.null(.session)) {
    history <- list(list(role='user',content=query$user)); accumulated <- ''; original <- query$raw_prompt
  } else {
    history <- c(.session$history,list(list(role='user',content=query$raw_prompt)))
    accumulated <- .session$text; original <- .session$question
  }
  request_options <- c(list(viewer=viewer,window=window,stream=stream,echo=echo,max_tokens=max_tokens,
    auto_continue=auto_continue,max_continuations=max_continuations,system_prompt=system_prompt),dots)
  session <- list(provider=provider,model=model,base_url=base_url,question=original,
    history=history,text=accumulated,finish_reason='interrupted',options=request_options)
  state$busy <- TRUE;state$cancel_requested <- FALSE
  on.exit(state$busy <- FALSE,add=TRUE)
  view <- if(isTRUE(viewer)) .ai_viewer(paste0(toupper(provider),' | RStudio Viewer'),original) else NULL
  if(!is.null(view) && is.function(view$close)) on.exit(view$close(),add=TRUE)
  if(!is.null(view) && nzchar(accumulated)) view$push(accumulated)
  round <- 0L
  repeat {
    current <- ''
    session$history <- history
    # Do not replace a previous response until a new request is ready to run.
    state$last <- session
    receive <- function(chunk) {
      current <<- paste0(current,chunk)
      session$text <<- paste0(accumulated,current)
      session$history <<- c(history,list(list(role='assistant',content=current)))
      state$last <- session
      if(is.function(on_chunk)) on_chunk(chunk)
    }
    result <- tryCatch(do.call(.ai_send_once,c(list(provider=provider,prompt=original,
      model=model,base_url=base_url,api_key=api_key,viewer=FALSE,.view=view,window=window,stream=stream,
      echo=echo,on_chunk=receive,max_tokens=max_tokens,system_prompt=system_prompt,history=history,
      progress=if(is.null(dots$progress)) isTRUE(viewer) else dots$progress),dots[setdiff(names(dots),'progress')])),
      error=function(e) {
        if(inherits(e,'ai_cancelled')) return(e)
        message <- conditionMessage(e)
        if(nzchar(api_key)) message <- gsub(api_key,'[REDACTED]',message,fixed=TRUE)
        if(!is.null(view)) view$status(paste0('\uc694\uccad \uc2e4\ud328 | ',message))
        stop(message,call.=FALSE)
      })
    if(inherits(result,'ai_cancelled')) {
      accumulated <- session$text;reason <- 'cancelled';session$finish_reason <- reason;state$last <- session
      break
    }
    accumulated <- paste0(accumulated,as.character(result))
    history <- c(history,list(list(role='assistant',content=as.character(result))))
    reason <- attr(result,'finish_reason')
    session$text <- accumulated; session$history <- history; session$finish_reason <- reason
    session$metrics <- attr(result,'metrics')
    state$last <- session
    if(!.ai_truncated(reason) || !isTRUE(auto_continue) || round >= max_continuations) break
    round <- round+1L
    if(!is.null(view)) view$status(paste0('\ud1a0\ud070 \uc81c\ud55c \u00b7 \uc790\ub3d9 \uc774\uc5b4\uc4f0\uae30 ',round,'/',max_continuations))
    if(isTRUE(viewer) || isTRUE(dots$progress)) cat(sprintf('[AI] \ud1a0\ud070 \uc81c\ud55c: \uc790\ub3d9 \uc774\uc5b4\uc4f0\uae30 %d/%d\n',round,max_continuations))
    history <- c(history,list(list(role='user',content=.ai_continue_prompt)))
  }
  if(identical(reason,'cancelled')) {
    if(!is.null(view)) view$status('\uc911\uc9c0\ub428 \u00b7 \ubc1b\uc740 \ubcf8\ubb38\uc740 \ubcf4\uc874\ub418\uc5c8\uc2b5\ub2c8\ub2e4.')
  } else if(.ai_truncated(reason)) {
    if(!is.null(view)) view$status('\ud1a0\ud070 \uc81c\ud55c\uc73c\ub85c \uc911\ub2e8 \u00b7 ai_continue()\ub85c \uc774\uc5b4\uc4f0\uae30')
    warning('\ucd9c\ub825 \ud1a0\ud070 \uc81c\ud55c\uc5d0 \ub3c4\ub2ec\ud588\uc2b5\ub2c8\ub2e4. ai_continue()\ub85c \uc774\uc5b4 \uc4f0\uac70\ub098 max_tokens\ub97c \ub298\ub9ac\uc2ed\uc2dc\uc624.',call.=FALSE)
  } else if(!is.null(view)) view$status(paste0('\uc644\ub8cc | ',.ai_metric_text(session$metrics,session$metrics$elapsed_seconds,TRUE)))
  invisible(accumulated)
}
#' Continue the last AI response with its original data and conversation
#' @param prompt Instructions for the next section.
#' @param provider Optional provider; must match the saved conversation.
#' @param ... Options overriding the previous call, e.g. max_tokens or viewer.
#' @return The complete accumulated response invisibly.
#' @export
ai_continue <- function(prompt=.ai_continue_prompt,provider=NULL,...) {
  last <- .ai_state()$last
  if(is.null(last) || !nzchar(last$text)) stop('\uc774\uc5b4 \uc4f8 \uc774\uc804 \uc751\ub2f5\uc774 \uc5c6\uc2b5\ub2c8\ub2e4.')
  if(!is.null(provider) && provider!=last$provider) stop('\uc774\uc804 \uc751\ub2f5\uacfc \ub3d9\uc77c\ud55c \uacf5\uae09\uc790\ub97c \uc120\ud0dd\ud558\uc2ed\uc2dc\uc624.')
  args <- c(list(provider=last$provider,prompt=prompt,model=last$model,
    base_url=last$base_url,.session=last),last$options)
  do.call(.ai_chat,utils::modifyList(args,list(...),keep.null=TRUE))
}
#' Retrieve the most recent accumulated response
#' @return A character string; empty when there is no response.
#' @export
ai_last_response <- function() {
  last <- .ai_state()$last
  if(is.null(last)) '' else last$text
}
