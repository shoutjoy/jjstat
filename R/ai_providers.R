#' ollama chat with a live Markdown Viewer
#' @param data Optional analysis data, compatible with a pipe.
#' @param prompt User instructions.
#' @param model Model identifier; defaults to saved settings or first available model.
#' @param base_url Server address; defaults to saved settings.
#' @param api_key Optional session-only API key.
#' @param window Also open a text window.
#' @param viewer Display a live RStudio Markdown Viewer (default TRUE).
#' @param stream Stream response fragments as they arrive (default TRUE).
#' @param ... Generation options including max_tokens, auto_continue, and max_continuations.
#' @return The accumulated answer invisibly.
#' @export
ollama <- function(data=NULL,prompt=NULL,model=NULL,base_url=NULL,api_key=NULL,
                     window=FALSE,viewer=TRUE,stream=TRUE,...) {
  .ai_chat('ollama',data=data,prompt=prompt,model=model,base_url=base_url,api_key=api_key,
           window=window,viewer=viewer,stream=stream,...)
}
#' Configure ollama in a separate window
#' @param base_url Optional server address.
#' @param port Optional local port override.
#' @param model Optional model identifier.
#' @param api_key Optional API key; retained in this session only.
#' @param open Open the settings window; FALSE applies settings programmatically.
#' @param save Persist address and model only, never the key.
#' @return A Tk window or safe settings invisibly.
#' @export
ollama_setting <- function(base_url=NULL,port=NULL,model=NULL,api_key=NULL,open=TRUE,save=FALSE) {
  .ai_setting('ollama',base_url,port,model,api_key,open,save)
}

#' lmstudio chat with a live Markdown Viewer
#' @param data Optional analysis data, compatible with a pipe.
#' @param prompt User instructions.
#' @param model Model identifier; defaults to saved settings or first available model.
#' @param base_url Server address; defaults to saved settings.
#' @param api_key Optional session-only API key.
#' @param window Also open a text window.
#' @param viewer Display a live RStudio Markdown Viewer (default TRUE).
#' @param stream Stream response fragments as they arrive (default TRUE).
#' @param ... Generation options including max_tokens, auto_continue, and max_continuations.
#' @return The accumulated answer invisibly.
#' @export
lmstudio <- function(data=NULL,prompt=NULL,model=NULL,base_url=NULL,api_key=NULL,
                     window=FALSE,viewer=TRUE,stream=TRUE,...) {
  .ai_chat('lmstudio',data=data,prompt=prompt,model=model,base_url=base_url,api_key=api_key,
           window=window,viewer=viewer,stream=stream,...)
}
#' Configure lmstudio in a separate window
#' @param base_url Optional server address.
#' @param port Optional local port override.
#' @param model Optional model identifier.
#' @param api_key Optional API key; retained in this session only.
#' @param open Open the settings window; FALSE applies settings programmatically.
#' @param save Persist address and model only, never the key.
#' @return A Tk window or safe settings invisibly.
#' @export
lmstudio_setting <- function(base_url=NULL,port=NULL,model=NULL,api_key=NULL,open=TRUE,save=FALSE) {
  .ai_setting('lmstudio',base_url,port,model,api_key,open,save)
}

#' aistudio chat with a live Markdown Viewer
#' @param data Optional analysis data, compatible with a pipe.
#' @param prompt User instructions.
#' @param model Model identifier; defaults to saved settings or first available model.
#' @param base_url Server address; defaults to saved settings.
#' @param api_key Optional session-only API key.
#' @param window Also open a text window.
#' @param viewer Display a live RStudio Markdown Viewer (default TRUE).
#' @param stream Stream response fragments as they arrive (default TRUE).
#' @param ... Generation options including max_tokens, auto_continue, and max_continuations.
#' @return The accumulated answer invisibly.
#' @export
aistudio <- function(data=NULL,prompt=NULL,model=NULL,base_url=NULL,api_key=NULL,
                     window=FALSE,viewer=TRUE,stream=TRUE,...) {
  .ai_chat('aistudio',data=data,prompt=prompt,model=model,base_url=base_url,api_key=api_key,
           window=window,viewer=viewer,stream=stream,...)
}
#' Configure aistudio in a separate window
#' @param base_url Optional server address.
#' @param port Optional local port override.
#' @param model Optional model identifier.
#' @param api_key Optional API key; retained in this session only.
#' @param open Open the settings window; FALSE applies settings programmatically.
#' @param save Persist address and model only, never the key.
#' @return A Tk window or safe settings invisibly.
#' @export
aistudio_setting <- function(base_url=NULL,port=NULL,model=NULL,api_key=NULL,open=TRUE,save=FALSE) {
  .ai_setting('aistudio',base_url,port,model,api_key,open,save)
}

#' lemonade chat with a live Markdown Viewer
#' @param data Optional analysis data, compatible with a pipe.
#' @param prompt User instructions.
#' @param model Model identifier; defaults to saved settings or first available model.
#' @param base_url Server address; defaults to saved settings.
#' @param api_key Optional session-only API key.
#' @param window Also open a text window.
#' @param viewer Display a live RStudio Markdown Viewer (default TRUE).
#' @param stream Stream response fragments as they arrive (default TRUE).
#' @param ... Generation options including max_tokens, auto_continue, and max_continuations.
#' @return The accumulated answer invisibly.
#' @export
lemonade <- function(data=NULL,prompt=NULL,model=NULL,base_url=NULL,api_key=NULL,
                     window=FALSE,viewer=TRUE,stream=TRUE,...) {
  .ai_chat('lemonade',data=data,prompt=prompt,model=model,base_url=base_url,api_key=api_key,
           window=window,viewer=viewer,stream=stream,...)
}
#' Configure lemonade in a separate window
#' @param base_url Optional server address.
#' @param port Optional local port override.
#' @param model Optional model identifier.
#' @param api_key Optional API key; retained in this session only.
#' @param open Open the settings window; FALSE applies settings programmatically.
#' @param save Persist address and model only, never the key.
#' @return A Tk window or safe settings invisibly.
#' @export
lemonade_setting <- function(base_url=NULL,port=NULL,model=NULL,api_key=NULL,open=TRUE,save=FALSE) {
  .ai_setting('lemonade',base_url,port,model,api_key,open,save)
}

#' Chat with LG EXAONE using only an API key
#' @inheritParams lmstudio
#' @param reasoning_effort Reasoning intensity: low (default), high or max.
#' @param temperature Sampling temperature; defaults to the official recommendation.
#' @param max_tokens Output budget including reasoning tokens (default 16384).
#' @details Defaults to https://api.exaone.ai/v1 and k-exaone_v2.5.
#'   Configure a session key with exaone_setting() or EXAONE_API_KEY.
#' @return The accumulated final answer invisibly; reasoning is excluded.
#' @export
exaone <- function(data=NULL,prompt=NULL,model=NULL,base_url=NULL,api_key=NULL,
                   window=FALSE,viewer=TRUE,stream=TRUE,reasoning_effort='low',
                   temperature=1,max_tokens=16384L,...) {
  .ai_chat('exaone',data=data,prompt=prompt,model=model,base_url=base_url,api_key=api_key,
    window=window,viewer=viewer,stream=stream,reasoning_effort=reasoning_effort,
    temperature=temperature,max_tokens=max_tokens,...)
}
#' Configure LG EXAONE; address and model are prefilled
#' @inheritParams lmstudio_setting
#' @details API keys remain session-only. EXAONE_API_KEY is also supported.
#' @return A settings window or safe settings invisibly.
#' @export
exaone_setting <- function(api_key=NULL,open=TRUE,save=FALSE,
                           base_url=NULL,model=NULL) {
  .ai_setting('exaone',base_url=base_url,model=model,api_key=api_key,open=open,save=save)
}
