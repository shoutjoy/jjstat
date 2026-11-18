# Run after R CMD INSTALL. Tests the actual installed jjstat namespace.
Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
library(jjstat)
run_installed_ai_smoke <- function() {
  fixture <- normalizePath('tools/ai/verify_installed_mock.cjs',winslash='/')
  logfile <- tempfile();window <- NULL
  server<-processx::process$new('node',c(fixture,logfile),stdout='|',stderr='|',windows_hide_window=TRUE)
  on.exit({if(!is.null(window))window$close();if(server$is_alive())server$kill();unlink(logfile)},add=TRUE)
  port<-character();for(i in seq_len(100L)){server$poll_io(100L);port<-server$read_output_lines();if(length(port))break};stopifnot(length(port)==1L)
  old<-getOption('jjstat.ai.state');on.exit(options(jjstat.ai.state=old),add=TRUE)
  state<-new.env(parent=emptyenv());state$config<-list();state$keys<-list();state$last<-NULL;state$busy<-FALSE
  options(jjstat.ai.state=state)
  ns<-asNamespace('jjstat')
  stopifnot(identical(environment(exaone),ns))
  exports<-c('ai_continue','ai_history','ai_last_response','ai_load_model','ai_models','ai_settings','ai_stop','ai_test_connection','ai_viewer','ai_window',outer(c('ollama','lmstudio','aistudio','lemonade','exaone'),c('','_setting','_window'),paste0))
  stopifnot(all(exports %in% getNamespaceExports(ns)))
  stopifnot(file.exists(system.file('assets/ui/panel.js',package='jjstat')),file.exists(system.file('assets/katex/katex.min.js',package='jjstat')),file.exists(system.file('doc/AI_GUIDE.md',package='jjstat')))
  providers<-c('ollama','lmstudio','aistudio','lemonade','exaone')
  base<-paste0('http://127.0.0.1:',port)
  bases<-setNames(paste0(base,c('/oll','/lm','/gem/v1beta','/lem/api/v1','/exa/v1')),providers)
  for(provider in providers) {
    setting<-getExportedValue('jjstat',paste0(provider,'_setting'))
    if(provider=='exaone')setting(api_key='mock-secret',open=FALSE) else setting(base_url=bases[[provider]],model='mock-model',api_key='mock-secret',open=FALSE)
    for(stream in c(TRUE,FALSE)) {
      answer<-getExportedValue('jjstat',provider)(data.frame(value=7),prompt='explain',base_url=bases[[provider]],viewer=FALSE,echo=FALSE,stream=stream)
      stopifnot(identical(answer,'mock answer'))
    }
  }
  stopifnot(identical(ai_settings('exaone')$base_url,'https://api.exaone.ai/v1'),identical(ai_settings('exaone')$model,'k-exaone_v2.5'))
  window<-exaone_window(data.frame(value=7),base_url=bases[['exaone']],launch=FALSE,history_dir=tempfile('installed-ai-history-'))
  http<-get('.ai_http',ns);endpoint<-function(path)sub('index.html$',path,window$url)
  response<-http('exaone',endpoint('api/ask'),list(question='installed request'))
  deadline<-Sys.time()+30
  repeat {
    snapshot<-http('exaone',endpoint('api/state'))
    if(identical(snapshot$id,response$id)&&!isTRUE(snapshot$running)&&nzchar(snapshot$markdown))break
    if(Sys.time()>deadline)stop('Installed EXAONE worker timeout')
    Sys.sleep(.1)
  }
  stopifnot(identical(snapshot$markdown,'mock answer'),identical(snapshot$provider,'exaone'))
  cat('PASS: installed jjstat ',as.character(packageVersion('jjstat')),'; 25 AI exports; all five providers stream/nonstream; key-only EXAONE; installed assets/help; real namespace-loaded EXAONE web workers\n',sep='')
  cat('Installed package: ',find.package('jjstat'),'\n',sep='')
}
run_installed_ai_smoke()
