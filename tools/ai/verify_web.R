# Development test: Node, Playwright, and Edge are required, not runtime dependencies.
Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
if(file.exists('ai_clients.R')){source('ai_clients.R',encoding='UTF-8');td<-'jjstat_integration/tools'}else{
 for(f in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE))source(f,encoding='UTF-8')
 .ai_math_assets<-normalizePath('inst/assets/katex',winslash='/');td<-'tools/ai'
}
run_test<-function(){
 out<-file.path(normalizePath(getwd(),winslash='/'),'verification','web-smoke');dir.create(out,recursive=TRUE,showWarnings=FALSE)
 fixture<-normalizePath(file.path(td,'verify_web_fixture.md'),winslash='/');log<-tempfile()
 server<-processx::process$new('node',c(normalizePath(file.path(td,'verify_web_mock.cjs'),winslash='/'),fixture,log),stdout='|',stderr='|',windows_hide_window=TRUE)
 on.exit(if(server$is_alive())server$kill(),add=TRUE)
 port<-character();for(i in seq_len(100)){server$poll_io(100);port<-server$read_output_lines();if(length(port))break};stopifnot(length(port)==1)
 ns<-asNamespace('rstudioapi');names<-c('isAvailable','viewer','sendToConsole');old<-lapply(names,function(n)get(n,ns));consolelog<-tempfile()
 on.exit({for(i in seq_along(names))assignInNamespace(names[i],old[[i]],'rstudioapi');bridge<-.ai_state()$console_bridge;if(!is.null(bridge))bridge$close()},add=TRUE)
 assignInNamespace('isAvailable',function(...)TRUE,'rstudioapi');assignInNamespace('viewer',function(...)NULL,'rstudioapi')
 assignInNamespace('sendToConsole',function(code,execute,echo,focus,...)jsonlite::write_json(list(code=code,execute=execute),consolelog,auto_unbox=TRUE),'rstudioapi')
 w<-lmstudio_window(data.frame(group=c('A','B'),mean=c(3.2,2.8)),model='mock',base_url=paste0('http://127.0.0.1:',port,'/v1'),api_key='secret-test-key',launch=FALSE,history_dir=tempfile('history_'),auto_continue=FALSE)
 on.exit(w$close(),add=TRUE);stopifnot(!file.exists(log))
 v<-.ai_viewer('test','viewer question');v$push(readChar(fixture,file.info(fixture)$size,useBytes=TRUE));v$status('complete');v$close()
 cfg<-file.path(out,'test-config.json');.ai_json_write(list(url=w$url,viewer=v$path,requests=log,output=out,console_capture=consolelog),cfg)
 r<-processx::process$new('node',c(normalizePath(file.path(td,'verify_web_browser.cjs'),winslash='/'),cfg),stdout='|',stderr='|',windows_hide_window=TRUE)
 on.exit(if(r$is_alive())r$kill(),add=TRUE)
 until<-Sys.time()+90
 while(r$is_alive()&&Sys.time()<until){later::run_now(.025);Sys.sleep(.005)}
 if(r$is_alive())stop('Browser test timed out.')
 cat(r$read_all_output(),r$read_all_error());stopifnot(r$get_exit_status()==0)
}
run_test()
