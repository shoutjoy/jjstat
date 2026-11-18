Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');for(f in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE))source(f,encoding='UTF-8');.ai_math_assets<-normalizePath('inst/assets/katex',winslash='/')
ns<-asNamespace('rstudioapi');old<-lapply(c('isAvailable','sendToConsole'),function(n)get(n,ns))
assignInNamespace('isAvailable',function(...)TRUE,'rstudioapi')
got<-NULL
assignInNamespace('sendToConsole',function(code,execute,echo,focus,...){got<<-list(code=code,execute=execute,echo=echo,focus=focus);invisible(NULL)},'rstudioapi')
url<-.ai_console_bridge();bridge<-.ai_state()$console_bridge
code<-paste0('# \ud55c\uae00 \ucf54\ub4dc\n',paste(rep('x <- "\uad04\ud638 ( ); \ub530\uc634\ud45c \\""\n',400),collapse=''),'stop("MUST_NOT_EXECUTE")')
file<-tempfile();jsonlite::write_json(list(code=code),file,auto_unbox=TRUE)
client<-tempfile(fileext='.js');writeLines("const fs=require('fs');fetch(process.argv[2],{method:'POST',headers:{'Content-Type':'text/plain;charset=UTF-8'},body:fs.readFileSync(process.argv[3],'utf8')}).then(r=>r.text()).then(console.log).catch(e=>{console.error(e);process.exit(1)});",client)
p<-processx::process$new('node',c(client,url,file),stdout='|',stderr='|')
tryCatch({
 deadline<-Sys.time()+8
 while(p$is_alive()&&Sys.time()<deadline){later::run_now(.05);Sys.sleep(.01)}
 stopifnot(!p$is_alive(),p$get_exit_status()==0L,identical(got$code,code),identical(got$execute,FALSE),got$echo,got$focus)
 out<-p$read_all_output();stopifnot(grepl('"executed":false',out,fixed=TRUE))
 cat('PASS: persistent main-session timer bridge, Unicode and >8KB code, exact insertion, execute=FALSE, no evaluation\n')
},finally={if(p$is_alive())p$kill();bridge$close();assignInNamespace('isAvailable',old[[1]],'rstudioapi');assignInNamespace('sendToConsole',old[[2]],'rstudioapi');unlink(file)})
