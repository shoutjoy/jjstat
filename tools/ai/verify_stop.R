Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
for(file in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE)) source(file,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
serverfile<-tempfile(fileext='.js');clientfile<-tempfile(fileext='.js');stopfile<-tempfile()
serverjs<-r"---(
const http=require('http');const server=http.createServer((req,res)=>{let b='';req.on('data',x=>b+=x);req.on('end',()=>{
 const p=JSON.parse(b);res.writeHead(200,{'Content-Type':'text/event-stream'});
 const event=o=>res.write('data: '+JSON.stringify(o)+'\n\n');
 if(p.messages.some(x=>x.role==='assistant')){event({choices:[{delta:{content:' continued'}}]});event({choices:[{delta:{},finish_reason:'stop'}]});res.end('data: [DONE]\n\n');return}
 let n=0;const timer=setInterval(()=>{n++;event({choices:[{delta:{content:'part'+n+' '}}]});if(n===30){clearInterval(timer);event({choices:[{delta:{},finish_reason:'stop'}],usage:{prompt_tokens:10,completion_tokens:30}});res.end('data: [DONE]\n\n')}},p.model==='slow-start'?600:100);
 res.on('close',()=>clearInterval(timer));
})});server.listen(0,'127.0.0.1',()=>console.log(server.address().port));
)---"
clientjs<-r"---(
const fs=require('fs'),vm=require('vm'),http=require('http');const file=process.argv[2],early=process.argv[3]==='early';
const started=Date.now();const timer=setInterval(()=>{try{const ctx={window:{}};vm.runInNewContext(fs.readFileSync(file,'utf8'),ctx);const s=ctx.window.__ai_snapshot;if(!s.stop_url||(!early&&!s.html.includes('part'))){if(Date.now()-started>5000)process.exit(2);return}clearInterval(timer);fs.writeFileSync(process.argv[4],String(Date.now()/1000));const req=http.request(s.stop_url,{method:'POST'},res=>{res.resume();res.on('end',()=>console.log('STOP_HTTP_'+res.statusCode))});req.setTimeout(4000,()=>req.destroy());req.on('error',()=>process.exit(3));req.end()}catch{}},25);
)---"
writeLines(serverjs,serverfile);writeLines(clientjs,clientfile)
server<-processx::process$new('node',serverfile,stdout='|',stderr='|');client<-NULL
ns<-asNamespace('rstudioapi');old_available<-get('isAvailable',ns);old_viewer<-get('viewer',ns)
assignInNamespace('isAvailable',function(...)TRUE,'rstudioapi')
index<-NULL;early<-FALSE
assignInNamespace('viewer',function(path){index<<-path;client<<-processx::process$new('node',c(clientfile,file.path(dirname(path),'state.js'),if(early)'early' else 'partial',stopfile),stdout='|',stderr='|')},'rstudioapi')
tryCatch({
 port<-character();for(i in seq_len(100L)){server$poll_io(100L);port<-server$read_output_lines();if(length(port))break}
 if(length(port)!=1L)stop(paste(server$read_all_error(),collapse='\n'))
 url<-paste0('http://127.0.0.1:',port)
 started<-proc.time()[['elapsed']]
 out<-capture.output(partial<-lmstudio('test',model='mock',base_url=url))
 stopifnot(nzchar(partial),grepl('part',partial,fixed=TRUE),file.exists(stopfile),as.numeric(Sys.time())-as.numeric(readLines(stopfile,warn=FALSE))<3.5,
   identical(.ai_state()$last$finish_reason,'cancelled'),!any(grepl('100%',out,fixed=TRUE)))
 stopifnot(any(grepl('Output = ≈',out,fixed=TRUE)),any(grepl('| Context =',out,fixed=TRUE)))
 client$wait(1000);stopifnot(grepl('STOP_HTTP_200',paste(client$read_all_output(),collapse=''),fixed=TRUE))
 snapshot<-paste(readLines(file.path(dirname(index),'state.js'),warn=FALSE),collapse='')
 stopifnot(grepl('"can_stop":false',snapshot,fixed=TRUE))
 # Resume retains the cancelled prefix and does not reuse the old stop signal.
 resumed<-ai_continue(viewer=FALSE,echo=FALSE,progress=FALSE)
 stopifnot(startsWith(resumed,partial),endsWith(resumed,' continued'))
 early<-TRUE
 out<-capture.output(empty<-lmstudio('test',model='slow-start',base_url=url))
 stopifnot(identical(empty,''),identical(.ai_state()$last$finish_reason,'cancelled'),!any(grepl('100%',out,fixed=TRUE)))
 # Callback-based stop works with no Viewer.
 out<-capture.output(partial<-lmstudio('test',model='mock',base_url=url,viewer=FALSE,echo=FALSE,progress=TRUE,on_chunk=function(x)ai_stop()))
 stopifnot(nzchar(partial),identical(.ai_state()$last$finish_reason,'cancelled'))
 cat('PASS: Viewer HTTP stop during streaming and before first token; curl terminated promptly; partial response preserved; continue after stop; callback ai_stop; no false 100%\n')
},finally={if(server$is_alive())server$kill();if(!is.null(client)&&client$is_alive())client$kill();assignInNamespace('isAvailable',old_available,'rstudioapi');assignInNamespace('viewer',old_viewer,'rstudioapi');unlink(c(serverfile,clientfile,stopfile))})
