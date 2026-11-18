Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
for(file in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE)) source(file,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
for(p in c('ollama','lmstudio','lemonade','aistudio'))stopifnot(identical(formals(get(p))$stream,TRUE))
file <- tempfile(fileext='.js');done <- tempfile();sent <- tempfile();log <- tempfile()
js <- r"---(
const http=require('http'),fs=require('fs');const [done,sent,log]=process.argv.slice(2);
const server=http.createServer((req,res)=>{let b='';req.on('data',x=>b+=x);req.on('end',()=>{
 const p=JSON.parse(b);fs.appendFileSync(log,JSON.stringify({method:req.method,stream:p.stream})+'\n');
 try{fs.unlinkSync(done)}catch{};try{fs.unlinkSync(sent)}catch{};
 const oll=req.url.includes('/oll/');
 const finish=()=>{fs.writeFileSync(done,'done');res.end()};
 if(!p.stream){return setTimeout(()=>{fs.writeFileSync(done,'done');res.writeHead(200,{'Content-Type':'application/json'});res.end(JSON.stringify(oll?{message:{content:'first answer'},done:true,done_reason:'stop'}:{choices:[{message:{content:'first answer'},finish_reason:'stop'}]}))},1400)}
 res.writeHead(200,{'Content-Type':oll?'application/x-ndjson':'text/event-stream'});
 const event=x=>res.write((oll?'':'data: ')+JSON.stringify(x)+'\n\n');
 setTimeout(()=>event(oll?{message:{thinking:'private model reasoning'},done:false}:{choices:[{delta:{reasoning_content:'private model reasoning'}}]}),100);
 setTimeout(()=>{fs.writeFileSync(sent,String(Date.now()/1000));event(oll?{message:{content:'first'},done:false}:{choices:[{delta:{content:'first'}}]})},350);
 setTimeout(()=>event(oll?{message:{content:' answer'},done:false}:{choices:[{delta:{content:' answer'}}]}),850);
 setTimeout(()=>{if(oll)event({done:true,done_reason:'stop'});else{event({choices:[{delta:{},finish_reason:'stop'}]});res.write('data: [DONE]\n\n')}finish()},1400);
})});server.listen(0,'127.0.0.1',()=>console.log(server.address().port));
)---"
writeLines(js,file)
server<-processx::process$new('node',c(file,done,sent,log),stdout='|',stderr='|')
ns<-asNamespace('rstudioapi');old_available<-get('isAvailable',ns);old_viewer<-get('viewer',ns)
assignInNamespace('isAvailable',function(...)TRUE,'rstudioapi');assignInNamespace('viewer',function(...)NULL,'rstudioapi')
original <- .ai_viewer;records<-list();statuses<-character();callback_before_end<-logical()
.ai_viewer <- function(...) {
 v <- original(...);push<-v$push;status<-v$status
 v$push <- function(text){push(text); records[[length(records)+1L]] <<- list(before_end=!file.exists(done),
   lag=if(file.exists(sent))as.numeric(Sys.time())-as.numeric(readLines(sent,warn=FALSE)) else NA_real_,
   snapshot=paste(readLines(file.path(dirname(v$path),'state.js'),warn=FALSE,encoding='UTF-8'),collapse=''))}
 v$status <- function(text){statuses<<-c(statuses,text);status(text)}
 v
}
tryCatch({
 port<-character();for(i in seq_len(100L)){server$poll_io(100L);port<-server$read_output_lines();if(length(port))break}
 if(length(port)!=1L)stop(paste(server$read_all_error(),collapse='\n'))
 url<-paste0('http://127.0.0.1:',port)
 for(provider in c('ollama','lmstudio','lemonade')) for(stream in c(TRUE,FALSE)) {
  records<-list();statuses<-character();callback_before_end<-logical()
  base<-paste0(url,switch(provider,ollama='/oll',lmstudio='/lm',lemonade='/lem/api/v1'))
  get(paste0(provider,'_setting'))(base_url=base,model='mock',open=FALSE)
  output<-capture.output(answer<-get(provider)('test',stream=stream,progress=FALSE,echo=FALSE,
    on_chunk=function(x){callback_before_end<<-c(callback_before_end,!file.exists(done))}))
  stopifnot(identical(answer,'first answer'))
  if(stream){stopifnot(length(records)==2L,records[[1L]]$before_end,all(callback_before_end),
    grepl('first',records[[1L]]$snapshot,fixed=TRUE),records[[1L]]$lag<0.8,
    any(grepl('모델 추론 중',statuses,fixed=TRUE)))
    cat(provider,': first snapshot lag',round(records[[1L]]$lag*1000),'ms; published before server completion\n')
  }else stopifnot(length(records)==1L,!records[[1L]]$before_end,!any(callback_before_end))
 }
 requests<-lapply(readLines(log,warn=FALSE),jsonlite::fromJSON)
 stopifnot(all(vapply(requests,function(x)x$method=='POST',logical(1))))
 # Explicit console echo must not wait for completion or duplicate the answer.
 output<-capture.output(answer<-lmstudio('test',viewer=FALSE,stream=TRUE,echo=TRUE,progress=TRUE))
 stopifnot(identical(answer,'first answer'),!any(grepl('[AI]',output,fixed=TRUE)),
   sum(gregexpr('first answer',paste(output,collapse=''),fixed=TRUE)[[1L]]>0)==1L)
 cat('PASS: explicit stream defaults; direct POST without model rechecks; live callbacks and real Viewer snapshots before completion; reasoning status; nonstream buffering; immediate console echo\n')
},finally={if(server$is_alive())server$kill();assignInNamespace('isAvailable',old_available,'rstudioapi');assignInNamespace('viewer',old_viewer,'rstudioapi');unlink(c(file,done,sent,log))})