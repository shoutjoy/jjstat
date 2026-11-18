# Local contract tests; never call the official API or use real credentials.
Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
for(f in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE)) source(f,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
.ai_config_file <- local({file<-tempfile(fileext='.rds');function()file})
options(jjstat.ai.state=NULL)
run_exaone_checks <- function() {
  original_env <- Sys.getenv('EXAONE_API_KEY',unset=NA_character_)
  on.exit({if(is.na(original_env))Sys.unsetenv('EXAONE_API_KEY') else Sys.setenv(EXAONE_API_KEY=original_env);unlink(.ai_config_file())},add=TRUE)
  Sys.unsetenv('EXAONE_API_KEY')
  cfg <- ai_settings('exaone')
  stopifnot(identical(cfg$base_url,'https://api.exaone.ai/v1'),identical(cfg$model,'k-exaone_v2.5'),!cfg$api_key_set)
  stopifnot(identical(ai_models('exaone'),'k-exaone_v2.5'),is.na(ai_models('exaone',details=TRUE)$loaded))
  err <- tryCatch(exaone('test',viewer=FALSE),error=conditionMessage)
  stopifnot(grepl('EXAONE API key required',err,fixed=TRUE))
  Sys.setenv(EXAONE_API_KEY='mock-exaone-key')
  stopifnot(ai_settings('exaone')$api_key_set)
  f <- tempfile(fileext='.cjs');log <- tempfile();w <- NULL
  js <- r"---(
const http=require('http'),fs=require('fs');
http.createServer((req,res)=>{let body='';req.on('data',x=>body+=x);req.on('end',()=>{
const p=body?JSON.parse(body):{};
const authorized=req.headers.authorization==='Bearer mock-exaone-key';
fs.appendFileSync(process.argv[2],JSON.stringify({path:req.url,method:req.method,authorized,body:p})+'\n');
const json=(status,value)=>{res.writeHead(status,{'Content-Type':'application/json'});res.end(JSON.stringify(value))};
if(!authorized||p.model==='bad-key')return json(401,{error:{message:'Invalid key mock-exaone-key'}});
if(req.url!=='/v1/chat/completions')return json(404,{error:{message:'Unexpected endpoint'}});
const prev=p.messages.some(x=>x.role==='assistant');
const text=p.model==='truncate'?(prev?'continued':'partial'):'final answer';
const reason=p.model==='truncate'&&!prev?'length':'stop';
const usage={prompt_tokens:22,completion_tokens:218,total_tokens:240,reasoning_tokens:209};
if(!p.stream)return json(200,{choices:[{message:{content:text,reasoning_content:'private reasoning'},finish_reason:reason}],usage});
res.writeHead(200,{'Content-Type':'text/event-stream'});
const event=x=>res.write('data: '+JSON.stringify(x)+'\n\n');
event({choices:[{delta:{reasoning_content:'private reasoning'}}]});
setTimeout(()=>event({choices:[{delta:{content:text}}]}),100);
setTimeout(()=>{event({choices:[{delta:{},finish_reason:reason}]});event({choices:[],usage});res.end('data: [DONE]\n\n')},350);
})}).listen(0,'127.0.0.1',function(){console.log(this.address().port)});
)---"
  writeLines(js,f)
  server<-processx::process$new('node',c(f,log),stdout='|',stderr='|',windows_hide_window=TRUE)
  on.exit({if(!is.null(w))w$close();if(server$is_alive())server$kill();unlink(c(f,log))},add=TRUE)
  port<-character();for(i in seq_len(100L)){server$poll_io(100L);port<-server$read_output_lines();if(length(port))break}
  stopifnot(length(port)==1L);base<-paste0('http://127.0.0.1:',port,'/v1')
  exaone_setting(api_key='mock-exaone-key',open=FALSE,save=TRUE)
  stopifnot(identical(ai_settings('exaone')$model,'k-exaone_v2.5'))
  saved<-readRDS(.ai_config_file());stopifnot(identical(sort(names(saved$exaone)),c('base_url','model')))
  for(stream in c(TRUE,FALSE)) {
    answer<-exaone(data.frame(value=7),prompt='interpret',base_url=base,stream=stream,viewer=FALSE,echo=FALSE)
    stopifnot(identical(answer,'final answer'),identical(.ai_state()$last$metrics$thinking_tokens,209),identical(.ai_state()$last$metrics$input_tokens,22))
  }
  stopifnot(identical(ai_test_connection('exaone',base_url=base),'final answer'))
  stopifnot(identical(ai_load_model('exaone','k-exaone_v2.5',base_url=base)$status,'available'))
  answer<-exaone('interpret',model='truncate',base_url=base,viewer=FALSE,echo=FALSE)
  stopifnot(identical(answer,'partialcontinued'))
  answer<-exaone('interpret',base_url=base,viewer=FALSE,echo=FALSE,on_chunk=function(x)ai_stop())
  stopifnot(identical(answer,'final answer'),identical(.ai_state()$last$finish_reason,'cancelled'))
  err<-tryCatch(exaone('test',model='bad-key',base_url=base,viewer=FALSE,echo=FALSE,retries=0),error=conditionMessage)
  stopifnot(!grepl('mock-exaone-key',err,fixed=TRUE),grepl('401',err,fixed=TRUE))
  # A real pair of background R workers: submit a question over local HTTP.
  w<-exaone_window(data.frame(value=7),base_url=base,launch=FALSE,history_dir=tempfile('exaone-history-'))
  endpoint<-function(path)sub('index.html$',path,w$url)
  stopifnot(!grepl('mock-exaone-key',paste(readLines(file.path(w$path,'state.json')),collapse=''),fixed=TRUE))
  response<-.ai_http('exaone',endpoint('api/ask'),list(question='interpret'))
  stopifnot(response$accepted)
  deadline<-Sys.time()+20
  repeat {
    state<-.ai_http('exaone',endpoint('api/state'))
    if(identical(state$id,response$id)&&!isTRUE(state$running)&&nzchar(state$markdown))break
    if(Sys.time()>deadline)stop('EXAONE web worker timed out')
    Sys.sleep(.1)
  }
  stopifnot(identical(state$markdown,'final answer'),identical(state$provider,'exaone'),identical(state$model,'k-exaone_v2.5'))
  history<-.ai_http('exaone',endpoint('api/history'))
  stopifnot(length(history$records)==1L)
  raw_history<-paste(readLines(list.files(w$history_dir,full.names=TRUE)[1L]),collapse='')
  stopifnot(!grepl('mock-exaone-key',raw_history,fixed=TRUE))
  w$close()
  reqs<-lapply(readLines(log),jsonlite::fromJSON)
  stopifnot(all(vapply(reqs,function(x)x$authorized,logical(1))),all(vapply(reqs,function(x)identical(x$path,'/v1/chat/completions'),logical(1))))
  stopifnot(identical(reqs[[1]]$body$model,'k-exaone_v2.5'),identical(reqs[[1]]$body$reasoning_effort,'low'),reqs[[1]]$body$temperature==1,reqs[[1]]$body$max_tokens==16384,reqs[[1]]$body$stream_options$include_usage)
  last<-tail(reqs,1)[[1]]$body;stopifnot(last$max_tokens==16384,identical(last$reasoning_effort,'low'))
  cat('PASS: EXAONE key-only defaults, env/session key, safe settings, Bearer auth, no model-list/load requests, SSE/nonstream, reasoning separation/usage, continuation, stop, redaction, real web workers and history\n')
}
run_exaone_checks()
