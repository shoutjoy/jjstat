Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
for(file in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE)) source(file,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
# Provider-native timings and missing/invalid counters.
m <- .ai_finalize_metrics(.ai_usage(list(prompt_eval_count=1234,eval_count=112,eval_duration=1e10),'ollama'),120)
stopifnot(m$input_tokens==1234,m$output_tokens==112,m$tokens_per_second==11.2,m$rate_source=='server')
m <- .ai_finalize_metrics(.ai_usage(list(usage=list(prompt_tokens=1234,completion_tokens=112),timings=list(predicted_per_second=11.2)),'lemonade'),120)
stopifnot(m$tokens_per_second==11.2,m$rate_source=='server')
m <- .ai_finalize_metrics(.ai_usage(list(usageMetadata=list(promptTokenCount=1234,candidatesTokenCount=112,thoughtsTokenCount=30,totalTokenCount=1376)),'aistudio'),10)
stopifnot(m$tokens_per_second==11.2,m$rate_source=='request_average',m$thinking_tokens==30)
stopifnot(grepl('Context = 1,234 tok',.ai_metric_text(m,10,TRUE),fixed=TRUE))
stopifnot(grepl('요청 평균',.ai_metric_text(m,10,TRUE),fixed=TRUE))
stopifnot(grepl('미제공',.ai_metric_text(list(),10,TRUE),fixed=TRUE))
stopifnot(length(.ai_usage(list(usage=list(prompt_tokens=-1,completion_tokens='100')),'lmstudio'))==0L)
stopifnot(is.null(.ai_finalize_metrics(list(output_tokens=10,generation_seconds=0),0)$tokens_per_second))
js <- r"---(
const http=require('http');
const server=http.createServer((req,res)=>{let body='';req.on('data',x=>body+=x);req.on('end',()=>{
 const p=JSON.parse(body);const gem=req.url.includes('/gem/'),oll=req.url.includes('/oll/');
 const stream=gem?req.url.includes('streamGenerateContent'):p.stream;
 if(!gem&&!oll&&stream&&(!p.stream_options||p.stream_options.include_usage!==true)){res.writeHead(400);return res.end('{}')}
 const usage={prompt_tokens:1234,completion_tokens:112,total_tokens:1346};
 const timings={predicted_ms:10000,predicted_per_second:11.2};
 const meta={promptTokenCount:1234,candidatesTokenCount:112,totalTokenCount:1346};
 const stats={tokens_per_second:11.2};
 const text='metrics answer';
 if(!stream){res.writeHead(200,{'Content-Type':'application/json'});
  return res.end(JSON.stringify(gem?{candidates:[{content:{parts:[{text}]},finishReason:'STOP'}],usageMetadata:meta}:
   oll?{message:{content:text},done:true,done_reason:'stop',prompt_eval_count:1234,eval_count:112,eval_duration:10000000000}:
   {choices:[{message:{content:text},finish_reason:'stop'}],usage,...(req.url.includes('/lem/')?{timings}:{stats})}))}
 res.writeHead(200,{'Content-Type':oll?'application/x-ndjson':'text/event-stream'});
 const event=x=>res.write((oll?'':'data: ')+JSON.stringify(x)+'\n\n');
 if(gem){event({candidates:[{content:{parts:[{text}]},finishReason:'STOP'}]});event({usageMetadata:meta})}
 else if(oll){event({message:{content:text},done:false});event({done:true,done_reason:'stop',prompt_eval_count:1234,eval_count:112,eval_duration:10000000000})}
 else{event({choices:[{delta:{content:text}}],usage:{prompt_tokens:1234,completion_tokens:50}});event({choices:[{delta:{},finish_reason:'stop'}]});event({choices:[],usage,...(req.url.includes('/lem/')?{timings}:{stats})});res.write('data: [DONE]\n\n')}
 res.end();
})});server.listen(0,'127.0.0.1',()=>console.log(server.address().port));
)---"
file <- tempfile(fileext='.js');writeLines(js,file)
server <- processx::process$new('node',file,stdout='|',stderr='|')
tryCatch({
 port <- character()
 for(i in seq_len(100L)){server$poll_io(100L);port<-server$read_output_lines();if(length(port))break}
 if(length(port)!=1L)stop(paste(server$read_all_error(),collapse='\n'))
 url <- paste0('http://127.0.0.1:',port)
 statuses <- character(); .ai_viewer <- function(...) list(push=function(...)NULL,status=function(text){statuses<<-c(statuses,text)})
 for(provider in c('lmstudio','ollama','lemonade','aistudio')) for(stream in c(TRUE,FALSE)) {
  base <- paste0(url,switch(provider,lmstudio='/lm',ollama='/oll',lemonade='/lem/api/v1',aistudio='/gem/v1beta'))
  out <- capture.output(answer <- get(provider)('test',model='mock',base_url=base,api_key='mock-key',stream=stream))
  stopifnot(identical(answer,'metrics answer'),any(grepl('Context = 1,234 tok',out,fixed=TRUE)),
    any(grepl('Output = 112 tok',out,fixed=TRUE)),any(grepl('100%',out,fixed=TRUE)))
  metrics <- .ai_state()$last$metrics
  stopifnot(metrics$input_tokens==1234,metrics$output_tokens==112)
  if(provider!='aistudio')stopifnot(metrics$tokens_per_second==11.2,any(grepl('11.2 tok/s',out,fixed=TRUE)))
  else stopifnot(metrics$rate_source=='request_average',any(grepl('요청 평균',out,fixed=TRUE)))
 }
 stopifnot(any(grepl('완료 | Context = 1,234 tok',statuses,fixed=TRUE)))
 cat('PASS: four-provider streamed/nonstream metrics, usage-only final chunks, include_usage requests, native throughput, labeled average, missing/invalid counters, Viewer status\n')
},finally={if(server$is_alive())server$kill();unlink(file)})