Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
for (file in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE)) source(file,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
script <- tempfile(fileext='.js'); requests <- tempfile(fileext='.jsonl')
js <- r"---(
const http=require('http'),fs=require('fs');
const log=process.argv[2];
const server=http.createServer((req,res)=>{let body='';req.on('data',s=>body+=s);req.on('end',()=>{
 const p=body?JSON.parse(body):{};const path=req.url;
 fs.appendFileSync(log,JSON.stringify({path,body:p,auth:!!(req.headers.authorization||req.headers['x-goog-api-key'])})+'\n');
 const json=obj=>{res.writeHead(200,{'Content-Type':'application/json'});res.end(JSON.stringify(obj))};
 if(req.method==='GET'){
  if(path.includes('/lm/'))return json({models:[{key:'mock-model',type:'llm',loaded_instances:[]},{key:'embed',type:'embedding'}]});
  if(path.includes('/oll/'))return json({models:[{name:'mock-model'}]});
  if(path.includes('/lem/'))return json({data:[{id:'mock-model',labels:['llm']},{id:'image-model',labels:['image']}]});
  if(path.includes('/gem/'))return json({models:[{name:'models/mock-model',supportedGenerationMethods:['generateContent']},{name:'models/embed',supportedGenerationMethods:['embedContent']}]});
 }
 if(path.endsWith('/models/load'))return json({status:'loaded',instance_id:'mock-model'});
 if(path.endsWith('/load')||path.endsWith('/generate'))return json({status:'success',done:true});
 const gem=path.includes('/gem/'),oll=path.includes('/oll/');
 const messages=p.messages||p.contents||[];const previous=messages.some(x=>x.role==='assistant'||x.role==='model');
 const model=p.model||(path.includes('alwayslength')?'alwayslength':path.includes('truncate')?'truncate':path.includes('incomplete')?'incomplete':'mock-model');
 const trunc=(model==='truncate'&&!previous)||model==='alwayslength';
 const incomplete=model==='incomplete'&&!previous;
 const text=trunc?'첫 문장 미완':previous?'성. 다음 문단.':'mock answer';
 const reason=trunc?(gem?'MAX_TOKENS':'length'):(gem?'STOP':'stop');
 const stream=gem?path.includes('streamGenerateContent'):p.stream;
 if(!stream){if(gem)return json({candidates:[{content:{parts:[{text}]},finishReason:reason}]});if(oll)return json({message:{content:text},done:true,done_reason:reason});return json({choices:[{message:{content:text},finish_reason:reason}]})}
 res.writeHead(200,{'Content-Type':oll?'application/x-ndjson':'text/event-stream'});
 const event=obj=>res.write((oll?'':'data: ')+JSON.stringify(obj)+'\n\n');
 if(gem)event({candidates:[{content:{parts:[{text}]},...(incomplete?{}:{finishReason:reason})}]});
 else if(oll){event({message:{content:text},done:false});if(!incomplete)event({message:{content:''},done:true,done_reason:reason})}
 else {event({choices:[{delta:{content:text}}]});if(!incomplete){event({choices:[{delta:{},finish_reason:reason}]});res.write('data: [DONE]\n\n')}}
 res.end();
})});server.listen(0,'127.0.0.1',()=>console.log(server.address().port));
)---"
writeLines(js,script)
server <- processx::process$new('node',c(script,requests),stdout='|',stderr='|')
tryCatch({
  port <- character()
  for(i in seq_len(100L)) {server$poll_io(100L);port<-server$read_output_lines();if(length(port)) break}
  if(length(port)!=1L) stop(paste(server$read_all_error(),collapse='\n'))
  url <- paste0('http://127.0.0.1:',port)
  providers <- c('ollama','lmstudio','aistudio','lemonade')
  bases <- setNames(paste0(url,c('/oll','/lm','/gem/v1beta','/lem/api/v1')),providers)
  cfgfile <- tempfile(fileext='.rds'); .ai_config_file <- function() cfgfile
  options(jjstat.ai.state=NULL)
  for(provider in providers) {
    f <- get(paste0(provider,'_setting'))
    f(base_url=bases[[provider]],model='mock-model',api_key='mock-secret',open=FALSE,save=TRUE)
    stopifnot(identical(ai_models(provider),'mock-model'),ai_settings(provider)$api_key_set)
    stopifnot(!'api_key' %in% names(ai_settings(provider)))
    ai_load_model(provider,'mock-model')
    stopifnot(identical(ai_test_connection(provider),'mock answer'))
    fun <- get(provider)
    # Both protocol formats, automatic continuation, and original data preservation.
    for(stream in c(TRUE,FALSE)) {
      result <- fun(data=data.frame(value=7),prompt='original data request',model='truncate',
        viewer=FALSE,echo=FALSE,stream=stream,auto_continue=TRUE)
      stopifnot(identical(result,'첫 문장 미완성. 다음 문단.'),identical(ai_last_response(),result))
    }
    warning <- NULL
    partial <- withCallingHandlers(fun('original',model='truncate',viewer=FALSE,echo=FALSE,auto_continue=FALSE),
      warning=function(w){warning<<-conditionMessage(w);invokeRestart('muffleWarning')})
    stopifnot(identical(partial,'첫 문장 미완'),!is.null(warning))
    full <- ai_continue(viewer=FALSE,echo=FALSE)
    stopifnot(identical(full,'첫 문장 미완성. 다음 문단.'))
    failed <- FALSE
    tryCatch(fun('original',model='incomplete',viewer=FALSE,echo=FALSE),error=function(e) failed<<-TRUE)
    stopifnot(failed,identical(ai_last_response(),'mock answer'))
    stopifnot(identical(ai_continue(viewer=FALSE,echo=FALSE),'mock answer성. 다음 문단.'))
  }
  stopifnot(!grepl('mock-secret',paste(readLines(cfgfile,warn=FALSE),collapse=''),fixed=TRUE))
  saved <- readRDS(cfgfile);stopifnot(all(vapply(saved,function(x) identical(sort(names(x)),c('base_url','model')),logical(1))))
  records <- lapply(readLines(requests,warn=FALSE),jsonlite::fromJSON,simplifyVector=FALSE)
  continuations <- Filter(function(x) !is.null(x$body$messages) && length(x$body$messages)>3L,records)
  stopifnot(length(continuations)>0L)
  stopifnot(any(vapply(continuations,function(x) grepl('value',x$body$messages[[2L]]$content,fixed=TRUE),logical(1))))
  limit_warning <- NULL
  bounded <- withCallingHandlers(lmstudio('bounded',model='alwayslength',viewer=FALSE,echo=FALSE,max_continuations=1L),
    warning=function(w){limit_warning<<-conditionMessage(w);invokeRestart('muffleWarning')})
  stopifnot(identical(bounded,'첫 문장 미완첫 문장 미완'),!is.null(limit_warning))
  # Exercise real Tcl/Tk widgets against the same mock endpoints.
  if(capabilities('tcltk')) {
    for(provider in providers) {
      top <- get(paste0(provider,'_setting'))(base_url=bases[[provider]])
      tcltk::tcl('update')
      children <- as.character(tcltk::tcl('winfo','children',top))
      bar <- children[vapply(children,function(w) as.character(tcltk::tcl('winfo','class',w))=='Frame',logical(1))]
      buttons <- as.character(tcltk::tcl('winfo','children',bar[[1L]]))
      for(i in seq_along(buttons)) {
        tcltk::tcl(buttons[[i]],'invoke')
        labels_now <- children[vapply(children,function(w) as.character(tcltk::tcl('winfo','class',w))=='Label',logical(1))]
        status_now <- as.character(tcltk::tcl(tail(labels_now,1L),'cget','-textvariable'))
        text_now <- paste(as.character(tcltk::tcl('set',status_now)),collapse=' ')
        expected <- c('연결 성공',if(provider=='aistudio') '모델 사용 가능' else '모델 로딩 성공','응답 성공','설정을 적용')
        stopifnot(grepl(expected[[i]],text_now,fixed=TRUE))
      }
      labels <- children[vapply(children,function(w) as.character(tcltk::tcl('winfo','class',w))=='Label',logical(1))]
      statusvar <- as.character(tcltk::tcl(tail(labels,1L),'cget','-textvariable'))
      message(provider, ': ', paste(as.character(tcltk::tcl('set',statusvar)),collapse=' '))
      stopifnot(grepl('설정을 적용',paste(as.character(tcltk::tcl('set',statusvar)),collapse=' '),fixed=TRUE))
      tcltk::tkdestroy(top)
    }
  }
  unlink(cfgfile)
  cat('PASS: four providers, model list/load/generation, stream/nonstream continuation, original data, partial recovery, bounded continuation, safe settings, real Tk window actions\n')
},finally={if(server$is_alive())server$kill();unlink(c(script,requests))})