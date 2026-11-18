Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
for(file in list.files('R',pattern='^ai_.*[.]R$',full.names=TRUE)) source(file,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
file<-tempfile(fileext='.js');log<-tempfile();done<-tempfile()
js<-r"---(
const http=require('http'),fs=require('fs');const [log,done]=process.argv.slice(2);
const server=http.createServer((req,res)=>{let b='';req.on('data',x=>b+=x);req.on('end',()=>{
 const p=JSON.parse(b),gem=req.url.includes('/gem/'),oll=req.url.includes('/oll/');
 fs.appendFileSync(log,JSON.stringify({path:req.url,body:p})+'\n');try{fs.unlinkSync(done)}catch{}
 if(p.model==='error'){res.writeHead(500,{'Content-Type':'application/json'});return res.end(JSON.stringify({error:{message:'WINDOW_TEST_ERROR'}}))}
 const stream=gem?req.url.includes('streamGenerateContent'):p.stream;
 const text='FIRST_WINDOW_TEXT_SECOND';
 if(!stream){return setTimeout(()=>{fs.writeFileSync(done,'done');res.writeHead(200,{'Content-Type':'application/json'});res.end(JSON.stringify(gem?{candidates:[{content:{parts:[{text}]},finishReason:'STOP'}]}:oll?{message:{content:text},done:true,done_reason:'stop'}:{choices:[{message:{content:text},finish_reason:'stop'}]}))},700)}
 res.writeHead(200,{'Content-Type':oll?'application/x-ndjson':'text/event-stream'});const event=o=>res.write((oll?'':'data: ')+JSON.stringify(o)+'\n\n');
 const timers=[];timers.push(setTimeout(()=>event(gem?{candidates:[{content:{parts:[{text:'MODEL_THINKING',thought:true}]}}]}:oll?{message:{thinking:'MODEL_THINKING'},done:false}:{choices:[{delta:{reasoning_content:'MODEL_THINKING'}}]}),50));
 timers.push(setTimeout(()=>event(gem?{candidates:[{content:{parts:[{text:'FIRST_WINDOW_TEXT'}]}}]}:oll?{message:{content:'FIRST_WINDOW_TEXT'},done:false}:{choices:[{delta:{content:'FIRST_WINDOW_TEXT'}}]}),200));
 timers.push(setTimeout(()=>event(gem?{candidates:[{content:{parts:[{text:'_SECOND'}]}}]}:oll?{message:{content:'_SECOND'},done:false}:{choices:[{delta:{content:'_SECOND'}}]}),450));
 timers.push(setTimeout(()=>{fs.writeFileSync(done,'done');event(gem?{candidates:[{finishReason:'STOP'}]}:oll?{done:true,done_reason:'stop'}:{choices:[{delta:{},finish_reason:'stop'}]});res.end(oll||gem?'':'data: [DONE]\n\n')},700));
 res.on('close',()=>timers.forEach(clearTimeout));
})});server.listen(0,'127.0.0.1',()=>console.log(server.address().port));
)---"
writeLines(js,file);server<-processx::process$new('node',c(file,log,done),stdout='|',stderr='|')
windows<-list()
tree<-function(w){kids<-as.character(tcltk::tcl('winfo','children',w));c(w,unlist(lapply(kids,tree)))}
widget_value<-function(w,...)tcltk::tclvalue(tcltk::tcl(w,...))
widgets_for<-function(top){
 ids<-tree(top$ID);classes<-vapply(ids,function(w)tcltk::tclvalue(tcltk::tcl('winfo','class',w)),character(1))
 buttons<-ids[classes=='Button'];texts<-vapply(buttons,function(w)widget_value(w,'cget','-text'),character(1))
 list(run=buttons[texts=='질문 실행'],stop=buttons[texts=='응답 중지'],text=ids[classes=='Text'])
}
tryCatch({
 port<-character();for(i in 1:100){server$poll_io(100);port<-server$read_output_lines();if(length(port))break}
 if(length(port)!=1L)stop(paste(server$read_all_error(),collapse='\n'))
 url<-paste0('http://127.0.0.1:',port)
 for(provider in c('ollama','lmstudio','lemonade','aistudio')) for(stream in c(TRUE,FALSE)) {
  base<-paste0(url,switch(provider,ollama='/oll',lmstudio='/lm',lemonade='/lem/api/v1',aistudio='/gem/v1beta'))
  get(paste0(provider,'_setting'))(base_url=base,model='mock-model',api_key='test-key',open=FALSE)
  forced<-FALSE;chunks<-character();statuses<-character();first_before_end<-NULL;found<-NULL
  top<-get(paste0(provider,'_window'))({forced<<-TRUE;data.frame(marker_result=c('평균 α','p < .001'))},
   engine='tk',prompt='분석 데이터를 학술체로 설명해줘',stream=stream,viewer=FALSE,window=FALSE,echo=FALSE,
   on_chunk=function(text){
    chunks<<-c(chunks,text)
    if(is.null(first_before_end))first_before_end<<-!file.exists(done)
    stopifnot(grepl(text,widget_value(found$text,'get','1.0','end'),fixed=TRUE),
      widget_value(found$run,'cget','-state')=='disabled',widget_value(found$stop,'cget','-state')=='normal')
   },on_status=function(text){statuses<<-c(statuses,text)})
  windows[[length(windows)+1L]]<-top;found<-widgets_for(top)
  stopifnot(forced,length(chunks)==0L)
  tcltk::tcl(found$run,'invoke')
  text<-widget_value(found$text,'get','1.0','end')
  stopifnot(grepl('[질문]',text,fixed=TRUE),grepl('FIRST_WINDOW_TEXT_SECOND',text,fixed=TRUE),
    grepl('[완료]',text,fixed=TRUE),!grepl('MODEL_THINKING',text,fixed=TRUE),
    widget_value(found$run,'cget','-state')=='normal',widget_value(found$stop,'cget','-state')=='disabled')
  if(stream)stopifnot(length(chunks)==2L,first_before_end,any(grepl('모델 추론 중',statuses,fixed=TRUE)))
  else stopifnot(length(chunks)==1L,!first_before_end)
  tcltk::tkdestroy(top);windows<-list()
 }
 # Visible request failures and repeatable execution.
 top<-lmstudio_window(engine="tk",data.frame(value=1),model='error',base_url=paste0(url,'/lm'))
 windows<-list(top);found<-widgets_for(top);tcltk::tcl(found$run,'invoke')
 stopifnot(grepl('WINDOW_TEST_ERROR',widget_value(found$text,'get','1.0','end'),fixed=TRUE),widget_value(found$run,'cget','-state')=='normal')
 tcltk::tkdestroy(top);windows<-list()
 # A real Tk stop button cancels the active request and leaves the received text.
 stopped<-FALSE
 top<-lmstudio_window(engine="tk",data.frame(value=1),model='mock-model',base_url=paste0(url,'/lm'),
  on_chunk=function(text){if(!stopped){stopped<<-TRUE;tcltk::tcl(found$stop,'invoke')}})
 windows<-list(top);found<-widgets_for(top);tcltk::tcl(found$run,'invoke')
 text<-widget_value(found$text,'get','1.0','end')
 stopifnot(grepl('FIRST_WINDOW_TEXT',text,fixed=TRUE),grepl('[중지됨',text,fixed=TRUE),
  !grepl('[완료]',text,fixed=TRUE),identical(.ai_state()$last$finish_reason,'cancelled'))
 tcltk::tkdestroy(top);windows<-list()
 original_chat <- .ai_chat
 .ai_chat <- function(...) {state <- .ai_state();state$last <- list(finish_reason='stop');'RECOVERED_FULL_RESPONSE_WITHOUT_CHUNKS'}
 top <- lmstudio_window(engine="tk",data.frame(value=1));windows<-list(top);found<-widgets_for(top)
 tcltk::tcl(found$run,'invoke')
 stopifnot(grepl('RECOVERED_FULL_RESPONSE_WITHOUT_CHUNKS',widget_value(found$text,'get','1.0','end'),fixed=TRUE))
 .ai_chat <- original_chat;tcltk::tkdestroy(top);windows<-list()
 top <- lmstudio_window(engine="tk",initial_response='RESTORED_EXISTING_RESPONSE');windows<-list(top);found<-widgets_for(top)
 stopifnot(grepl('RESTORED_EXISTING_RESPONSE',widget_value(found$text,'get','1.0','end'),fixed=TRUE))
 tcltk::tkdestroy(top);windows<-list()
 records<-lapply(readLines(log,warn=FALSE,encoding='UTF-8'),jsonlite::fromJSON,simplifyVector=FALSE)
 stopifnot(length(records)==10L)
 for(rec in records[1:8]){
  payload<-rec$body
  user<-if(!is.null(payload$messages))payload$messages[[2]]$content else payload$contents[[1]]$parts[[1]]$text
  stopifnot(grepl('marker_result',user,fixed=TRUE),grepl('분석 데이터를',user,fixed=TRUE))
 }
 cat('PASS: four actual Tk question windows; manual button only; streamed/nonstream body display; first text visible before completion; input data and callbacks preserved; reasoning status; errors visible; stop button; final-answer recovery without callbacks; restore without a model call\n')
},finally={for(w in windows)try(tcltk::tkdestroy(w),silent=TRUE);if(server$is_alive())server$kill();unlink(c(file,log,done))})
