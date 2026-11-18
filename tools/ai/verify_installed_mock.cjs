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