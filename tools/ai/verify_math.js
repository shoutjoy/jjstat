const fs = require('fs');
const path = require('path');
const vm = require('vm');
const assert = require('assert');
const root = path.resolve(__dirname, '../..');
const html = fs.readFileSync(path.join(root, 'verification/index.html'), 'utf8');
const script=fs.readFileSync(path.join(root,'inst/assets/ui/panel.js'),'utf8');
new vm.Script(script);
const katex = require(path.join(root,'inst/assets/katex/katex.min.js'));
const elements = ['M = 2.81', 't(40.1) = 0.71, p = .48', 'x_i^2', '\\frac{a}{b}', '\\sum_{i=1}^n x_i', 'P(A|B)'].map((tex,i) => ({
  dataset: { display: i > 2 ? 'true' : 'false' },
  getAttribute: () => tex,
  innerHTML: ''
}));
const helper=script.slice(script.indexOf('function renderMath('),script.indexOf('function sanitizedHTML('));
for(let index=0;index<elements.length;index++)elements[index].dataset.tex=elements[index].getAttribute();
const context={window:{katex}};vm.createContext(context);vm.runInContext(helper,context);
context.renderMath({querySelectorAll:()=>elements});
for (const el of elements) {
  assert(el.innerHTML.includes('class="katex"'));
  assert(el.innerHTML.includes('<math'));
  assert(!el.innerHTML.includes('katex-error'));
}
const css = fs.readFileSync(path.join(root, 'inst/assets/katex/katex.min.css'), 'utf8');
for(const match of css.matchAll(/url\(([^)]+\.woff2)\)/g)) {
  assert(fs.existsSync(path.join(root, 'inst/assets/katex', match[1])));
}
console.log('PASS: Viewer JavaScript syntax, 6 real KaTeX renderings, all WOFF2 font files');
