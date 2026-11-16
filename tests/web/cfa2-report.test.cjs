// Run: node --test tests/web/cfa2-report.test.cjs (CFA2_TEST_REPORT can override the fixture path)
// Tests the generated script without launching a browser or touching the OS clipboard.
const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const reportPath = process.env.CFA2_TEST_REPORT || path.join(process.env.TEMP, 'jjstat-cfa2-report.html');
const html = fs.readFileSync(reportPath, 'utf8');
const decode = text => text.replace(/&lt;/g, '<').replace(/&gt;/g, '>')
  .replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&amp;/g, '&');
const script = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)].at(-1)[1];
const markdown = decode(html.match(/<textarea id="markdown-source"[^>]*>([\s\S]*?)<\/textarea>/)[1]);
const tables = [...html.matchAll(/<table\b[^>]*>[\s\S]*?<\/table>/g)].map(match => ({
  outerHTML: match[0],
  rows: [...match[0].matchAll(/<tr\b[^>]*>([\s\S]*?)<\/tr>/g)].map(row => ({
    cells: [...row[1].matchAll(/<t[hd]\b[^>]*>([\s\S]*?)<\/t[hd]>/g)]
      .map(cell => ({innerText: decode(cell[1].replace(/<[^>]+>/g, ''))}))
  }))
}));
const codeNodes = Object.fromEntries([...html.matchAll(/<code id="([^"]+)">([\s\S]*?)<\/code>/g)]
  .map(match => [match[1], {textContent: decode(match[2])}]));
const imageLinks = [...html.matchAll(/<a class="download-image" href="([^"]+)" download="([^"]+)">/g)]
  .map(match => ({href: match[1], name: match[2]}));
function harness({modern = true, reject = false, fallbackOK = true} = {}) {
  const events = new Map(), copies = [], downloads = [], blobs = new Map();
  const element = () => ({listeners: {}, addEventListener(event, fn) {this.listeners[event] = fn;}});
  const ids = {'download-html': element(), 'download-md': element(), 'markdown-source': {value: markdown}, ...codeNodes};
  const statuses = [];
  const buttons = tables.map(table => {
    const status = {textContent: ''}; statuses.push(status);
    const card = {querySelector(selector) {return selector === 'table' ? table : status;}};
    return Object.assign(element(), {closest() {return card;}});
  });
  const codeStatuses = [];
  const codeButtons = Object.keys(codeNodes).map(id => {
    const status = {textContent: ''}; codeStatuses.push(status);
    const card = {querySelector() {return status;}};
    return Object.assign(element(), {
      getAttribute(name) {assert.equal(name, 'data-copy-target'); return id;},
      closest() {return card;}
    });
  });
  const selection = {rangeCount: 0, removeAllRanges() {}, addRange() {}};
  const document = {
    documentElement: {outerHTML: html.replace(/^<!doctype html>\s*/i, '')},
    getElementById(id) {return ids[id];},
    querySelectorAll(selector) {return selector === '.copy-table' ? buttons : codeButtons;},
    createElement(tag) {
      assert.equal(tag, 'a');
      return {click() {downloads.push({name: this.download, blob: blobs.get(this.href)});}, remove() {}};
    },
    body: {appendChild() {}},
    createRange() {return {selectNodeContents() {}};},
    addEventListener(name, fn) {events.set(name, fn);},
    removeEventListener(name) {events.delete(name);},
    execCommand(name) {
      assert.equal(name, 'copy');
      if (!fallbackOK) return false;
      const data = {};
      events.get('copy')({clipboardData: {setData(type, text) {data[type] = text;}}, preventDefault() {}});
      copies.push(data); return true;
    }
  };
  class ClipboardItem {constructor(data) {this.data = data;}}
  const navigator = modern ? {clipboard: {async write(items) {
    if (reject) throw new Error('Permission denied');
    copies.push(items[0].data);
  }, async writeText(text) {
    if (reject) throw new Error('Permission denied');
    copies.push({'text/plain': text});
  }}} : {};
  vm.runInNewContext(script, {
    document, navigator, window: {ClipboardItem, getSelection() {return selection;}}, ClipboardItem,
    Blob, URL: {createObjectURL(blob) {const id = 'blob:' + blobs.size; blobs.set(id, blob); return id;},
                revokeObjectURL() {}}, setTimeout(fn) {fn();}
  });
  return {ids, buttons, statuses, codeButtons, codeStatuses, copies, downloads, events};
}
test('report contains every table, embedded graph, and hover/focus copy controls', () => {
  assert.equal(tables.length, 10);
  assert.equal((html.match(/class="copy-table"/g) || []).length, tables.length);
  assert.match(html, /\.table-card:hover \.copy-table/);
  assert.match(html, /\.table-card:focus-within \.copy-table/);
  assert.match(html, /data:image\/png;base64,/);
  assert.match(markdown, /## Model/);
  assert.match(markdown, /## Variable order/);
});
test('HTML and Markdown download real content with correct file types', async () => {
  const h = harness();
  h.ids['download-html'].listeners.click();
  h.ids['download-md'].listeners.click();
  assert.equal(h.downloads[0].name, 'cfa2-results.html');
  assert.equal(h.downloads[1].name, 'cfa2-results.md');
  const exportedHTML = await h.downloads[0].blob.text();
  assert.ok(exportedHTML.startsWith('<!doctype html>\n<html'));
  assert.match(exportedHTML, /HTML 다운로드/);
  assert.match(exportedHTML, /data:image\/png;base64,/);
  assert.equal(await h.downloads[1].blob.text(), markdown);
  assert.match(h.downloads[1].blob.type, /^text\/markdown/);
  assert.match(markdown, /## 10 Item loadings \/ CR \/ AVE/);
  assert.match(markdown, /\|.*Est.*\|/);
});
test('every copy button produces rich HTML and tab separated plain text', async () => {
  const h = harness();
  for (let i = 0; i < tables.length; i++) {
    await h.buttons[i].listeners.click();
    assert.equal(h.statuses[i].textContent, '복사 완료');
    const copied = h.copies[i];
    const rich = await copied['text/html'].text();
    const plain = await copied['text/plain'].text();
    assert.match(rich, /<table/);
    assert.equal(plain.split('\n').length, tables[i].rows.length);
    assert.equal(plain.split('\n')[0].split('\t').length, tables[i].rows[0].cells.length);
    assert.ok(!plain.includes('표 복사'));
  }
});
test('selection fallback supports browsers without ClipboardItem', async () => {
  const h = harness({modern: false});
  await h.buttons[0].listeners.click();
  assert.equal(h.statuses[0].textContent, '복사 완료');
  assert.match(h.copies[0]['text/html'], /<table/);
  assert.ok(h.copies[0]['text/plain'].includes('\t'));
  assert.equal(h.events.size, 0);
});
test('denied modern clipboard falls back, and unavailable copy shows an error', async () => {
  const denied = harness({reject: true});
  await denied.buttons[0].listeners.click();
  assert.equal(denied.statuses[0].textContent, '복사 완료');
  const unavailable = harness({modern: false, fallbackOK: false});
  await unavailable.buttons[0].listeners.click();
  assert.match(unavailable.statuses[0].textContent, /복사할 수 없습니다/);
  assert.equal(unavailable.events.size, 0);
});

test('loading plot and both model diagrams expose downloadable PNGs', () => {
  assert.deepEqual(imageLinks.map(link => link.name), ['cfa2-factor-loadings.png', 'cfa2-model-unstandardized.png', 'cfa2-model-standardized.png']);
  for (const link of imageLinks) {
    assert.match(link.href, /^data:image\/png;base64,/);
    const png = Buffer.from(link.href.split(',')[1], 'base64');
    assert.equal(png.subarray(1, 4).toString(), 'PNG');
    assert.ok(png.length > 1000);
    assert.ok(png.readUInt32BE(16) >= 1000);
    assert.ok(png.readUInt32BE(20) >= 800);
  }
});
test('model and semPaths2 code copy preserves exact text including newlines', async () => {
  assert.deepEqual(Object.keys(codeNodes), ['model-code', 'diagram-code-unstandardized', 'diagram-code-standardized']);
  assert.match(codeNodes['diagram-code-unstandardized'].textContent, /semPaths2\([\s\S]*fit_7f/);
  assert.match(codeNodes['diagram-code-unstandardized'].textContent, /curve = 3/);
  for (const modern of [true, false]) {
    const h = harness({modern});
    for (let i = 0; i < h.codeButtons.length; i++) {
      await h.codeButtons[i].listeners.click();
      assert.equal(h.copies[i]['text/plain'], Object.values(codeNodes)[i].textContent);
      assert.equal(h.copies[i]['text/html'], undefined);
      assert.equal(h.codeStatuses[i].textContent, '코드 복사 완료');
    }
  }
});
test('code copying falls back after a clipboard rejection', async () => {
  const h = harness({reject: true});
  await h.codeButtons[0].listeners.click();
  assert.equal(h.copies[0]['text/plain'], codeNodes['model-code'].textContent);
  assert.equal(h.codeStatuses[0].textContent, '코드 복사 완료');
});

test('HTML and Markdown place both coefficient diagrams after all other results', () => {
  const lastOtherSection = html.indexOf('<h2>Variable order</h2>');
  const unstd = html.indexOf('download="cfa2-model-unstandardized.png"');
  const std = html.indexOf('download="cfa2-model-standardized.png"');
  assert.ok(lastOtherSection > 0 && lastOtherSection < unstd && unstd < std);
  assert.ok(markdown.indexOf('## Variable order') < markdown.indexOf('## Model diagram — Unstandardized'));
  assert.ok(markdown.indexOf('## Model diagram — Unstandardized') < markdown.indexOf('## Model diagram — Standardized'));
  assert.match(codeNodes['diagram-code-unstandardized'].textContent, /whatLabels = "est"/);
  assert.match(codeNodes['diagram-code-standardized'].textContent, /whatLabels = "std"/);
  assert.notEqual(imageLinks[1].href, imageLinks[2].href);
});
