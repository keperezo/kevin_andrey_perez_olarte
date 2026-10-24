/* =====================================================================
   scripts.js · Entorno de informe en R Markdown (pestañas + panel lateral)
   Todo se configura desde los params del Rmd (se leen de #kdd-config); no hace falta editar este archivo.
   Funciones:
     pestañas y sub-pestañas, acordeones, bloques de código (copiar / ocultar),
     panel lateral derecho (identidad, buscador, navegación, modo nocturno, modo PDF,
     visor de scraper.R y SQL, descarga de la base, copiar código, avance),
     lightbox de evidencias, checklist, atajos de teclado.
   JavaScript puro, sin dependencias. Compatible con DT y plotly (htmlwidgets).
   ===================================================================== */
(function () {
  'use strict';

  var CONFIG = {
    prefix: 'kdd:',                       // prefijo de localStorage (cámbielo por proyecto)
    logoSelector: '#unal-logo-src',       // <img> oculta que Pandoc incrusta (base64)
    logoFallbackSrc: 'UNAL_logo.png',
    sidebarMinWidth: 1180,                // por debajo, el panel es un cajón superpuesto
    codeDefault: 'shown'                  // 'shown' | 'hidden'
  };

  var root = document.documentElement;
  root.classList.add('js');

  /* ---------- utilidades ---------- */
  function store(k, v) {
    try { if (v === undefined) return localStorage.getItem(CONFIG.prefix + k); localStorage.setItem(CONFIG.prefix + k, v); }
    catch (e) { return null; }
  }
  function $(s, c) { return (c || document).querySelector(s); }
  function $$(s, c) { return Array.prototype.slice.call((c || document).querySelectorAll(s)); }
  function el(tag, cls, html) { var n = document.createElement(tag); if (cls) n.className = cls; if (html !== undefined) n.innerHTML = html; return n; }
  function esc(s) { return String(s).replace(/[&<>"']/g, function (c) { return ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]; }); }
  function ready(fn) { if (document.readyState !== 'loading') fn(); else document.addEventListener('DOMContentLoaded', fn); }
  function unprotect(s) { return s.split('<@@').join('<'); }          // deshace la protección de </script> hecha en R

  var ICONS = {
    home: '<path d="M3 11l9-8 9 8v10a1 1 0 0 1-1 1h-5v-7H9v7H4a1 1 0 0 1-1-1z"/>',
    globe: '<circle cx="12" cy="12" r="9"/><path d="M3 12h18M12 3c3 3.5 3 14.5 0 18M12 3c-3 3.5-3 14.5 0 18"/>',
    filter: '<path d="M3 4h18l-7 8v6l-4 2v-8z"/>',
    database: '<ellipse cx="12" cy="5" rx="8" ry="3"/><path d="M4 5v14c0 1.7 3.6 3 8 3s8-1.3 8-3V5M4 12c0 1.7 3.6 3 8 3s8-1.3 8-3"/>',
    chart: '<path d="M3 3v18h18"/><path d="M7 15l4-5 3 3 5-7"/>',
    check: '<rect x="3" y="3" width="18" height="18" rx="3"/><path d="M8 12l3 3 5-6"/>',
    sun: '<circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/>',
    moon: '<path d="M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z"/>',
    code: '<path d="M8 8l-5 4 5 4M16 8l5 4-5 4M14 4l-4 16"/>',
    file: '<path d="M14 3H6a1 1 0 0 0-1 1v16a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V8zM14 3v5h5"/>',
    chevron: '<path d="M9 6l6 6-6 6"/>',
    menu: '<path d="M4 6h16M4 12h16M4 18h16"/>',
    close: '<path d="M6 6l12 12M18 6L6 18"/>',
    print: '<path d="M6 9V3h12v6M6 18H4v-7h16v7h-2M7 14h10v7H7z"/>',
    keyboard: '<rect x="2" y="6" width="20" height="12" rx="2"/><path d="M6 10h.01M10 10h.01M14 10h.01M18 10h.01M7 14h10"/>',
    up: '<path d="M12 19V5M5 12l7-7 7 7"/>',
    down: '<path d="M12 5v14M5 12l7 7 7-7"/>',
    copy: '<rect x="9" y="9" width="11" height="11" rx="2"/><path d="M5 15V6a2 2 0 0 1 2-2h9"/>',
    download: '<path d="M12 4v11M7 11l5 5 5-5M5 20h14"/>',
    layers: '<path d="M12 3l9 5-9 5-9-5z"/><path d="M3 13l9 5 9-5"/>',
    link: '<path d="M10 14a4 4 0 0 0 5.7 0l3-3a4 4 0 0 0-5.7-5.7l-1 1M14 10a4 4 0 0 0-5.7 0l-3 3a4 4 0 0 0 5.7 5.7l1-1"/>',
    width: '<path d="M3 12h18M7 8l-4 4 4 4M17 8l4 4-4 4"/>',
    list: '<path d="M8 6h13M8 12h13M8 18h13M3 6h.01M3 12h.01M3 18h.01"/>',
    flag: '<path d="M5 21V4M5 4h11l-2 4 2 4H5"/>',
    info: '<circle cx="12" cy="12" r="9"/><path d="M12 11v5M12 8h.01"/>',
    search: '<circle cx="11" cy="11" r="7"/><path d="M20 20l-4-4"/>',
    external: '<path d="M14 4h6v6M20 4l-9 9M18 14v5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V7a1 1 0 0 1 1-1h5"/>',
    pdf: '<path d="M14 3H6a1 1 0 0 0-1 1v16a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V8zM14 3v5h5"/><path d="M8 15h8M8 18h5"/>',
    eye: '<path d="M2 12s4-7 10-7 10 7 10 7-4 7-10 7S2 12 2 12z"/><circle cx="12" cy="12" r="3"/>'
  };
  function icon(n) {
    return '<svg class="ico" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' + (ICONS[n] || ICONS.file) + '</svg>';
  }

  /* ---------- configuración inyectada desde R ---------- */
  var cfg = {};
  function readConfig() {
    var n = $('#kdd-config');
    if (!n) return;
    try { cfg = JSON.parse(n.textContent); } catch (e) { cfg = {}; }
  }

  /* ---------- toasts y portapapeles ---------- */
  var toastBox;
  function toast(msg) {
    if (!toastBox) { toastBox = el('div', 'toasts'); toastBox.setAttribute('role', 'status'); document.body.appendChild(toastBox); }
    var t = el('div', 'toast', esc(msg)); toastBox.appendChild(t); setTimeout(function () { t.remove(); }, 2300);
  }
  function copyText(text, okMsg) {
    function fallback() {
      var ta = el('textarea'); ta.value = text; ta.style.position = 'fixed'; ta.style.opacity = '0';
      document.body.appendChild(ta); ta.select();
      try { document.execCommand('copy'); toast(okMsg || 'Copiado'); } catch (e) { toast('No se pudo copiar'); }
      ta.remove();
    }
    if (navigator.clipboard && window.isSecureContext !== false) navigator.clipboard.writeText(text).then(function () { toast(okMsg || 'Copiado'); }, fallback);
    else fallback();
  }
  function saveBlob(blob, name) {
    var a = el('a'); a.href = URL.createObjectURL(blob); a.download = name;
    document.body.appendChild(a); a.click(); a.remove(); setTimeout(function () { URL.revokeObjectURL(a.href); }, 800);
  }

  /* ---------- 1. TEMA ---------- */
  var theme = store('theme') || (window.matchMedia && matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
  root.setAttribute('data-theme', theme);
  function setTheme(t) {
    theme = t; store('theme', t);
    if (!document.body.classList.contains('pdf-mode')) root.setAttribute('data-theme', t);
    var sw = $('#sw-theme'); if (sw) sw.setAttribute('aria-checked', String(t === 'dark'));
  }

  /* ---------- 2. PESTAÑAS ---------- */
  var tabs = [], activeTab = null;
  function statusOf(t) { return t.getAttribute('data-status') || 'pendiente'; }
  var STATUS_LABEL = { pendiente: 'Pendiente', progreso: 'En progreso', listo: 'Listo' };

  function buildTabs() {
    tabs = $$('.kdd-tab');
    if (!tabs.length) return;
    var nav = el('div', 'kdd-tabs'); nav.setAttribute('role', 'tablist'); nav.setAttribute('aria-label', 'Secciones del informe');
    tabs.forEach(function (t, i) {
      if (!t.id) t.id = 'tab-' + (i + 1);
      var b = el('button', '', icon(t.getAttribute('data-icon') || 'file') + '<span class="tab-n">' + (i + 1) + '</span><span class="label">' + esc(t.getAttribute('data-title') || t.id) + '</span><span class="dot" data-status="' + statusOf(t) + '" title="' + STATUS_LABEL[statusOf(t)] + '"></span>');
      b.type = 'button'; b.setAttribute('role', 'tab'); b.setAttribute('data-target', t.id);
      b.addEventListener('click', function () { activateTab(t.id, { scroll: true }); });
      nav.appendChild(b); t.setAttribute('role', 'tabpanel');
    });
    tabs[0].parentNode.insertBefore(nav, tabs[0]);
  }

  function activateTab(id, opt) {
    opt = opt || {};
    var target = document.getElementById(id);
    if (!target || !target.classList.contains('kdd-tab')) return;
    tabs.forEach(function (t) { t.classList.toggle('is-active', t === target); });
    $$('.kdd-tabs button').forEach(function (b) {
      var on = b.getAttribute('data-target') === id; b.setAttribute('aria-selected', String(on));
      if (on) { var bar = b.parentNode; if (b.offsetLeft < bar.scrollLeft || b.offsetLeft + b.offsetWidth > bar.scrollLeft + bar.clientWidth) bar.scrollLeft = Math.max(0, b.offsetLeft - 24); }
    });
    activeTab = target; store('tab', id);
    if (opt.updateHash !== false && history.replaceState) history.replaceState(null, '', '#' + id);
    updateSidebarNav();
    if (opt.scroll) { var nav = $('.kdd-tabs'); if (nav && window.pageYOffset > nav.offsetTop) window.scrollTo({ top: Math.max(0, nav.offsetTop - 8), behavior: 'auto' }); }
    if (!opt.keepSearch) refreshSearch();
    setTimeout(fixWidgets, 60);
  }

  function buildSubtabs() {
    $$('.kdd-subtabs').forEach(function (group, gi) {
      var subs = $$(':scope > .kdd-subtab', group);
      if (!subs.length) return;
      var nav = el('div', 'kdd-subtabs-nav'); nav.setAttribute('role', 'tablist');
      subs.forEach(function (s, i) {
        if (!s.id) s.id = 'sub-' + gi + '-' + i;
        var b = el('button', '', esc(s.getAttribute('data-title') || s.id)); b.type = 'button'; b.setAttribute('role', 'tab'); b.setAttribute('data-target', s.id);
        b.addEventListener('click', function () { activateSub(s.id); }); nav.appendChild(b);
      });
      group.insertBefore(nav, subs[0]);
      var saved = store('sub:' + (group.id || gi));
      activateSub((subs.filter(function (s) { return s.id === saved; })[0] || subs[0]).id, true);
    });
  }
  function activateSub(id, silent) {
    var target = document.getElementById(id); if (!target) return;
    var group = target.parentNode;
    $$(':scope > .kdd-subtab', group).forEach(function (s) { s.classList.toggle('is-active', s === target); });
    $$(':scope > .kdd-subtabs-nav button', group).forEach(function (b) { b.setAttribute('aria-selected', String(b.getAttribute('data-target') === id)); });
    store('sub:' + (group.id || ''), id);
    if (!silent) { buildToc(); refreshSearch(); setTimeout(fixWidgets, 60); }
  }

  function fixWidgets() {
    try { if (window.jQuery && jQuery.fn.dataTable && jQuery.fn.dataTable.tables) jQuery.fn.dataTable.tables({ visible: true, api: true }).columns.adjust(); } catch (e) {}
    try { if (window.Plotly) $$('.plotly.html-widget').forEach(function (p) { if (p.offsetParent !== null) { try { window.Plotly.Plots.resize(p); } catch (e) {} } }); } catch (e) {}
    try { window.dispatchEvent(new Event('resize')); } catch (e) {}
  }

  function reveal(node, doScroll) {
    var chain = [], n = node;
    while (n && n !== document.body) { chain.unshift(n); n = n.parentNode; }
    chain.forEach(function (x) {
      if (!x.classList) return;
      if (x.classList.contains('kdd-tab')) activateTab(x.id, { updateHash: false, keepSearch: true });
      if (x.classList.contains('kdd-subtab')) activateSub(x.id, true);
      if (x.classList.contains('acc') && !x.classList.contains('is-open')) setAcc(x, true);
    });
    if (doScroll) {
      if (node.classList && node.classList.contains('kdd-tab')) { var nav = $('.kdd-tabs'); if (nav) window.scrollTo({ top: Math.max(0, nav.offsetTop - 8), behavior: 'auto' }); }
      else setTimeout(function () { node.scrollIntoView({ block: 'start' }); }, 40);
    }
  }

  /* ---------- 3. ACORDEONES ---------- */
  function setAcc(acc, open) {
    acc.classList.toggle('is-open', open);
    var h = $(':scope > .acc-head', acc); if (h) h.setAttribute('aria-expanded', String(open));
  }
  function buildAccordions() {
    $$('.acc').forEach(function (acc) {
      var head = acc.firstElementChild;
      if (!head || !/^H[1-6]$/.test(head.tagName)) return;
      if (!acc.id && head.id) acc.id = head.id;
      var btn = el('button', 'acc-head', icon('chevron').replace('class="ico"', 'class="ico chev"') + '<span>' + head.innerHTML + '</span>'); btn.type = 'button';
      var body = el('div', 'acc-body'), inner = el('div', 'acc-inner');
      while (head.nextSibling) inner.appendChild(head.nextSibling);
      body.appendChild(inner); acc.removeChild(head); acc.appendChild(btn); acc.appendChild(body);
      setAcc(acc, !acc.classList.contains('closed'));
      btn.addEventListener('click', function () { setAcc(acc, !acc.classList.contains('is-open')); setTimeout(fixWidgets, 300); });
    });
  }
  function toggleAllAcc() {
    var all = $$('.acc'); if (!all.length) return;
    var anyClosed = all.some(function (a) { return !a.classList.contains('is-open'); });
    all.forEach(function (a) { setAcc(a, anyClosed); });
    toast(anyClosed ? 'Secciones expandidas' : 'Secciones contraídas'); setTimeout(fixWidgets, 320);
  }

  /* ---------- 4. BLOQUES DE CÓDIGO ---------- */
  var KNOWN = ['r', 'sql', 'python', 'bash', 'sh', 'yaml', 'html', 'css', 'javascript', 'js', 'json', 'markdown', 'cpp', 'text'];
  var LABEL = { r: 'R', sql: 'SQL', python: 'Python', bash: 'Bash', sh: 'Shell', yaml: 'YAML', html: 'HTML', css: 'CSS', javascript: 'JS', js: 'JS', json: 'JSON', markdown: 'Markdown', cpp: 'C++', text: 'Texto' };
  function langOf(pre) {
    var c = $('code', pre), cls = (pre.className + ' ' + (c ? c.className : '')).toLowerCase().split(/\s+/);
    for (var i = 0; i < cls.length; i++) if (KNOWN.indexOf(cls[i]) > -1) return cls[i];
    return 'code';
  }
  function isCodePre(pre) { return !pre.classList.contains('mermaid') && (pre.classList.contains('sourceCode') || KNOWN.some(function (k) { return pre.classList.contains(k); })); }
  var codeShown = true;

  function buildCodeBlock(host, lang) {
    var block = el('div', 'code-block'), head = el('div', 'code-head');
    var chip = el('span', 'lang-chip', esc(LABEL[lang] || lang.toUpperCase())); chip.setAttribute('data-lang', lang); head.appendChild(chip);
    var bCopy = el('button', '', icon('copy') + '<span>Copiar</span>'); bCopy.type = 'button';
    var bFold = el('button', 'fold', icon('code') + '<span>Ocultar</span>'); bFold.type = 'button';
    head.appendChild(bCopy); head.appendChild(bFold);
    var body = el('div', 'code-body'), inner = el('div', 'code-inner'); inner.appendChild(host); body.appendChild(inner);
    block.appendChild(head); block.appendChild(body);
    bCopy.addEventListener('click', function () {
      copyText(($('pre', host) || host).textContent.replace(/\n$/, ''), 'Código copiado');
      bCopy.classList.add('done'); bCopy.lastChild.textContent = 'Copiado';
      setTimeout(function () { bCopy.classList.remove('done'); bCopy.lastChild.textContent = 'Copiar'; }, 1400);
    });
    bFold.addEventListener('click', function () { setBlockCollapsed(block, !block.classList.contains('is-collapsed')); });
    return block;
  }
  function setBlockCollapsed(block, collapsed) {
    block.classList.toggle('is-collapsed', collapsed);
    var f = $('.fold span', block); if (f) f.textContent = collapsed ? 'Mostrar' : 'Ocultar';
  }
  function buildCode() {
    $$('pre').forEach(function (pre) {
      if (!isCodePre(pre) || pre.closest('.code-block')) return;
      var host = pre.parentNode && pre.parentNode.classList.contains('sourceCode') ? pre.parentNode : pre;
      var marker = document.createComment('code'), parent = host.parentNode;
      parent.insertBefore(marker, host);
      var block = buildCodeBlock(host, langOf(pre));
      parent.insertBefore(block, marker); parent.removeChild(marker);
    });
  }
  function setAllCode(show, silent, temp) {
    codeShown = show; if (!temp) store('code', show ? 'shown' : 'hidden');
    $$('.code-block').forEach(function (b) { if (!b.closest('.kdd-modal')) setBlockCollapsed(b, !show); });
    var sw = $('#sw-code'); if (sw) sw.setAttribute('aria-checked', String(show));
    var sp = $('#pdf-code'); if (sp) sp.setAttribute('aria-checked', String(show));
    if (!silent) toast(show ? 'Código visible' : 'Código oculto');
  }
  function wrapTables() {
    $$('.main-container table').forEach(function (t) {
      if (t.closest('.dataTables_wrapper, .datatables, .table-wrap, .kdd-modal, .schema-card')) return;
      var w = el('div', 'table-wrap'); t.parentNode.insertBefore(w, t); w.appendChild(t);
    });
  }
  function copyTabCode() {
    var parts = $$('.code-block pre', activeTab || document).map(function (p) { return p.textContent.replace(/\n$/, ''); });
    if (!parts.length) { toast('Esta pestaña no tiene bloques de código'); return; }
    copyText(parts.join('\n\n'), parts.length + ' bloques copiados');
  }
  function copyAllSql() {
    var parts = $$('pre.sql, pre.sourceCode.sql').filter(function (p) { return !p.closest('.kdd-modal'); }).map(function (p) { return p.textContent.replace(/\n$/, ''); });
    if (!parts.length) { toast('No hay bloques SQL en el documento'); return; }
    copyText(parts.join('\n\n'), parts.length + ' consultas SQL copiadas');
  }

  /* ---------- 5. MODALES ---------- */
  var modal, modalTitle, modalBody, modalActions;
  function ensureModal() {
    if (modal) return;
    modal = el('div', 'kdd-modal'); modal.setAttribute('role', 'dialog'); modal.setAttribute('aria-modal', 'true');
    modal.innerHTML = '<div class="kdd-modal-card"><div class="kdd-modal-head"><h3></h3><span class="m-actions"></span><button type="button" class="btn icon" aria-label="Cerrar">' + icon('close') + '</button></div><div class="kdd-modal-body"></div></div>';
    document.body.appendChild(modal);
    modalTitle = $('h3', modal); modalBody = $('.kdd-modal-body', modal); modalActions = $('.m-actions', modal);
    $('.btn.icon', modal).addEventListener('click', closeModal);
    modal.addEventListener('click', function (e) { if (e.target === modal) closeModal(); });
  }
  function openModal(title, node, actions, wide) {
    ensureModal();
    modalTitle.textContent = title; modalBody.innerHTML = ''; modalActions.innerHTML = '';
    modalBody.appendChild(node); (actions || []).forEach(function (a) { modalActions.appendChild(a); });
    $('.kdd-modal-card', modal).classList.toggle('wide', !!wide);
    modal.classList.add('show'); var c = $('.btn.icon', modal); if (c) c.focus();
  }
  function closeModal() { if (modal) modal.classList.remove('show'); }
  function actionBtn(ico, label, fn, cls) { var b = el('button', 'btn' + (cls ? ' ' + cls : ''), icon(ico) + esc(label)); b.type = 'button'; b.addEventListener('click', fn); return b; }

  /* ---- resaltado de sintaxis ligero para el visor (R y SQL) ---- */
  var KW = {
    r: /^(function|if|else|for|while|repeat|return|break|next|in|TRUE|FALSE|NULL|NA|NA_character_|NA_integer_|NA_real_|Inf|NaN|library|require|source|tryCatch|stop|warning|message|local)$/,
    sql: /^(SELECT|FROM|WHERE|GROUP|BY|ORDER|HAVING|JOIN|LEFT|RIGHT|INNER|OUTER|CROSS|ON|AS|AND|OR|NOT|IN|IS|NULL|LIKE|LIMIT|OFFSET|UNION|ALL|DISTINCT|CASE|WHEN|THEN|ELSE|END|WITH|OVER|PARTITION|CREATE|TABLE|VIEW|INDEX|TEMP|TEMPORARY|INSERT|INTO|VALUES|UPDATE|SET|DELETE|DROP|IF|EXISTS|PRIMARY|KEY|UNIQUE|REFERENCES|CHECK|DEFAULT|BETWEEN|ASC|DESC|PRAGMA|REPLACE|IGNORE|INTEGER|TEXT|REAL|BLOB)$/i
  };
  function highlight(code, lang) {
    var re = lang === 'sql'
      ? /(--[^\n]*)|('(?:[^']|'')*')|(\b\d+(?:\.\d+)?\b)|([A-Za-z_][A-Za-z0-9_]*)/g
      : /(#[^\n]*)|("(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')|(\b\d+(?:\.\d+)?L?\b)|([A-Za-z_.][A-Za-z0-9_.]*)/g;
    var out = '', last = 0, m;
    function emit(cls, text) {
      var t = esc(text);
      out += cls ? t.split('\n').map(function (p) { return p ? '<span class="' + cls + '">' + p + '</span>' : ''; }).join('\n') : t;
    }
    var kw = KW[lang] || /$^/;
    while ((m = re.exec(code))) {
      if (m.index > last) emit('', code.slice(last, m.index));
      if (m[1]) emit('tk-com', m[0]);
      else if (m[2]) emit('tk-str', m[0]);
      else if (m[3]) emit('tk-num', m[0]);
      else if (kw.test(m[4])) emit('tk-kw', m[0]);
      else if (/^\s*\(/.test(code.slice(re.lastIndex, re.lastIndex + 8))) emit('tk-fn', m[0]);
      else emit('', m[0]);
      last = re.lastIndex;
    }
    if (last < code.length) emit('', code.slice(last));
    return out;
  }
  function linesHtml(code, lang) {
    return highlight(code, lang).split('\n').map(function (l) { return '<span class="ln">' + (l || ' ') + '</span>'; }).join('');
  }

  /* ---- visor de archivos (scraper.R, consultas.sql, ...) ---- */
  function sources() {
    return $$('.kdd-source').map(function (s) {
      return { name: s.getAttribute('data-name'), lang: s.getAttribute('data-lang') || 'text', code: unprotect(s.textContent).replace(/^\n/, '') };
    });
  }
  var viewerIdx = 0;
  function openViewer(name) {
    var files = sources();
    if (!files.length) { toast('No hay archivos de código para mostrar (revise archivos_codigo en el Rmd)'); return; }
    if (name) { files.forEach(function (f, i) { if (f.name === name) viewerIdx = i; }); }
    viewerIdx = Math.min(viewerIdx, files.length - 1);
    var wrap = el('div', 'viewer'), tabsBar = el('div', 'viewer-tabs'), host = el('div', 'code-block tall viewer-code');
    var cBtn, dBtn, nBtn;
    function render() {
      var f = files[viewerIdx];
      $$('button', tabsBar).forEach(function (b, i) { b.setAttribute('aria-selected', String(i === viewerIdx)); });
      host.innerHTML = '<div class="code-head"><span class="code-title">' + esc(f.name) + '</span><span class="lang-chip" data-lang="' + esc(f.lang) + '">' + esc(LABEL[f.lang] || f.lang) + '</span><span class="code-title">' + f.code.split('\n').length + ' líneas</span></div><div class="code-body"><div class="code-inner"><pre class="viewer-pre"><code>' + linesHtml(f.code, f.lang) + '</code></pre></div></div>';
      modalTitle.textContent = 'Visor de código: ' + f.name;
    }
    files.forEach(function (f, i) {
      var b = el('button', '', icon(f.lang === 'sql' ? 'database' : 'file') + esc(f.name)); b.type = 'button';
      b.addEventListener('click', function () { viewerIdx = i; render(); }); tabsBar.appendChild(b);
    });
    wrap.appendChild(tabsBar); wrap.appendChild(host);
    cBtn = actionBtn('copy', 'Copiar', function () { copyText(files[viewerIdx].code, files[viewerIdx].name + ' copiado'); });
    dBtn = actionBtn('download', 'Descargar', function () { saveBlob(new Blob([files[viewerIdx].code], { type: 'text/plain;charset=utf-8' }), files[viewerIdx].name); });
    nBtn = actionBtn('external', 'Nueva pestaña', function () { openInBrowserTab(files[viewerIdx]); });
    openModal('Visor de código', wrap, [cBtn, nBtn, dBtn], true);
    render();
  }
  function openInBrowserTab(f) {
    var css = 'body{margin:0;background:#f4f6fa;color:#10233f;font:14px/1.55 ui-monospace,Consolas,monospace}h1{font:600 15px system-ui;margin:0;padding:12px 18px;background:#14335f;color:#fff}pre{margin:0;padding:16px 18px;counter-reset:l;overflow:auto}.ln{display:block;white-space:pre}.ln::before{counter-increment:l;content:counter(l);display:inline-block;width:3.2em;margin-right:1em;text-align:right;color:#8a97a8}.tk-kw{color:#1f4fb8;font-weight:600}.tk-fn{color:#7b3fa0}.tk-str{color:#0a7357}.tk-num{color:#b0501a}.tk-com{color:#75849a;font-style:italic}';
    var html = '<!doctype html><html lang="es"><meta charset="utf-8"><title>' + esc(f.name) + '</title><style>' + css + '</style><h1>' + esc(f.name) + '</h1><pre><code>' + linesHtml(f.code, f.lang) + '</code></pre></html>';
    var url = URL.createObjectURL(new Blob([html], { type: 'text/html;charset=utf-8' }));
    var w = window.open(url, '_blank'); if (!w) toast('El navegador bloqueó la nueva pestaña');
  }

  var SHORTCUTS = [
    ['1 – 9', 'Ir a la sección número 1 – 9'], ['/', 'Ir al buscador'], ['D', 'Modo nocturno / diurno'], ['C', 'Mostrar u ocultar todo el código'],
    ['V', 'Abrir el visor de código'], ['P', 'Activar o salir del modo PDF'], ['S', 'Abrir o cerrar el panel lateral'],
    ['E', 'Expandir o contraer secciones'], ['?', 'Ver esta ayuda'], ['Esc', 'Cerrar ventanas y vista PDF']
  ];
  function openHelp() {
    var dl = el('dl', 'shortcut-list');
    SHORTCUTS.forEach(function (s) { dl.appendChild(el('dt', '', '<kbd>' + esc(s[0]) + '</kbd>')); dl.appendChild(el('dd', '', esc(s[1]))); });
    openModal('Atajos de teclado', dl);
  }

  /* lightbox de evidencias */
  var lb;
  function initLightbox() {
    lb = el('div', 'lightbox'); lb.innerHTML = '<button type="button" class="btn" aria-label="Cerrar">' + icon('close') + 'Cerrar</button><img alt=""><p></p>';
    document.body.appendChild(lb);
    lb.addEventListener('click', function () { lb.classList.remove('show'); });
    document.addEventListener('click', function (e) {
      var img = e.target.closest && e.target.closest('.evidence-grid img'); if (!img) return;
      $('img', lb).src = img.src; $('img', lb).alt = img.alt;
      var cap = img.parentNode.querySelector('figcaption'); $('p', lb).textContent = cap ? cap.textContent : img.alt;
      lb.classList.add('show');
    });
  }

  /* ---------- 6. BASE DE DATOS ---------- */
  function downloadDb() {
    var d = cfg.db || {}, node = $('#kdd-db-b64');
    if (node) {
      try {
        var bin = atob(node.textContent.trim()), arr = new Uint8Array(bin.length);
        for (var i = 0; i < bin.length; i++) arr[i] = bin.charCodeAt(i);
        saveBlob(new Blob([arr], { type: 'application/x-sqlite3' }), d.name || 'base_de_datos.sqlite'); toast('Descargando ' + (d.name || 'la base de datos'));
      } catch (e) { toast('No se pudo decodificar la base incrustada'); }
    } else if (d.href) {
      var a = el('a'); a.href = d.href; a.download = d.name || ''; document.body.appendChild(a); a.click(); a.remove();
    } else toast('No se encontró la base de datos (revise db_path en el Rmd)');
  }

  /* ---------- 7. BUSCADOR ---------- */
  var S = { hits: [], i: -1, q: '' };
  function clearMarks() {
    $$('mark.kdd-hit').forEach(function (m) { var p = m.parentNode; if (!p) return; p.replaceChild(document.createTextNode(m.textContent), m); p.normalize(); });
    S.hits = []; S.i = -1;
  }
  function refreshSearch() { if (S.q && S.q.length >= 2 && !($('#sb-all') && $('#sb-all').checked)) runSearch(S.q, true); else if (!S.q) clearMarks(); }
  function runSearch(q, keepPos) {
    clearMarks(); S.q = q.trim();
    var all = $('#sb-all') && $('#sb-all').checked, scope = all ? $('.main-container') : activeTab;
    if (S.q.length < 2 || !scope) { searchUi(); return; }
    var re = new RegExp(S.q.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'gi'), nodes = [];
    var walker = document.createTreeWalker(scope, NodeFilter.SHOW_TEXT, { acceptNode: function (n) {
      var p = n.parentNode;
      if (!p || !n.nodeValue.trim() || /^(SCRIPT|STYLE|NOSCRIPT|TEXTAREA|SELECT|OPTION)$/.test(p.tagName)) return NodeFilter.FILTER_REJECT;
      if (p.closest('.kdd-tabs, .kdd-subtabs-nav, .kdd-source')) return NodeFilter.FILTER_REJECT;
      return NodeFilter.FILTER_ACCEPT;
    } });
    while (walker.nextNode()) nodes.push(walker.currentNode);
    nodes.forEach(function (n) {
      var txt = n.nodeValue, last = 0, m, frag = document.createDocumentFragment(), found = false;
      re.lastIndex = 0;
      while ((m = re.exec(txt))) {
        found = true;
        if (m.index > last) frag.appendChild(document.createTextNode(txt.slice(last, m.index)));
        var mk = el('mark', 'kdd-hit'); mk.textContent = m[0]; frag.appendChild(mk); S.hits.push(mk);
        last = m.index + m[0].length; if (!m[0].length) re.lastIndex++;
      }
      if (!found) return;
      if (last < txt.length) frag.appendChild(document.createTextNode(txt.slice(last)));
      n.parentNode.replaceChild(frag, n);
    });
    if (S.hits.length) goHit(0, !keepPos); else searchUi();
  }
  function goHit(i, scroll) {
    if (!S.hits.length) return;
    S.i = (i + S.hits.length) % S.hits.length;
    $$('mark.kdd-current').forEach(function (m) { m.classList.remove('kdd-current'); });
    var m = S.hits[S.i]; m.classList.add('kdd-current'); reveal(m, false);
    if (scroll !== false) m.scrollIntoView({ block: 'center' });
    searchUi();
  }
  function searchUi() {
    var c = $('#sb-count'), r = $('#sb-results'); if (!c) return;
    c.textContent = S.q.length < 2 ? '' : (S.hits.length ? (S.i + 1) + ' de ' + S.hits.length : 'Sin resultados');
    r.innerHTML = '';
    if (!($('#sb-all') && $('#sb-all').checked) || !S.hits.length) return;
    var counts = {};
    S.hits.forEach(function (m, idx) { var t = m.closest('.kdd-tab'); if (!t) return; if (!counts[t.id]) counts[t.id] = { n: 0, first: idx }; counts[t.id].n++; });
    tabs.forEach(function (t) {
      if (!counts[t.id]) return;
      var b = el('button', 'chip', esc(t.getAttribute('data-title') || t.id) + ' <b>' + counts[t.id].n + '</b>'); b.type = 'button';
      b.addEventListener('click', function () { goHit(counts[t.id].first); }); r.appendChild(b);
    });
  }

  /* ---------- 8. MODO PDF ---------- */
  var pdfPrev = null, pdfBar;
  function setPdf(on) {
    var body = document.body;
    if (on === body.classList.contains('pdf-mode')) return;
    if (on) {
      pdfPrev = { code: codeShown }; clearMarks();
      body.classList.add('pdf-mode'); root.setAttribute('data-theme', 'light');
      pdfBar = el('div', 'pdf-bar');
      pdfBar.innerHTML = '<span class="pdf-title">' + icon('pdf') + 'Vista PDF</span><span class="sb-row" style="padding:0"><span>Incluir código</span><button type="button" class="switch" role="switch" id="pdf-code" aria-checked="' + codeShown + '"></button></span>';
      var bp = actionBtn('print', 'Imprimir / Guardar PDF', function () { window.print(); }, 'primary'), bx = actionBtn('close', 'Salir', function () { setPdf(false); });
      pdfBar.appendChild(bp); pdfBar.appendChild(bx); body.appendChild(pdfBar);
      $('#pdf-code').addEventListener('click', function () { setAllCode(!codeShown, true, true); });
      window.scrollTo(0, 0); toast('Vista PDF: use Imprimir y elija «Guardar como PDF»');
    } else {
      body.classList.remove('pdf-mode'); root.setAttribute('data-theme', theme);
      if (pdfBar) { pdfBar.remove(); pdfBar = null; }
      if (pdfPrev) setAllCode(pdfPrev.code, true, true);
    }
    var sw = $('#btn-pdf'); if (sw) sw.classList.toggle('on', on);
    setTimeout(fixWidgets, 120);
  }

  /* ---------- 9. PANEL LATERAL ---------- */
  var sidebar, sbOpen = false;
  function logoNode() {
    var src = ($(CONFIG.logoSelector) || {}).src || CONFIG.logoFallbackSrc;
    var img = el('img', 'sidebar-logo-img mb-2'); img.alt = 'Logo ' + (cfg.universidad || ''); img.src = src;
    img.addEventListener('error', function () { var m = el('div', 'monogram', 'LOGO'); if (img.parentNode) img.parentNode.replaceChild(m, img); });
    return img;
  }
  function switchRow(id, ico, label, on) {
    return '<div class="sb-row"><span>' + icon(ico) + esc(label) + '</span><button type="button" class="switch" role="switch" id="' + id + '" aria-label="' + esc(label) + '" aria-checked="' + on + '"></button></div>';
  }
  function metaRow(k, v) { return v ? '<div class="sb-meta-row"><span>' + esc(k) + '</span><b>' + esc(v) + '</b></div>' : ''; }

  function buildSidebar() {
    sidebar = el('aside', 'kdd-sidebar right-sidebar'); sidebar.setAttribute('aria-label', 'Panel de control');
    var navItems = tabs.map(function (t, i) {
      return '<li data-tab="' + t.id + '"><a href="#' + t.id + '">' + icon(t.getAttribute('data-icon') || 'file') + '<span>' + esc(t.getAttribute('data-title') || t.id) + '</span><i class="dot" data-status="' + statusOf(t) + '"></i>' + (i < 9 ? '<kbd>' + (i + 1) + '</kbd>' : '') + '</a></li>';
    }).join('');
    var files = sources();
    var fileBtns = files.map(function (f, i) {
      return '<button type="button" class="btn ' + (i === 0 ? 'primary ' : '') + 'block" data-file="' + esc(f.name) + '">' + icon(f.lang === 'sql' ? 'database' : 'file') + 'Ver ' + esc(f.name) + '</button>';
    }).join('') || '<button type="button" class="btn primary block" data-file="">' + icon('file') + 'Ver código</button>';
    var db = cfg.db || {};
    var dbLabel = 'Descargar base de datos' + (db.size_mb ? ' (' + db.size_mb + ' MB)' : '');

    sidebar.innerHTML =
      '<div class="sb-top"><span class="sb-title">Panel de control</span><button type="button" class="btn icon" id="sb-close" aria-label="Cerrar panel">' + icon('close') + '</button></div>' +
      '<div class="sb-scroll">' +
        '<div class="sidebar-logo text-center sb-logo-slot"><h6 class="sidebar-title mt-1"></h6><p class="sidebar-subtitle text-muted mb-0"></p></div>' +
        '<div class="sb-meta">' + metaRow('Materia', cfg.materia) + metaRow('Taller', cfg.taller) + metaRow('Informe', cfg.titulo) + metaRow('Autor', cfg.autor) + '</div>' +
        '<div class="sb-group"><h5>Secciones</h5><ul class="sb-nav">' + navItems + '</ul></div>' +
        '<div class="sb-group"><h5>Buscar</h5>' +
          '<div class="sb-search">' + icon('search') + '<input type="search" id="sb-q" placeholder="Buscar en la página" aria-label="Buscar en la página" autocomplete="off">' +
          '<button type="button" class="btn icon" id="sb-prev" aria-label="Anterior">' + icon('up') + '</button><button type="button" class="btn icon" id="sb-next" aria-label="Siguiente">' + icon('down') + '</button></div>' +
          '<div class="sb-search-meta"><span id="sb-count"></span><label><input type="checkbox" id="sb-all"> Todo el documento</label></div><div id="sb-results"></div>' +
          '<p class="sb-hint">Las tablas interactivas tienen su propio buscador y filtros por columna.</p>' +
        '</div>' +
        '<div class="sb-group"><h5>Vista</h5>' +
          switchRow('sw-theme', 'moon', 'Modo nocturno', theme === 'dark') +
          switchRow('sw-code', 'code', 'Mostrar todo el código', codeShown) +
          switchRow('sw-wide', 'width', 'Ancho amplio', store('wide') === '1') +
          '<div class="sb-row"><span>' + icon('list') + 'Tamaño de texto</span><span class="btn-pair" style="grid-template-columns:auto auto"><button type="button" class="btn icon" id="fs-minus" aria-label="Reducir texto">A−</button><button type="button" class="btn icon" id="fs-plus" aria-label="Aumentar texto">A+</button></span></div>' +
          '<button type="button" class="btn block" id="btn-pdf">' + icon('pdf') + 'Modo PDF</button>' +
        '</div>' +
        '<div class="sb-group"><h5>Código y datos</h5>' + fileBtns +
          '<button type="button" class="btn block" id="btn-copy-tab">' + icon('copy') + 'Copiar código de esta sección</button>' +
          '<button type="button" class="btn block" id="btn-copy-sql">' + icon('copy') + 'Copiar todo el SQL</button>' +
          '<button type="button" class="btn block" id="btn-db">' + icon('download') + esc(dbLabel) + '</button>' +
        '</div>' +
        '<div class="sb-group"><h5>Herramientas</h5>' +
          '<button type="button" class="btn block" id="btn-acc">' + icon('layers') + 'Expandir / contraer secciones</button>' +
          '<button type="button" class="btn block" id="btn-link">' + icon('link') + 'Copiar enlace de esta sección</button>' +
          '<button type="button" class="btn block" id="btn-help">' + icon('keyboard') + 'Atajos de teclado <kbd style="margin-left:auto">?</kbd></button>' +
        '</div>' +
        '<div class="sb-group"><h5>Avance del informe</h5><div class="check-progress" id="sb-progress"><div class="bar"><i></i></div><b></b></div><p class="sb-hint">Cambie <code>data-status</code> de cada sección a <code>pendiente</code>, <code>progreso</code> o <code>listo</code>.</p></div>' +
      '</div>';
    document.body.appendChild(sidebar);

    var slot = $('.sb-logo-slot', sidebar); slot.insertBefore(logoNode(), slot.firstChild);
    $('.sidebar-title', sidebar).textContent = cfg.universidad || 'Universidad';
    $('.sidebar-subtitle', sidebar).textContent = cfg.dependencia || '';

    var fab = el('button', 'sb-fab', icon('menu')); fab.type = 'button'; fab.setAttribute('aria-label', 'Abrir panel de control');
    var backdrop = el('div', 'sb-backdrop');
    var top = el('button', 'btn icon to-top', icon('up')); top.type = 'button'; top.setAttribute('aria-label', 'Volver arriba');
    var prog = el('div', 'read-progress');
    [fab, backdrop, top, prog].forEach(function (n) { document.body.appendChild(n); });

    fab.addEventListener('click', function () { setSidebar(true); });
    backdrop.addEventListener('click', function () { setSidebar(false); });
    $('#sb-close').addEventListener('click', function () { setSidebar(false); });
    top.addEventListener('click', function () { window.scrollTo({ top: 0, behavior: 'smooth' }); });

    $('#sw-theme').addEventListener('click', function () { setTheme(theme === 'dark' ? 'light' : 'dark'); });
    $('#sw-wide').addEventListener('click', function () { setWide(store('wide') !== '1'); });
    $('#sw-code').addEventListener('click', function () { setAllCode(!codeShown); });
    $('#fs-minus').addEventListener('click', function () { setFont(-1); });
    $('#fs-plus').addEventListener('click', function () { setFont(1); });
    $('#btn-pdf').addEventListener('click', function () { setPdf(!document.body.classList.contains('pdf-mode')); });
    $$('[data-file]', sidebar).forEach(function (b) { b.addEventListener('click', function () { openViewer(b.getAttribute('data-file')); }); });
    $('#btn-copy-tab').addEventListener('click', copyTabCode);
    $('#btn-copy-sql').addEventListener('click', copyAllSql);
    $('#btn-db').addEventListener('click', downloadDb);
    $('#btn-acc').addEventListener('click', toggleAllAcc);
    $('#btn-link').addEventListener('click', function () { copyText(location.href.split('#')[0] + (activeTab ? '#' + activeTab.id : ''), 'Enlace copiado'); });
    $('#btn-help').addEventListener('click', openHelp);

    var q = $('#sb-q'), timer;
    q.addEventListener('input', function () { clearTimeout(timer); timer = setTimeout(function () { runSearch(q.value); }, 180); });
    q.addEventListener('keydown', function (e) {
      if (e.key === 'Enter') { e.preventDefault(); goHit(S.i + (e.shiftKey ? -1 : 1)); }
      else if (e.key === 'Escape') { q.value = ''; runSearch(''); q.blur(); }
    });
    $('#sb-next').addEventListener('click', function () { goHit(S.i + 1); });
    $('#sb-prev').addEventListener('click', function () { goHit(S.i - 1); });
    $('#sb-all').addEventListener('change', function () { runSearch(q.value); });

    var tick = false;
    window.addEventListener('scroll', function () {
      if (tick) return; tick = true;
      requestAnimationFrame(function () {
        tick = false; var h = document.documentElement, max = h.scrollHeight - h.clientHeight;
        prog.style.width = (max > 0 ? Math.min(100, (window.pageYOffset / max) * 100) : 0) + '%';
        top.classList.toggle('show', window.pageYOffset > 600); spy();
      });
    }, { passive: true });

    updateProgress();
    var saved = store('sidebar');
    setSidebar(saved ? saved === 'open' : window.innerWidth >= CONFIG.sidebarMinWidth, true);
  }
  function updateProgress() {
    var counted = tabs.filter(function (t) { return t.getAttribute('data-progress') !== 'no'; });
    var done = counted.filter(function (t) { return statusOf(t) === 'listo'; }).length, mid = counted.filter(function (t) { return statusOf(t) === 'progreso'; }).length;
    var p = $('#sb-progress'); if (!p) return;
    $('.bar i', p).style.width = (counted.length ? Math.round(100 * (done + mid * 0.5) / counted.length) : 0) + '%';
    $('b', p).textContent = done + ' / ' + counted.length;
  }
  function setSidebar(open, silent) {
    sbOpen = open; document.body.classList.toggle('sb-open', open);
    if (!silent || window.innerWidth >= CONFIG.sidebarMinWidth) store('sidebar', open ? 'open' : 'closed');
    setTimeout(fixWidgets, 320);
  }
  function setWide(on) {
    store('wide', on ? '1' : '0'); root.style.setProperty('--content-w', on ? '1280px' : '1040px');
    var sw = $('#sw-wide'); if (sw) sw.setAttribute('aria-checked', String(on)); setTimeout(fixWidgets, 280);
  }
  function setFont(delta) {
    var cur = parseInt(store('fs') || '17', 10), nv = Math.max(14, Math.min(22, cur + delta));
    store('fs', String(nv)); root.style.setProperty('--base-size', nv + 'px'); toast('Texto: ' + nv + ' px');
  }

  /* índice de la sección activa ("En esta sección") y scroll-spy */
  function updateSidebarNav() {
    if (!sidebar || !activeTab) return;
    $$('.sb-nav li', sidebar).forEach(function (li) {
      $('a', li).classList.toggle('is-active', li.getAttribute('data-tab') === activeTab.id);
      var old = $('.sb-toc', li); if (old) old.remove();
    });
    buildToc();
  }
  var tocItems = [];
  function buildToc() {
    if (!sidebar || !activeTab) return;
    var li = $('.sb-nav li[data-tab="' + activeTab.id + '"]', sidebar); if (!li) return;
    var old = $('.sb-toc', li); if (old) old.remove(); tocItems = [];
    var nodes = $$('h3, .acc', activeTab).filter(function (n) {
      var sub = n.closest('.kdd-subtab'); if (sub && !sub.classList.contains('is-active')) return false;
      if (n.tagName === 'H3' && n.closest('.acc')) return false; return !!n.id;
    }).slice(0, 18);
    if (!nodes.length) return;
    var ul = el('ul', 'sb-toc');
    nodes.forEach(function (n) {
      var label = n.classList.contains('acc') ? (($('.acc-head span', n) || {}).textContent || '') : n.textContent;
      var a = el('a', '', esc(label.trim())); a.href = '#' + n.id; var item = el('li'); item.appendChild(a); ul.appendChild(item); tocItems.push({ node: n, link: a });
    });
    li.appendChild(ul);
  }
  function spy() {
    if (!tocItems.length) return; var current = null;
    tocItems.forEach(function (t) { if (t.node.getBoundingClientRect().top < 140) current = t; });
    tocItems.forEach(function (t) { t.link.classList.toggle('is-active', t === current); });
  }

  /* ---------- 10. CHECKLIST ---------- */
  function initChecklists() {
    $$('.check-progress').forEach(function (p) {
      if (p.id === 'sb-progress') return;
      var list = p.nextElementSibling; while (list && !list.classList.contains('check-list')) list = list.nextElementSibling;
      if (!list) return;
      var items = $$('li', list);
      function update() {
        var ok = items.filter(function (li) { return li.classList.contains('ok'); }).length;
        $('.bar i', p).style.width = (items.length ? Math.round(100 * ok / items.length) : 0) + '%'; $('b', p).textContent = ok + ' / ' + items.length;
      }
      items.forEach(function (li) {
        var cb = $('input[type=checkbox]', li); if (!cb) return;
        var key = 'chk:' + (li.getAttribute('data-key') || li.textContent.trim().slice(0, 60));
        if (store(key) === '1') cb.checked = true; li.classList.toggle('ok', cb.checked);
        cb.addEventListener('change', function () { store(key, cb.checked ? '1' : '0'); li.classList.toggle('ok', cb.checked); update(); });
      });
      update();
    });
  }

  /* ---------- 11. ATAJOS Y ENLACES ---------- */
  function initKeys() {
    document.addEventListener('keydown', function (e) {
      var t = e.target, tag = t && t.tagName;
      if (e.key === 'Escape') { if (modal && modal.classList.contains('show')) closeModal(); else if (lb && lb.classList.contains('show')) lb.classList.remove('show'); else if (document.body.classList.contains('pdf-mode')) setPdf(false); return; }
      if (e.ctrlKey || e.metaKey || e.altKey) return;
      if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || (t && t.isContentEditable)) return;
      var k = e.key;
      if (/^[1-9]$/.test(k)) { var tb = tabs[parseInt(k, 10) - 1]; if (tb) activateTab(tb.id, { scroll: true }); }
      else if (k === '/') { e.preventDefault(); setSidebar(true); var q = $('#sb-q'); if (q) q.focus(); }
      else if (k === 'd' || k === 'D') setTheme(theme === 'dark' ? 'light' : 'dark');
      else if (k === 'c' || k === 'C') setAllCode(!codeShown);
      else if (k === 'v' || k === 'V') openViewer();
      else if (k === 'p' || k === 'P') setPdf(!document.body.classList.contains('pdf-mode'));
      else if (k === 's' || k === 'S') setSidebar(!sbOpen);
      else if (k === 'e' || k === 'E') toggleAllAcc();
      else if (k === '?') openHelp();
    });
    document.addEventListener('click', function (e) {
      var a = e.target.closest && e.target.closest('a[href^="#"]');
      if (!a || a.getAttribute('href').length < 2) return;
      var id = decodeURIComponent(a.getAttribute('href').slice(1)), node = document.getElementById(id); if (!node) return;
      e.preventDefault(); reveal(node, true); if (history.replaceState) history.replaceState(null, '', '#' + id);
      if (window.innerWidth < CONFIG.sidebarMinWidth && sbOpen && a.closest('.kdd-sidebar')) setSidebar(false, true);
    });
    window.addEventListener('hashchange', function () { var n = location.hash && document.getElementById(decodeURIComponent(location.hash.slice(1))); if (n) reveal(n, true); });
    window.addEventListener('beforeprint', function () { $$('.acc').forEach(function (a) { setAcc(a, true); }); });
  }

  /* ---------- INICIO ---------- */
  ready(function () {
    readConfig();
    root.style.setProperty('--base-size', parseInt(store('fs') || '17', 10) + 'px');
    if (store('wide') === '1') root.style.setProperty('--content-w', '1280px');
    var pref = store('code'); codeShown = pref ? pref === 'shown' : CONFIG.codeDefault === 'shown';

    // metadatos del encabezado (desde los params del Rmd)
    var hh = $('#header');
    if (hh && (cfg.materia || cfg.taller)) {
      var m = el('div', 'hero-meta');
      [['Materia', cfg.materia], ['Taller', cfg.taller], ['Fecha', cfg.fecha]].forEach(function (p) { if (p[1]) m.appendChild(el('span', '', '<b>' + esc(p[0]) + ':</b> ' + esc(p[1]))); });
      hh.appendChild(m);
    }

    buildTabs(); buildSubtabs(); buildAccordions(); buildCode(); wrapTables(); buildSidebar();
    setAllCode(codeShown, true); initLightbox(); initChecklists(); initKeys();

    var start = location.hash ? document.getElementById(decodeURIComponent(location.hash.slice(1))) : null;
    if (start) reveal(start, true);
    else if (tabs.length) { var st = store('tab'); activateTab(tabs.some(function (t) { return t.id === st; }) ? st : tabs[0].id, { updateHash: false }); }
    window.addEventListener('load', function () { setTimeout(fixWidgets, 150); });
  });
})();
