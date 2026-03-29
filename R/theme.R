@import url('https://fonts.googleapis.com/css2?family=DM+Mono:ital,wght@0,300;0,400;0,500;1,300&family=Syne:wght@400;700&display=swap');

/* ── Variables ─────────────────────────────────────────────── */
:root {
  --bg:          #080b10;
  --surface:     #0e1117;
  --card:        #131820;
  --card-hover:  #171e28;
  --border:      #1c2433;
  --border-hi:   #263245;
  --txt:         #dce6f0;
  --txt-dim:     #8496a9;
  --txt-faint:   #3d4f63;
  --accent:      #5bb8ff;
  --accent-dim:  rgba(91,184,255,.12);
  --accent-glow: rgba(91,184,255,.25);
  --green:       #3dffa0;
  --green-dim:   rgba(61,255,160,.1);
  --warn:        #ffb347;
  --danger:      #ff5c5c;
  --rail-w:      210px;
  --strip-h:     48px;
  --mono:        'DM Mono', 'Courier New', monospace;
  --display:     'Syne', sans-serif;
  --r:           6px;
  --r-lg:        10px;
}

*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
html, body { height: 100%; overflow: hidden; }

body {
  background:  var(--bg);
  color:       var(--txt);
  font-family: var(--mono);
  font-size:   13px;
  line-height: 1.5;
  -webkit-font-smoothing: antialiased;
}

.shell { display: flex; height: 100vh; width: 100vw; }

.rail {
  width:        var(--rail-w);
  min-width:    var(--rail-w);
  background:   var(--surface);
  border-right: 1px solid var(--border);
  display:      flex;
  flex-direction: column;
  overflow:     hidden;
}

.rail-wordmark {
  font-family:    var(--display);
  font-size:      16px;
  font-weight:    700;
  color:          var(--txt);
  letter-spacing: -.3px;
  padding:        22px 18px 18px;
  border-bottom:  1px solid var(--border);
  display:        flex;
  align-items:    center;
  gap:            8px;
}
.rail-wordmark::before {
  content:     '◈';
  color:       var(--accent);
  font-size:   14px;
}

.rail-nav { list-style: none; flex: 1; padding: 10px 8px; }
.rail-nav li { margin-bottom: 2px; }

.ri {
  width:       100%;
  background:  transparent;
  border:      1px solid transparent;
  border-radius: var(--r);
  color:       var(--txt-dim);
  cursor:      pointer;
  display:     flex;
  align-items: center;
  gap:         10px;
  padding:     9px 12px;
  text-align:  left;
  font-family: var(--mono);
  font-size:   12px;
  transition:  all .15s ease;
  position:    relative;
}
.ri:hover {
  background: rgba(255,255,255,.04);
  color:      var(--txt);
  border-color: var(--border);
}
.ri.active {
  background:   var(--accent-dim);
  border-color: var(--accent);
  color:        var(--accent);
}
.ri-num {
  font-size:  10px;
  color:      var(--txt-faint);
  min-width:  16px;
  font-style: italic;
}
.ri.active .ri-num { color: var(--accent); opacity: .7; }

.rail-bottom {
  padding:    14px 16px;
  border-top: 1px solid var(--border);
  display:    flex;
  flex-direction: column;
  gap:        8px;
}
.pipe-dots { display: flex; gap: 6px; align-items: center; }
.dot {
  width: 7px; height: 7px;
  border-radius: 50%;
  background:  var(--txt-faint);
  border:      1px solid var(--border-hi);
  transition:  all .3s;
}
.dot.done {
  background:  var(--green);
  border-color: var(--green);
  box-shadow:  0 0 6px var(--green);
}
.rail-ver { font-size: 10px; color: var(--txt-faint); }

.content {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  background: var(--bg);
}

.strip {
  height:        var(--strip-h);
  min-height:    var(--strip-h);
  background:    var(--surface);
  border-bottom: 1px solid var(--border);
  display:       flex;
  align-items:   center;
  justify-content: space-between;
  padding:       0 22px;
}
.strip-path {
  display:     flex;
  align-items: center;
  gap:         8px;
  font-size:   11px;
  color:       var(--txt-dim);
}
.strip-root  { color: var(--accent); font-weight: 500; }
.strip-arrow { color: var(--txt-faint); }
.strip-right { display: flex; align-items: center; gap: 10px; }
.step-counter {
  font-size:  10px;
  color:      var(--txt-faint);
  letter-spacing: .8px;
  background: var(--card);
  border:     1px solid var(--border);
  border-radius: var(--r);
  padding:    3px 10px;
}
.run-btn {
  background:  var(--accent-dim);
  border:      1px solid var(--accent);
  border-radius: var(--r);
  color:       var(--accent);
  cursor:      pointer;
  font-family: var(--mono);
  font-size:   11px;
  letter-spacing: .5px;
  padding:     5px 16px;
  transition:  all .15s;
}
.run-btn:hover {
  background: var(--accent);
  color:      var(--bg);
}

.pages    { flex: 1; overflow-y: auto; padding: 28px 28px 40px; }
.pg       { display: none; }
.pg.active{ display: block; }
.pg-inner { max-width: 900px; margin: 0 auto; }

.pg-head  { margin-bottom: 24px; }
.pg-title {
  font-family:    var(--display);
  font-size:      20px;
  font-weight:    700;
  color:          var(--txt);
  letter-spacing: -.4px;
  margin-bottom:  5px;
}
.pg-sub {
  font-size:   12px;
  color:       var(--txt-dim);
  max-width:   480px;
  line-height: 1.6;
}

.two-col {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 14px;
  margin-bottom: 14px;
}
.card {
  background:    var(--card);
  border:        1px solid var(--border);
  border-radius: var(--r-lg);
  padding:       18px;
  transition:    border-color .2s;
}
.card:hover    { border-color: var(--border-hi); }
.card-full     { grid-column: 1 / -1; }

.clabel {
  font-size:      10px;
  letter-spacing: 1.4px;
  text-transform: uppercase;
  color:          var(--txt-faint);
  margin-bottom:  14px;
  display:        flex;
  align-items:    center;
  gap:            6px;
}
.clabel::before {
  content: '';
  display: inline-block;
  width: 12px; height: 1px;
  background: var(--accent);
}

.kv {
  display:         flex;
  justify-content: space-between;
  align-items:     center;
  padding:         7px 0;
  border-bottom:   1px solid var(--border);
}
.kv:last-child { border-bottom: none; }
.kv-label { font-size: 11px; color: var(--txt-dim); }
.kv-value {
  font-size:  13px;
  color:      var(--accent);
  font-weight: 500;
  min-width:  60px;
  text-align: right;
}
.kv-value:empty::after { content: '—'; color: var(--txt-faint); }

.drop-zone {
  border:        2px dashed var(--border-hi);
  border-radius: var(--r-lg);
  padding:       32px 20px;
  text-align:    center;
  transition:    all .2s;
  cursor:        pointer;
  margin-bottom: 14px;
}
.drop-zone:hover {
  border-color: var(--accent);
  background:   var(--accent-dim);
}
.drop-icon  { font-size: 28px; margin-bottom: 8px; opacity: .5; }
.drop-label {
  font-size: 12px;
  color:     var(--txt-dim);
  line-height: 1.6;
}
.drop-label strong { color: var(--accent); }

.action-btn {
  width:          100%;
  margin-top:     14px;
  background:     var(--green-dim);
  border:         1px solid var(--green);
  border-radius:  var(--r);
  color:          var(--green);
  cursor:         pointer;
  font-family:    var(--mono);
  font-size:      12px;
  letter-spacing: .5px;
  padding:        9px;
  transition:     all .15s;
}
.action-btn:hover {
  background: var(--green);
  color:      var(--bg);
}

.genome-badge {
  display:       inline-block;
  background:    var(--accent-dim);
  border:        1px solid var(--accent);
  border-radius: 20px;
  color:         var(--accent);
  font-size:     10px;
  letter-spacing: .5px;
  padding:       3px 10px;
  margin-top:    6px;
}

.shiny-input-container { margin-bottom: 12px; }

label {
  font-family:    var(--mono);
  font-size:      11px;
  color:          var(--txt-dim);
  margin-bottom:  5px;
  display:        block;
  letter-spacing: .3px;
}

.form-control,
select,
input[type="number"],
input[type="text"] {
  background:    var(--surface) !important;
  border:        1px solid var(--border-hi) !important;
  border-radius: var(--r) !important;
  color:         var(--txt) !important;
  font-family:   var(--mono) !important;
  font-size:     12px !important;
  padding:       7px 10px !important;
  width:         100% !important;
  transition:    border-color .15s !important;
}
.form-control:focus,
select:focus,
input:focus {
  border-color: var(--accent) !important;
  outline:      none !important;
  box-shadow:   0 0 0 3px var(--accent-dim) !important;
}

.checkbox label {
  color:       var(--txt-dim);
  font-size:   12px;
  padding-left: 22px;
  cursor:      pointer;
}
input[type="checkbox"] { accent-color: var(--accent); }

.shiny-input-container .btn,
.btn-default,
.btn-file {
  background:    var(--surface) !important;
  border:        1px solid var(--border-hi) !important;
  border-radius: var(--r) !important;
  color:         var(--txt-dim) !important;
  font-family:   var(--mono) !important;
  font-size:     11px !important;
}
.shiny-input-container .btn:hover { border-color: var(--accent) !important; color: var(--accent) !important; }

/* Progress bar */
.progress { background: var(--surface) !important; height: 3px !important; border-radius: 2px; overflow: hidden; }
.progress-bar { background: var(--accent) !important; }

.dataTable,
.dataTables_wrapper {
  font-family: var(--mono) !important;
  font-size:   12px !important;
  color:       var(--txt) !important;
}
table.dataTable {
  border-collapse: collapse !important;
  width: 100% !important;
}
table.dataTable thead th {
  background:     var(--surface) !important;
  color:          var(--txt-dim) !important;
  border-bottom:  1px solid var(--border-hi) !important;
  font-size:      10px !important;
  letter-spacing: 1px !important;
  text-transform: uppercase !important;
  padding:        10px 12px !important;
}
table.dataTable tbody tr { background: var(--card) !important; }
table.dataTable tbody tr:nth-child(even) { background: var(--surface) !important; }
table.dataTable tbody tr:hover td { background: var(--card-hover) !important; }
table.dataTable tbody td {
  border-color: var(--border) !important;
  padding:      7px 12px !important;
  color:        var(--txt) !important;
}
.dataTables_info,
.dataTables_length,
.dataTables_filter label { color: var(--txt-dim) !important; font-size: 11px !important; }
.dataTables_filter input { width: 160px !important; }
.paginate_button        { color: var(--txt-dim) !important; border-radius: var(--r) !important; }
.paginate_button:hover  { background: var(--card-hover) !important; border-color: var(--border-hi) !important; color: var(--txt) !important; }
.paginate_button.current,
.paginate_button.current:hover {
  background:   var(--accent-dim) !important;
  border-color: var(--accent) !important;
  color:        var(--accent) !important;
}

::-webkit-scrollbar       { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: var(--border-hi); border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: var(--txt-faint); }
