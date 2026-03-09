# Critical Path Viewer — Standalone HTML Tool

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** A single HTML file that lets users drop a Shake `report.json`, computes the critical path, and renders copyable Mermaid chart source code for both the critical path and the full dependency graph.

**Architecture:** Single self-contained HTML file with embedded CSS and vanilla JavaScript. No build step. External dependency: pako (via CDN) for Mermaid playground link compression. The file parses Shake's positional-array profile JSON, builds a filtered dependency graph (transitive closure from Root), computes the critical path via topological sort + longest-path DP, then generates Mermaid `graph LR` source code.

**Tech Stack:** HTML5, vanilla JavaScript (ES2020+), pako via CDN (`https://cdn.jsdelivr.net/npm/pako@2/dist/pako.min.js`)

---

## Background: Shake report.json format

Each entry in the JSON array is a positional array:

```
[name, executionSeconds, built, changed, depends?, traces?]
```

| Index | Field | Type | Description |
|-------|-------|------|-------------|
| 0 | name | string | Rule key (file path, `OracleQ ...`, `getEnv ...`, etc.) |
| 1 | execution | number | Wall-clock seconds. 0 for cached rules. |
| 2 | built | int | Build step when last executed. 0 = this run. |
| 3 | changed | int | Build step when result last changed. |
| 4 | depends | `[[int]]` | Optional. Dependency groups — each group is a list of 0-based indices. |
| 5 | traces | `[[string, number, number]]` | Optional. Traced commands. |

There is a synthetic `Root` entry whose depends point to the user-requested top-level targets. A real report has ~25k entries; the build closure from Root is a subset of those.

### Rule kind classification (from name prefix)

| Prefix | Kind |
|--------|------|
| `OracleQ ` | Oracle |
| `doesFileExist ` | doesFileExist |
| `doesDirectoryExist ` | doesDirectoryExist |
| `getEnv ` | getEnv |
| `getDirectoryFiles ` | getDirectoryFiles |
| `getDirectoryContents ` | getDirectoryContents |
| `getDirectoryDirs ` | getDirectoryDirs |
| `Root` (exact) | Root |
| *(everything else)* | File |

### Mermaid playground link format

URL: `https://mermaid.live/edit#pako:<url-safe-base64-of-deflated-json>`

The JSON payload is: `{"code":"<mermaid source>","mermaid":{"theme":"default"},"autoSync":true,"updateDiagram":true}`

Compression: `pako.deflate(jsonString, { level: 9 })` → `base64url` encoding (replace `+` with `-`, `/` with `_`).

---

## Task 1: HTML skeleton with drag-and-drop

**Files:**
- Create: `tools/critical-path-viewer.html`

**Step 1: Write the HTML file with drag-and-drop UI**

Create the file with:
- A full-page drop zone that accepts `.json` files
- A results section (hidden initially) with two panels: "Critical Path" and "Full Graph"
- Each panel has: a `<textarea>` for the mermaid source, a [Copy] button, and a [Open in Mermaid Live] link placeholder
- Minimal, clean CSS (dark background for drop zone, monospace for textareas)
- Drop handler reads the file via `FileReader.readAsText`, calls a stub `processReport(jsonString)` function
- Also support a file `<input>` as fallback for click-to-browse

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Shake Critical Path Viewer</title>
  <script src="https://cdn.jsdelivr.net/npm/pako@2/dist/pako.min.js"></script>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; background: #1a1a2e; color: #e0e0e0; min-height: 100vh; }

    #drop-zone {
      display: flex; flex-direction: column; align-items: center; justify-content: center;
      min-height: 100vh; border: 3px dashed #4a4a6a; margin: 20px; border-radius: 12px;
      cursor: pointer; transition: border-color 0.2s, background 0.2s;
    }
    #drop-zone.dragover { border-color: #7b68ee; background: rgba(123, 104, 238, 0.1); }
    #drop-zone h1 { font-size: 1.5em; margin-bottom: 0.5em; color: #b0b0d0; }
    #drop-zone p { color: #8888aa; }
    #drop-zone.hidden { display: none; }

    #results { display: none; padding: 20px; }
    #results.visible { display: block; }

    #back-btn {
      background: #2a2a4a; color: #b0b0d0; border: 1px solid #4a4a6a; padding: 8px 16px;
      border-radius: 6px; cursor: pointer; margin-bottom: 16px; font-size: 0.9em;
    }
    #back-btn:hover { background: #3a3a5a; }

    #stats { background: #16213e; padding: 16px; border-radius: 8px; margin-bottom: 20px; font-size: 0.9em; line-height: 1.6; }
    #stats strong { color: #7b68ee; }

    .panel { background: #16213e; border-radius: 8px; padding: 16px; margin-bottom: 20px; }
    .panel h2 { font-size: 1.1em; margin-bottom: 12px; color: #b0b0d0; }
    .panel-actions { display: flex; gap: 8px; margin-bottom: 12px; }

    .btn {
      background: #7b68ee; color: white; border: none; padding: 8px 16px;
      border-radius: 6px; cursor: pointer; font-size: 0.85em; text-decoration: none;
      display: inline-flex; align-items: center; gap: 4px;
    }
    .btn:hover { background: #6a56d6; }
    .btn.secondary { background: #2a2a4a; border: 1px solid #4a4a6a; color: #b0b0d0; }
    .btn.secondary:hover { background: #3a3a5a; }
    .btn.copied { background: #2ecc71; }

    textarea {
      width: 100%; min-height: 300px; background: #0f0f23; color: #a0d0a0;
      border: 1px solid #333; border-radius: 6px; padding: 12px;
      font-family: "SF Mono", "Fira Code", "Consolas", monospace; font-size: 0.8em;
      resize: vertical; tab-size: 2;
    }

    #error { background: #4a1a1a; color: #ff6b6b; padding: 16px; border-radius: 8px; margin: 20px; display: none; }
    #error.visible { display: block; }
  </style>
</head>
<body>
  <div id="drop-zone">
    <h1>Shake Critical Path Viewer</h1>
    <p>Drop a Shake report.json here, or click to browse</p>
    <input type="file" id="file-input" accept=".json" style="display:none">
  </div>

  <div id="error"></div>

  <div id="results">
    <button id="back-btn">Load another file</button>
    <div id="stats"></div>

    <div class="panel">
      <h2>Critical Path</h2>
      <div class="panel-actions">
        <button class="btn" id="copy-critical">Copy</button>
        <a class="btn secondary" id="link-critical" href="#" target="_blank">Open in Mermaid Live</a>
      </div>
      <textarea id="mermaid-critical" readonly></textarea>
    </div>

    <div class="panel">
      <h2>Full Dependency Graph</h2>
      <div class="panel-actions">
        <button class="btn" id="copy-full">Copy</button>
        <a class="btn secondary" id="link-full" href="#" target="_blank">Open in Mermaid Live</a>
      </div>
      <textarea id="mermaid-full" readonly></textarea>
    </div>
  </div>

  <script>
    // --- Drag and drop ---
    const dropZone = document.getElementById('drop-zone');
    const fileInput = document.getElementById('file-input');
    const results = document.getElementById('results');
    const errorDiv = document.getElementById('error');

    dropZone.addEventListener('click', () => fileInput.click());
    dropZone.addEventListener('dragover', (e) => { e.preventDefault(); dropZone.classList.add('dragover'); });
    dropZone.addEventListener('dragleave', () => dropZone.classList.remove('dragover'));
    dropZone.addEventListener('drop', (e) => {
      e.preventDefault();
      dropZone.classList.remove('dragover');
      const file = e.dataTransfer.files[0];
      if (file) readFile(file);
    });
    fileInput.addEventListener('change', (e) => { if (e.target.files[0]) readFile(e.target.files[0]); });

    document.getElementById('back-btn').addEventListener('click', () => {
      results.classList.remove('visible');
      errorDiv.classList.remove('visible');
      dropZone.classList.remove('hidden');
      fileInput.value = '';
    });

    function readFile(file) {
      const reader = new FileReader();
      reader.onload = (e) => {
        try {
          processReport(e.target.result);
        } catch (err) {
          showError(err.message);
        }
      };
      reader.readAsText(file);
    }

    function showError(msg) {
      errorDiv.textContent = 'Error: ' + msg;
      errorDiv.classList.add('visible');
    }

    // --- Stub: will be implemented in subsequent tasks ---
    function processReport(jsonString) {
      dropZone.classList.add('hidden');
      errorDiv.classList.remove('visible');
      results.classList.add('visible');
      document.getElementById('mermaid-critical').value = 'TODO: critical path mermaid source';
      document.getElementById('mermaid-full').value = 'TODO: full graph mermaid source';
    }
  </script>
</body>
</html>
```

**Step 2: Verify the drag-and-drop works**

Open `tools/critical-path-viewer.html` in a browser. Drop `~/Downloads/report.json` on the page. Verify:
- The drop zone disappears
- The results section appears with two textareas showing "TODO" placeholders
- The "Load another file" button resets the view
- Click-to-browse also works

**Step 3: Commit**

```bash
git add tools/critical-path-viewer.html
git commit -m "feat: critical path viewer HTML skeleton with drag-and-drop"
```

---

## Task 2: Parse report.json and build the dependency graph

**Files:**
- Modify: `tools/critical-path-viewer.html` (the `<script>` section)

**Step 1: Implement `parseReport(jsonString)` → graph data structure**

Replace the `processReport` stub and add a `parseReport` function. The function:
1. Parses the JSON array
2. Creates a node for each entry with: `{ index, name, execSeconds, built, changed, depends, traces, kind }`
3. Classifies rule kind from name prefix
4. Returns `{ nodes: [...], entries: rawArray }`

Add this code inside the `<script>` tag, replacing the stub `processReport`:

```javascript
function classifyKind(name) {
  if (name === 'Root') return 'Root';
  if (name.startsWith('OracleQ ')) return 'Oracle';
  if (name.startsWith('doesFileExist ')) return 'doesFileExist';
  if (name.startsWith('doesDirectoryExist ')) return 'doesDirectoryExist';
  if (name.startsWith('getEnv ')) return 'getEnv';
  if (name.startsWith('getDirectoryFiles ')) return 'getDirectoryFiles';
  if (name.startsWith('getDirectoryContents ')) return 'getDirectoryContents';
  if (name.startsWith('getDirectoryDirs ')) return 'getDirectoryDirs';
  return 'File';
}

function parseReport(jsonString) {
  const raw = JSON.parse(jsonString);
  if (!Array.isArray(raw)) throw new Error('Expected a JSON array');

  const nodes = raw.map((entry, index) => ({
    index,
    name: entry[0],
    execSeconds: entry[1],
    built: entry[2],
    changed: entry[3],
    depends: entry[4] || [],  // [[int]] — array of dependency groups
    traces: entry[5] || [],
    kind: classifyKind(entry[0]),
  }));

  return { nodes };
}
```

**Step 2: Implement `filterBuildClosure(nodes)` → Set of indices in the closure**

Starts from the `Root` entry (or falls back to all entries with `built === 0`), then BFS through `depends` transitively.

```javascript
function filterBuildClosure(nodes) {
  const rootIdx = nodes.findIndex(n => n.name === 'Root');
  const seeds = new Set();

  if (rootIdx >= 0) {
    seeds.add(rootIdx);
  } else {
    // Fallback: all entries built in this run
    for (const n of nodes) {
      if (n.built === 0) seeds.add(n.index);
    }
  }

  const closure = new Set();
  const queue = [...seeds];
  while (queue.length > 0) {
    const idx = queue.pop();
    if (closure.has(idx)) continue;
    if (idx < 0 || idx >= nodes.length) continue;
    closure.add(idx);
    for (const group of nodes[idx].depends) {
      for (const dep of group) {
        if (!closure.has(dep)) queue.push(dep);
      }
    }
  }
  return closure;
}
```

**Step 3: Wire into `processReport` and display stats**

```javascript
function processReport(jsonString) {
  dropZone.classList.add('hidden');
  errorDiv.classList.remove('visible');

  const { nodes } = parseReport(jsonString);
  const closure = filterBuildClosure(nodes);

  // Collect edges within the closure
  const edges = []; // { from, to }
  for (const idx of closure) {
    const node = nodes[idx];
    for (const group of node.depends) {
      for (const dep of group) {
        if (closure.has(dep)) {
          edges.push({ from: idx, to: dep });
        }
      }
    }
  }

  document.getElementById('stats').innerHTML =
    `<strong>Total entries:</strong> ${nodes.length} | ` +
    `<strong>Build closure:</strong> ${closure.size} nodes, ${edges.length} edges`;

  results.classList.add('visible');
  document.getElementById('mermaid-critical').value = 'TODO: critical path';
  document.getElementById('mermaid-full').value = 'TODO: full graph';
}
```

**Step 4: Verify parsing works**

Open in browser, drop `~/Downloads/report.json`. Verify:
- The stats line shows total entries (~24,968), build closure size, and edge count
- No errors in the console

**Step 5: Commit**

```bash
git add tools/critical-path-viewer.html
git commit -m "feat: parse report.json and compute build closure"
```

---

## Task 3: Compute the critical path

**Files:**
- Modify: `tools/critical-path-viewer.html` (the `<script>` section)

**Step 1: Implement topological sort (Kahn's algorithm)**

```javascript
function topologicalSort(closure, nodes) {
  // Build adjacency list and in-degree within closure
  // Edge direction: from → to means "from depends on to"
  // For longest path, we want: to → from (dependency points to dependent)
  // Actually for critical path: a node's earliest finish = its own duration +
  //   max(earliest finish of all its dependencies)
  // So we process in dependency order: dependencies first.
  //
  // depends means "this node depends on these indices"
  // So topological order = process dependencies before dependents.
  // in-degree = number of nodes that this node depends on (within closure).

  const closureArr = [...closure];
  const inDegree = new Map(); // idx -> count of dependencies
  const dependents = new Map(); // idx -> list of indices that depend on this

  for (const idx of closureArr) {
    inDegree.set(idx, 0);
    dependents.set(idx, []);
  }

  for (const idx of closureArr) {
    const node = nodes[idx];
    let depCount = 0;
    for (const group of node.depends) {
      for (const dep of group) {
        if (closure.has(dep)) {
          depCount++;
          dependents.get(dep).push(idx);
        }
      }
    }
    inDegree.set(idx, depCount);
  }

  const queue = [];
  for (const idx of closureArr) {
    if (inDegree.get(idx) === 0) queue.push(idx);
  }

  const sorted = [];
  while (queue.length > 0) {
    const idx = queue.shift();
    sorted.push(idx);
    for (const dependent of dependents.get(idx)) {
      const newDeg = inDegree.get(dependent) - 1;
      inDegree.set(dependent, newDeg);
      if (newDeg === 0) queue.push(dependent);
    }
  }

  return sorted; // Dependencies come before dependents
}
```

**Step 2: Implement longest-path DP to find critical path**

```javascript
function computeCriticalPath(closure, nodes) {
  const sorted = topologicalSort(closure, nodes);

  // eft[idx] = earliest finish time of this node
  //          = node.execSeconds + max(eft of all dependencies)
  const eft = new Map();
  const predecessor = new Map(); // For backtracking the path

  for (const idx of sorted) {
    const node = nodes[idx];
    let maxDepEft = 0;
    let maxDepIdx = -1;

    for (const group of node.depends) {
      for (const dep of group) {
        if (closure.has(dep) && eft.has(dep)) {
          const depEft = eft.get(dep);
          if (depEft > maxDepEft) {
            maxDepEft = depEft;
            maxDepIdx = dep;
          }
        }
      }
    }

    eft.set(idx, node.execSeconds + maxDepEft);
    if (maxDepIdx >= 0) predecessor.set(idx, maxDepIdx);
  }

  // Find the node with maximum EFT — that's the end of the critical path
  let maxEft = 0;
  let endIdx = -1;
  for (const [idx, val] of eft) {
    if (val > maxEft) {
      maxEft = val;
      endIdx = idx;
    }
  }

  // Backtrack to build the critical path
  const path = [];
  let current = endIdx;
  while (current >= 0 && current !== undefined) {
    path.push(current);
    current = predecessor.get(current);
  }
  // path is from end to start (most dependent → leaf dependency)

  return { path, totalSeconds: maxEft, eft };
}
```

**Step 3: Wire critical path into `processReport` and display stats**

Update `processReport` to call `computeCriticalPath` and show results in stats:

```javascript
// Inside processReport, after computing edges:
const { path: criticalPath, totalSeconds: criticalSeconds, eft } = computeCriticalPath(closure, nodes);

document.getElementById('stats').innerHTML =
  `<strong>Total entries:</strong> ${nodes.length}<br>` +
  `<strong>Build closure:</strong> ${closure.size} nodes, ${edges.length} edges<br>` +
  `<strong>Critical path:</strong> ${criticalPath.length} nodes, ` +
  `${(criticalSeconds / 60).toFixed(1)} minutes total`;
```

**Step 4: Verify critical path computation**

Open in browser, drop `~/Downloads/report.json`. Verify:
- Critical path stats show a reasonable number of nodes and total minutes
- No errors in console
- The critical path total time should be significantly less than the sum of all execution times (parallelism)

**Step 5: Commit**

```bash
git add tools/critical-path-viewer.html
git commit -m "feat: compute critical path via topological sort + longest-path DP"
```

---

## Task 4: Generate Mermaid source code

**Files:**
- Modify: `tools/critical-path-viewer.html` (the `<script>` section)

**Step 1: Implement node label formatting**

```javascript
function formatNodeLabel(node) {
  const minutes = (node.execSeconds / 60).toFixed(2);
  let name = node.name;
  // Truncate long names — keep last 60 chars with ellipsis
  if (name.length > 60) {
    name = '...' + name.slice(-57);
  }
  return `${node.kind} ${name} [${minutes}m]`;
}

function sanitizeMermaidId(index) {
  return `n${index}`;
}

function escapeMermaidLabel(label) {
  // Mermaid uses quotes for labels; escape internal quotes and special chars
  return label.replace(/"/g, '#quot;').replace(/</g, '#lt;').replace(/>/g, '#gt;');
}
```

**Step 2: Implement critical path Mermaid generation**

```javascript
function generateCriticalPathMermaid(criticalPath, nodes) {
  const lines = ['graph LR'];

  // Define nodes
  for (const idx of criticalPath) {
    const node = nodes[idx];
    const label = escapeMermaidLabel(formatNodeLabel(node));
    lines.push(`  ${sanitizeMermaidId(idx)}["${label}"]`);
  }

  // Define edges (path is ordered end→start, so reverse for LR flow)
  const reversed = [...criticalPath].reverse();
  for (let i = 0; i < reversed.length - 1; i++) {
    const from = sanitizeMermaidId(reversed[i]);
    const to = sanitizeMermaidId(reversed[i + 1]);
    lines.push(`  ${from} --> ${to}`);
  }

  // Style critical path nodes
  lines.push('');
  lines.push('  classDef critical fill:#ff6b6b,stroke:#c0392b,color:#fff');
  const classNodes = criticalPath.map(sanitizeMermaidId).join(',');
  lines.push(`  class ${classNodes} critical`);

  return lines.join('\n');
}
```

**Step 3: Implement full graph Mermaid generation**

```javascript
function generateFullGraphMermaid(closure, edges, criticalPath, nodes) {
  const lines = ['graph LR'];
  const criticalSet = new Set(criticalPath);

  // Define nodes
  for (const idx of closure) {
    const node = nodes[idx];
    // Skip the Root node — it's synthetic
    if (node.kind === 'Root') continue;
    const label = escapeMermaidLabel(formatNodeLabel(node));
    lines.push(`  ${sanitizeMermaidId(idx)}["${label}"]`);
  }

  // Define edges (skip edges involving Root)
  for (const { from, to } of edges) {
    if (nodes[from].kind === 'Root' || nodes[to].kind === 'Root') continue;
    lines.push(`  ${sanitizeMermaidId(from)} --> ${sanitizeMermaidId(to)}`);
  }

  // Style critical path nodes
  if (criticalPath.length > 0) {
    lines.push('');
    lines.push('  classDef critical fill:#ff6b6b,stroke:#c0392b,color:#fff');
    const classNodes = criticalPath
      .filter(idx => nodes[idx].kind !== 'Root')
      .map(sanitizeMermaidId).join(',');
    if (classNodes) lines.push(`  class ${classNodes} critical`);
  }

  return lines.join('\n');
}
```

**Step 4: Wire Mermaid generation into `processReport`**

Replace the TODO placeholders in `processReport`:

```javascript
const criticalMermaid = generateCriticalPathMermaid(criticalPath, nodes);
const fullMermaid = generateFullGraphMermaid(closure, edges, criticalPath, nodes);

document.getElementById('mermaid-critical').value = criticalMermaid;
document.getElementById('mermaid-full').value = fullMermaid;
```

**Step 5: Verify Mermaid output**

Open in browser, drop `~/Downloads/report.json`. Verify:
- Both textareas contain valid-looking Mermaid source
- Critical path Mermaid starts with `graph LR`, has node definitions with `["..."]` labels, has `-->` edges, has `classDef` and `class` statements
- Full graph Mermaid has many more nodes and edges
- Copy one of the outputs and paste it into https://mermaid.live to verify it renders

**Step 6: Commit**

```bash
git add tools/critical-path-viewer.html
git commit -m "feat: generate mermaid chart source for critical path and full graph"
```

---

## Task 5: Copy buttons and Mermaid playground links

**Files:**
- Modify: `tools/critical-path-viewer.html` (the `<script>` section)

**Step 1: Implement copy-to-clipboard**

```javascript
function setupCopyButton(btnId, textareaId) {
  document.getElementById(btnId).addEventListener('click', async () => {
    const textarea = document.getElementById(textareaId);
    const btn = document.getElementById(btnId);
    try {
      await navigator.clipboard.writeText(textarea.value);
      btn.textContent = 'Copied!';
      btn.classList.add('copied');
      setTimeout(() => { btn.textContent = 'Copy'; btn.classList.remove('copied'); }, 2000);
    } catch {
      // Fallback for older browsers
      textarea.select();
      document.execCommand('copy');
      btn.textContent = 'Copied!';
      btn.classList.add('copied');
      setTimeout(() => { btn.textContent = 'Copy'; btn.classList.remove('copied'); }, 2000);
    }
  });
}

setupCopyButton('copy-critical', 'mermaid-critical');
setupCopyButton('copy-full', 'mermaid-full');
```

**Step 2: Implement Mermaid Live playground link generation**

```javascript
function generateMermaidLiveUrl(mermaidSource) {
  const state = JSON.stringify({
    code: mermaidSource,
    mermaid: { theme: 'default' },
    autoSync: true,
    updateDiagram: true,
  });
  const data = new TextEncoder().encode(state);
  const compressed = pako.deflate(data, { level: 9 });

  // URL-safe base64: replace + with -, / with _
  let base64 = btoa(String.fromCharCode(...compressed));
  base64 = base64.replace(/\+/g, '-').replace(/\//g, '_');

  return `https://mermaid.live/edit#pako:${base64}`;
}
```

**Step 3: Wire links into `processReport`**

Add after the Mermaid generation code in `processReport`:

```javascript
// Playground links
const criticalLink = document.getElementById('link-critical');
const fullLink = document.getElementById('link-full');

try {
  criticalLink.href = generateMermaidLiveUrl(criticalMermaid);
  criticalLink.style.display = '';
} catch {
  criticalLink.style.display = 'none';
}

try {
  // Full graph may be too large for a URL — hide link if so
  const fullUrl = generateMermaidLiveUrl(fullMermaid);
  if (fullUrl.length > 32000) {
    fullLink.style.display = 'none';
    fullLink.title = 'Graph too large for URL';
  } else {
    fullLink.href = fullUrl;
    fullLink.style.display = '';
  }
} catch {
  fullLink.style.display = 'none';
}
```

**Step 4: Verify copy and links work**

Open in browser, drop `~/Downloads/report.json`. Verify:
- Click [Copy] on critical path → paste into a text editor → valid Mermaid source
- Click [Copy] on full graph → paste → valid Mermaid source
- Click [Open in Mermaid Live] on critical path → opens mermaid.live with the chart loaded
- The full graph link may be hidden if too large (expected for ~25k node reports) — verify the link hides gracefully

**Step 5: Commit**

```bash
git add tools/critical-path-viewer.html
git commit -m "feat: add copy buttons and mermaid playground links"
```

---

## Task 6: Final polish and edge cases

**Files:**
- Modify: `tools/critical-path-viewer.html`

**Step 1: Handle edge cases in `processReport`**

Add error handling for:
- Empty JSON array
- No Root entry and no `built === 0` entries
- Report with only one node

```javascript
// At the start of processReport, after parseReport:
if (nodes.length === 0) throw new Error('Report is empty');
if (closure.size === 0) throw new Error('No build closure found — no Root entry and no entries with built=0');
```

**Step 2: Add loading indicator for large files**

Since parsing 25k entries + computing critical path takes a moment, show a brief "Processing..." message. Wrap the processing in a `requestAnimationFrame` or `setTimeout(0)` to let the UI update:

```javascript
function processReport(jsonString) {
  dropZone.classList.add('hidden');
  errorDiv.classList.remove('visible');
  results.classList.remove('visible');

  // Show a brief processing message
  document.getElementById('stats').innerHTML = 'Processing...';
  results.classList.add('visible');

  // Defer heavy work to next frame so "Processing..." renders
  requestAnimationFrame(() => {
    try {
      // ... all the parsing, closure, critical path, mermaid generation ...
    } catch (err) {
      showError(err.message);
      results.classList.remove('visible');
      dropZone.classList.remove('hidden');
    }
  });
}
```

**Step 3: Auto-resize textareas to content**

After setting textarea values, auto-size them:

```javascript
function autoResize(textareaId) {
  const ta = document.getElementById(textareaId);
  ta.style.height = 'auto';
  ta.style.height = Math.min(ta.scrollHeight, 600) + 'px';
}
// Call after setting values:
autoResize('mermaid-critical');
autoResize('mermaid-full');
```

**Step 4: Final verification with `~/Downloads/report.json`**

Open in browser, drop the file. Verify end-to-end:
1. Drop zone accepts the file
2. "Processing..." appears briefly
3. Stats show total entries, closure size, edge count, critical path length and time
4. Critical path textarea has a readable Mermaid chart with the critical path nodes
5. Full graph textarea has a larger chart
6. [Copy] works for both
7. [Open in Mermaid Live] works for critical path
8. [Load another file] resets and allows re-dropping
9. No console errors

**Step 5: Commit**

```bash
git add tools/critical-path-viewer.html
git commit -m "feat: polish critical path viewer with error handling and loading state"
```
