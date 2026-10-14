// Run in an isolated VS Code acceptance workspace with the packaged extension.
const vscode = require('vscode');
const fs = require('fs');
const path = require('path');
const assert = require('assert');
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

exports.run = async function () {
  const folder = vscode.workspace.workspaceFolders.find(f => f.name === 'Native') || vscode.workspace.workspaceFolders[0];
  const root = folder.uri.fsPath;
  const uri = vscode.Uri.file(path.join(root, 'Main.rvn'));
  const original = fs.readFileSync(uri.fsPath, 'utf8');
  const report = { passed: false, samples: [] };
  let document;
  async function replace(text) {
    const edit = new vscode.WorkspaceEdit();
    edit.replace(uri, new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)), text);
    assert(await vscode.workspace.applyEdit(edit));
  }
  async function sample(label, range) {
    const started = Date.now();
    const result = await vscode.commands.executeCommand('vscode.executeInlayHintProvider', uri, range);
    const hints = (result || []).map(h => ({ line: h.position.line, character: h.position.character,
      label: typeof h.label === 'string' ? h.label : h.label.map(p => p.value).join('') }));
    report.samples.push({ label, version: document.version, elapsedMs: Date.now() - started, hints });
    return hints;
  }
  const fullRange = () => new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length));
  try {
    await vscode.extensions.getExtension('raven.raven-vscode').activate();
    document = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(document);
    // Keep declarations intact while exercising background full-document refreshes.
    const source = 'func Main() -> int {\n    let number = 42\n    let text = "hello"\n    return number\n}\n' +
      Array.from({length: 100}, (_, i) => `// padding ${i}\n`).join('');
    await replace(source);
    let baseline;
    for (let i = 0; i < 30; i++) {
      baseline = await sample('warmup', fullRange());
      if (baseline.some(h => h.label.includes(': int')) && baseline.some(h => h.label.includes(': string'))) break;
      await delay(500);
    }
    assert(baseline.some(h => h.label.includes(': int')), 'No baseline number hint');
    assert(baseline.some(h => h.label.includes(': string')), 'No baseline text hint');
    for (let edit = 0; edit < 4; edit++) {
      await replace(source + `// edit ${edit}\n`);
      for (const wait of [100, 350, 900]) {
        await delay(wait);
        const hints = await sample(`comment-edit-${edit}-${wait}ms`, fullRange());
        assert(hints.some(h => h.label.includes(': int')), 'Number hint disappeared after a comment edit');
        assert(hints.some(h => h.label.includes(': string')), 'Text hint disappeared after a comment edit');
      }
    }
    // VS Code can request multiple visible ranges for one document concurrently.
    const ranges = [new vscode.Range(1, 0, 1, 100), new vscode.Range(2, 0, 2, 100)];
    for (let i = 0; i < 4; i++) {
      const results = await Promise.all(ranges.map((range, j) => sample(`concurrent-${i}-${j}`, range)));
      assert(results[0].some(h => h.label.includes(': int')), 'Concurrent number range lost hints');
      assert(results[1].some(h => h.label.includes(': string')), 'Concurrent text range lost hints');
    }
    report.passed = true;
  } catch (error) {
    report.error = String(error.stack || error);
    throw error;
  } finally {
    if (document) await replace(original);
    fs.writeFileSync(path.join(root, 'vscode-inlay-acceptance.json'), JSON.stringify(report, null, 2) + '\n');
  }
};
