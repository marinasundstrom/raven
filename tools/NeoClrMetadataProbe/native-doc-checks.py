#!/usr/bin/env python3
"""Focused native RavenDoc loader checks using --native-doc-fixture output."""
import argparse
import hashlib
from html.parser import HTMLParser
from urllib.parse import unquote, urlsplit
import json
from pathlib import Path
import shutil
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
for name in ('ravendoc', 'core', 'fixtures', 'output'):
    parser.add_argument('--' + name, required=True, type=Path)
args = parser.parse_args()
root = args.output.resolve()
root.mkdir(exist_ok=False)
core = args.core.resolve()
fixtures = args.fixtures.resolve()
models = fixtures / 'Renamed.dll'
library = fixtures / 'Library.dll'
report = {'commands': []}


def run(arguments, expected=0):
    command = ['dotnet', str(args.ravendoc.resolve()), *map(str, arguments)]
    result = subprocess.run(command, text=True, capture_output=True, timeout=90)
    report['commands'].append({'command': command, 'exitCode': result.returncode,
                               'stdout': result.stdout, 'stderr': result.stderr})
    (root / 'validation.json').write_text(json.dumps(report, indent=2) + '\n')
    assert result.returncode == expected, result.stdout + result.stderr


# An adjacent invalid DLL must never enter the explicitly selected native load set.
(fixtures / 'Poison.dll').write_bytes(b'not metadata')
direct = root / 'direct'
run([library, '--native-core-reference', core, '--reference', models, '-o', direct])
member = (direct / 'Example/Books/method_Echo.html').read_text()
assert 'Library.dll' in member and 'Returns the supplied native book.' in member
assert not (direct / 'Example/Book/index.html').exists(), 'Dependency became a documentation target'
# Namespace documentation is part of the same external-comment contract as types.
xml = fixtures / 'Renamed.xml'
xml.write_text(xml.read_text().replace('<member name="N:Example"><summary>Native namespace overview.</summary></member>', '').replace('<members>', '<members><member name="N:Example"><summary>Native namespace overview.</summary></member>', 1))
sidecar = fixtures / 'Renamed.docs'
(sidecar / 'symbols/P').mkdir(parents=True, exist_ok=True)
(sidecar / 'manifest.json').write_text(json.dumps({'formatVersion': 1, 'symbolsPath': 'symbols'}))
comment = sidecar / 'symbols/P' / (hashlib.sha256(b'Example.Book.Count').hexdigest() + '.md')
comment.write_text('---\nxref: P:Example.Book.Count\n---\nNative Markdown property documentation.\n')
config = root / 'site.json'
site = root / 'site'
(root / 'models.cs').write_text('namespace Example; public class Book { public int Code; public int Count => 1; }')
(root / 'content').mkdir()
(root / 'content/book.md').write_text('---\nuid: T:Example.Book\n---\nAdditional native API guidance.')
(root / 'index.md').write_text('# Native libraries\n[Book](xref:T:Example.Book)')
settings = {'name': 'Native libraries', 'output': str(site), 'apiPath': 'docs',
            'apiInputs': [str(models), str(library)], 'nativeCoreReference': str(core), 'search': True, 'extensionNamespaces': ['Example'], 'apiContent': 'content',
            'sourceRepository': {'url': 'https://github.com/example/fixture', 'revision': 'fixture', 'root': '.', 'paths': ['models.cs']}, 'pages': [{'source': 'index.md', 'output': 'index.html'}]}
config.write_text(json.dumps(settings))
run(['--site', config])
book = (site / 'docs/Example/Book/index.html').read_text()
member = (site / 'docs/Example/Books/method_Echo.html').read_text()
assert 'Models.dll' in book and 'A native book with preserved documentation.' in book
assert 'Additional native API guidance.' in book and '/blob/fixture/models.cs' in book
assert 'Library.dll' in member and 'Book/index.html' in member
assert 'book: Book' in member and 'number: int' in member
assert 'The original book.' in member and 'The same book instance.' in member
assert 'Native namespace overview.' in (site / 'docs/Example/index.html').read_text()
assert 'Rating' in book and 'BookExtensions/method_Rating.html' in book
assert 'val Count: int' in (site / 'docs/Example/Book/property_Count.html').read_text()
assert 'Native Markdown property documentation.' in (site / 'docs/Example/Book/property_Count.html').read_text()
assert 'Code: int' in (site / 'docs/Example/Book/field_Code.html').read_text()
derived = (site / 'docs/Example/SpecialBook/index.html').read_text()
assert 'data-member-inherited="true"' in derived and '../Book/property_Count.html' in derived
assert 'Value: T' in (site / 'docs/Example/Box`1/field_Value.html').read_text()
assert 'Read()' in (site / 'docs/Example/Reader`1/method_Read.html').read_text()
search = (site / 'search-index.json').read_text()
assert 'Book' in search and 'Echo' in search
class Links(HTMLParser):
    def handle_starttag(self, tag, attrs):
        for key, value in attrs:
            if key != 'href' or not value:
                continue
            url = urlsplit(value)
            if not url.scheme and not url.netloc and url.path:
                assert (self.page.parent / unquote(url.path)).exists(), (self.page, value)
for page in site.rglob('*.html'):
    links = Links()
    links.page = page
    links.feed(page.read_text())
pages = '\n'.join(path.read_text() for path in site.rglob('*.html'))
assert 'CoreProbe' not in pages and 'Probe.dll' not in pages and 'RavenDoc.NativeMetadataHost' not in pages
assert not (site / 'docs/System').exists(), 'Core declarations leaked into selected targets'
# Inherited config also works for an explicit API group.
grouped = dict(settings, output=str(root / 'grouped'))
grouped.pop('apiInputs')
grouped.pop('nativeCoreReference')
grouped['apis'] = [{'inputs': [str(models), str(library)], 'path': 'docs', 'nativeCoreReference': str(core)}]
config.write_text(json.dumps(grouped))
run(['--site', config])
assert (root / 'grouped/docs/Example/Book/index.html').exists()
# Validation precedes rendering; failed direct input preserves an existing output.
sentinel = direct / 'sentinel.txt'
sentinel.write_text('preserve')
for extra in ([], ['--reference', fixtures / 'Poison.dll']):
    run([library, '--native-core-reference', core, *extra, '-o', direct], 1)
    assert sentinel.read_text() == 'preserve'
# Duplicate identities and ambiguous type URLs must fail before staged site publication.
copy = fixtures / 'Copied.dll'
shutil.copyfile(models, copy)
for additional in (copy, fixtures / 'Conflict.dll'):
    rejected = dict(settings, apiInputs=[str(models), str(library), str(additional)])
    config.write_text(json.dumps(rejected))
    run(['--site', config], 1)
    assert (site / 'docs/Example/Book/index.html').read_text() == book
run([models, '--native-core-reference', core, '-o', fixtures], 1)
assert models.exists()
report['result'] = 'PASS native docs: grouped navigation, XML/Markdown, namespace comments, overloads, generics, fields/properties, inheritance/extensions, API content/source links, search, ownership, local links and input/output guards'
(root / 'validation.json').write_text(json.dumps(report, indent=2) + '\n')
print(report['result'])
