#!/usr/bin/env python3
"""Check nested inhabited-unit constructors against an explicit native SDK bundle."""
import argparse
import json
from pathlib import Path
import subprocess
import xml.etree.ElementTree as ET

parser = argparse.ArgumentParser(description=__doc__)
for name in ('compiler', 'bundle', 'output'):
    parser.add_argument('--' + name, type=Path, required=True)
args = parser.parse_args()
output = args.output.resolve()
output.mkdir(parents=True, exist_ok=False)
results = []
for spelling in ('System.Void', 'unit'):
    work = output / ('nominal' if spelling == 'System.Void' else 'unit')
    work.mkdir()
    (work / 'Main.rvn').write_text('''import System.*
import System.Tasks.*
union RequestError {
    case Failed
}
class RequestContext {
    private var completion: Promise<Result<UNIT, RequestError>> = Promise<Result<UNIT, RequestError>>()
    func Pending() -> Task<Result<UNIT, RequestError>> => completion.Task
}
func Main() -> int {
    let context = RequestContext()
    _ = context.Pending()
    return 42
}
'''.replace('UNIT', spelling))
    project = ET.Element('Project', Sdk='Microsoft.NET.Sdk')
    ET.SubElement(project, 'Import', Project=str(args.bundle.resolve() / 'NeoCLR.ClassLibrary.props'))
    properties = ET.SubElement(project, 'PropertyGroup')
    for name, value in [('TargetFramework', 'net10.0'), ('OutputType', 'Exe'), ('AssemblyName', 'UnitOwner')]:
        ET.SubElement(properties, name).text = value
    project_path = work / 'UnitOwner.rvnproj'
    ET.ElementTree(project).write(project_path)
    command = ['dotnet', str(args.compiler.resolve()), 'neoclr', '--project', str(project_path), '--no-build-references']
    result = subprocess.run(command, capture_output=True, text=True, timeout=120)
    results.append(dict(spelling=spelling, exitCode=result.returncode, stdout=result.stdout, stderr=result.stderr))
    (output / 'validation.json').write_text(json.dumps(results, indent=2) + '\n')
    if result.returncode:
        raise RuntimeError(result.stdout + result.stderr)
print('PASS nested unit promise constructors for nominal and unit spellings')
