#!/usr/bin/env bash

_raven_env_sourced=0
if [[ -n "${ZSH_VERSION:-}" ]]; then
  case ":${ZSH_EVAL_CONTEXT:-}:" in
    *:file:*) _raven_env_sourced=1 ;;
  esac
  _raven_env_script="$0"
elif [[ -n "${BASH_SOURCE:-}" ]]; then
  [[ "${BASH_SOURCE[0]}" != "$0" ]] && _raven_env_sourced=1
  _raven_env_script="${BASH_SOURCE[0]}"
else
  _raven_env_script="$0"
fi

if [[ "$_raven_env_sourced" != "1" ]]; then
  echo "This script must be sourced:"
  echo "  source scripts/raven-env.sh"
  exit 1
fi

_raven_env_dir="$(cd "$(dirname "$_raven_env_script")/.." && pwd)"
_raven_env_configuration="${RAVEN_CONFIGURATION:-Debug}"
_raven_env_framework="${RAVEN_FRAMEWORK:-net10.0}"

rvn() {
  dotnet "$_raven_env_dir/src/Raven/bin/$_raven_env_configuration/$_raven_env_framework/rvn.dll" "$@"
}

rvnc() {
  dotnet "$_raven_env_dir/src/Raven.Compiler/bin/$_raven_env_configuration/$_raven_env_framework/rvnc.dll" "$@"
}

echo "Raven shell helpers loaded."
echo "  rvn  -> src/Raven/bin/$_raven_env_configuration/$_raven_env_framework/rvn.dll"
echo "  rvnc -> src/Raven.Compiler/bin/$_raven_env_configuration/$_raven_env_framework/rvnc.dll"
echo "Override with RAVEN_CONFIGURATION or RAVEN_FRAMEWORK before sourcing."
