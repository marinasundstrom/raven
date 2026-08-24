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

if [[ "${RAVEN_DEVELOPMENT_ENVIRONMENT:-}" == "repository" ]] &&
   typeset -f deactivate_raven >/dev/null 2>&1; then
  _raven_env_current_script="$_raven_env_script"
  deactivate_raven >/dev/null
  _raven_env_script="$_raven_env_current_script"
  unset _raven_env_current_script
fi

_raven_env_dir="$(cd "$(dirname "$_raven_env_script")/.." && pwd)"
_raven_env_configuration="${RAVEN_CONFIGURATION:-Debug}"
_raven_env_framework="${RAVEN_FRAMEWORK:-net11.0}"
_raven_env_bin_dir="$_raven_env_dir/eng/development"
_raven_env_sdk_root="$_raven_env_dir/artifacts/development/sdk/$_raven_env_configuration/$_raven_env_framework"
_raven_env_rvn_dir="$_raven_env_dir/src/Raven/bin/$_raven_env_configuration/$_raven_env_framework"
_raven_env_rvnc_dir="$_raven_env_dir/src/Raven.Compiler/bin/$_raven_env_configuration/$_raven_env_framework"
_raven_env_language_server_dir="$_raven_env_dir/src/Raven.LanguageServer/bin/$_raven_env_configuration/$_raven_env_framework"
_raven_env_core_dir="$_raven_env_dir/src/Raven.Core/bin/$_raven_env_configuration/$_raven_env_framework"
_raven_env_macros_dir="$_raven_env_dir/src/Raven.Macros/bin/$_raven_env_configuration/$_raven_env_framework"

_raven_env_required_files=(
  "$_raven_env_rvn_dir/rvn.dll"
  "$_raven_env_rvnc_dir/rvnc.dll"
  "$_raven_env_language_server_dir/Raven.LanguageServer.dll"
  "$_raven_env_core_dir/Raven.Core.dll"
  "$_raven_env_macros_dir/Raven.Macros.dll"
)
_raven_env_missing_files=()
for _raven_env_file in "${_raven_env_required_files[@]}"; do
  [[ -f "$_raven_env_file" ]] || _raven_env_missing_files+=("$_raven_env_file")
done

if (( ${#_raven_env_missing_files[@]} > 0 )); then
  echo "Raven repository build outputs are missing:" >&2
  printf '  %s\n' "${_raven_env_missing_files[@]}" >&2
  echo "Build the repository tools first with scripts/build-development-environment.sh, then try again." >&2
  unset _raven_env_required_files _raven_env_missing_files _raven_env_file
  return 1
fi

_raven_env_saved_PATH="$PATH"
_raven_env_had_RAVEN_CONFIGURATION="${RAVEN_CONFIGURATION+x}"
_raven_env_saved_RAVEN_CONFIGURATION="${RAVEN_CONFIGURATION-}"
_raven_env_had_RAVEN_FRAMEWORK="${RAVEN_FRAMEWORK+x}"
_raven_env_saved_RAVEN_FRAMEWORK="${RAVEN_FRAMEWORK-}"
_raven_env_had_RAVEN_DEVELOPMENT_ENVIRONMENT="${RAVEN_DEVELOPMENT_ENVIRONMENT+x}"
_raven_env_saved_RAVEN_DEVELOPMENT_ENVIRONMENT="${RAVEN_DEVELOPMENT_ENVIRONMENT-}"
_raven_env_had_RAVEN_REPOSITORY_ROOT="${RAVEN_REPOSITORY_ROOT+x}"
_raven_env_saved_RAVEN_REPOSITORY_ROOT="${RAVEN_REPOSITORY_ROOT-}"
_raven_env_had_RAVEN_SDK_ROOT="${RAVEN_SDK_ROOT+x}"
_raven_env_saved_RAVEN_SDK_ROOT="${RAVEN_SDK_ROOT-}"
_raven_env_had_RAVEN_LANGUAGE_SERVER_PATH="${RAVEN_LANGUAGE_SERVER_PATH+x}"
_raven_env_saved_RAVEN_LANGUAGE_SERVER_PATH="${RAVEN_LANGUAGE_SERVER_PATH-}"
_raven_env_had_RavenCompilerHost="${RavenCompilerHost+x}"
_raven_env_saved_RavenCompilerHost="${RavenCompilerHost-}"
_raven_env_had_RavenBuildConfiguration="${RavenBuildConfiguration+x}"
_raven_env_saved_RavenBuildConfiguration="${RavenBuildConfiguration-}"
_raven_env_had_RavenCompilerFramework="${RavenCompilerFramework+x}"
_raven_env_saved_RavenCompilerFramework="${RavenCompilerFramework-}"

mkdir -p "$_raven_env_sdk_root/tools" "$_raven_env_sdk_root/sdk"
ln -sfn "$_raven_env_rvn_dir" "$_raven_env_sdk_root/tools/rvn"
ln -sfn "$_raven_env_rvnc_dir" "$_raven_env_sdk_root/tools/rvnc"
ln -sfn "$_raven_env_language_server_dir" "$_raven_env_sdk_root/tools/language-server"
ln -sfn "$_raven_env_core_dir/Raven.Core.dll" "$_raven_env_sdk_root/sdk/Raven.Core.dll"
ln -sfn "$_raven_env_macros_dir/Raven.Macros.dll" "$_raven_env_sdk_root/sdk/Raven.Macros.dll"
ln -sfn "$_raven_env_dir/build" "$_raven_env_sdk_root/sdk/build"
printf 'repository.%s\n' "$(git -C "$_raven_env_dir" rev-parse --short HEAD 2>/dev/null || printf unknown)" \
  > "$_raven_env_sdk_root/VERSION"

case ":$PATH:" in
  *":$_raven_env_bin_dir:"*) ;;
  *) export PATH="$_raven_env_bin_dir:$PATH" ;;
esac

export RAVEN_DEVELOPMENT_ENVIRONMENT="repository"
export RAVEN_REPOSITORY_ROOT="$_raven_env_dir"
export RAVEN_SDK_ROOT="$_raven_env_sdk_root"
export RAVEN_LANGUAGE_SERVER_PATH="$_raven_env_language_server_dir/Raven.LanguageServer.dll"
export RAVEN_CONFIGURATION="$_raven_env_configuration"
export RAVEN_FRAMEWORK="$_raven_env_framework"
export RavenCompilerHost="$_raven_env_rvnc_dir/rvnc.dll"
export RavenBuildConfiguration="$_raven_env_configuration"
export RavenCompilerFramework="$_raven_env_framework"

rvn() {
  dotnet "$RAVEN_REPOSITORY_ROOT/src/Raven/bin/$RAVEN_CONFIGURATION/$RAVEN_FRAMEWORK/rvn.dll" "$@"
}

rvnc() {
  dotnet "$RAVEN_REPOSITORY_ROOT/src/Raven.Compiler/bin/$RAVEN_CONFIGURATION/$RAVEN_FRAMEWORK/rvnc.dll" "$@"
}

raven-env-info() {
  echo "Raven environment: repository"
  echo "  Repository:      $RAVEN_REPOSITORY_ROOT"
  echo "  Configuration:   $RAVEN_CONFIGURATION"
  echo "  Tool framework:  $RAVEN_FRAMEWORK"
  echo "  SDK root:        $RAVEN_SDK_ROOT"
  echo "  rvn:             $RAVEN_REPOSITORY_ROOT/src/Raven/bin/$RAVEN_CONFIGURATION/$RAVEN_FRAMEWORK/rvn.dll"
  echo "  rvnc:            $RavenCompilerHost"
  echo "  Language server: $RAVEN_REPOSITORY_ROOT/src/Raven.LanguageServer/bin/$RAVEN_CONFIGURATION/$RAVEN_FRAMEWORK/Raven.LanguageServer.dll"
}

deactivate_raven() {
  export PATH="$_raven_env_saved_PATH"

  if [[ "$_raven_env_had_RAVEN_CONFIGURATION" == "x" ]]; then
    export RAVEN_CONFIGURATION="$_raven_env_saved_RAVEN_CONFIGURATION"
  else
    unset RAVEN_CONFIGURATION
  fi
  if [[ "$_raven_env_had_RAVEN_FRAMEWORK" == "x" ]]; then
    export RAVEN_FRAMEWORK="$_raven_env_saved_RAVEN_FRAMEWORK"
  else
    unset RAVEN_FRAMEWORK
  fi
  if [[ "$_raven_env_had_RAVEN_DEVELOPMENT_ENVIRONMENT" == "x" ]]; then
    export RAVEN_DEVELOPMENT_ENVIRONMENT="$_raven_env_saved_RAVEN_DEVELOPMENT_ENVIRONMENT"
  else
    unset RAVEN_DEVELOPMENT_ENVIRONMENT
  fi
  if [[ "$_raven_env_had_RAVEN_REPOSITORY_ROOT" == "x" ]]; then
    export RAVEN_REPOSITORY_ROOT="$_raven_env_saved_RAVEN_REPOSITORY_ROOT"
  else
    unset RAVEN_REPOSITORY_ROOT
  fi
  if [[ "$_raven_env_had_RAVEN_SDK_ROOT" == "x" ]]; then
    export RAVEN_SDK_ROOT="$_raven_env_saved_RAVEN_SDK_ROOT"
  else
    unset RAVEN_SDK_ROOT
  fi
  if [[ "$_raven_env_had_RAVEN_LANGUAGE_SERVER_PATH" == "x" ]]; then
    export RAVEN_LANGUAGE_SERVER_PATH="$_raven_env_saved_RAVEN_LANGUAGE_SERVER_PATH"
  else
    unset RAVEN_LANGUAGE_SERVER_PATH
  fi
  if [[ "$_raven_env_had_RavenCompilerHost" == "x" ]]; then
    export RavenCompilerHost="$_raven_env_saved_RavenCompilerHost"
  else
    unset RavenCompilerHost
  fi
  if [[ "$_raven_env_had_RavenBuildConfiguration" == "x" ]]; then
    export RavenBuildConfiguration="$_raven_env_saved_RavenBuildConfiguration"
  else
    unset RavenBuildConfiguration
  fi
  if [[ "$_raven_env_had_RavenCompilerFramework" == "x" ]]; then
    export RavenCompilerFramework="$_raven_env_saved_RavenCompilerFramework"
  else
    unset RavenCompilerFramework
  fi

  unset -f rvn rvnc raven-env-info deactivate-raven deactivate_raven
  unset _raven_env_sourced _raven_env_script _raven_env_dir
  unset _raven_env_configuration _raven_env_framework _raven_env_bin_dir _raven_env_sdk_root
  unset _raven_env_rvn_dir _raven_env_rvnc_dir _raven_env_language_server_dir
  unset _raven_env_core_dir _raven_env_macros_dir _raven_env_required_files
  unset _raven_env_missing_files _raven_env_file
  unset _raven_env_saved_PATH
  unset _raven_env_had_RAVEN_CONFIGURATION _raven_env_saved_RAVEN_CONFIGURATION
  unset _raven_env_had_RAVEN_FRAMEWORK _raven_env_saved_RAVEN_FRAMEWORK
  unset _raven_env_had_RAVEN_DEVELOPMENT_ENVIRONMENT _raven_env_saved_RAVEN_DEVELOPMENT_ENVIRONMENT
  unset _raven_env_had_RAVEN_REPOSITORY_ROOT _raven_env_saved_RAVEN_REPOSITORY_ROOT
  unset _raven_env_had_RAVEN_SDK_ROOT _raven_env_saved_RAVEN_SDK_ROOT
  unset _raven_env_had_RAVEN_LANGUAGE_SERVER_PATH _raven_env_saved_RAVEN_LANGUAGE_SERVER_PATH
  unset _raven_env_had_RavenCompilerHost _raven_env_saved_RavenCompilerHost
  unset _raven_env_had_RavenBuildConfiguration _raven_env_saved_RavenBuildConfiguration
  unset _raven_env_had_RavenCompilerFramework _raven_env_saved_RavenCompilerFramework

  echo "Raven repository environment deactivated."
}

deactivate-raven() {
  deactivate_raven "$@"
}

echo "Raven repository environment activated for this shell."
raven-env-info
echo "Run deactivate-raven to restore the previous environment."
echo "Override the build with RAVEN_CONFIGURATION or RAVEN_FRAMEWORK before sourcing."
