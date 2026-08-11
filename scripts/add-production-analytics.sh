#!/usr/bin/env bash
set -euo pipefail

site_directory="${1:-}"
measurement_id="G-RWCPR800RE"

if [[ -z "$site_directory" || ! -d "$site_directory" ]]; then
    echo "Usage: $0 <published-site-directory>" >&2
    exit 1
fi

html_count=0
injected_count=0

while IFS= read -r -d '' html_file; do
    html_count=$((html_count + 1))

    if grep -Fq "$measurement_id" "$html_file"; then
        continue
    fi

    if ! grep -Fqi '</head>' "$html_file"; then
        echo "Published HTML page has no closing head element: $html_file" >&2
        exit 1
    fi

    perl -0pi -e 's{</head>}{    <!-- Google tag (gtag.js) -->\n    <script async src="https://www.googletagmanager.com/gtag/js?id=G-RWCPR800RE"></script>\n    <script>\n      window.dataLayer = window.dataLayer || [];\n      function gtag(){dataLayer.push(arguments);}\n      gtag('\''js'\'', new Date());\n\n      gtag('\''config'\'', '\''G-RWCPR800RE'\'');\n    </script>\n</head>}i' "$html_file"

    if ! grep -Fq "$measurement_id" "$html_file"; then
        echo "Failed to add Google Analytics to: $html_file" >&2
        exit 1
    fi

    injected_count=$((injected_count + 1))
done < <(find "$site_directory" -type f -name '*.html' -print0)

if [[ "$html_count" -eq 0 ]]; then
    echo "Published site contains no HTML pages: $site_directory" >&2
    exit 1
fi

echo "Google Analytics is present in $html_count published HTML pages ($injected_count updated)."
