#!/usr/bin/env bash
# check-signing.sh — verify Aquamacs code signing, notarization, and stapling
# Usage: ./check-signing.sh /path/to/Aquamacs-alpha2.app
# Exits immediately on first failure.

set -euo pipefail

APP="${1:?Usage: $0 /path/to/Aquamacs.app}"
MAIN_EXEC="$APP/Contents/MacOS/Aquamacs"

echo "Checking: $APP"

echo ""
echo "=== 1. Stapling ==="
xcrun stapler validate "$APP"

echo ""
echo "=== 2. Gatekeeper assessment ==="
spctl --assess --verbose=4 --type exec "$APP"

echo ""
echo "=== 3. Code signature (deep + strict) ==="
codesign --verify --deep --strict --verbose=2 "$APP"

echo ""
echo "=== 4. Entitlements on main executable ==="
ENTITLEMENTS=$(codesign -d --entitlements - "$MAIN_EXEC" 2>&1)
echo "$ENTITLEMENTS"
grep -q "com.apple.security.automation.apple-events"            <<< "$ENTITLEMENTS" || { echo "missing: apple-events"; exit 1; }
grep -q "com.apple.security.cs.allow-jit"                       <<< "$ENTITLEMENTS" || { echo "missing: allow-jit"; exit 1; }
grep -q "com.apple.security.cs.disable-library-validation"      <<< "$ENTITLEMENTS" || { echo "missing: disable-library-validation"; exit 1; }
grep -q "com.apple.security.cs.allow-unsigned-executable-memory" <<< "$ENTITLEMENTS" || { echo "missing: allow-unsigned-executable-memory"; exit 1; }

echo ""
echo "=== 5. Unsigned nested code ==="
! codesign --verify --deep --strict "$APP" 2>&1 | grep -qi "not signed\|invalid\|failed"

echo ""
echo "=== 6. Signing identity ==="
codesign -dv "$MAIN_EXEC" 2>&1 | grep -E "Authority|TeamIdentifier|Identifier"

echo ""
echo "All checks passed."
