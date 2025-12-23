#!/bin/bash
# Start the Emacs MCP server
# This script is called by Claude's MCP configuration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Ensure emacsclient is available
if ! command -v emacsclient &> /dev/null; then
    echo "Error: emacsclient not found in PATH" >&2
    exit 1
fi

# Check if Emacs server is running
if ! emacsclient -e "t" &> /dev/null; then
    echo "Warning: Emacs server not running. Start Emacs with (server-start)" >&2
fi

cd "$SCRIPT_DIR"

# Run the MCP server
exec clojure -X:mcp
