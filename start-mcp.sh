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

# Chroma configuration - override via environment or Emacs config
# These can be set in your shell profile, Emacs config, or systemd unit
export CHROMA_HOST="${CHROMA_HOST:-localhost}"
export CHROMA_PORT="${CHROMA_PORT:-8000}"
export OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"

# Run the MCP server
exec clojure -X:mcp
