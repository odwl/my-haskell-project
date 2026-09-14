#!/usr/bin/env python3
"""
sync_google_doc.py - Push updated master markdown itinerary and package orders to Google Docs.

Uses /google/bin/releases/codemind-mcp-servers/workspace_server.par (workspace MCP)
to synchronize the master markdown content into Google Doc 1N4YjroxgRAdQPOrMrNQYGiRiMMyX4GkNAat9yOWvpk4.
"""

import sys
import os
import json
import subprocess

DOC_ID = "1N4YjroxgRAdQPOrMrNQYGiRiMMyX4GkNAat9yOWvpk4"
MD_PATH = "/usr/local/google/home/odwl/Documents/dev/my-haskell-project/docs/knowledge_base/travel_itinerary_2026.md"
WORKSPACE_SERVER = "/google/bin/releases/codemind-mcp-servers/workspace_server.par"

def sync_doc(doc_id=DOC_ID, md_path=MD_PATH):
    if not os.path.exists(md_path):
        print(f"Error: {md_path} not found.", file=sys.stderr)
        return False
        
    with open(md_path, "r", encoding="utf-8") as f:
        content = f.read()

    init_req = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "sync_orders_client", "version": "1.0"}
        }
    }
    init_notif = {
        "jsonrpc": "2.0",
        "method": "notifications/initialized"
    }
    call_req = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {
            "name": "update_document",
            "arguments": {
                "doc_id": doc_id,
                "markdown_text": content
            }
        }
    }

    input_data = "\n".join([json.dumps(init_req), json.dumps(init_notif), json.dumps(call_req)]) + "\n"
    
    proc = subprocess.Popen(
        [WORKSPACE_SERVER],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    stdout, stderr = proc.communicate(input=input_data)
    
    for line in stdout.strip().split("\n"):
        if not line: continue
        try:
            msg = json.loads(line)
            if msg.get("id") == 2:
                print("Result:", msg.get("result"))
                return True
        except Exception:
            pass
            
    print("Failed to get successful response. Stdout:", stdout[:500], "Stderr:", stderr[:500], file=sys.stderr)
    return False

if __name__ == "__main__":
    success = sync_doc()
    sys.exit(0 if success else 1)
