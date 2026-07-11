import sys
import os

# Fallback path for local packages
sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

import traceback

LOG_FILE = "/tmp/gemini_history_server.log"

try:
    import glob
    import json
    from mcp.server.fastmcp import FastMCP

    mcp = FastMCP("GeminiHistoryServer")

    CHATS_DIR = os.path.expanduser("~/.gemini/tmp/odwl/chats")

    def load_conversations():
        conversations = []
        if not os.path.exists(CHATS_DIR):
            return conversations
            
        files = glob.glob(os.path.join(CHATS_DIR, "**/*.jsonl"), recursive=True)
        files.sort(key=os.path.getmtime, reverse=True)
        
        for f in files:
            session_id = os.path.splitext(os.path.basename(f))[0]
            messages = []
            try:
                with open(f, "r", encoding="utf-8") as file:
                    for line in file:
                        line = line.strip()
                        if not line:
                            continue
                        data = json.loads(line)
                        if "$set" in data:
                            continue
                        msg_type = data.get("type")
                        if msg_type in ["user", "gemini"]:
                            content = data.get("content")
                            text_content = ""
                            if isinstance(content, list):
                                for part in content:
                                    if isinstance(part, dict) and "text" in part:
                                        text_content += part["text"]
                            elif isinstance(content, str):
                                text_content = content
                            
                            messages.append({
                                "id": data.get("id"),
                                "timestamp": data.get("timestamp"),
                                "role": "user" if msg_type == "user" else "assistant",
                                "text": text_content
                            })
            except Exception as ex:
                with open(LOG_FILE, "a") as logf:
                    logf.write(f"Error parsing file {f}: {ex}\n")
                continue
                
            if messages:
                first_user_msg = next((m["text"] for m in messages if m["role"] == "user"), "")
                title = first_user_msg[:60] + "..." if len(first_user_msg) > 60 else (first_user_msg or "Empty Chat")
                conversations.append({
                    "id": session_id,
                    "title": title,
                    "timestamp": os.path.getmtime(f),
                    "messages": messages
                })
                
        return conversations

    @mcp.tool()
    def list_conversations() -> str:
        """List all available local Gemini CLI/Jetski conversation history files.
        
        Returns:
            A JSON string listing the conversations with their IDs, titles, and timestamps.
        """
        try:
            data = load_conversations()
            summary = []
            for c in data:
                summary.append({
                    "id": c["id"],
                    "title": c["title"],
                    "timestamp": c["timestamp"],
                    "message_count": len(c["messages"])
                })
            return json.dumps(summary, indent=2)
        except Exception as ex:
            with open(LOG_FILE, "a") as logf:
                logf.write(f"Error in list_conversations tool: {ex}\n")
                traceback.print_exc(file=logf)
            raise ex

    @mcp.tool()
    def get_conversation(conversation_id: str) -> str:
        """Get the full message history of a specific conversation.
        
        Args:
            conversation_id: The unique identifier (session ID) of the conversation.
            
        Returns:
            A JSON string containing the conversation title, timestamp, and all messages.
        """
        try:
            data = load_conversations()
            for c in data:
                if c["id"] == conversation_id:
                    return json.dumps(c, indent=2)
            return json.dumps({"error": f"Conversation with ID {conversation_id} not found."})
        except Exception as ex:
            with open(LOG_FILE, "a") as logf:
                logf.write(f"Error in get_conversation tool: {ex}\n")
                traceback.print_exc(file=logf)
            raise ex

    @mcp.tool()
    def search_conversations(query: str) -> str:
        """Search for a query string in all conversation history (titles and messages).
        
        Args:
            query: The term or phrase to search for.
            
        Returns:
            A JSON string listing matching conversations and the matching messages.
        """
        try:
            data = load_conversations()
            results = []
            query_lower = query.lower()
            for c in data:
                matches = []
                title_match = query_lower in c["title"].lower()
                for msg in c["messages"]:
                    if query_lower in msg["text"].lower():
                        matches.append(msg)
                if title_match or matches:
                    results.append({
                        "id": c["id"],
                        "title": c["title"],
                        "timestamp": c["timestamp"],
                        "title_match": title_match,
                        "matching_messages": matches
                    })
            return json.dumps(results, indent=2)
        except Exception as ex:
            with open(LOG_FILE, "a") as logf:
                logf.write(f"Error in search_conversations tool: {ex}\n")
                traceback.print_exc(file=logf)
            raise ex

    if __name__ == "__main__":
        mcp.run()

except Exception as e:
    with open(LOG_FILE, "a") as f:
        f.write("Global server exception:\n")
        f.write(str(e) + "\n")
        traceback.print_exc(file=f)
    raise e
