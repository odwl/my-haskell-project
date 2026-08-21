---
name: retrieve-person-chats
description: Ultra-fast retrieval and formatting of Google Chat conversations (both persistent 1:1 Direct Messages and meeting/group rooms) with any named individual or email alias across corporate environments, enforcing sub-second latency, precise sender display name resolution, and clickable interactive hyperlinks.
---

# Retrieve Person Chats Skill

This skill defines an ultra-low-latency protocol and automated execution engine (`scripts/fetch_person_chat.py`) to instantly locate, extract, and cleanly format recent Google Chat exchanges with any specified person, colleague, or email address.

## Core Architecture & Latency Optimization

To achieve sub-second execution speeds without hanging or excessive API overhead, all retrievals follow a three-tier concurrent optimization pipeline:

1. **⚡ Fast Path (`findDirectMessage` - ~100ms)**: 
   - Whenever an email alias or username is provided (e.g., `tpylak` -> `tpylak@google.com`), immediately execute `service.spaces().findDirectMessage(name="users/tpylak@google.com")`. This bypasses scanning hundreds of rooms and retrieves the primary persistent 1:1 Direct Message (`DIRECT_MESSAGE`) thread in a single HTTP network round-trip.

2. **🌐 Parallel Space Scanning & Room Catch-All (~400ms)**: 
   - Simultaneously, use a thread pool to list active rooms and meeting spaces, checking for recent participant interactions or room titles involving the target person to capture secondary meetings and 1:1 project rooms.

3. **👤 Precise User ID Resolution (Zero "Unknown Sender" entries)**: 
   - Upon locating the target chat space, automatically execute a single lightweight member list query (`service.spaces().members().list()`) to map raw Google Chat user identifiers (e.g., `users/101577...`) directly to human display names (e.g., *Tomek Pylak* vs *Olivier de Wolf / You*).

---

## Mandatory Formatting & Interactive Link Rules

When displaying extracted chat conversations to the user or generating briefings, you MUST apply these strict aesthetic and interactive formatting rules:

1. **Strict Chronological Order**: 
   - Always display the retrieved chat messages in forward chronological order (oldest to newest at the bottom, matching natural conversational flow).
2. **Interactive Clickable Hyperlinks**: 
   - Extract any embedded URLs or linked reference text (such as design docs, screenshots, prototypes, or PRDs) and format them as clear GitHub-style Markdown hyperlinks: `🌐 [Description or URL](https://...)`.
3. **Clean Visual Hierarchy**: 
   - Group messages by space title/room, label individual timestamps in clean readable formats (e.g., `YYYY-MM-DD HH:MM UTC` or `3:24 PM`), and distinctly distinguish between messages sent by the target individual vs replies sent by the user (`You`).
4. **Performance Attribution**: 
   - Always report the exact end-to-end execution latency in seconds at the bottom of the retrieval summary.

---

## Script Usage Example

To retrieve the latest 15 messages with an individual (e.g., `Tomek` / `tpylak@google.com`), execute the dedicated engine:

```bash
/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/new_venv/bin/python \
  /usr/local/google/home/odwl/Documents/dev/my-haskell-project/.agents/skills/retrieve-person-chats/scripts/fetch_person_chat.py \
  --person "tpylak@google.com" --limit 15
```
