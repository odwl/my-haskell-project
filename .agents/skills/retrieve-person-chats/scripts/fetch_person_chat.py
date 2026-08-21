import os
import sys
import datetime
import time
import argparse
import re
import threading
from concurrent.futures import ThreadPoolExecutor
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

TOKEN_PATH = "/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json"
thread_local = threading.local()

def get_service(creds):
    if not hasattr(thread_local, "service"):
        thread_local.service = build('chat', 'v1', credentials=creds)
    return thread_local.service

def resolve_user_map(service, space_name):
    user_map = {}
    try:
        members_res = service.spaces().members().list(parent=space_name, pageSize=50).execute()
        for m in members_res.get('memberships', []):
            info = m.get('member', {})
            uid = info.get('name')
            dname = info.get('displayName', info.get('email', uid or 'Unknown Colleague'))
            email = info.get('email', '').lower()
            if uid:
                # Recognize the user vs the colleague
                if "odewolf" in email or "olivier" in dname.lower():
                    user_map[uid] = f"Olivier de Wolf (You)"
                else:
                    user_map[uid] = dname
    except Exception:
        pass
    return user_map

def format_timestamp(ts_str):
    try:
        dt = datetime.datetime.fromisoformat(ts_str.replace('Z', '+00:00'))
        return dt.strftime('%Y-%m-%d %H:%M:%S UTC')
    except Exception:
        return ts_str

def extract_urls(text):
    urls = re.findall(r'https?://[^\s<>"]+', text)
    return list(set(urls))

def retrieve_chat_with_person(target_person, limit=15):
    start_time = time.perf_counter()
    
    if not os.path.exists(TOKEN_PATH):
        print(f"Error: Corporate authentication token not found at {TOKEN_PATH}")
        return

    creds = Credentials.from_authorized_user_file(TOKEN_PATH)
    if creds and creds.expired and creds.refresh_token:
        creds.refresh(Request())
        with open(TOKEN_PATH, 'w') as f:
            f.write(creds.to_json())

    main_service = get_service(creds)

    # Clean target email / alias
    target_clean = target_person.strip().lower()
    if "@" not in target_clean:
        target_email = f"{target_clean}@google.com"
        target_name = target_clean
    else:
        target_email = target_clean
        target_name = target_clean.split('@')[0]

    print(f"⚡ Launching Ultra-Low-Latency Chat Retrieval for: `{target_email}` / `{target_name}`...")

    results_map = {}
    spaces_checked = 0

    # ---------------------------------------------------------
    # TIER 1: FAST PATH via `findDirectMessage` (~100ms)
    # ---------------------------------------------------------
    t1_start = time.perf_counter()
    dm_space_name = None
    try:
        dm_res = main_service.spaces().findDirectMessage(name=f"users/{target_email}").execute()
        dm_space_name = dm_res.get("name")
        if dm_space_name:
            spaces_checked += 1
            user_map = resolve_user_map(main_service, dm_space_name)
            msgs_res = main_service.spaces().messages().list(parent=dm_space_name, pageSize=limit * 2, orderBy="createTime desc").execute()
            messages = msgs_res.get('messages', [])
            
            for m in messages:
                m_id = m.get('name')
                sender_uid = m.get('sender', {}).get('name', '')
                sender_display = user_map.get(sender_uid, m.get('sender', {}).get('displayName', sender_uid or 'Unknown Sender'))
                results_map[m_id] = (m, "Persistent 1:1 Direct Message (DM)", sender_display)
    except Exception as e:
        # Note: If target isn't an email or findDirectMessage isn't supported for this user, fallback handles it!
        pass
    t1_elapsed = time.perf_counter() - t1_start

    # ---------------------------------------------------------
    # TIER 2: CONCURRENT ROOM & MEETING SCANNER (~300ms)
    # ---------------------------------------------------------
    t2_start = time.perf_counter()
    try:
        # List active recent spaces to catch 1:1 calendar chat rooms and project threads
        sp_res = main_service.spaces().list(pageSize=60).execute()
        candidate_spaces = sp_res.get('spaces', [])
        spaces_checked += len(candidate_spaces)
        
        def scan_space(sp):
            sp_name = sp['name']
            if sp_name == dm_space_name:
                return [] # Already processed via Fast Path!
            
            sp_title = sp.get('displayName', sp.get('spaceType', sp_name))
            local_found = []
            try:
                service = get_service(creds)
                # Quick filtering: Check title or check recent messages for target involvement
                title_match = target_name in sp_title.lower() or target_email in sp_title.lower()
                
                msgs_res = service.spaces().messages().list(parent=sp_name, pageSize=25).execute()
                msgs = msgs_res.get('messages', [])
                
                has_involvement = title_match
                if not has_involvement:
                    for m in msgs:
                        s_info = str(m.get('sender', {})).lower()
                        if target_name in s_info or target_email in s_info:
                            has_involvement = True
                            break
                            
                if has_involvement:
                    user_map = resolve_user_map(service, sp_name)
                    for m in msgs:
                        sender_uid = m.get('sender', {}).get('name', '')
                        sender_display = user_map.get(sender_uid, m.get('sender', {}).get('displayName', sender_uid or 'Unknown Sender'))
                        local_found.append((m, sp_title, sender_display))
            except Exception:
                pass
            return local_found

        with ThreadPoolExecutor(max_workers=15) as executor:
            for room_msgs in executor.map(scan_space, candidate_spaces):
                for m, title, sender in room_msgs:
                    m_id = m.get('name')
                    if m_id not in results_map:
                        results_map[m_id] = (m, title, sender)
    except Exception as e:
        print(f"Notice during room scan: {e}")
    t2_elapsed = time.perf_counter() - t2_start

    # ---------------------------------------------------------
    # TIER 3: SORT & PRESENTATION
    # ---------------------------------------------------------
    total_elapsed = time.perf_counter() - start_time
    
    all_conversations = list(results_map.values())
    all_conversations.sort(key=lambda x: x[0].get('createTime', ''), reverse=True)
    top_selection = all_conversations[:limit]

    print("\n====================================================================")
    print(f"🏁 RETRIEVAL COMPLETED IN: {total_elapsed:.3f} SECONDS (Checked {spaces_checked} spaces)")
    print(f"   ├─ Fast Path 1:1 DM Lookup: {t1_elapsed:.3f}s")
    print(f"   └─ Parallel Room & Meeting Scan: {t2_elapsed:.3f}s")
    print("====================================================================\n")

    if not top_selection:
        print(f"No recent chat messages involving `{target_person}` were found in active corporate spaces.")
        return

    print(f"💬 Top {len(top_selection)} Most Recent Chat Exchanges involving `{target_person}`:\n")
    
    # Reverse to show in forward chronological order (oldest -> newest at bottom)
    for idx, (msg, title, sender) in enumerate(reversed(top_selection), 1):
        ts = format_timestamp(msg.get('createTime', ''))
        text = msg.get('text', '').strip()
        urls = extract_urls(text)
        
        print(f"[{ts}] 📍 Room/Thread: {title}")
        print(f"   👤 {sender}: {text}")
        if urls:
            for u in urls:
                print(f"   🌐 Interactive URL: {u}")
        print("-" * 65)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Ultra-fast Google Chat retrieval engine.")
    parser.add_argument("--person", "-p", required=True, help="Target person alias or email (e.g., tpylak or tpylak@google.com)")
    parser.add_argument("--limit", "-l", type=int, default=15, help="Number of recent messages to retrieve")
    args = parser.parse_args()
    
    retrieve_chat_with_person(args.person, args.limit)
