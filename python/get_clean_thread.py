import os
import sys
import pickle
import base64
import re

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build

def get_part_by_mime(payload, mime_target):
    body = ""
    if 'parts' in payload:
        for part in payload['parts']:
            body += get_part_by_mime(part, mime_target)
    else:
        mime_type = payload.get('mimeType', '')
        if mime_type == mime_target:
            data = payload.get('body', {}).get('data', '')
            if data:
                try:
                    body += base64.urlsafe_b64decode(data).decode('utf-8', errors='ignore')
                except Exception:
                    pass
    return body

def get_body(payload):
    body_text = get_part_by_mime(payload, 'text/plain')
    if body_text.strip():
        return body_text
    return get_part_by_mime(payload, 'text/html')

def clean_message_body(body):
    lines = body.splitlines()
    clean_lines = []
    
    for line in lines:
        stripped = line.strip()
        # Look for the start of quoted email history
        if stripped.startswith('On ') and (' wrote:' in stripped or ' wrote:' in stripped.lower()):
            break
        if stripped.startswith('---------- Forwarded message ---------'):
            break
        if stripped.startswith('On ') and stripped.endswith('wrote:'):
            break
        if stripped.startswith('________________________________'):
            break
        clean_lines.append(line)
        
    return "\n".join(clean_lines).strip()

def main():
    if not os.path.exists('token.pickle'):
        print('Error: token.pickle not found.')
        return
        
    with open('token.pickle', 'rb') as token:
        creds = pickle.load(token)
        
    service = build('gmail', 'v1', credentials=creds)
    thread = service.users().threads().get(userId='me', id='19ed6f40507cedfa', format='full').execute()
    
    output = []
    messages = thread.get('messages', [])
    
    output.append(f"# Email Thread: Re: URGENT: Admin access and shared code")
    output.append(f"**Thread ID:** `{thread['id']}` | **Total Messages:** {len(messages)}")
    output.append("\n" + "_"*80 + "\n")
    
    for i, msg in enumerate(messages):
        payload = msg.get('payload', {})
        headers = payload.get('headers', [])
        
        subject = next((h['value'] for h in headers if h['name'] == 'Subject'), 'No Subject')
        sender = next((h['value'] for h in headers if h['name'] == 'From'), 'Unknown')
        date = next((h['value'] for h in headers if h['name'] == 'Date'), '')
        
        body = get_body(payload)
        if not body.strip():
            body = msg.get('snippet', '')
            
        clean_body = clean_message_body(body)
        if not clean_body.strip():
            clean_body = msg.get('snippet', '')
            
        output.append(f"## Email {i+1} of {len(messages)}")
        output.append(f"- **From:** {sender}")
        output.append(f"- **Date:** {date}")
        output.append(f"- **Subject:** {subject}")
        output.append("\n" + "_"*40 + "\n")
        output.append(clean_body.strip())
        output.append("\n" + "="*80 + "\n")
        
    with open('thread_content_clean.txt', 'w') as f:
        f.write("\n".join(output))
        
    print("Thread content written to thread_content_clean.txt successfully.")

if __name__ == '__main__':
    main()
