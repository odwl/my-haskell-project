import os
import sys
import pickle
import base64

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

def main():
    if not os.path.exists('token.pickle'):
        print('Error: token.pickle not found.')
        return
        
    with open('token.pickle', 'rb') as token:
        creds = pickle.load(token)
        
    service = build('gmail', 'v1', credentials=creds)
    
    # Search for the same thread
    results = service.users().threads().list(userId='me', q='"Custom Bidding GAds UI scoping"').execute()
    threads = results.get('threads', [])
    if not threads:
        print("No threads found.")
        return
        
    thread = service.users().threads().get(userId='me', id=threads[0]['id'], format='full').execute()
    messages = thread.get('messages', [])
    print(f"Total messages in thread: {len(messages)}")
    
    for i, msg in enumerate(messages):
        payload = msg.get('payload', {})
        headers = payload.get('headers', [])
        sender = next((h['value'] for h in headers if h['name'] == 'From'), 'Unknown')
        date = next((h['value'] for h in headers if h['name'] == 'Date'), '')
        subject = next((h['value'] for h in headers if h['name'] == 'Subject'), 'No Subject')
        print(f"[{i+1}] From: {sender} | Date: {date} | Subject: {subject}")
        if i >= 6: # print details of the last few messages
            body = get_body(payload)
            print(f"Body snippet:\n{body[:500]}\n...")
            print("="*60)

if __name__ == '__main__':
    main()
