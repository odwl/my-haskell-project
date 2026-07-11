import os
import sys
import pickle

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build

def main():
    if not os.path.exists('token.pickle'):
        print('Error: token.pickle not found.')
        return
        
    with open('token.pickle', 'rb') as token:
        creds = pickle.load(token)
        
    service = build('gmail', 'v1', credentials=creds)
    
    # Try searching for chats in Gmail
    # Chats are often matching query "label:CHAT" or "is:chat"
    queries = [
        'label:CHAT',
        'is:chat',
        '"Nick Bone"',
        'nicholasbone@google.com'
    ]
    
    for q in queries:
        print(f"Searching Gmail with query: '{q}'")
        results = service.users().messages().list(userId='me', q=q, maxResults=10).execute()
        messages = results.get('messages', [])
        print(f"Found {len(messages)} messages.")
        for msg in messages:
            m = service.users().messages().get(userId='me', id=msg['id'], format='minimal').execute()
            print(f" - Message ID: {msg['id']} | Snippet: {m.get('snippet')}")
        print("="*60)

if __name__ == '__main__':
    main()
