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
    
    # Query for subject matching the search string
    q = 'travel-qpx-staff OR jstimmel OR ehustvedt'
    print(f"Searching for threads with query: {q}")
    results = service.users().threads().list(userId='me', q=q).execute()
    threads = results.get('threads', [])

    if not threads:
        print('No matching threads found.')
        return

    print(f"Found {len(threads)} matching thread(s):\n" + "="*50)
    for t in threads:
        thread = service.users().threads().get(userId='me', id=t['id']).execute()
        messages = thread.get('messages', [])
        
        # Details of the first message in the thread
        headers = messages[0].get('payload', {}).get('headers', [])
        subject = next((h['value'] for h in headers if h['name'] == 'Subject'), 'No Subject')
        sender = next((h['value'] for h in headers if h['name'] == 'From'), 'Unknown')
        date = next((h['value'] for h in headers if h['name'] == 'Date'), '')
        snippet = messages[0].get('snippet', '')
        
        print(f"Thread ID: {t['id']}")
        print(f"From: {sender}")
        print(f"Date: {date}")
        print(f"Subject: {subject}")
        print(f"Snippet: {snippet}")
        print("="*50)

if __name__ == '__main__':
    main()
