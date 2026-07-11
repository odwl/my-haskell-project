import os
import pickle
import sys

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build

def main():
    if not os.path.exists('token.pickle'):
        print('Error: token.pickle not found. Run get_last_email.py first to authenticate.')
        return
        
    with open('token.pickle', 'rb') as token:
        creds = pickle.load(token)

    service = build('gmail', 'v1', credentials=creds)

    # Fetch 2 messages to get the second last one
    results = service.users().messages().list(userId='me', maxResults=2).execute()
    messages = results.get('messages', [])

    if len(messages) < 2:
        print('No second last email found.')
        return

    msg_id = messages[1]['id']
    message = service.users().messages().get(userId='me', id=msg_id, format='full').execute()
    
    payload = message.get('payload', {})
    headers = payload.get('headers', [])
    
    subject = ''
    sender = ''
    date = ''
    for h in headers:
        if h['name'] == 'Subject':
            subject = h['value']
        elif h['name'] == 'From':
            sender = h['value']
        elif h['name'] == 'Date':
            date = h['value']
            
    snippet = message.get('snippet', '')
    
    print('='*50)
    print(f'From: {sender}')
    print(f'Date: {date}')
    print(f'Subject: {subject}')
    print('='*50)
    print(f'Snippet: {snippet}')
    print('='*50)

if __name__ == '__main__':
    main()
