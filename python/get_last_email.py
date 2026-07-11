import os
import pickle
import sys

# Ensure local user packages are visible
sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

SCOPES = ['https://www.googleapis.com/auth/gmail.readonly']

def main():
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first time.
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
            
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json/client_secret_174494759576-knjcl7dvkg0hlnaef6en9uugp5302jtj.apps.googleusercontent.com.json', SCOPES)
            # Use run_local_server to trigger the browser OAuth consent flow
            creds = flow.run_local_server(port=8080)
        # Save the credentials for the next run
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)

    service = build('gmail', 'v1', credentials=creds)

    # Call the Gmail API to list the most recent message
    results = service.users().messages().list(userId='me', maxResults=1).execute()
    messages = results.get('messages', [])

    if not messages:
        print('No messages found.')
        return

    msg_id = messages[0]['id']
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
