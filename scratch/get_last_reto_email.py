import os
import sys
from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

SCOPES = ["https://www.googleapis.com/auth/gmail.readonly"]

def get_clean_last_reto_email():
    token_file = '/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json'
    
    if os.path.exists(token_file):
        creds = Credentials.from_authorized_user_file(token_file, SCOPES)
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
    else:
        print("Error: No auth token found at " + token_file)
        sys.exit(1)

    try:
        service = build('gmail', 'v1', credentials=creds)
        # Query specifically for direct emails from reto@google.com
        results = service.users().messages().list(userId='me', q='from:reto@google.com', maxResults=1).execute()
        messages = results.get('messages', [])

        if not messages:
            print("No direct messages from reto@google.com found.")
            return
        
        msg = messages[0]
        msg_data = service.users().messages().get(userId='me', id=msg['id'], format='full').execute()
        
        subject = ""
        sender = ""
        date = ""
        for header in msg_data['payload']['headers']:
            if header['name'] == 'Subject':
                subject = header['value']
            elif header['name'] == 'From':
                sender = header['value']
            elif header['name'] == 'Date':
                date = header['value']
        
        snippet = msg_data.get('snippet', '')
        
        # Extract body content safely
        body = ""
        if 'parts' in msg_data['payload']:
            parts = msg_data['payload']['parts']
            for part in parts:
                if part['mimeType'] == 'text/plain':
                    import base64
                    body = base64.urlsafe_b64decode(part['body']['data'].encode('ASCII')).decode('utf-8')
                    break
        elif 'body' in msg_data['payload'] and 'data' in msg_data['payload']['body']:
            import base64
            body = base64.urlsafe_b64decode(msg_data['payload']['body']['data'].encode('ASCII')).decode('utf-8')
            
        print(f"Date: {date}")
        print(f"From: {sender}")
        print(f"Subject: {subject}")
        print(f"Snippet: {snippet}")
        if body:
            # Print first 30 lines to avoid massive nested threads
            body_lines = body.split('\n')
            print("\nBody (First 30 lines):")
            for line in body_lines[:30]:
                print(line)
        else:
            print("Body: (HTML only)")
            
    except Exception as error:
        print(f"An error occurred: {error}")

if __name__ == "__main__":
    get_clean_last_reto_email()
