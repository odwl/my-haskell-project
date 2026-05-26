import os
import sys
from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

SCOPES = ["https://www.googleapis.com/auth/gmail.readonly"]

def get_emails():
    # Point to the token file in the gmail_mcp directory since it was pushed there
    token_file = '/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json'
    
    if os.path.exists(token_file):
        creds = Credentials.from_authorized_user_file(token_file, SCOPES)
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
            # We don't need to write back if it's read-only, but let's keep it if we can
            # to avoid re-refreshing. Wait, write might fail if we don't have permissions
            # to write to the other folder, but since user pushed it, we should have perms.
            # Let's just try to write back if it refresh, or skip it. Let's skip write back for now
            # as we don't want to corrupt the file if multiple agents are running.
    else:
        print("Error: No auth token found at " + token_file)
        sys.exit(1)

    try:
        service = build('gmail', 'v1', credentials=creds)
        # Fetch last 3 messages
        results = service.users().messages().list(userId='me', maxResults=3).execute()
        messages = results.get('messages', [])

        if not messages:
            print("No messages found.")
            return
        
        for msg in messages:
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
            print(f"Date: {date}")
            print(f"From: {sender}")
            print(f"Subject: {subject}")
            print(f"Snippet: {snippet}")
            print("\n===================================\n")
            
    except Exception as error:
        print(f"An error occurred: {error}")

if __name__ == "__main__":
    get_emails()
