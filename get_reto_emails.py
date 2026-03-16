import os
import sys
from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

SCOPES = [
    "https://www.googleapis.com/auth/gmail.readonly",
    "https://www.googleapis.com/auth/calendar.readonly",
    "https://www.googleapis.com/auth/drive.readonly"
]

def get_creds():
    token_file = '/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json'
    if os.path.exists(token_file):
        creds = Credentials.from_authorized_user_file(token_file, SCOPES)
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
            with open(token_file, 'w') as token:
                token.write(creds.to_json())
        return creds
    return None

def main():
    creds = get_creds()
    if not creds:
        print("Error: User is not authenticated.", file=sys.stderr)
        sys.exit(1)
    
    try:
        service = build('gmail', 'v1', credentials=creds)
        results = service.users().messages().list(userId='me', q='from:reto', maxResults=5).execute()
        messages = results.get('messages', [])

        if not messages:
            print("No messages found from reto.")
            return
        
        email_summaries = []
        for msg in messages:
            msg_data = service.users().messages().get(userId='me', id=msg['id'], format='metadata', metadataHeaders=['Subject', 'From', 'Date']).execute()
            
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
            email_summaries.append(f"Date: {date}\nFrom: {sender}\nSubject: {subject}\nSnippet: {snippet}")
        
        print("\n---\n".join(email_summaries))
    
    except Exception as error:
        print(f"An error occurred while fetching emails: {error}", file=sys.stderr)

if __name__ == "__main__":
    main()
