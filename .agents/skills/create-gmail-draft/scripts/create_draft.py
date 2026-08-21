import os
import sys
import pickle
import base64
import argparse
from email.message import EmailMessage

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
from googleapiclient.discovery import build

def main():
    parser = argparse.ArgumentParser(description="Create a Gmail draft email.")
    parser.add_argument("--to", required=True, help="Recipient email address(es), comma-separated")
    parser.add_argument("--cc", required=False, default=None, help="Cc recipient email address(es), comma-separated")
    parser.add_argument("--subject", required=True, help="Email subject")
    parser.add_argument("--body", required=True, help="Email body content")
    parser.add_argument("--token", default="token.pickle", help="Path to token.pickle file")
    
    args = parser.parse_args()
    
    if not os.path.exists(args.token):
        print(f"Error: Token file '{args.token}' not found.")
        sys.exit(1)
        
    with open(args.token, 'rb') as token_file:
        creds = pickle.load(token_file)
        
    service = build('gmail', 'v1', credentials=creds)
    
    mime_message = EmailMessage()
    mime_message.set_content(args.body)
    mime_message['To'] = args.to
    if args.cc:
        mime_message['Cc'] = args.cc
    mime_message['Subject'] = args.subject
    
    encoded_message = base64.urlsafe_b64encode(mime_message.as_bytes()).decode()
    create_draft_body = {
        'message': {
            'raw': encoded_message
        }
    }
    
    draft = service.users().drafts().create(userId='me', body=create_draft_body).execute()
    print(f"Draft created successfully! Draft ID: {draft['id']}")

if __name__ == '__main__':
    main()
