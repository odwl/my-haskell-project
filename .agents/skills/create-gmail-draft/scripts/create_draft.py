import os
import sys
import pickle
import base64
import argparse
from email.message import EmailMessage

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

def main():
    parser = argparse.ArgumentParser(description="Create a Gmail draft email.")
    parser.add_argument("--to", required=True, help="Recipient email address(es), comma-separated")
    parser.add_argument("--cc", required=False, default=None, help="Cc recipient email address(es), comma-separated")
    parser.add_argument("--subject", required=True, help="Email subject")
    parser.add_argument("--body", required=True, help="Email body content")
    parser.add_argument("--html", required=False, default=None, help="HTML content for email")
    parser.add_argument("--attachment", action="append", help="Path to file attachment (can specify multiple)")
    parser.add_argument("--token", default="token.pickle", help="Path to token.pickle or token.json file")
    
    args = parser.parse_args()
    
    if not os.path.exists(args.token):
        print(f"Error: Token file '{args.token}' not found.")
        sys.exit(1)
        
    if args.token.endswith('.json'):
        creds = Credentials.from_authorized_user_file(args.token)
    else:
        with open(args.token, 'rb') as token_file:
            creds = pickle.load(token_file)
        
    service = build('gmail', 'v1', credentials=creds)
    
    mime_message = EmailMessage()
    mime_message.set_content(args.body)
    if args.html:
        mime_message.add_alternative(args.html, subtype='html')
    mime_message['To'] = args.to
    if args.cc:
        mime_message['Cc'] = args.cc
    mime_message['Subject'] = args.subject
    
    if args.attachment:
        for attach_path in args.attachment:
            if os.path.exists(attach_path):
                with open(attach_path, 'rb') as f:
                    file_data = f.read()
                file_name = os.path.basename(attach_path)
                maintype = 'image' if file_name.lower().endswith(('.png', '.jpg', '.jpeg')) else 'application'
                subtype = 'png' if file_name.lower().endswith('.png') else ('jpeg' if file_name.lower().endswith(('.jpg', '.jpeg')) else 'octet-stream')
                mime_message.add_attachment(
                    file_data,
                    maintype=maintype,
                    subtype=subtype,
                    filename=file_name
                )
    
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
