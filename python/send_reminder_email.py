import os
import sys
import pickle
import base64
from email.message import EmailMessage

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build

def main():
    token_path = '/usr/local/google/home/odwl/Documents/dev/my-haskell-project/token.pickle'
    if not os.path.exists(token_path):
        print('Error: token.pickle not found.')
        return
        
    with open(token_path, 'rb') as token:
        creds = pickle.load(token)
        
    service = build('gmail', 'v1', credentials=creds)
    
    body_text = (
        "Hi Olivier,\n\n"
        "This is your scheduled reminder to ask for the status of the Promote PRD and particularly about the auto-approval of the Merchant Center - CID link.\n\n"
        "Best,\n"
        "Your AI Assistant"
    )
    
    mime_message = EmailMessage()
    mime_message.set_content(body_text)
    mime_message['To'] = 'odwl@google.com'
    mime_message['From'] = 'odwl@google.com'
    mime_message['Subject'] = 'Reminder: Ask status on the Promote PRD'
    
    encoded_message = base64.urlsafe_b64encode(mime_message.as_bytes()).decode()
    
    send_body = {
        'raw': encoded_message
    }
    
    message = service.users().messages().send(userId='me', body=send_body).execute()
    print(f"Reminder email sent successfully! Message ID: {message['id']}")

if __name__ == '__main__':
    main()
