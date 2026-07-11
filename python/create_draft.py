import os
import pickle
import sys
import base64
from email.message import EmailMessage

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build
from google.auth.transport.requests import Request

def main():
    creds = None
    token_path = 'token.pickle'
    if os.path.exists(token_path):
        with open(token_path, 'rb') as token:
            creds = pickle.load(token)
            
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            print("Error: credentials not found or invalid.")
            return

    service = build('gmail', 'v1', credentials=creds)

    body_text = (
        "Hi Ali, Dave,\n\n"
        "Hope you're doing well.\n\n"
        "I wanted to check in on the YCP to Google Ads DG Prefill proposal (https://docs.google.com/document/d/1x6hK2Itvd9ImuIJCvvadFf0l3JJzethZTmFV7t6SLfs/edit?resourcekey=0-QzMB68yhkZGtymBITH2rhw&tab=t.0) that Anton shared last week. First of all, wanted to check if you are both fully aware of this?\n\n"
        "A few specific questions as we look ahead:\n\n"
        "*   For Ali:\n"
        "    *   Do we more or less know what we should expect for the mocks (e.g., a simple prefill button)?\n"
        "    *   I guess enabling this for only Google Ads and not DV3 shouldn't be a problem, right?\n"
        "*   For Dave: Is the prefill strategy fairly straightforward from your side, or are there potential surprises we should watch out for?\n"
        "*   For both:\n"
        "    *   The thread mentions the possibility of prefilling multiple videos/packages (since there's a parallel video packaging workstream). Have you thought about how we'd handle prefilling multiple videos, both visually (mocks) and from the prefill side?\n"
        "    *   The thread also mentions a button/mechanism for prefilling affiliation (Affiliate Boost campaigns). Have we started thinking about how this fits in, both from a mocks and prefill perspective?\n\n"
        "Let me know what you think, or if we should schedule a quick sync.\n\n"
        "Olivier"
    )

    mime_message = EmailMessage()
    mime_message.set_content(body_text)
    mime_message['To'] = 'lagily@google.com, djbr@google.com'
    mime_message['From'] = 'odwl@google.com'
    mime_message['Subject'] = 'Status check: YCP/DG prefill alignment'

    encoded_message = base64.urlsafe_b64encode(mime_message.as_bytes()).decode()

    create_draft_body = {
        'message': {
            'raw': encoded_message
        }
    }

    draft = service.users().drafts().create(userId='me', body=create_draft_body).execute()
    print(f'Draft created successfully! Draft ID: {draft["id"]}')

if __name__ == '__main__':
    main()
