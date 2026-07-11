import os
import pickle
import sys
import datetime

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

SCOPES = [
    'https://www.googleapis.com/auth/gmail.readonly',
    'https://www.googleapis.com/auth/calendar.readonly'
]

def main():
    creds = None
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
            
    scopes_match = creds and all(s in creds.scopes for s in SCOPES)
    
    if not creds or not creds.valid or not scopes_match:
        if creds and creds.expired and creds.refresh_token and scopes_match:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json/client_secret_174494759576-knjcl7dvkg0hlnaef6en9uugp5302jtj.apps.googleusercontent.com.json', SCOPES)
            creds = flow.run_local_server(port=8080)
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)

    # Enable Google Calendar API client
    service = build('calendar', 'v3', credentials=creds)

    now = datetime.datetime.utcnow().isoformat() + 'Z'
    print('Querying live calendar for upcoming events with Reto...')
    
    events_result = service.events().list(
        calendarId='primary', 
        timeMin=now,
        maxResults=10, 
        singleEvents=True,
        orderBy='startTime',
        q='Reto'
    ).execute()
    events = events_result.get('items', [])

    if not events:
        print('No upcoming events with Reto found.')
        return

    print('='*50)
    for event in events:
        start = event['start'].get('dateTime', event['start'].get('date'))
        print(f"Start: {start}")
        print(f"Summary: {event.get('summary', 'No Title')}")
        print(f"Description: {event.get('description', 'No Description')}")
        print('='*50)

if __name__ == '__main__':
    main()
