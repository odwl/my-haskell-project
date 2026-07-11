import os
import pickle
import sys

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

SCOPES = [
    'https://www.googleapis.com/auth/gmail.readonly',
    'https://www.googleapis.com/auth/calendar.readonly',
    'https://www.googleapis.com/auth/directory.readonly'
]

def main():
    creds = None
    script_dir = os.path.dirname(os.path.abspath(__file__))
    token_path = os.path.join(script_dir, 'token.pickle')
    credentials_path = os.path.join(script_dir, 'credentials.json/client_secret_174494759576-knjcl7dvkg0hlnaef6en9uugp5302jtj.apps.googleusercontent.com.json')

    if os.path.exists(token_path):
        with open(token_path, 'rb') as token:
            creds = pickle.load(token)
            
    scopes_match = creds and all(s in creds.scopes for s in SCOPES)
    
    if not creds or not creds.valid or not scopes_match:
        if creds and creds.expired and creds.refresh_token and scopes_match:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                credentials_path, SCOPES)
            creds = flow.run_local_server(port=8080)
        with open(token_path, 'wb') as token:
            pickle.dump(creds, token)

    service = build('people', 'v1', credentials=creds)

    print('Searching Google Directory for "Reto Strobl"...')
    try:
        results = service.people().searchDirectoryPeople(
            query='Reto Strobl',
            readMask='names,emailAddresses,phoneNumbers,organizations,relations',
            sources=['DIRECTORY_SOURCE_TYPE_DOMAIN_PROFILE', 'DIRECTORY_SOURCE_TYPE_DOMAIN_CONTACT']
        ).execute()
        
        people = results.get('people', [])
        if not people:
            print('No profile found in Directory.')
            return
            
        print('='*50)
        for person in people:
            names = person.get('names', [])
            name = names[0].get('displayName') if names else 'No Name'
            emails = person.get('emailAddresses', [])
            email = emails[0].get('value') if emails else 'No Email'
            phones = person.get('phoneNumbers', [])
            phone = phones[0].get('value') if phones else 'No Phone'
            
            orgs = person.get('organizations', [])
            title = orgs[0].get('title') if orgs else 'No Title'
            dept = orgs[0].get('department') if orgs else 'No Department'
            
            relations = person.get('relations', [])
            relations_str = []
            for r in relations:
                relations_str.append(f"{r.get('type')}: {r.get('person')}")
                
            print(f"Name: {name}")
            print(f"Email: {email}")
            print(f"Phone: {phone}")
            print(f"Job Title: {title}")
            print(f"Department: {dept}")
            if relations_str:
                print(f"Relations: {', '.join(relations_str)}")
            print('='*50)
            
    except Exception as e:
        print(f"Error querying People API: {e}")

if __name__ == '__main__':
    main()
