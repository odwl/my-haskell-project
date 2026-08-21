import os
import sys
import pickle

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

# If modifying these scopes, delete the file token.pickle.
SCOPES = [
    'https://www.googleapis.com/auth/gmail.readonly',
    'https://www.googleapis.com/auth/gmail.compose',
    'https://www.googleapis.com/auth/gmail.modify',
    'https://www.googleapis.com/auth/calendar.readonly',
    'https://www.googleapis.com/auth/contacts.readonly',
    'https://www.googleapis.com/auth/documents',
    'https://www.googleapis.com/auth/drive'
]

def main():
    creds = None
    token_path = '/usr/local/google/home/odwl/Documents/dev/my-haskell-project/python/token.pickle'
    
    # Check possible credential file locations
    cred_candidates = [
        '/usr/local/google/home/odwl/Documents/dev/my-haskell-project/python/credentials.json/client_secret_174494759576-knjcl7dvkg0hlnaef6en9uugp5302jtj.apps.googleusercontent.com.json',
        '/usr/local/google/home/odwl/Documents/dev/my-haskell-project/python/credentials.json/client_secret_2_583844170650-di8af78l18g8fah107fr8nj3l7n6b470.apps.googleusercontent.com.json',
        '/usr/local/google/home/odwl/Documents/dev/my-haskell-project/python/credentials.json'
    ]
    
    cred_path = next((p for p in cred_candidates if os.path.isfile(p)), None)
    if not cred_path:
        print("Error: No valid client_secret/credentials.json file found.")
        sys.exit(1)
        
    print(f"Using client secrets from: {cred_path}")
    print(f"Targeting OAuth scopes: {SCOPES}")

    if os.path.exists(token_path):
        print(f"Found existing {token_path}, checking validity and scopes...")
        with open(token_path, 'rb') as token:
            creds = pickle.load(token)

    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid or not all(scope in getattr(creds, 'scopes', []) for scope in SCOPES):
        if creds and creds.expired and creds.refresh_token:
            try:
                print("Attempting to refresh expired token...")
                creds.refresh(Request())
            except Exception as e:
                print(f"Token refresh failed ({e}), initiating full OAuth flow...")
                creds = None
                
        if not creds or not all(scope in getattr(creds, 'scopes', []) for scope in SCOPES):
            print("\nInitiating OAuth flow. A browser window should open or an auth URL will be displayed.")
            flow = InstalledAppFlow.from_client_secrets_file(cred_path, SCOPES)
            creds = flow.run_local_server(port=0)

        # Save the credentials for the next run
        print(f"Saving new authorized token to {token_path}...")
        with open(token_path, 'wb') as token:
            pickle.dump(creds, token)
            
        # Also copy to root just in case
        root_token = '/usr/local/google/home/odwl/Documents/dev/my-haskell-project/token.pickle'
        with open(root_token, 'wb') as token:
            pickle.dump(creds, token)
            
        print("Successfully generated and saved new token.pickle with full read/write/compose scopes!")
    else:
        print("Existing token.pickle is valid and already has all required scopes!")

if __name__ == '__main__':
    main()
