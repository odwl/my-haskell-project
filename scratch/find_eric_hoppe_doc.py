import os
import sys
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

SCOPES = ["https://www.googleapis.com/auth/drive", "https://www.googleapis.com/auth/gmail.readonly"]

def main():
    # Use the same token file that jetski uses
    token_file = '/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json'
    if not os.path.exists(token_file):
        print(f"Error: Token file not found at {token_file}")
        return
        
    try:
        creds = Credentials.from_authorized_user_file(token_file, SCOPES)
        
        # Drive Search
        drive_service = build('drive', 'v3', credentials=creds)
        search_query = "Eric Hoppe"
        search_email = "ehoppe@google.com"
        
        print(f"Searching for documents authored by {search_query} ({search_email})...")
        
        results = drive_service.files().list(
            pageSize=100, 
            fields="files(id, name, modifiedTime, owners, webViewLink)",
            orderBy="modifiedTime desc"
        ).execute()
        
        items = results.get('files', [])
        found_doc = False
        if items:
            for item in items:
                owners = item.get('owners', [])
                owner_name = owners[0].get('displayName', '') if owners else ''
                owner_email = owners[0].get('emailAddress', '') if owners else ''
                
                if (search_query.lower() in owner_name.lower()) or (search_email.lower() in owner_email.lower()):
                    print("\n[DRIVE] Last document authored by Eric Hoppe:")
                    print(f"Name:          {item['name']}")
                    print(f"Modified Time: {item['modifiedTime']}")
                    print(f"Owner Name:    {owner_name}")
                    print(f"Owner Email:   {owner_email}")
                    print(f"Link:          {item.get('webViewLink', 'N/A')}")
                    found_doc = True
                    break
        
        if not found_doc:
            print(f"No documents authored by {search_query} found in the 100 most recent files.")

        # Gmail Search
        print(f"\nSearching for last email from {search_email}...")
        gmail_service = build('gmail', 'v1', credentials=creds)
        
        gmail_results = gmail_service.users().messages().list(userId='me', q=f"from:{search_email}", maxResults=1).execute()
        messages = gmail_results.get('messages', [])
        
        if messages:
            msg = messages[0]
            msg_data = gmail_service.users().messages().get(userId='me', id=msg['id'], format='full').execute()
            
            headers = msg_data['payload']['headers']
            subject = next((h['value'] for h in headers if h['name'] == 'Subject'), 'No Subject')
            date = next((h['value'] for h in headers if h['name'] == 'Date'), 'No Date')
            
            print(f"\n[GMAIL] Last email from Eric Hoppe:")
            print(f"Subject: {subject}")
            print(f"Date:    {date}")
            print(f"Snippet: {msg_data['snippet']}")
        else:
            print(f"No emails from {search_email} found.")
            
    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    main()
