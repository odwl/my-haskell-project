import os
import pickle
import sys
import base64
from email.message import EmailMessage
from mcp.server.fastmcp import FastMCP

sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
sys.path.append(os.path.expanduser("~/.local/lib/python3.13/site-packages"))

from googleapiclient.discovery import build

mcp = FastMCP("LocalGmail")

TOKEN_PATH = '/usr/local/google/home/odwl/Documents/dev/my-haskell-project/token.pickle'

def get_gmail_service():
    if not os.path.exists(TOKEN_PATH):
        raise ValueError("token.pickle not found. Please run authenticate first.")
    with open(TOKEN_PATH, 'rb') as token:
        creds = pickle.load(token)
    return build('gmail', 'v1', credentials=creds)

def get_people_service():
    if not os.path.exists(TOKEN_PATH):
        raise ValueError("token.pickle not found. Please run authenticate first.")
    with open(TOKEN_PATH, 'rb') as token:
        creds = pickle.load(token)
    return build('people', 'v1', credentials=creds)

@mcp.tool()
def get_recent_emails(max_results: int = 5) -> str:
    """Fetch the list of recent emails from Gmail.
    
    Args:
        max_results: The number of emails to retrieve (default 5).
    """
    service = get_gmail_service()
    results = service.users().messages().list(userId='me', maxResults=max_results).execute()
    messages = results.get('messages', [])
    
    if not messages:
        return "No emails found."
        
    out = []
    for i, msg in enumerate(messages):
        message = service.users().messages().get(userId='me', id=msg['id'], format='full').execute()
        headers = message.get('payload', {}).get('headers', [])
        subject = next((h['value'] for h in headers if h['name'] == 'Subject'), 'No Subject')
        sender = next((h['value'] for h in headers if h['name'] == 'From'), 'Unknown')
        date = next((h['value'] for h in headers if h['name'] == 'Date'), '')
        snippet = message.get('snippet', '')
        
        out.append(f"[{i+1}] From: {sender}\nDate: {date}\nSubject: {subject}\nSnippet: {snippet}\n" + "-"*40)
    return "\n".join(out)

@mcp.tool()
def create_gmail_draft(to_email: str, subject: str, body: str) -> str:
    """Create a new draft in Gmail.
    
    Args:
        to_email: Recipient email address.
        subject: Subject line of the email.
        body: Text body of the email.
    """
    service = get_gmail_service()
    mime_message = EmailMessage()
    mime_message.set_content(body)
    mime_message['To'] = to_email
    mime_message['From'] = 'me'
    mime_message['Subject'] = subject

    encoded_message = base64.urlsafe_b64encode(mime_message.as_bytes()).decode()
    create_draft_body = {'message': {'raw': encoded_message}}
    
    draft = service.users().drafts().create(userId='me', body=create_draft_body).execute()
    return f"Draft created successfully! Draft ID: {draft['id']}"

@mcp.tool()
def lookup_person_profile(query: str) -> str:
    """Lookup a person's profile in the Google Directory by name or email.
    
    Args:
        query: Name or email address of the person (e.g. 'Reto Strobl' or 'lagily@google.com').
    """
    service = get_people_service()
    results = service.people().searchDirectoryPeople(
        query=query,
        readMask='names,emailAddresses,phoneNumbers,organizations,relations',
        sources=['DIRECTORY_SOURCE_TYPE_DOMAIN_PROFILE', 'DIRECTORY_SOURCE_TYPE_DOMAIN_CONTACT']
    ).execute()
    
    people = results.get('people', [])
    if not people:
        return f"No results found for '{query}' in Google Directory."
        
    out = []
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
        relations_str = [f"{r.get('type')}: {r.get('person')}" for r in relations]
        
        out.append(
            f"Name: {name}\n"
            f"Email: {email}\n"
            f"Phone: {phone}\n"
            f"Job Title: {title}\n"
            f"Department: {dept}\n"
            f"Relations: {', '.join(relations_str)}\n"
            + "-"*40
        )
    return "\n".join(out)

if __name__ == "__main__":
    mcp.run()
