import os
import sys
import datetime
import base64
import re
import json
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

ACCOUNTS = [
    {"name": "Personal (odewolf@gmail.com)", "token_path": "/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/odewolf_token.json"},
    {"name": "Corporate (odwl@google.com)", "token_path": "/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json"}
]

# Upgraded multilingual & corporate keyword query ensuring Bay View, Mountain View, and Silicon Valley stays are never missed!
QUERY = (
    '(flight OR booking OR reservation OR hotel OR resort OR car OR Europcar OR Sixt OR Uber OR restaurant OR dining OR Kieliszki OR Bellavista OR "Made in Asia" OR "La Scarpetta" OR "Bay View" OR "Mountain View" OR Bayview OR Concur OR Shoup OR Ames) '
    'after:2025/06/01 -label:trash -SBB -SwissPass -PubliBike -komoot'
)

def extract_text(payload):
    text = ""
    if "data" in payload.get("body", {}):
        try:
            text += base64.urlsafe_b64decode(payload["body"]["data"]).decode("utf-8", errors="ignore")
        except Exception:
            pass
    if "parts" in payload:
        for part in payload["parts"]:
            text += extract_text(part)
    return text

def clean_text(html_content):
    no_style = re.sub(r'<style[^>]*>.*?</style>', ' ', html_content, flags=re.DOTALL)
    no_script = re.sub(r'<script[^>]*>.*?</script>', ' ', no_style, flags=re.DOTALL)
    no_tags = re.sub(r'<[^>]+>', ' ', no_script)
    clean = re.sub(r'\s+', ' ', no_tags).strip()
    return clean

def main():
    print(f"=== MULTILINGUAL & CORPORATE GMAIL TRAVEL SCANNER ===")
    print(f"Executing upgraded query: {QUERY}\n")
    
    for acc in ACCOUNTS:
        path = acc["token_path"]
        if not os.path.exists(path):
            print(f"[{acc['name']}] Token not found at {path}. Skipping...")
            continue
        print(f"--- Scanning Account: {acc['name']} ---")
        try:
            creds = Credentials.from_authorized_user_file(path)
            service = build('gmail', 'v1', credentials=creds)
            res = service.users().messages().list(userId='me', q=QUERY, maxResults=30).execute()
            messages = res.get('messages', [])
            print(f"Found {len(messages)} matching travel/hospitality items.")
            
            for msg in messages:
                mdata = service.users().messages().get(userId='me', id=msg['id'], format='full').execute()
                headers = mdata.get('payload', {}).get('headers', [])
                subj = max([h['value'] for h in headers if h['name'].lower() == 'subject'], default='No Subject')
                sender = max([h['value'] for h in headers if h['name'].lower() == 'from'], default='Unknown')
                date_str = max([h['value'] for h in headers if h['name'].lower() == 'date'], default='')
                
                body_text = clean_text(extract_text(mdata.get('payload', {})))
                
                # Check for SBB exclusion
                if any(x in subj.lower() for x in ['sbb', 'swisspass', 'publi', 'komoot']):
                    continue
                    
                print(f"  [{date_str[:22]}] From: {sender[:40]} | Subject: {subj[:70]}")
                print(f"    Excerpt: {body_text[:140]}...\n")
        except Exception as e:
            print(f"Error scanning {acc['name']}: {e}\n")

if __name__ == '__main__':
    main()
