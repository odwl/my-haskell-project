import os
import sys
import base64
import subprocess
sys.path.append("/usr/local/google/home/odwl/.local/lib/python3.13/site-packages")
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from googleapiclient.http import MediaFileUpload
from google.auth.transport.requests import Request

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 extract_uber.py <email_message_id>")
        sys.exit(1)
        
    msg_id = sys.argv[1]
    token_path = '/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/odewolf_token.json'
    SCOPES = [
        'https://www.googleapis.com/auth/drive',
        'https://www.googleapis.com/auth/gmail.modify',
        'https://www.googleapis.com/auth/calendar'
    ]
    
    creds = Credentials.from_authorized_user_file(token_path, SCOPES)
    if creds and creds.expired and creds.refresh_token:
        creds.refresh(Request())
        with open(token_path, 'w') as token:
            token.write(creds.to_json())
            
    gmail_service = build('gmail', 'v1', credentials=creds)
    drive_service = build('drive', 'v3', credentials=creds)
    
    print(f"Fetching message ID: {msg_id}")
    msg_data = gmail_service.users().messages().get(userId='me', id=msg_id, format='full').execute()
    
    # Extract HTML body
    html_data = ""
    def get_html(parts):
        for part in parts:
            if part['mimeType'] == 'text/html':
                return base64.urlsafe_b64decode(part['body']['data']).decode('utf-8')
            if 'parts' in part:
                res = get_html(part['parts'])
                if res: return res
        return ""

    if msg_data['payload']['mimeType'] == 'text/html':
        html_data = base64.urlsafe_b64decode(msg_data['payload']['body']['data']).decode('utf-8')
    elif 'parts' in msg_data['payload']:
        html_data = get_html(msg_data['payload']['parts'])

    if not html_data:
        print("Could not extract HTML from the email.")
        return
        
    html_file = f'/tmp/email_{msg_id}.html'
    pdf_file = f'/tmp/email_{msg_id}.pdf'
    
    with open(html_file, 'w', encoding='utf-8') as f:
        f.write(html_data)
        
    print("Converting HTML to PDF using Puppeteer...")
    puppeteer_script = f"""
const puppeteer = require('puppeteer');
(async () => {{
  const browser = await puppeteer.launch({{ args: ['--no-sandbox', '--disable-setuid-sandbox'] }});
  const page = await browser.newPage();
  await page.goto('file://{html_file}', {{ waitUntil: 'networkidle0' }});
  await page.pdf({{ path: '{pdf_file}', format: 'A4', printBackground: true }});
  await browser.close();
}})();
"""
    with open('/tmp/puppeteer_test/run.js', 'w') as f:
        f.write(puppeteer_script)
        
    subprocess.run(['node', 'run.js'], cwd='/tmp/puppeteer_test', check=True)
    print(f"PDF generated at {pdf_file}")
    
    # Upload to Google Drive Travel folder
    results = drive_service.files().list(q="mimeType='application/vnd.google-apps.folder' and name contains 'Travel'", spaces='drive', fields='files(id, name)').execute()
    folders = results.get('files', [])
    folder_id = folders[0]['id'] if folders else None
    
    file_metadata = {'name': f'Receipt_{msg_id}.pdf'}
    if folder_id:
        file_metadata['parents'] = [folder_id]
        
    media = MediaFileUpload(pdf_file, mimetype='application/pdf')
    file = drive_service.files().create(body=file_metadata, media_body=media, fields='id').execute()
    print(f"Uploaded to Drive successfully! File ID: {file.get('id')}")

if __name__ == '__main__':
    main()
