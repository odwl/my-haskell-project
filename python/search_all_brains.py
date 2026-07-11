import os
import json
import glob

def main():
    brain_dir = "/usr/local/google/home/odwl/.gemini/jetski/brain"
    pattern = os.path.join(brain_dir, "*", ".system_generated", "logs", "transcript_full.jsonl")
    
    files = glob.glob(pattern)
    print(f"Scanning {len(files)} conversation histories for 'Nick'...")
    
    for path in files:
        conv_id = path.split(os.sep)[-5]
        try:
            with open(path, 'r', encoding='utf-8') as f:
                for idx, line in enumerate(f):
                    data = json.loads(line)
                    if data.get('type') == 'USER_INPUT':
                        content = data.get('content', '')
                        if "Bonjour" in content or "PM nick" in content or "Nick Bone" in content:
                            # Let's verify if there is an untruncated transcript of the chat with Nick
                            print(f"Match found in Conv ID: {conv_id} | Line {idx+1}")
                            print("="*60)
                            print(content[:1500]) # Print first 1500 chars
                            print("="*60)
        except Exception as e:
            pass

if __name__ == '__main__':
    main()
