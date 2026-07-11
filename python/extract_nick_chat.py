import json
import os

def main():
    transcript_path = "/usr/local/google/home/odwl/.gemini/jetski/brain/87b7c253-29eb-42a7-997d-9c6d884eadcb/.system_generated/logs/transcript_full.jsonl"
    if not os.path.exists(transcript_path):
        print(f"Error: Transcript file not found at {transcript_path}")
        return
        
    with open(transcript_path, 'r', encoding='utf-8') as f:
        for i, line in f:
            try:
                data = json.loads(line)
                if data.get('type') == 'USER_INPUT':
                    content = data.get('content', '')
                    if "Nick" in content:
                        print(f"Match found in Step {data.get('step_index')}:")
                        print("="*80)
                        print(content)
                        print("="*80)
            except Exception as e:
                pass

if __name__ == '__main__':
    # Fix loop to enumerate properly
    with open("/usr/local/google/home/odwl/.gemini/jetski/brain/87b7c253-29eb-42a7-997d-9c6d884eadcb/.system_generated/logs/transcript_full.jsonl", 'r', encoding='utf-8') as f:
        for idx, line in enumerate(f):
            try:
                data = json.loads(line)
                if data.get('type') == 'USER_INPUT':
                    content = data.get('content', '')
                    if "Nick" in content:
                        print(f"Match found in Line {idx+1} (Step {data.get('step_index', 'N/A')}):")
                        print("="*40)
                        print(content[:300] + " ... [TRUNCATED PREVIEW]")
                        print("="*40)
            except Exception as e:
                pass
