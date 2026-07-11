import json
import os

def main():
    other_conv_id = "f01190f5-9502-491c-8535-bbccd06fb626"
    transcript_path = f"/usr/local/google/home/odwl/.gemini/jetski/brain/{other_conv_id}/.system_generated/logs/transcript_full.jsonl"
    if not os.path.exists(transcript_path):
        print(f"Transcript for {other_conv_id} not found.")
        return
        
    with open(transcript_path, 'r', encoding='utf-8') as f:
        for idx, line in enumerate(f):
            try:
                data = json.loads(line)
                if data.get('type') == 'USER_INPUT':
                    content = data.get('content', '')
                    if "Nick" in content:
                        print(f"Match found in {other_conv_id} Line {idx+1}:")
                        print("="*80)
                        print(content)
                        print("="*80)
            except Exception as e:
                pass

if __name__ == '__main__':
    main()
