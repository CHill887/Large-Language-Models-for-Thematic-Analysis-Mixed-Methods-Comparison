import os
import json
import datetime
from openai import OpenAI

client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

def validate_inductive_output(output, segments_json):
    codes = output.get("codes", [])
    themes = output.get("themes", [])

    if not (15 <= len(codes) <= 50):
        print(f"Expected 15-50 codes, got {len(codes)}")
    if not (4 <= len(themes) <= 10):
        print(f"Expected 4-10 themes, got {len(themes)}")

    valid_ids = {s["id"] for s in segments_json}
    for code in codes:
        if len(code.get("instances", [])) < 3:
            print(f"Code '{code.get('label')}' has fewer than 3 instances")
        for inst in code.get("instances", []):
            if inst.get("id") not in valid_ids:
                print(f"Invalid id in code '{code.get('label')}'")

    for theme in themes:
        evidence_ids = {ev.get("id") for ev in theme.get("evidence", [])}
        if len(evidence_ids) < 3:
            print(f"Theme '{theme.get('label')}' cites only {len(evidence_ids)} distinct segments")
        for ev in theme.get("evidence", []):
            if ev.get("id") not in valid_ids:
                print(f"Invalid id in theme '{theme.get('label')}'")

    return output

def inductive_analysis(segments_json, model="gpt-5-chat-latest"):
    run_id = "run_" + datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

    system_prompt = {
        "role": "system",
        "content": """InductiveAnalysis
You are a qualitative coding assistant optimised for inductive analysis.

Task Specification:
1) Analyse the transcript and create codes for relevant segment IDs.
2) Write a description for each code.
3) Add segment IDs and verbatim quotes to each code template.
4) Repeat until 15–50 codes are reached.
5) Group codes into 4–10 themes according to the theme template below.
6) Produce concise outputs and always link codes to quotes with IDs.

LLM Task Specification
INPUTS – Segments JSON: <SEGMENTS_JSON>
OUTPUT – JSON OBJECT:
{
  "_model": "<model_name>",
  "_run_id": "<unique_run_id>",
  "codes": [
    {
      "label": "<code_label_#>",
      "definition": "<what_this_code_means>",
      "instances": [
        { "id": "", "quote": "<verbatim_quote>" }
      ]
    }
  ],
  "themes": [
    {
      "label": "<theme_label_#>",
      "definition": "<what_this_theme_means>",
      "codes": ["<code_label_#>", "<code_label_#>"],
      "evidence": [
        { "id": "", "quote": "<verbatim_quote_supporting_theme_#>" }
      ]
    }
  ]
}

RecurrentCodingRulesSystemPrimer:
1) All codes and themes must have supporting evidence of id and verbatim quotes.
2) Cite as many quotes, whether explicit or implied, to each code.
3) Coding should be unbiased and based solely on what is stated in the transcript.
4) Each transcript segment can be used to support one, multiple, or no codes.
5) Both the LLM and researchers conducting the analysis should be blinded to each other's output.
6) JSON hygiene: Valid JSON only; include '_model' and '_run_id'; no trailing commas.           
7) Do not code researcher quotes labelled "R".

Constraints:
[Repeat code blocks until 15-50 codes are created]
[Repeat until 4-10 themes listed] 

"""
    }

    user_prompt = {
        "role": "user",
        "content": f"Segments JSON:\n{json.dumps(segments_json, indent=2)}"
    }

    response = client.chat.completions.create(
        model=model,
        messages=[system_prompt, user_prompt],
        temperature=1
    )

    raw_content = response.choices[0].message.content

    if raw_content.startswith("```"):
        raw_content = "\n".join(raw_content.splitlines()[1:-1])

    try:
        output = json.loads(raw_content)
    except json.JSONDecodeError:
        output = {
            "_model": model,
            "_run_id": run_id,
            "error": "Invalid JSON returned by model",
            "raw": raw_content
        }
        return output

    output.setdefault("_model", model)
    output.setdefault("_run_id", run_id)

    return validate_inductive_output(output, segments_json)

if __name__ == "__main__":
    transcript_file = "my_transcript.json"
    output_file = "inductive_output.json"

    with open(transcript_file, "r") as f:
        segments_json = json.load(f)

    result = inductive_analysis(segments_json)

    with open(output_file, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)

    print(f"Inductive analysis saved to {output_file}")
