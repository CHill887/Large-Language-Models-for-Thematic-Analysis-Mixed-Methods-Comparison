# -*- coding: utf-8 -*-
"""
Created on Sat Nov  1 14:26:39 2025

@author: hillc
"""

import os
import json
import datetime
from anthropic import Anthropic

client = Anthropic(api_key=os.getenv("ANTHROPIC_API_KEY"))

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

def inductive_analysis(segments_json, model="claude-sonnet-4-20250514"):
    run_id = "run_" + datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

    system_prompt = (
        "InductiveAnalysis\n"
        "You are a qualitative coding assistant optimised for inductive analysis.\n\n"
        "Task Specification:\n"
        "1) Analyse the transcript and create codes for relevant segment IDs.\n"
        "2) Write a description for each code.\n"
        "3) Add segment IDs and verbatim quotes to each code template.\n"
        "4) Repeat until 15–50 codes are reached.\n"
        "5) Group codes into 4–10 themes according to the theme template below.\n"
        "6) Produce concise outputs and always link codes to quotes with IDs.\n\n"
        "LLM Task Specification\n"
        "INPUTS – Segments JSON: <SEGMENTS_JSON>\n"
        "OUTPUT – JSON OBJECT:\n"
        "{\n"
        '  "_model": "<model_name>",\n'
        '  "_run_id": "<unique_run_id>",\n'
        '  "codes": [\n'
        "    {\n"
        '      "label": "<code_label_#>",\n'
        '      "definition": "<what_this_code_means>",\n'
        '      "instances": [\n'
        '        { "id": "", "quote": "<verbatim_quote>" }\n'
        "      ]\n"
        "    }\n"
        "  ],\n"
        '  "themes": [\n'
        "    {\n"
        '      "label": "<theme_label_#>",\n'
        '      "definition": "<what_this_theme_means>",\n'
        '      "codes": ["<code_label_#>", "<code_label_#>"],\n'
        '      "evidence": [\n'
        '        { "id": "", "quote": "<verbatim_quote_supporting_theme_#>" }\n'
        "      ]\n"
        "    }\n"
        "  ]\n"
        "}\n\n"
        "RecurrentCodingRulesSystemPrimer:\n"
        "1) All codes and themes must have supporting evidence of id and verbatim quotes.\n"
        "2) Cite as many quotes, whether explicit or implied, to each code.\n"
        "3) Coding should be unbiased and based solely on what is stated in the transcript.\n"
        "4) Each transcript segment can be used to support one, multiple, or no codes.\n"
        "5) Both the LLM and researchers conducting the analysis should be blinded to each other's output.\n"
        "6) JSON hygiene: Valid JSON only; include '_model' and '_run_id'; no trailing commas.\n"
        '7) Do not code researcher quotes labelled "R".\n\n'
        "Constraints:\n"
        "[Repeat code blocks until 15-50 codes are created]\n"
        "[Repeat until 4-10 themes listed]\n"
        "Each theme cites at least 3 distinct IDs; verbatim evidence only.\n"
    )

    user_prompt = f"Segments JSON:\n{json.dumps(segments_json, indent=2, ensure_ascii=False)}"

    msg = client.messages.create(
        model=model,
        max_tokens=8000,
        temperature=1,
        system=system_prompt,
        messages=[{"role": "user", "content": [{"type": "text", "text": user_prompt}]}],
    )

    if not msg.content or getattr(msg.content[0], "type", None) != "text":
        return {"_model": model, "_run_id": run_id, "error": "No text content returned by model"}

    raw_content = msg.content[0].text

    if raw_content.startswith("```"):
        raw_content = "\n".join(raw_content.splitlines()[1:-1])

    try:
        output = json.loads(raw_content)
    except json.JSONDecodeError:
        return {"_model": model, "_run_id": run_id, "error": "Invalid JSON returned by model", "raw": raw_content}

    output.setdefault("_model", model)
    output.setdefault("_run_id", run_id)

    return validate_inductive_output(output, segments_json)

if __name__ == "__main__":
    transcript_file = "segments_aligned.json"
    output_file = "inductive_output.json"

    with open(transcript_file, "r", encoding="utf-8") as f:
        segments_json = json.load(f)

    result = inductive_analysis(segments_json)

    with open(output_file, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)

    print(f"Inductive analysis saved to {output_file}")
