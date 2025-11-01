import json
import os
import csv
from openai import OpenAI

openai_api_key = os.getenv("OPENAI_API_KEY")
client = OpenAI(api_key=openai_api_key)

DEDUCTIVE_SYSTEM_PROMPT = """
You are an expert qualitative methodologist (Braun & Clarke aligned). 
1) Analyze the transcript according to the codes listed for each theme.
2) List segment IDs and verbatim quotes as evidence for each code within the themes.
3) List codes as evidence for each theme. Each code and its evidence should be listed according to its theme.
4) Make only claims backed by transcript evidence.
5) Cite segment IDs for every quote.
6) Output valid JSON exactly as specified.
7) Cite all instances of each code.
"""

RECURRENT_CODING_RULES_PROMPT = """
Recurrent Coding Rules:
1) All codes and themes must have supporting evidence of segment ID and verbatim quotes.
2) Cite as many quotes, whether explicit or implied, to each code.
3) Coding should be unbiased, based only on what is stated in the transcript.
4) Each transcript segment can support one, multiple, or no codes.
5) Both the LLM and researchers conducting the analysis should be blinded to each other's output.
6) JSON hygiene: Valid JSON only; include '_model' and '_run_id'; no trailing commas.
7) Do not code researcher quotes labelled "R"
"""

TASK_SPEC_PROMPT = """
TaskSpec(Inputs,Outputs,Constraints,Metrics):
INPUTS - Transcript text: <SEGMENTS_JSON>
OUTPUT(JSON OBJECT):
{
    "_model": "<model_name>",
    "_run_id": "<run_id>",
    "themes": [
        {
            "label": "TailoringCare forMultimorbidity",
            "codes": [
                {
                    "label": "Needforpersonalisation",
                    "instances": [
                        {"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}
                    ]
                },
                {
                    "label": "Patientexpertise",
                    "instances": [
                        {"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}
                    ]
                }
            ]
        },
        {
            "label": "Perceptionsof theHealthSystem",
            "codes": [
                {"label": "Trust/mistrust", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]},
                {"label": "Carefragmentation", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]}
            ]
        },
        {
            "label": "RoleofAIinHealthcare",
            "codes": [
                {"label": "Systemintegration", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]},
                {"label": "HumanvAIinteractionsinhealthcare", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]}
            ]
        },
        {
            "label": "DataSecurityandEthicsinAI",
            "codes": [
                {"label": "Privacyanddataconcerns", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]},
                {"label": "Consentandtransparency", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]}
            ]
        },
        {
            "label": "GroupCohesion",
            "codes": [
                {"label": "Positivepeersupport", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]}
            ]
        },
        {
            "label": "Co-production",
            "codes": [
                {"label": "Collaborativeinvolvementofpatientsindesign", "instances": [{"segment_id": "<T###>", "quote": "<verbatim_quote_here>"}]}
            ]
        }
    ]
}

"""

def validate_segments(segments_json):
    for s in segments_json:
        if "segment_id" not in s and "id" in s:
            s["segment_id"] = s.pop("id")
        if "segment_id" not in s or "text" not in s:
            raise ValueError(f"Segment missing required keys: {s}")
    return segments_json

def deductive_analysis(segments_json):
    segments_json = validate_segments(segments_json)
    prompt = f"{DEDUCTIVE_SYSTEM_PROMPT}\n{RECURRENT_CODING_RULES_PROMPT}\n{TASK_SPEC_PROMPT}\n\nTranscript:\n{json.dumps(segments_json, indent=2)}"
    response = client.chat.completions.create(
        model="gpt-5",
        messages=[{"role": "user", "content": prompt}]
    )
    raw_output = response.choices[0].message.content
    print(" Raw model output preview:\n", raw_output[:500])
    try:
        result = json.loads(raw_output)
    except json.JSONDecodeError:
        raise ValueError(" Invalid JSON returned from model.")
    return result

def export_to_csv(segments_json, analysis_json, csv_file_path):
    code_columns = []
    for theme in analysis_json["themes"]:
        theme_label = theme["label"]
        for code in theme["codes"]:
            col_name = f"{theme_label}: {code['label']}"
            code_columns.append((theme_label, code['label'], col_name))

    rows = []
    for segment in segments_json:
        segment_id = segment.get("segment_id", segment.get("id"))
        row = {"id": segment_id}
        if "speaker_label" in segment:
            row["speaker_label"] = segment["speaker_label"]
        for _, _, col_name in code_columns:
            row[col_name] = 0
        rows.append(row)

    for theme in analysis_json["themes"]:
        for code in theme["codes"]:
            for instance in code["instances"]:
                seg_id = instance["segment_id"]
                for row in rows:
                    if row["id"] == seg_id:
                        col_name = f"{theme['label']}: {code['label']}"
                        row[col_name] = 1
                        break

    fieldnames = ["id", "speaker_label"] + [col_name for _, _, col_name in code_columns]
    with open(csv_file_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    print(f" CSV exported to {csv_file_path}")

if __name__ == "__main__":
    transcript_file = "my_transcript.json"
    output_file = "deductive_output.json"

    with open(transcript_file, "r", encoding="utf-8") as f:
        segments_json = json.load(f)

    result = deductive_analysis(segments_json)

    with open(output_file, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)

    print(f" Deductive analysis complete. See {output_file}")

    csv_file = "deductive_output.csv"
    export_to_csv(segments_json, result, csv_file)
