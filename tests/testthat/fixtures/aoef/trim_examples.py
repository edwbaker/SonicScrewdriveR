"""Trim the example AOEF files of soundevent to the fixtures here.

soundevent's tests/data/example_evaluation_set.json and
tests/data/example_model_run.json (version 2.10.0) are cut to their first two
clips and what those clips refer to, and the email address of their user is
taken out. Run with the directory holding the two files:

    python trim_examples.py <directory>
"""

import json
import sys
from pathlib import Path

HERE = Path(__file__).parent


def trim(src, dest, clip_key, event_key):
    with open(src, encoding="utf-8") as f:
        doc = json.load(f)
    data = doc["data"]
    clips = data[clip_key][:2]
    clip_ids = {c["clip"] for c in clips}
    event_ids = {e for c in clips for e in c.get("sound_events") or []}
    events = [e for e in data[event_key] if e["uuid"] in event_ids]
    sound_event_ids = {e["sound_event"] for e in events}
    data["clips"] = [c for c in data["clips"] if c["uuid"] in clip_ids]
    recording_ids = {c["recording"] for c in data["clips"]}
    data["recordings"] = [r for r in data["recordings"] if r["uuid"] in recording_ids]
    data["sound_events"] = [s for s in data["sound_events"] if s["uuid"] in sound_event_ids]
    data[clip_key] = clips
    data[event_key] = events
    for user in data.get("users") or []:
        user.pop("email", None)
    with open(dest, "w", encoding="utf-8") as f:
        json.dump(doc, f, ensure_ascii=False, separators=(",", ":"))


source = Path(sys.argv[1])
trim(
    source / "example_evaluation_set.json",
    HERE / "soundevent_example_evaluation_set.json",
    "clip_annotations",
    "sound_event_annotations",
)
trim(
    source / "example_model_run.json",
    HERE / "soundevent_example_model_run.json",
    "clip_predictions",
    "sound_event_predictions",
)
