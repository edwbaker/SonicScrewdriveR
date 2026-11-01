"""Write the AOEF fixtures that soundevent itself makes.

Run with soundevent 2.10.0 installed, from this directory:

    python -X utf8 make_fixtures.py

It writes soundevent_annotation_project.json, an annotation project holding a
sound event of every geometry soundevent has, an event with two tags, one with
none, tags and notes on whole clips, a clip annotated as holding nothing, and
two users. The identifiers and times are fixed, so the file differs between
runs only in the time it was written.

soundevent writes with the encoding of the locale, which on Windows is not
UTF-8, hence -X utf8.
"""

import datetime
import uuid
from pathlib import Path

from soundevent import data, io

HERE = Path(__file__).parent
WHEN = datetime.datetime(2026, 10, 30, 12, 0, 0)
NAMESPACE = uuid.UUID("0b8a6f8e-3c55-4d39-9d52-5a3c0e2f0d11")


def ident(name):
    """A fixed identifier for each object, so the file does not change."""
    return uuid.uuid5(NAMESPACE, name)


def tag(key, value):
    return data.Tag(key=key, value=value)


ed = data.User(uuid=ident("user-ed"), username="ebaker", name="Ed Baker")
anon = data.User(uuid=ident("user-anon"), name="A. Recordist")

long_recording = data.Recording(
    uuid=ident("recording-long"),
    path=Path("site1/long.wav"),
    duration=20.0,
    channels=1,
    samplerate=48000,
    date=datetime.date(2026, 5, 1),
    time=datetime.time(5, 30, 0),
    latitude=51.4967,
    longitude=-0.1764,
    rights="CC BY 4.0",
)
short_recording = data.Recording(
    uuid=ident("recording-short"),
    path=Path("site2/short.wav"),
    duration=5.0,
    channels=2,
    samplerate=96000,
    time_expansion=10.0,
)

first_clip = data.Clip(
    uuid=ident("clip-first"), recording=long_recording, start_time=0.0, end_time=10.0
)
second_clip = data.Clip(
    uuid=ident("clip-second"), recording=long_recording, start_time=10.0, end_time=20.0
)
empty_clip = data.Clip(
    uuid=ident("clip-empty"), recording=short_recording, start_time=0.0, end_time=5.0
)

species = tag("species", "Turdus merula")
call = tag("call_type", "song")
wren = tag("species", "Troglodytes troglodytes")
accented = tag("species", "Pipéstrelle")
noisy = tag("quality", "noisy")
nothing = tag("event", "none")


def event(name, recording, geometry, features=None):
    return data.SoundEvent(
        uuid=ident("event-" + name),
        recording=recording,
        geometry=geometry,
        features=[
            data.Feature(term=data.term_from_key(k), value=v)
            for k, v in (features or {}).items()
        ],
    )


def annotated(name, sound_event, tags, user=ed, notes=()):
    return data.SoundEventAnnotation(
        uuid=ident("annotation-" + name),
        sound_event=sound_event,
        tags=tags,
        created_by=user,
        created_on=WHEN,
        notes=[
            data.Note(
                uuid=ident("note-" + name + str(i)),
                message=message,
                created_by=user,
                created_on=WHEN,
            )
            for i, message in enumerate(notes)
        ],
    )


box = annotated(
    "box",
    event(
        "box",
        long_recording,
        data.BoundingBox(coordinates=[1.0, 2000.0, 2.5, 6000.0]),
        {"snr": 12.5},
    ),
    [species, call],
    notes=["Clear song", "Second phrase overlaps"],
)
interval = annotated(
    "interval",
    event("interval", long_recording, data.TimeInterval(coordinates=[3.0, 4.25])),
    [wren],
    user=anon,
)
stamp = annotated(
    "stamp",
    event("stamp", long_recording, data.TimeStamp(coordinates=5.5)),
    [accented],
)
point = annotated(
    "point",
    event("point", long_recording, data.Point(coordinates=[6.0, 4000.0])),
    [species],
)
line = annotated(
    "line",
    event(
        "line",
        long_recording,
        data.LineString(coordinates=[[7.0, 3000.0], [7.5, 5000.0], [8.0, 4000.0]]),
    ),
    [species],
)
polygon = annotated(
    "polygon",
    event(
        "polygon",
        long_recording,
        data.Polygon(
            coordinates=[
                [[11.0, 1000.0], [12.0, 1000.0], [12.0, 3000.0], [11.0, 3000.0], [11.0, 1000.0]]
            ]
        ),
    ),
    [wren],
)
untagged = annotated(
    "untagged",
    event("untagged", long_recording, data.TimeInterval(coordinates=[13.0, 14.0])),
    [],
)
multipoint = annotated(
    "multipoint",
    event(
        "multipoint",
        long_recording,
        data.MultiPoint(coordinates=[[15.0, 2000.0], [15.5, 2500.0]]),
    ),
    [species],
)
multiline = annotated(
    "multiline",
    event(
        "multiline",
        long_recording,
        data.MultiLineString(
            coordinates=[[[16.0, 1000.0], [16.5, 1500.0]], [[17.0, 2000.0], [17.2, 2600.0]]]
        ),
    ),
    [species],
)
multipolygon = annotated(
    "multipolygon",
    event(
        "multipolygon",
        long_recording,
        data.MultiPolygon(
            coordinates=[
                [[[18.0, 500.0], [18.5, 500.0], [18.5, 900.0], [18.0, 500.0]]],
                [[[19.0, 700.0], [19.5, 700.0], [19.5, 1200.0], [19.0, 700.0]]],
            ]
        ),
    ),
    [wren],
)

clip_annotations = [
    data.ClipAnnotation(
        uuid=ident("clip-annotation-first"),
        clip=first_clip,
        tags=[noisy],
        sound_events=[box, interval, stamp, point, line],
        notes=[
            data.Note(
                uuid=ident("note-clip"),
                message="Wind from 8 s",
                created_by=ed,
                created_on=WHEN,
            )
        ],
        created_on=WHEN,
    ),
    data.ClipAnnotation(
        uuid=ident("clip-annotation-second"),
        clip=second_clip,
        sound_events=[polygon, untagged, multipoint, multiline, multipolygon],
        created_on=WHEN,
    ),
    data.ClipAnnotation(
        uuid=ident("clip-annotation-empty"),
        clip=empty_clip,
        tags=[nothing],
        created_on=WHEN,
    ),
]

project = data.AnnotationProject(
    uuid=ident("project"),
    name="Fixture project",
    description="Every geometry soundevent has",
    instructions="Box every call",
    clip_annotations=clip_annotations,
    tasks=[
        data.AnnotationTask(uuid=ident("task-" + c.clip.uuid.hex), clip=c.clip, created_on=WHEN)
        for c in clip_annotations
    ],
    annotation_tags=[species, wren, call],
    created_on=WHEN,
)

io.save(project, HERE / "soundevent_annotation_project.json")
