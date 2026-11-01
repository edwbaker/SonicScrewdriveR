# AOEF fixtures

Files in the Acoustic Objects Exchange Format, for the tests of `readAOEF()` and
`writeAOEF()`. All of them were written by soundevent itself.

- `soundevent_annotation_project.json` is written by `make_fixtures.py` with
  soundevent 2.10.0: an annotation project with a sound event of every shape
  soundevent has, an event with two tags and one with none, tags and notes on
  clips, a clip annotated as holding nothing, two users, and paths written on
  Windows, with backslashes.
- `soundevent_example_evaluation_set.json` and
  `soundevent_example_model_run.json` are soundevent's own example files
  `tests/data/example_evaluation_set.json` and `tests/data/example_model_run.json`
  at version 2.10.0, cut to their first two clips by `trim_examples.py`, which
  also takes out the email address of their user. The model run is in the form
  written before soundevent gave the model a field of its own.
- `soundevent_nips4b_plus_sample.json` is soundevent's
  `docs/user_guide/nips4b_plus_sample.json` at version 2.10.0, unchanged: clips
  annotated as holding nothing.

The files taken from soundevent are under its licence:

```
MIT License

Copyright (c) 2023 Santiago Martinez Balvanera

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
