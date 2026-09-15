"""Render one repository-owned Canvas Markdown page as a JSON body."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from eco230_canvas.configuration import resolve_section_config
from eco230_canvas.content import _render_operation_body, _section_template_values


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--course-repo", required=True, type=Path)
    parser.add_argument("--source-path", required=True)
    parser.add_argument("--title", required=True)
    parser.add_argument("--config", type=Path)
    parser.add_argument("--section")
    args = parser.parse_args()

    if bool(args.config) != bool(args.section):
        parser.error("--config and --section must be provided together")

    template_values: dict[str, str] = {}
    if args.config and args.section:
        resolved = resolve_section_config(args.config, args.section)
        template_values = _section_template_values(args.course_repo, resolved)

    operation = {
        "title": args.title,
        "content": {
            "body_mode": "render_canvas_markdown",
            "source_path": args.source_path,
        },
    }
    body = _render_operation_body(args.course_repo, operation, template_values)
    print(json.dumps({"body": body}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
