#!/usr/bin/env python3
"""Build an allowlisted student repository from an assignment manifest."""

from __future__ import annotations

import argparse
import json
import shutil
import sys
from pathlib import Path, PurePosixPath


FORBIDDEN_PARTS = {".git", ".Rproj.user", ".Rhistory", ".RData"}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", help="Manifest path relative to the repository root")
    parser.add_argument(
        "--output",
        help="Optional output directory relative to the repository root",
    )
    return parser.parse_args()


def resolve_inside(root: Path, relative: str, label: str) -> Path:
    candidate = (root / relative).resolve()
    if candidate != root and root not in candidate.parents:
        raise ValueError(f"{label} escapes the repository root: {relative}")
    return candidate


def validate_destination(value: str) -> PurePosixPath:
    destination = PurePosixPath(value)
    if destination.is_absolute() or ".." in destination.parts:
        raise ValueError(f"Unsafe destination path: {value}")
    if any(part in FORBIDDEN_PARTS for part in destination.parts):
        raise ValueError(f"Forbidden destination path: {value}")
    return destination


def main() -> int:
    args = parse_args()
    repository_root = Path(__file__).resolve().parent.parent
    manifest_path = resolve_inside(repository_root, args.manifest, "Manifest")

    with manifest_path.open(encoding="utf-8") as handle:
        manifest = json.load(handle)

    if manifest.get("schema_version") != 1:
        raise ValueError("Only assignment manifest schema_version 1 is supported")

    output_relative = args.output or manifest.get("output_directory")
    if not output_relative:
        raise ValueError("The manifest must define output_directory")

    output_path = resolve_inside(repository_root, output_relative, "Output directory")
    build_root = (repository_root / ".assignment-build").resolve()
    if output_path == build_root or build_root not in output_path.parents:
        raise ValueError("Output must be a child of .assignment-build")

    entries = manifest.get("files")
    if not isinstance(entries, list) or not entries:
        raise ValueError("The manifest must contain a non-empty files list")

    planned: list[tuple[Path, PurePosixPath]] = []
    destinations: set[PurePosixPath] = set()

    for entry in entries:
        source_relative = entry.get("source")
        destination_relative = entry.get("destination")
        if not source_relative or not destination_relative:
            raise ValueError("Each files entry requires source and destination")

        source = resolve_inside(repository_root, source_relative, "Source")
        destination = validate_destination(destination_relative)
        if not source.is_file():
            raise FileNotFoundError(f"Manifest source does not exist: {source_relative}")
        if destination in destinations:
            raise ValueError(f"Duplicate destination: {destination_relative}")

        destinations.add(destination)
        planned.append((source, destination))

    if output_path.exists():
        shutil.rmtree(output_path)
    output_path.mkdir(parents=True)

    total_bytes = 0
    for source, destination in planned:
        target = output_path.joinpath(*destination.parts)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
        total_bytes += target.stat().st_size

    for required in manifest.get("required_files", []):
        required_path = validate_destination(required)
        if not output_path.joinpath(*required_path.parts).is_file():
            raise FileNotFoundError(f"Required output is missing: {required}")

    print(f"Built {manifest['id']} at {output_path}")
    print(f"Copied {len(planned)} files ({total_bytes:,} bytes)")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
