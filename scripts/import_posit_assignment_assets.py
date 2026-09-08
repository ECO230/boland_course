#!/usr/bin/env python3
"""Import the small, approved assets from local Posit Cloud export ZIPs."""

from __future__ import annotations

import shutil
from pathlib import Path, PurePosixPath
from zipfile import ZipFile


ASSETS = {
    "Lab_1.zip": {
        "Hats_Off.jpg": "week01/labs/media/images/Hats_Off.jpg",
    },
    "Lab_14.zip": {
        "studentathletes.csv": "week14/labs/data/studentathletes.csv",
    },
}


def member_name(archive: ZipFile, relative: str) -> str:
    matches = [
        name
        for name in archive.namelist()
        if not name.endswith("/")
        and PurePosixPath(name).parts[1:] == PurePosixPath(relative).parts
    ]
    if len(matches) != 1:
        raise ValueError(f"Expected one ZIP member for {relative!r}; found {matches}")
    return matches[0]


def main() -> int:
    root = Path(__file__).resolve().parent.parent
    export_root = root / "posit_assignments"
    for archive_name, files in ASSETS.items():
        archive_path = export_root / archive_name
        if not archive_path.is_file():
            raise FileNotFoundError(f"Missing local Posit export: {archive_path}")
        with ZipFile(archive_path) as archive:
            for source, destination in files.items():
                target = root / destination
                target.parent.mkdir(parents=True, exist_ok=True)
                with archive.open(member_name(archive, source)) as input_file:
                    with target.open("wb") as output_file:
                        shutil.copyfileobj(input_file, output_file)
                print(f"Imported {archive_name}:{source} -> {destination}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
