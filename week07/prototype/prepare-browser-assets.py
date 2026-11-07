"""Prepare WebR assets from the installed, pinned pure-R eco230r package."""
import argparse
import csv
from pathlib import Path
import tarfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("installed_package", type=Path)
parser.add_argument("output", type=Path, help="Build data directory")
args = parser.parse_args()
package = args.installed_package.resolve()
if "Package: eco230r\n" not in (package / "DESCRIPTION").read_text(encoding="utf-8"):
    parser.error("Expected an installed eco230r package")
if not (package / "LICENSE").is_file() or (package / "libs").exists():
    parser.error("Expected the licensed, pure-R course package")
args.output.mkdir(parents=True, exist_ok=True)
with tarfile.open(args.output / "eco230r-course.tar.gz", "w:gz", format=tarfile.USTAR_FORMAT) as archive:
    archive.add(package, arcname="eco230r")
source = Path(__file__).resolve().parents[2] / "shared/data/airbnb_chicago.csv"
columns = ["bathrooms", "cleaning_fee"]
with source.open(encoding="utf-8-sig", newline="") as src, (
    args.output / "airbnb_chicago.csv"
).open("w", encoding="utf-8", newline="") as dst:
    reader = csv.DictReader(src)
    if not set(columns).issubset(reader.fieldnames or []):
        parser.error("Homework 6 CSV is missing the required columns")
    writer = csv.DictWriter(dst, fieldnames=columns, extrasaction="ignore")
    writer.writeheader()
    writer.writerows(reader)
