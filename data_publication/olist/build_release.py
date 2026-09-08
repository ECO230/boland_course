#!/usr/bin/env python3
"""Build the immutable local Olist release candidate."""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
SOURCES = {
    "CUSTOMERS": "olist_customers_dataset.csv",
    "ORDER_ITEMS": "olist_order_items_dataset.csv",
    "PAYMENTS": "olist_order_payments_dataset.csv",
    "REVIEWS": "olist_order_reviews_dataset.csv",
    "ORDERS": "olist_orders_dataset.csv",
    "PRODUCTS": "olist_products_dataset.csv",
    "SELLERS": "olist_sellers_dataset.csv",
    "CATEGORY_TRANSLATION": "product_category_name_translation.csv",
    "MARKETING_LEADS": "olist_marketing_qualified_leads_dataset.csv",
    "CLOSED_DEALS": "olist_closed_deals_dataset.csv",
}
ARTIFACTS = {
    "orders": "olist_orders.parquet",
    "order_items": "olist_order_items.parquet",
    "payments": "olist_payments.parquet",
    "reviews": "olist_reviews.parquet",
    "products": "olist_products.parquet",
    "sellers": "olist_sellers.parquet",
    "marketing_leads": "olist_marketing_leads.parquet",
    "order_analysis": "olist_order_analysis.parquet",
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def run_sql(connection: duckdb.DuckDBPyConnection, path: Path, **values: str) -> None:
    started = time.perf_counter()
    sql = path.read_text(encoding="utf-8")
    for key, value in values.items():
        sql = sql.replace("{{" + key + "}}", value)
    if "{{" in sql or "}}" in sql:
        raise ValueError(f"Unresolved SQL placeholder in {path}")
    print(f"Starting {path.name}...", flush=True)
    connection.execute(sql)
    print(f"Completed {path.name} in {time.perf_counter() - started:,.1f}s", flush=True)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", default=".data-build/olist/brazilian-ecommerce-2016-2018-v1/source")
    parser.add_argument("--output", default=".data-build/olist/brazilian-ecommerce-2016-2018-v1")
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}")

    dataset_dir = Path(__file__).resolve().parent
    source_dir = Path(args.source_dir).resolve()
    output_root = Path(args.output).resolve()
    work_dir, release_dir = output_root / "work", output_root / "release"
    if work_dir.exists() or release_dir.exists():
        raise FileExistsError(f"Refusing to overwrite existing build: {output_root}")
    source_paths = {key: source_dir / name for key, name in SOURCES.items()}
    missing = [str(path) for path in [*source_paths.values(), source_dir / "source_manifest.json"] if not path.is_file()]
    if missing:
        raise FileNotFoundError(f"Missing required sources: {missing}")
    work_dir.mkdir(parents=True)
    release_dir.mkdir(parents=True)

    connection = duckdb.connect(str(work_dir / "olist.duckdb"))
    generated = {}
    try:
        run_sql(connection, dataset_dir / "sql" / "10_ingest.sql", **{k: sql_path(v) for k, v in source_paths.items()})
        run_sql(connection, dataset_dir / "sql" / "20_model.sql")
        run_sql(connection, dataset_dir / "sql" / "30_validate.sql")
        output_paths = {name: release_dir / filename for name, filename in ARTIFACTS.items()}
        run_sql(connection, dataset_dir / "sql" / "40_publish.sql", **{f"OUTPUT_{name.upper()}": sql_path(path) for name, path in output_paths.items()})
        for name, path in output_paths.items():
            schema_rows = connection.execute(f"DESCRIBE SELECT * FROM curated.{name}").fetchall()
            generated[name] = {
                "filename": path.name, "sha256": sha256(path), "bytes": path.stat().st_size,
                "rows": connection.execute(f"SELECT count(*) FROM curated.{name}").fetchone()[0],
                "columns": len(schema_rows),
                "schema": [{"name": row[0], "duckdb_type": row[1], "nullable": row[2] == "YES"} for row in schema_rows],
            }
    finally:
        connection.close()

    metadata = json.loads((dataset_dir / "dataset.json").read_text(encoding="utf-8"))
    metadata["release_status"] = "local-candidate"
    metadata["built_at_utc"] = datetime.now(timezone.utc).isoformat()
    metadata["build"] = {"duckdb_version": duckdb.__version__}
    for artifact in metadata["artifacts"]:
        artifact.update(generated[artifact["name"]])
    (release_dir / "metadata.json").write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
    (release_dir / "schemas.json").write_text(json.dumps({k: v["schema"] for k, v in generated.items()}, indent=2) + "\n", encoding="utf-8")
    shutil.copy2(source_dir / "source_manifest.json", release_dir)
    shutil.copy2(dataset_dir / "LICENSE-DATA.txt", release_dir)
    shutil.copy2(dataset_dir / "data_dictionary.md", release_dir / "DATA-DICTIONARY.md")
    for details in generated.values():
        (release_dir / f"{details['filename']}.sha256").write_text(f"{details['sha256']}  {details['filename']}\n", encoding="ascii")
    print(f"Built Olist local candidate at {release_dir}")
    for name, details in generated.items():
        print(f"{name}: {details['rows']:,} rows, {details['bytes']:,} bytes")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
