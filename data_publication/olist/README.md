# Olist Marketplace Customer Experience Dataset

This directory defines the ECO 230 curated release of the real, anonymized
Brazilian e-commerce and marketing-funnel datasets published by Olist.

## Curated tables

- `olist_orders.parquet`: one row per order, including customer geography and
  order/delivery timestamps;
- `olist_order_items.parquet`: one row per order item with product, seller,
  price, and freight fields;
- `olist_payments.parquet`: one row per order/payment sequence;
- `olist_reviews.parquet`: one row per order with aggregated review scores and
  no review text;
- `olist_products.parquet`: one row per product with category and physical
  attributes;
- `olist_sellers.parquet`: one row per seller with coarse geography;
- `olist_marketing_leads.parquet`: one row per sampled marketing-qualified
  lead, including acquisition and closed-deal attributes;
- `olist_order_analysis.parquet`: one row per order with carefully defined
  price, freight, payment, review, and delivery summaries.

Release-local keys replace the source identifiers. Customer and seller names,
addresses, review comments, and sales-representative identifiers are not
included. The source contains no student data.

## Published release

The verified release was published on September 6, 2026, at:

```text
https://data.60land.com/project1/2026-fall/v1/olist/
```

## Build locally

The downloader uses Kaggle's dataset-download API. Public Kaggle downloads may
require a free account and credentials in `%USERPROFILE%\.kaggle\kaggle.json`.
Existing downloaded ZIP files can instead be supplied explicitly.

```powershell
python data_publication/olist/download_sources.py
python data_publication/olist/build_release.py
python data_publication/olist/verify_release.py
```

Or with existing source archives:

```powershell
python data_publication/olist/download_sources.py `
  --commerce-archive C:\path\brazilian-ecommerce.zip `
  --marketing-archive C:\path\marketing-funnel-olist.zip
```

Generated files remain under `.data-build/olist/` and outside Git.

## License

Both Olist Kaggle datasets are identified as CC BY-NC-SA 4.0. The adapted
release is for noncommercial educational use, preserves attribution, and is
offered under the same license. See `LICENSE-DATA.txt`.
