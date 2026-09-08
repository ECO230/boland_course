CREATE SCHEMA IF NOT EXISTS curated;

CREATE OR REPLACE TABLE curated.orders AS
SELECT
  'ord_' || substr(sha256('olist-order:' || o.order_id), 1, 24) AS order_key,
  'cus_' || substr(sha256('olist-customer:' || c.customer_unique_id), 1, 24) AS customer_key,
  lower(nullif(trim(o.order_status), '')) AS order_status,
  try_cast(o.order_purchase_timestamp AS TIMESTAMP) AS purchased_at,
  try_cast(o.order_approved_at AS TIMESTAMP) AS approved_at,
  try_cast(o.order_delivered_carrier_date AS TIMESTAMP) AS delivered_to_carrier_at,
  try_cast(o.order_delivered_customer_date AS TIMESTAMP) AS delivered_to_customer_at,
  try_cast(o.order_estimated_delivery_date AS TIMESTAMP) AS estimated_delivery_at,
  upper(nullif(trim(c.customer_state), '')) AS customer_state,
  lower(nullif(trim(c.customer_city), '')) AS customer_city,
  lpad(nullif(trim(c.customer_zip_code_prefix), ''), 5, '0') AS customer_zip_prefix
FROM raw.orders o
JOIN raw.customers c USING (customer_id);

CREATE OR REPLACE TABLE curated.order_items AS
SELECT
  'ord_' || substr(sha256('olist-order:' || order_id), 1, 24) AS order_key,
  try_cast(order_item_id AS INTEGER) AS order_item_number,
  'prd_' || substr(sha256('olist-product:' || product_id), 1, 24) AS product_key,
  'sel_' || substr(sha256('olist-seller:' || seller_id), 1, 24) AS seller_key,
  try_cast(shipping_limit_date AS TIMESTAMP) AS shipping_limit_at,
  try_cast(price AS DECIMAL(12,2)) AS item_price_brl,
  try_cast(freight_value AS DECIMAL(12,2)) AS freight_value_brl
FROM raw.order_items;

CREATE OR REPLACE TABLE curated.payments AS
SELECT
  'ord_' || substr(sha256('olist-order:' || order_id), 1, 24) AS order_key,
  try_cast(payment_sequential AS INTEGER) AS payment_sequence,
  lower(nullif(trim(payment_type), '')) AS payment_type,
  try_cast(payment_installments AS INTEGER) AS payment_installments,
  try_cast(payment_value AS DECIMAL(12,2)) AS payment_value_brl
FROM raw.payments;

CREATE OR REPLACE TABLE curated.reviews AS
SELECT
  'ord_' || substr(sha256('olist-order:' || order_id), 1, 24) AS order_key,
  count(*)::INTEGER AS review_record_count,
  round(avg(try_cast(review_score AS DOUBLE)), 3) AS mean_review_score,
  min(try_cast(review_score AS INTEGER)) AS minimum_review_score,
  max(try_cast(review_score AS INTEGER)) AS maximum_review_score,
  min(try_cast(review_creation_date AS TIMESTAMP)) AS first_review_created_at,
  max(try_cast(review_answer_timestamp AS TIMESTAMP)) AS last_review_answered_at
FROM raw.reviews
GROUP BY order_id;

CREATE OR REPLACE TABLE curated.products AS
SELECT
  'prd_' || substr(sha256('olist-product:' || p.product_id), 1, 24) AS product_key,
  lower(nullif(trim(p.product_category_name), '')) AS product_category_portuguese,
  lower(nullif(trim(t.product_category_name_english), '')) AS product_category_english,
  try_cast(p.product_name_lenght AS INTEGER) AS product_name_length,
  try_cast(p.product_description_lenght AS INTEGER) AS product_description_length,
  try_cast(p.product_photos_qty AS INTEGER) AS product_photo_count,
  try_cast(p.product_weight_g AS DOUBLE) AS product_weight_g,
  try_cast(p.product_length_cm AS DOUBLE) AS product_length_cm,
  try_cast(p.product_height_cm AS DOUBLE) AS product_height_cm,
  try_cast(p.product_width_cm AS DOUBLE) AS product_width_cm
FROM raw.products p
LEFT JOIN raw.category_translation t USING (product_category_name);

CREATE OR REPLACE TABLE curated.sellers AS
SELECT
  'sel_' || substr(sha256('olist-seller:' || seller_id), 1, 24) AS seller_key,
  upper(nullif(trim(seller_state), '')) AS seller_state,
  lower(nullif(trim(seller_city), '')) AS seller_city,
  lpad(nullif(trim(seller_zip_code_prefix), ''), 5, '0') AS seller_zip_prefix
FROM raw.sellers;

CREATE OR REPLACE TABLE curated.marketing_leads AS
SELECT
  'lea_' || substr(sha256('olist-lead:' || l.mql_id), 1, 24) AS lead_key,
  try_cast(l.first_contact_date AS DATE) AS first_contact_date,
  lower(nullif(trim(l.origin), '')) AS acquisition_origin,
  'lan_' || substr(sha256('olist-landing:' || l.landing_page_id), 1, 20) AS landing_page_key,
  d.mql_id IS NOT NULL AS converted_to_seller,
  try_cast(d.won_date AS TIMESTAMP) AS won_at,
  CASE WHEN d.seller_id IS NULL THEN NULL ELSE 'sel_' || substr(sha256('olist-seller:' || d.seller_id), 1, 24) END AS seller_key,
  lower(nullif(trim(d.business_segment), '')) AS business_segment,
  lower(nullif(trim(d.lead_type), '')) AS lead_type,
  lower(nullif(trim(d.lead_behaviour_profile), '')) AS lead_behaviour_profile,
  CASE WHEN lower(trim(d.has_company)) IN ('true','1','yes') THEN true WHEN lower(trim(d.has_company)) IN ('false','0','no') THEN false END AS has_company,
  CASE WHEN lower(trim(d.has_gtin)) IN ('true','1','yes') THEN true WHEN lower(trim(d.has_gtin)) IN ('false','0','no') THEN false END AS has_gtin,
  lower(nullif(trim(d.average_stock), '')) AS average_stock_band,
  lower(nullif(trim(d.business_type), '')) AS business_type,
  try_cast(d.declared_product_catalog_size AS INTEGER) AS declared_product_catalog_size,
  try_cast(d.declared_monthly_revenue AS DECIMAL(14,2)) AS declared_monthly_revenue_brl
FROM raw.marketing_leads l
LEFT JOIN raw.closed_deals d USING (mql_id);

CREATE OR REPLACE TABLE curated.order_analysis AS
WITH item_summary AS (
  SELECT i.order_key, count(*)::INTEGER AS item_count,
    count(DISTINCT i.product_key)::INTEGER AS distinct_product_count,
    count(DISTINCT i.seller_key)::INTEGER AS distinct_seller_count,
    count(DISTINCT p.product_category_english)::INTEGER AS distinct_category_count,
    sum(i.item_price_brl) AS item_value_brl,
    sum(i.freight_value_brl) AS freight_value_brl
  FROM curated.order_items i LEFT JOIN curated.products p USING (product_key)
  GROUP BY i.order_key
), payment_summary AS (
  SELECT order_key, count(*)::INTEGER AS payment_record_count,
    count(DISTINCT payment_type)::INTEGER AS payment_type_count,
    max(payment_installments) AS maximum_installments,
    sum(payment_value_brl) AS payment_value_brl
  FROM curated.payments GROUP BY order_key
)
SELECT o.*,
  i.item_count, i.distinct_product_count, i.distinct_seller_count,
  i.distinct_category_count, i.item_value_brl, i.freight_value_brl,
  p.payment_record_count, p.payment_type_count, p.maximum_installments,
  p.payment_value_brl, r.review_record_count, r.mean_review_score,
  date_diff('day', o.purchased_at, o.delivered_to_customer_at) AS delivery_days,
  date_diff('day', o.estimated_delivery_at, o.delivered_to_customer_at) AS days_late_vs_estimate,
  CASE WHEN o.delivered_to_customer_at IS NULL OR o.estimated_delivery_at IS NULL THEN NULL
       ELSE o.delivered_to_customer_at > o.estimated_delivery_at END AS delivered_late
FROM curated.orders o
LEFT JOIN item_summary i USING (order_key)
LEFT JOIN payment_summary p USING (order_key)
LEFT JOIN curated.reviews r USING (order_key);
