# Olist Marketplace Data Dictionary

All monetary values are Brazilian reais (BRL). All timestamps preserve the
source clock representation; the source does not supply a timezone. Keys are
release-local pseudonyms and have meaning only inside this release.

## Important analytical cautions

- Orders, items, payments, and reviews have different grains. Joining them
  directly can multiply prices, freight, and payment totals.
- A customer may place multiple orders. `customer_key`, not `order_key`, is the
  repeat-customer key.
- Review scores are submitted only for reviewed orders and are not a random
  satisfaction survey of all customers.
- Delivery outcomes occur after purchase and cause leakage when used in a
  purchase-time prediction.
- Marketing leads are a sampled funnel. A lead can appear more than once in the
  source acquisition process, and not every converted seller necessarily has
  marketplace activity during the commerce observation window.
- The historical Brazilian sample should not be generalized to current U.S.
  e-commerce without explicit justification.

## `olist_orders.parquet`

One row per order. `order_key` is primary. `customer_key` supports repeat-order
analysis. Status and purchase/approval/carrier/customer/estimated-delivery
timestamps describe fulfillment. Customer geography is limited to state,
normalized city, and five-digit Brazilian postal-code prefix.

## `olist_order_items.parquet`

One row per `(order_key, order_item_number)`. Each row identifies a release-local
product and seller and contains the item price, freight value, and seller
shipping-limit timestamp. Freight is item-grain and must be summed once per
item, not once per later joined payment or review row.

## `olist_payments.parquet`

One row per `(order_key, payment_sequence)`. An order can use more than one
payment record or type. Fields include payment type, installment count, and
payment value.

## `olist_reviews.parquet`

One row per reviewed order. Multiple source review records are reduced to a
count, mean/minimum/maximum score, first review creation time, and last answer
time. Scores range from 1 to 5. Titles, comments, and review identifiers are
excluded.

## `olist_products.parquet`

One row per product. Fields include Portuguese and English category labels,
name and description lengths, photo count, weight, and package dimensions.
Missing category translations remain missing rather than being guessed.

## `olist_sellers.parquet`

One row per seller with release-local seller key, state, normalized city, and
Brazilian postal-code prefix. No seller name or address is included.

## `olist_marketing_leads.parquet`

One row per sampled marketing-qualified lead. It contains first-contact date,
acquisition origin, a release-local landing-page key, conversion indicator,
win timestamp, linked seller key when converted, and supplied business
attributes. SDR and sales-representative source identifiers are excluded.

## `olist_order_analysis.parquet`

One row per order. It combines order/customer fields with safe aggregates:
item, product, seller and category counts; item and freight totals; payment
counts and value; review count and mean score; delivery duration; difference
between actual and estimated delivery; and a late-delivery indicator.

`days_late_vs_estimate` is positive for delivery after the estimate and
negative for early delivery. It is missing when either timestamp is missing.
