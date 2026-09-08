SELECT CASE WHEN (SELECT count(*) FROM curated.orders) BETWEEN 99000 AND 100000
  THEN true ELSE error('Unexpected Olist order count') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.order_items) > 110000
  THEN true ELSE error('Unexpectedly few Olist order items') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.marketing_leads) >= 7900
  THEN true ELSE error('Unexpectedly few Olist marketing leads') END;

SELECT CASE WHEN (SELECT count(*) = count(DISTINCT order_key) FROM curated.orders)
  THEN true ELSE error('Duplicate order_key') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT (order_key, order_item_number)) FROM curated.order_items)
  THEN true ELSE error('Duplicate order item key') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT (order_key, payment_sequence)) FROM curated.payments)
  THEN true ELSE error('Duplicate payment key') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT order_key) FROM curated.reviews)
  THEN true ELSE error('Duplicate aggregated review order') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT product_key) FROM curated.products)
  THEN true ELSE error('Duplicate product_key') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT seller_key) FROM curated.sellers)
  THEN true ELSE error('Duplicate seller_key') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT lead_key) FROM curated.marketing_leads)
  THEN true ELSE error('Duplicate lead_key') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT order_key) FROM curated.order_analysis)
  THEN true ELSE error('Duplicate analysis order_key') END;

SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.order_items i LEFT JOIN curated.orders o USING (order_key) WHERE o.order_key IS NULL
) THEN true ELSE error('Order item references missing order') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.payments p LEFT JOIN curated.orders o USING (order_key) WHERE o.order_key IS NULL
) THEN true ELSE error('Payment references missing order') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.order_items i LEFT JOIN curated.products p USING (product_key) WHERE p.product_key IS NULL
) THEN true ELSE error('Order item references missing product') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.order_items i LEFT JOIN curated.sellers s USING (seller_key) WHERE s.seller_key IS NULL
) THEN true ELSE error('Order item references missing seller') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.reviews WHERE minimum_review_score NOT BETWEEN 1 AND 5 OR maximum_review_score NOT BETWEEN 1 AND 5
) THEN true ELSE error('Review score outside 1-5') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.order_items WHERE item_price_brl < 0 OR freight_value_brl < 0
) THEN true ELSE error('Negative item price or freight') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.order_analysis) = (SELECT count(*) FROM curated.orders)
  THEN true ELSE error('Analysis is not one row per order') END;
