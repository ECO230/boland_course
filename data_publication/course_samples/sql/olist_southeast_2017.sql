COPY (
    WITH eligible AS (
        SELECT
            order_key AS order_id,
            customer_key AS customer_id,
            purchased_at,
            month(purchased_at) AS purchase_month,
            dayofweek(purchased_at) AS purchase_day_of_week,
            customer_state,
            customer_city,
            customer_zip_prefix,
            item_count,
            distinct_product_count,
            distinct_seller_count,
            distinct_category_count,
            item_value_brl,
            freight_value_brl,
            payment_record_count,
            payment_type_count,
            maximum_installments,
            payment_value_brl,
            review_record_count,
            mean_review_score,
            delivery_days,
            days_late_vs_estimate,
            delivered_late
        FROM read_parquet('{{SOURCE}}')
        WHERE order_status = 'delivered'
          AND purchased_at >= TIMESTAMP '2017-01-01'
          AND purchased_at < TIMESTAMP '2018-01-01'
          AND customer_state IN ('SP', 'RJ', 'MG')
    )
    SELECT *
    FROM eligible
    ORDER BY md5(order_id || '{{SAMPLE_SALT}}')
    LIMIT {{ROW_LIMIT}}
) TO '{{OUTPUT}}' (FORMAT CSV, HEADER true);
