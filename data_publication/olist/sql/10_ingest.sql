CREATE SCHEMA IF NOT EXISTS raw;

CREATE OR REPLACE TABLE raw.customers AS SELECT * FROM read_csv('{{CUSTOMERS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.order_items AS SELECT * FROM read_csv('{{ORDER_ITEMS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.payments AS SELECT * FROM read_csv('{{PAYMENTS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.reviews AS SELECT * FROM read_csv('{{REVIEWS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.orders AS SELECT * FROM read_csv('{{ORDERS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.products AS SELECT * FROM read_csv('{{PRODUCTS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.sellers AS SELECT * FROM read_csv('{{SELLERS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.category_translation AS SELECT * FROM read_csv('{{CATEGORY_TRANSLATION}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.marketing_leads AS SELECT * FROM read_csv('{{MARKETING_LEADS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
CREATE OR REPLACE TABLE raw.closed_deals AS SELECT * FROM read_csv('{{CLOSED_DEALS}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
