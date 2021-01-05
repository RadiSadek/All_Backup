DROP TABLE test.data_final;

CREATE TABLE test.data1 AS (
SELECT 
id, client_id, SUBSTRING(signed_at,1,10) AS date, credit_number, 
has_prev_credits, has_prev_applications, has_prev_company_credits, 
product_id, status, sub_status, 
signed_at, deactivated_at, updated_at, created_at 
AS date_entry, office_id, number AS request_number, source, 
IF(product_id IN (55,58,57,56,9,48,25,36,41,43,50,28,26,37,42,44,49,27) ,"online", "offline")
AS online_offline
FROM citycash_db.credits_applications a 
WHERE status IN (1,2,3,4,5));

CREATE TABLE test.data2 AS (
SELECT a.*, 
b.amount, b.installments
FROM test.data1 a 
LEFT JOIN citycash_db.credits_plan_contract b 
ON a.id=b.application_id);

CREATE TABLE test.data3 AS (
SELECT a.*, 
b.egn,  b.ownership, b.education, b.marital_status, b.household_children, b.household_total, b.on_address
FROM test.data2 a 
LEFT JOIN citycash_db.credits_applications_clients b 
ON a.id=b.application_id);

CREATE TABLE test.data5 AS (
SELECT a.*, 
b.income
FROM test.data3 a 
LEFT JOIN(SELECT application_id, SUM(amount) AS income 
FROM citycash_db.credits_applications_clients_money_income 
WHERE type=1 AND sub_type=1 AND deleted_at IS NULL
GROUP BY application_id) AS b 
ON a.id=b.application_id);
                            
CREATE TABLE test.data6 AS (
SELECT a.*, 
b.additional_income
FROM test.data5 a 
LEFT JOIN(SELECT application_id, SUM(amount) AS additional_income 
FROM citycash_db.credits_applications_clients_money_income 
WHERE deleted_at IS NULL AND type<>1 OR sub_type<>1
GROUP BY application_id) AS b 
ON a.id=b.application_id);

CREATE TABLE test.data7 AS (
SELECT a.*, 
b.days_delay_max AS days_delay, b.pay_day
FROM test.data6 a 
LEFT JOIN (SELECT *, MAX(days_delay) AS days_delay_max 
FROM citycash_db.credits_plan_main 
GROUP BY application_id) AS b 
ON a.id=b.application_id);

CREATE TABLE test.data7s AS (
SELECT a.*, 
b.period
FROM test.data7 a 
LEFT JOIN citycash_db.products b 
ON a.product_id=b.id);

CREATE TABLE test.data8 AS (
SELECT *, 
CASE WHEN SUBSTRING(egn,9,1) IN (0,2,4,6,8) THEN 1 ELSE 0 END AS gender,
IF(LEFT(egn,2) IN ("00","01","02","03"),
   LEFT(date_entry,4)-CONCAT('20',LEFT(egn,2)),
   IF(LEFT(egn,2) IN ("10","11"),NULL,
   LEFT(date_entry,4)-CONCAT('19',LEFT(egn,2)))) AS age,
CASE WHEN days_delay>=90 THEN 1 ELSE 0 END AS default_flag,
CASE WHEN income IS NULL THEN 0 ELSE income END AS income_corr,
CASE WHEN additional_income IS NULL THEN 0 ELSE additional_income END AS additional_income_corr,
CASE WHEN period=1 THEN installments*7/30 
ELSE (CASE WHEN period=2 THEN installments*14/30 
ELSE installments END) END AS maturity,
CASE WHEN sub_status=126 THEN 1 ELSE 0 END AS refinance,
CASE WHEN sub_status=124 THEN 1 ELSE 0 END AS cession
FROM test.data7s);

CREATE TABLE test.data9 AS (
SELECT a.*, 
b.amount_paid
FROM test.data8 a LEFT JOIN(
SELECT object_id, SUM(amount) AS amount_paid 
FROM citycash_db.cash_flow
WHERE nomenclature_id in (90,100,101,102) 
AND deleted_at IS NULL 
AND object_type=4
GROUP BY object_id)
AS b 
ON a.id=b.object_id);

CREATE TABLE test.data10 AS (
SELECT a.*, 
b.installment_amount
FROM test.data9 a 
LEFT JOIN (
SELECT application_id, MAX(pmt_final) AS installment_amount 
FROM citycash_db.credits_plan_main
GROUP BY application_id) AS b
ON a.id=b.application_id);
			
CREATE TABLE test.data11 AS (
SELECT *, 
income_corr+additional_income_corr AS total_income 
FROM test.data10);

CREATE TABLE test.data12 AS (
SELECT a.*, 
b.experience, b.experience_employer, b.status AS status_work
FROM test.data11 a 
LEFT JOIN(
SELECT * FROM citycash_db.credits_applications_clients_work) AS b 
ON a.id=b.application_id);

CREATE TABLE test.data13 AS (
SELECT a.*, 
b.purpose
FROM test.data12 a 
LEFT JOIN(
SELECT * FROM citycash_db.credits_applications_data_other) AS b 
ON a.id=b.application_id);

CREATE TABLE test.data13s AS (
SELECT a.*, 
b.max_pay_day
FROM test.data13 a LEFT JOIN(
SELECT  application_id, max(pay_day) AS max_pay_day 
FROM citycash_db.credits_plan_main 
GROUP BY application_id) AS b 
ON a.id=b.application_id);

CREATE TABLE test.data14 AS (
SELECT a.*,
b.name AS office_current, b.zone_id 
FROM test.data13s a LEFT JOIN(
SELECT * from citycash_db.structure_offices) AS b 
ON a.office_id=b.id);

CREATE TABLE test.data15 AS (
SELECT a.*, 
b.name AS zone_current
FROM test.data14 a LEFT JOIN(
SELECT * FROM citycash_db.structure_zones) AS b 
ON a.zone_id=b.id);

CREATE TABLE test.data16 AS (
SELECT a.*, 
b.date_default 
FROM test.data15 a LEFT JOIN(
SELECT MIN(application_id) AS appl_id, pay_day, days_delay, 
DATE_ADD(pay_day, INTERVAL 90 DAY) AS date_default 
FROM citycash_db.credits_plan_main 
WHERE days_delay>=90 
GROUP BY application_id) AS b 
ON a.id=b.appl_id);

CREATE TABLE test.data17 AS (SELECT *, 
CASE WHEN period=3 THEN installment_amount/total_income ELSE (
CASE WHEN period=2 THEN installment_amount/(total_income*14/30) 
ELSE installment_amount/(total_income*7/30) END) END 
AS ratio_installment_income FROM test.data16 ORDER BY date);

CREATE TABLE test.data18 AS (
SELECT a.*, b.total_amount
FROM test.data17 a 
LEFT JOIN(SELECT final_credit_amount AS total_amount, application_id 
FROM citycash_db.credits_plan_contract)
AS b ON a.id=b.application_id);
                                
CREATE TABLE test.data19 AS (
SELECT a.*, b.last_pay_date
FROM test.data18 a LEFT JOIN(SELECT object_id, max(pay_date) AS last_pay_date 
FROM citycash_db.cash_flow
WHERE nomenclature_id IN (90,100,101) AND deleted_at IS NULL AND object_type = 4
GROUP BY object_id) AS b 
ON a.id=b.object_id);

CREATE TABLE test.data_final AS (
SELECT a.*, b.name AS product_name
FROM test.data19 a LEFT JOIN(SELECT id, name FROM citycash_db.products) AS b 
ON a.product_id=b.id);

DROP TABLE test.data1;
DROP TABLE test.data2;
DROP TABLE test.data3;
DROP TABLE test.data5;
DROP TABLE test.data6;
DROP TABLE test.data7;
DROP TABLE test.data7s;
DROP TABLE test.data8;
DROP TABLE test.data9;
DROP TABLE test.data10;
DROP TABLE test.data11;
DROP TABLE test.data12;
DROP TABLE test.data13;
DROP TABLE test.data13s;
DROP TABLE test.data14;
DROP TABLE test.data15;
DROP TABLE test.data16;
DROP TABLE test.data17;
DROP TABLE test.data18;
DROP TABLE test.data19;