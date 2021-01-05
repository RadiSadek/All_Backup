
DROP TABLE test.data_last_pay_day;
DROP TABLE test.data_max_pay_day;
DROP TABLE test.data_deactivated_at;
DROP TABLE test.data_cessions;
DROP TABLE test.offices_zones;
 
CREATE TABLE test.data_last_pay_day AS (
SELECT a.id, a.credit_number, b.last_pay_day
FROM citycash_db.credits_applications a LEFT JOIN(SELECT application_id,  last_pay_day
FROM citycash_db.credits_plan_contract) AS b on a.id=b.application_id);

CREATE TABLE test.data_max_pay_day AS (
SELECT a.id, a.credit_number, b.max_pay_day
FROM citycash_db.credits_applications a LEFT JOIN(SELECT application_id, max(pay_day) AS max_pay_day 
FROM citycash_db.credits_plan_main GROUP BY application_id) AS b on a.id=b.application_id);

CREATE TABLE test.data_deactivated_at AS (
SELECT credit_number, date(deactivated_at) as deactivated_at FROM citycash_db.credits_applications);

CREATE TABLE test.data_cessions AS (
SELECT credit_number, CASE WHEN sub_status=124 THEN 1 ELSE 0 END AS cession FROM citycash_db.credits_applications);

CREATE TABLE test.offices_zones AS (
SELECT citycash_db.credits_applications.credit_number, citycash_db.credits_applications.office_id,
citycash_db.structure_offices.name as name_office,
citycash_db.structure_zones.name as name_zones
FROM citycash_db.credits_applications
LEFT JOIN citycash_db.structure_offices ON citycash_db.credits_applications.office_id=citycash_db.structure_offices.id
LEFT JOIN citycash_db.structure_zones ON citycash_db.structure_offices.zone_id=citycash_db.structure_zones.id);