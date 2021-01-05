DROP TABLE test.paid_per_client;

CREATE TABLE test.paid_per_client1 AS (SELECT object_id, amount, pay_date FROM citycash_db.cash_flow
							WHERE nomenclature_id in (90,100,101) AND deleted_at IS NULL AND object_type=4);
                            
CREATE TABLE test.paid_per_client AS (SELECT object_id, amount, pay_date, egn, credit_number FROM test.paid_per_client1 a
LEFT JOIN (SELECT id, egn, credit_number FROM test.data_final) AS b
ON a.object_id=b.id);

DROP TABLE test.paid_per_client1;