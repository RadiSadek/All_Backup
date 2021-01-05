ALTER TABLE citycash_db.credits_applications_scoring
DROP COLUMN PD;

ALTER TABLE citycash_db.credits_applications_scoring
ADD COLUMN pd DECIMAL(6,4) AFTER color;