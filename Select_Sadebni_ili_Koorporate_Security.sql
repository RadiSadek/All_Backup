SELECT id, judicial_date
FROM citycash_db.credits_applications 
WHERE judicial_date is not null AND is_corporative_security =1;