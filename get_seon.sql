SELECT a.application_id, b.registered
FROM citycash_db.seon_requests a
JOIN citycash_db.seon_requests_accounts b
ON a.id=b.requests_id
WHERE a.type=1 AND b.type=7;