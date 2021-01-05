SELECT egn, 
SUM(CASE WHEN status = 4 then 1 else 0 end) as actives,
SUM(CASE WHEN status = 5 then 1 else 0 end) as terminated_credits
FROM test.data_final
GROUP BY egn;