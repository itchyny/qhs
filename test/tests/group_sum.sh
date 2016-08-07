qhs -H -O "SELECT foo,sum(bar),COUNT(*) cnt FROM big.csv WHERE bar IS NOT NULL GROUP BY foo ORDER BY cnt DESC LIMIT 3"
