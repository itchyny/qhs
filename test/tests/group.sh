qhs -H -O "SELECT foo,COUNT(*) cnt FROM big.csv GROUP BY foo ORDER BY cnt DESC LIMIT 3"
