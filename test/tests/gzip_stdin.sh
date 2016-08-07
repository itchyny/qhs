cat basic.csv.gz | qhs -H -z "SELECT foo,baz FROM - WHERE bar IS NOT NULL"
