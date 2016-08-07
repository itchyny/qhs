cat basic.csv | qhs -H "SELECT foo,baz FROM - WHERE bar IS NOT NULL"
