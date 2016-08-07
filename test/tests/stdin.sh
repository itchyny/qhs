cat basic.csv | qhs -H "SELECT foo,baz FROM stdin WHERE bar IS NOT NULL"
