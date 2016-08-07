qhs -H -O -T "SELECT basic.foo,big.baz,big.bar FROM basic.csv basic JOIN big.csv big ON basic.baz = big.baz WHERE basic.bar IS NOT NULL"
