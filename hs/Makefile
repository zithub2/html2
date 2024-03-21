T=html2
$T:	$T.hs
	ghc -dynamic $^

test.html:	test.html2
	cat $^ | ./$T > $@
