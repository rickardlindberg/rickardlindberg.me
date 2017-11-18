.PHONY: meta1.py
meta1.py:
	cat meta1.src | python meta0.py > meta1.py
	cat meta1.src | python meta1.py > meta1.py.tmp
	mv meta1.py.tmp meta1.py
	bash -c 'diff meta1.py <(cat meta1.src | python meta1.py)'
	echo OK
