clean:
	find ./ -name *.fasl -exec rm \{\} \;
	find ./ -name *.nfasl -exec rm \{\} \;
	find ./ -name *~ -exec rm \{\} \;
