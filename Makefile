clean:
	find ./ -name "*.fasl" \
	  -o -name "*.faslmt" \
	  -o -name "*~" \
	  -o -name "*.err" \
	  -o -name "*.x86f" \
	  -o -name "*.nfasl" \
	 | xargs rm 
