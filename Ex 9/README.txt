-- Steps required to run the server-side script (using XAMPP):

1. Put "palseq.php" file in htdocs folder of XAMPP.
2. Run the server.
3. Open your browser and type the address "http://localhost/palseq.php"

-- Steps required to run the client-side script:

1. Make sure you have the following packages in Python:
	- bs4 (required for html parsing)
	- requests (required for http requests between the client and the server)

	If not, install them via the pip commands:

	$ pip install bs4
	$ pip install requests

2. Run through the terminal the script using Python, and add as an argument the URL where the server-side script is hosted:

	$ python client.py http://localhost/palseq.php

An example output should be like this:

	$ python client.py http://localhost/palseq.php
	Round 1, length: 5, duwad
	Answer: 2
	Right!  :-)
	Round 2, length: 10, aajueuueja
	Answer: 2
	Right!  :-)
	Round 3, length: 15, jdgifigxxadgiig
	Answer: 5
	Right!  :-)
	Round 4, length: 20, xxeqoclgngtwnelhnjhl
	Answer: 15
	Right!  :-)
	Round 5, length: 25, etcjarqumbskyyksbmuqracte
	Answer: 1
	Right!  :-)
	Round 6, length: 30, sfrfwfagltqcttjfcqjftxsafrifrf
	Answer: 12
	Right!  :-)
	Round 7, length: 35, wjyrgnooqqcdaamjjawdkkacqawchfayuow
	Answer: 21
	Right!  :-)
	Round 8, length: 40, ogwexyukeuzcotbqziziikitkqtlgezanermyjku
	Answer: 24
	Right!  :-)
	Round 9, length: 45, hzpmgtuyxbojhpphbfquscxxxmqaefdjmjlzexodukbvz
	Answer: 30
	Right!  :-)
	Round 10, length: 50, gylgpoedmhcanqdqnmggrmfeqzrhbkkbxhvrzqmrminqndhpyg
	Answer: 18
	Right!  :-)
	Congratulations! You passed the quiz! :-)