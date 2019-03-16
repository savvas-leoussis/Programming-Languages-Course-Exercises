import sys
import requests
from bs4 import BeautifulSoup

def lps(string):
	n = len(string)
	L = [[0 for x in range(n)] for y in range(n)]
	for i in range(n):
		L[i][i]=1
	for cl in range(2,n+1):
		for i in range(n-cl+1):
			j = i+cl-1
			if ((string[i]==string[j]) and (cl == 2)):
				L[i][j] = 2
			elif (string[i] == string[j]):
				L[i][j] = L[i+1][j-1]+2
			else:
				L[i][j] = max(L[i][j-1],L[i+1][j])
	return L[0][n-1]
 
def minimumNumberOfDeletions(string):
	n = len(string)
	length = lps(string)
	return n-length

if (len(sys.argv)!=2):
	print("Please provide the game's URL. Example: \"./client.py http://localhost/palseq.php\"")
	exit(1)
s = requests.Session()
r = s.get(sys.argv[1])
while (True):
	post_dictionary = {}
	parsed_html = BeautifulSoup(r.text,'html.parser')
	if (parsed_html.find(id="question") and parsed_html.find(id="questNo") and parsed_html.find(id="length") and parsed_html.find(id="answer")):
		print ("Round "+parsed_html.find(id="questNo").text+", length: "+parsed_html.find(id="length").text+", "+parsed_html.find(id="question").text)
		answer = minimumNumberOfDeletions(parsed_html.find(id="question").text)
		print ("Answer: "+str(answer))
		post_dictionary["answer"] = answer
		r = s.post(sys.argv[1],post_dictionary)
		next
	elif (parsed_html.find(id="question") and parsed_html.find(id="questNo") and parsed_html.find(id="length") and parsed_html.find(id="again")):
		print (parsed_html.find(id="response").text)
		post_dictionary["again"] = "Continue!"
		r = s.post(sys.argv[1],post_dictionary)
		next
	elif (parsed_html.find(id="finish")):
		print (parsed_html.find(id="finish").text)
		exit(0)