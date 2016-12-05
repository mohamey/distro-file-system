import requests

payload = {'path' : '/temp/test.txt'}
res = requests.get('http://localhost:8080/files', params=payload)
print(res.url)
print(res.text)
