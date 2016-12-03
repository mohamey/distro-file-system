#! /usr/bin/python3

import requests
import json

payload = {
    "path" : "/temp/test.txt",
    "fileContent" : "This is a test file"
}

# Convert payload to json
jsonPayload = json.dumps(payload)
# Set the headers
headers = {'Content-Type': 'application/json'}

# Send the request, print the result
res = requests.post('http://localhost:8080/upload', headers=headers, data=jsonPayload)
print(res.text)
