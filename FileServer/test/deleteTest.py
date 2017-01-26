#! /usr/bin/python3

import requests
import json

payload = {
    "filePath" : "/foo/test.txt",
}

# Convert payload to json
jsonPayload = json.dumps(payload)
# Set the headers
headers = {'Content-Type': 'application/json'}

# Send the request, print the result
res = requests.delete('http://localhost:8002/removeSecondary', headers=headers, data=jsonPayload)
print(res.text)
