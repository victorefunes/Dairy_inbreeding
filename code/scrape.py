import requests
from bs4 import BeautifulSoup

url = "https://webconnect.uscdcb.com/path-you-want"

response = requests.get(url)
soup = BeautifulSoup(response.text, "html.parser")

for item in soup.select("your-css-selector"):
    print(item.get_text())