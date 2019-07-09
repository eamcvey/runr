import requests

URL = 'http://api.athlinks.com/results/list/405160/610335/A?format=json&key=153d7a275e11990b9b2eb4602c0ab541&page=%d&pageSize=100'


keys = [
    'Age',
    'BibNum',
    'DisplayName',
    'Gender',
    'RankA',
    'RankG',
    'RankO',
    'Ticks',
    'TicksString',
]

rows = []
for page in range(1, 72):
    url = URL % page
    r = requests.get(url)
    data = r.json()
    for row in data['RaceEntries']['List']:
        x = [row.get(key, '') for key in keys]
        print ','.join(map(str, x))