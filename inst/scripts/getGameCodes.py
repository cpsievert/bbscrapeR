import requests
from bs4 import BeautifulSoup
from datetime import datetime
url2014 = 'http://www.basketball-reference.com/leagues/NBA_2014_games.html'
url2015 = 'http://www.basketball-reference.com/leagues/NBA_2015_games.html'

# pick url as needed.
r = requests.get(url2015)
doc = BeautifulSoup(r2015.content)
# not too smart, just grabs all rows
games = doc.findAll('tr')
fmt = "%a, %b %d, %Y"

# I think the home/away designation is incorrect on basketball-reference.com
# whichever way, this script writes game codes as seen on nba.com
head = ["date", "visitor", "home"]
data = []
# keep a set of teams to write dictionary later
teams = set()
# skip header
for g in games[1:]:
    out = []
    d = g.findAll('td')
    try:
        out.append(datetime.strptime(d[0].text, fmt))
        out.append(d[2].text)
        out.append(d[4].text)
        data.append(out)
        teams.add(d[2].text)
    except IndexError:
        continue


# translate to abbreviations
tdict = {'Boston Celtics': 'BOS', 'Oklahoma City Thunder': 'OKC', 'Cleveland Cavaliers': 'CLE',
         'Philadelphia 76ers': 'PHI', 'Milwaukee Bucks': 'MIL', 'Portland Trail Blazers': 'POR', 
         'New Orleans Pelicans': 'NOP', 'Utah Jazz': 'UTA', 'Denver Nuggets': 'DEN', 'Indiana Pacers': 'IND', 
         'Phoenix Suns': 'PHX', 'Washington Wizards': 'WAS', 'Memphis Grizzlies': 'MEM', 'Dallas Mavericks': 'DAL', 
         'Detroit Pistons': 'DET', 'Toronto Raptors': 'TOR', 'Golden State Warriors': 'GSW', 'Charlotte Bobcats': 'CHA', 
         'Orlando Magic': 'ORL', 'Sacramento Kings': 'SAC', 'San Antonio Spurs': 'SAS', 'Los Angeles Lakers': 'LAL', 
         'Los Angeles Clippers': 'LAC', 'Minnesota Timberwolves': 'MIN', 'Atlanta Hawks': 'ATL', 'Brooklyn Nets': 'BKN', 
         'New York Knicks': 'NYK', 'Miami Heat': 'MIA', 'Houston Rockets': 'HOU', 'Chicago Bulls': 'CHI',
         'Charlotte Hornets': 'CHA'}

with open('gamecodes.txt', 'w') as gamecodes:
    for d in data:
        ## code is %Y%m%d/homeaway
        gc = "00/" + datetime.strftime(d[0], "%Y%m%d") + "/" + tdict[d[1]] + tdict[d[2]]
        print(gc)
        gamecodes.write(gc + "\n")
