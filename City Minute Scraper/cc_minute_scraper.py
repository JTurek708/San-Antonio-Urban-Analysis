import logging
import requests
import os
from bs4 import BeautifulSoup

"""
    Work in progress. Not running yet. 
    Definition of Done: Scraper can pull city meeting minutes and parse out voting roll call
"""

def main(): 
    #logging.basicConfig(level=logging.INFO)
    logging.basicConfig(level=logging.DEBUG)

    
    # If we don't have the landing page, scrape the landing page and output file
    if check_contents_dir('landing.html') == False:
        soup = get_landing_page()
        output_contents(soup, 'landing.html')
    else:
        meeting_minute_hrefs = parse_hrefs('landing.html')
    
    #print(meeting_minute_hrefs[0])
    get_meeting_minutes(meeting_minute_hrefs)
    
    
def check_contents_dir(web_pg):
    """
    Check which pages we already have to minimize superfluous GET requests 
    """
    cwd = os.path.dirname(__file__)
    logging.debug(f'Current working directory is {cwd}')

    # if contents folder does not exist, no html files have been scraped yet
    if 'contents' not in os.listdir(cwd):
        os.mkdir('contents')
        logging.info('No contents directory found.')
        return False
    else:
        contents_folder = os.path.join(cwd, 'contents')
        os.chdir(contents_folder)
        logging.debug(f'Changed directory. Current working directory is {os.getcwd()}')
        logging.debug(f'Files in contents directory are {os.listdir()}')

    # check if the web page is already a file in the directory to determine if we need to scrape the page
    if web_pg not in os.listdir():
        logging.info(f'Cannot find {web_pg}. Return False')
        return False 
    else:
        logging.info(f'{web_pg} found. No scraping needed.')
        return True


def get_landing_page():
    # City Clerk meeting minutes landing page
    meeting_min_url = 'https://www.sanantonio.gov/Clerk/Legislative/City-Council-Agendas-Minutes#132762778-meeting-minutes'

    response = requests.get(meeting_min_url)

    soup = BeautifulSoup(response.text, 'html.parser')

    return soup

    
def output_contents(soup, web_pg):

    with open(web_pg, "w") as pg_content:
        pg_content.write(str(soup))
        
    logging.debug(f'Output contents to {web_pg}')


def parse_hrefs(html_file):
    href_list = []
    cwd = os.path.dirname(__file__)
    contents_folder = os.path.join(cwd, 'contents')
    os.chdir(contents_folder)
    logging.debug(f'Changed directory. Current working directory is {os.getcwd()}')
    logging.debug(f'Files in directory are {os.listdir()}')

    with open(html_file, 'r') as f:

        contents = f.read()
        soup = BeautifulSoup(contents, 'html.parser')

        for a in soup.find_all('a', href=True):
            href_list.append(a['href'])
    
    logging.debug(f'Found {len(href_list)} total hrefs in {html_file}')
    logging.debug(f'Searching hrefs found for city meeting minutes...')

    # since we only care about hrefs (links) with city minutes, we'll dump those to a separate list
    meeting_minutes_hrefs = []
    href_dict = {}

    for a in soup.select('.ccMeetingMinutes a'):
        print(a)
        #print(a['title'])
        #href_dict[title] = a['href']
        #meeting_minutes_hrefs.append(a['href'])
        #logging.debug(a['href'], a.get_text(strip=True))
              
    """
    for href in href_list:
        if '/Portals/0/Files/Clerk/Minutes' in href:
            meeting_minutes_hrefs.append(href)
    """
    print(href_dict)
    logging.debug(f'Found {len(meeting_minutes_hrefs)} hrefs pointing to /Portals/0/Files/Clerk/Minutes within {html_file}')


    return meeting_minutes_hrefs

def get_meeting_minutes(meeting_minute_hrefs):
    #logging.debug(f'Getting {meeting_minute_hrefs[0]}...')
    url = meeting_minute_hrefs[0]

    response = requests.get(url)

    soup = BeautifulSoup(response.text, 'html.parser')
    print(soup)

    #output_contents(soup, )


    return soup


 
    

if __name__ == '__main__':
    main()