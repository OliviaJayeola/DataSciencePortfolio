import webscraper
from datetime import datetime 

scraper = webscraper(last_page=1, date=datetime.date().today())
scraper.create_pdf_list()