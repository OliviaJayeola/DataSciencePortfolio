import requests
from bs4 import BeautifulSoup as btfs
import re 
from datetime import datetime,timedelta
import PyPDF2


class webscraper:
    """
    This class creates a list of full texts from research papers using the keyword 'Machine Learning' 
    in the computer science section of arxiv.com

    Attributes:

    date (date): The date you want to look back from. Format = "YYYY-MM-DD"
    last_page (int): each page goes up by 200, specify the final page to scrape from then add 1 ie 1 = page 1, 201 = page 2, 401 = page 3...
    num_days (int): number of days you want to collect articles for. Default 7
    link (str): Link to arxiv page

    Methods:




    """

    def __init__(self, last_page, num_days = 7, date=datetime.now().date(), link = 'https://arxiv.org/search/?searchtype=all&query=MACHINE+LEARNING&\
                                                                abstracts=show&size=200&order=-announced_date_first&start='+str(i)):
        
        self.date = date
        self.last_page = last_page
        self.num_days = num_days
        self.link = link
        self.__pdf_list = None

    @property
    def get_pdf_list(self):
        return self.__pdf_list
    

    def set_pdf_list(self, pdf_list):
        self.__pdf_list = pdf_list

    def get_html(self):
           
           for i in range(0,self.last_page,200):
                html = requests.get(self.link).text
                soup = btfs(html,'html.parser')
                print(self.link)

                yield soup
           
    
    def create_pdf_list(self):
        
        links = []

        
        html = self.get_html()

        for link in html:
                
                try:
                        date_string = link.text.split('Submitted ')[-1].split(';')[0].strip()
                        date_obj = datetime.strptime(date_string, '%d %B, %Y').date()
                        if (self.date - timedelta(days=7)) <= date_obj <= self.date:     
                                pdf = link.find('a', string='pdf')['href']
                                print(pdf, "NEXT")
                                
                except ValueError:
                                print("Error not date", "NEXT")
