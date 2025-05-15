import pandas as pd
import requests
from bs4 import BeautifulSoup
import time
import csv
import random
from urllib.parse import quote
import re
from tqdm import tqdm

class JobScraper:
    def __init__(self, input_file, output_file):
        """
        Initialize the job scraper with input and output file paths.
        
        Args:
            input_file (str): Path to the input CSV file with company names
            output_file (str): Path to save the output CSV file with job openings
        """
        self.input_file = input_file
        self.output_file = output_file
        self.headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
        }
        
    def load_companies(self):
        """Load companies from the input CSV file."""
        df = pd.read_csv(self.input_file)
        return df['company'].tolist()
    
    def search_company_jobs(self, company_name):
        """
        Search for job openings for a specific company.
        
        Args:
            company_name (str): Name of the company to search for
            
        Returns:
            list: List of dictionaries containing job information
        """
        # List to store job data
        jobs = []
        
        try:
            # Method 1: Try searching on LinkedIn Jobs
            linkedin_jobs = self._search_linkedin_jobs(company_name)
            jobs.extend(linkedin_jobs)
            
            # Method 2: Try searching on company careers page via Google search
            company_site_jobs = self._search_company_careers(company_name)
            jobs.extend(company_site_jobs)
            
            # Method 3: Try searching on Indeed
            indeed_jobs = self._search_indeed_jobs(company_name)
            jobs.extend(indeed_jobs)
            
            # Add more methods as needed...
            
        except Exception as e:
            print(f"Error while searching for {company_name}: {str(e)}")
        
        # Add company name to each job entry
        for job in jobs:
            job['company_name'] = company_name
        
        return jobs
    
    def _search_linkedin_jobs(self, company_name):
        """Search for jobs on LinkedIn for the given company."""
        jobs = []
        search_query = f"{company_name} jobs"
        encoded_query = quote(search_query)
        url = f"https://www.google.com/search?q={encoded_query}+site:linkedin.com"
        
        try:
            response = requests.get(url, headers=self.headers)
            if response.status_code == 200:
                soup = BeautifulSoup(response.text, 'html.parser')
                search_results = soup.find_all('div', class_='g')
                
                for result in search_results[:5]:  # Limit to first 5 results
                    a_tag = result.find('a')
                    if a_tag and 'href' in a_tag.attrs:
                        job_url = a_tag['href']
                        job_title_elem = result.find('h3')
                        job_title = job_title_elem.text if job_title_elem else "Unknown Title"
                        
                        job_description_elem = result.find('div', class_='VwiC3b')
                        job_description = job_description_elem.text if job_description_elem else "No description available"
                        
                        department = self._extract_department(job_title, job_description)
                        
                        if 'linkedin.com/jobs' in job_url:
                            jobs.append({
                                'company_url': f"https://linkedin.com/company/{company_name.lower().replace(' ', '-')}",
                                'department': department,
                                'job_title': job_title,
                                'job_description': job_description[:200] + '...',
                                'job_url': job_url
                            })
            
            return jobs
        
        except Exception as e:
            print(f"Error in LinkedIn search for {company_name}: {str(e)}")
            return []
    
    def _search_company_careers(self, company_name):
        """Search for the company's careers page via Google."""
        jobs = []
        search_query = f"{company_name} careers jobs hiring"
        encoded_query = quote(search_query)
        url = f"https://www.google.com/search?q={encoded_query}"
        
        try:
            response = requests.get(url, headers=self.headers)
            if response.status_code == 200:
                soup = BeautifulSoup(response.text, 'html.parser')
                search_results = soup.find_all('div', class_='g')
                
                company_url = ""
                for result in search_results[:3]:  # Check first 3 results
                    a_tag = result.find('a')
                    if a_tag and 'href' in a_tag.attrs:
                        link = a_tag['href']
                        link_text = a_tag.text.lower()
                        
                        # Try to find the company's career page
                        if any(keyword in link_text for keyword in ['career', 'job', 'work with us']):
                            company_url = link
                            break
                
                # For demonstration, create a couple of sample job entries
                if company_url:
                    base_domain = re.search(r'https?://(?:www\.)?([^/]+)', company_url)
                    if base_domain:
                        company_site = f"https://{base_domain.group(1)}"
                        
                        # These would normally be scraped from the careers page
                        sample_departments = ["Engineering", "Marketing", "Sales", "Product"]
                        sample_titles = [
                            "Software Engineer", 
                            "Marketing Specialist", 
                            "Sales Representative", 
                            "Product Manager"
                        ]
                        
                        for i in range(min(2, len(sample_departments))):
                            jobs.append({
                                'company_url': company_site,
                                'department': sample_departments[i],
                                'job_title': f"{sample_titles[i]} at {company_name}",
                                'job_description': f"This is a placeholder job description for a {sample_titles[i]} position at {company_name}.",
                                'job_url': company_url
                            })
            
            return jobs
        
        except Exception as e:
            print(f"Error in company careers search for {company_name}: {str(e)}")
            return []
    
    def _search_indeed_jobs(self, company_name):
        """Search for jobs on Indeed for the given company."""
        jobs = []
        search_query = f"{company_name} jobs site:indeed.com"
        encoded_query = quote(search_query)
        url = f"https://www.google.com/search?q={encoded_query}"
        
        try:
            response = requests.get(url, headers=self.headers)
            if response.status_code == 200:
                soup = BeautifulSoup(response.text, 'html.parser')
                search_results = soup.find_all('div', class_='g')
                
                for result in search_results[:3]:  # Limit to first 3 results
                    a_tag = result.find('a')
                    if a_tag and 'href' in a_tag.attrs:
                        job_url = a_tag['href']
                        job_title_elem = result.find('h3')
                        job_title = job_title_elem.text if job_title_elem else "Unknown Title"
                        
                        job_description_elem = result.find('div', class_='VwiC3b')
                        job_description = job_description_elem.text if job_description_elem else "No description available"
                        
                        department = self._extract_department(job_title, job_description)
                        
                        if 'indeed.com' in job_url:
                            jobs.append({
                                'company_url': f"https://indeed.com/cmp/{company_name.lower().replace(' ', '-')}",
                                'department': department,
                                'job_title': job_title,
                                'job_description': job_description[:200] + '...',
                                'job_url': job_url
                            })
            
            return jobs
        
        except Exception as e:
            print(f"Error in Indeed search for {company_name}: {str(e)}")
            return []
    
    def _extract_department(self, job_title, job_description):
        """Extract department from job title or description."""
        departments = {
            'Engineering': ['engineer', 'developer', 'programmer', 'technical', 'data scientist'],
            'Marketing': ['marketing', 'growth', 'seo', 'content'],
            'Sales': ['sales', 'account executive', 'business development'],
            'Product': ['product', 'design', 'ux', 'ui'],
            'HR': ['hr', 'human resources', 'talent', 'recruiting'],
            'Finance': ['finance', 'accounting', 'accountant'],
            'Operations': ['operations', 'logistics', 'supply chain'],
            'Customer Service': ['customer', 'support', 'service'],
            'Legal': ['legal', 'lawyer', 'attorney'],
            'Executive': ['executive', 'ceo', 'cfo', 'coo', 'director']
        }
        
        combined_text = (job_title + ' ' + job_description).lower()
        
        for dept, keywords in departments.items():
            if any(keyword in combined_text for keyword in keywords):
                return dept
        
        return "Other"
    
    def run(self, max_companies=None, delay_range=(2, 5)):
        """
        Run the job scraper for all companies or a limited number.
        
        Args:
            max_companies (int, optional): Maximum number of companies to process. If None, process all.
            delay_range (tuple, optional): Range of seconds to wait between requests to avoid rate limiting.
            
        Returns:
            pd.DataFrame: DataFrame containing all job openings found
        """
        companies = self.load_companies()
        
        if max_companies is not None:
            companies = companies[:max_companies]
        
        all_jobs = []
        
        # Create and open the output file with CSV writer
        with open(self.output_file, 'w', newline='', encoding='utf-8') as f:
            fieldnames = ['company_name', 'company_url', 'department', 'job_title', 'job_description', 'job_url']
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            for company in tqdm(companies, desc="Processing companies"):
                print(f"\nSearching jobs for: {company}")
                jobs = self.search_company_jobs(company)
                
                # Write jobs to CSV as they are found
                for job in jobs:
                    writer.writerow(job)
                    all_jobs.append(job)
                
                # Random delay to avoid being blocked
                delay = random.uniform(delay_range[0], delay_range[1])
                time.sleep(delay)
        
        print(f"Job scraping complete. Found {len(all_jobs)} job openings across {len(companies)} companies.")
        print(f"Results saved to {self.output_file}")
        
        return pd.DataFrame(all_jobs)


# Example usage
if __name__ == "__main__":
    # Create a scraper instance
    scraper = JobScraper(
        input_file="connected_companies.csv", 
        output_file="company_job_openings.csv"
    )
    
    # Run the scraper for a limited number of companies for testing
    # Remove the max_companies parameter to process all companies
    results = scraper.run(max_companies=2)
    
    # Display the first few results
    print(results.head())
