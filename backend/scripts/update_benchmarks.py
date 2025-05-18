from bs4 import BeautifulSoup
from pyreadr import read_r
from pathlib import Path
import pandas as pd
import requests
import re

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException



SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR.parent.joinpath("data")

UA = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36 Edg/118.0.2088.57"

#### Update avg fps data from Tom's Hardware ####
#################################################
def get_th_avg_fps() :
    response = requests.get(
        "https://www.tomshardware.com/reviews/gpu-hierarchy,4388.html",
        headers={"User-Agent" : UA})
        
    webpage = BeautifulSoup(response.text, "lxml")
    webpage_tables = webpage.find_all("table")

    def build_table(body_obj):
        row_objs = body_obj.find_all("tr")
        rows = list()
        for obj in row_objs:
            rowitem_objs = obj.find_all("td")
            row_data = [i.get_text() for i in rowitem_objs]
            rows.append(row_data)
        return rows

    def get_fps_val(string):
        re_match = re.search(r"\(([\d.]+)\)", string)
        return float(re_match.group(1)) if re_match else None

    tbl_titles = ["raster", "rt"]
    tbl_header = [
        "model", "price_low", "price_msrp",
        "fhd_medium", "fhd_ultra", "qhd_ultra",
        "uhd_ultra", "specs"
    ]

    for table, title in zip(webpage_tables, tbl_titles):
        tbl_body = table.find("tbody")
        body = build_table(tbl_body)

        df = pd.DataFrame(body, columns=tbl_header)
        for colname in tbl_header[3:7]:
            df[colname] = df[colname].apply(get_fps_val)
        
        df.to_csv(DATA_DIR.joinpath(f"tomshardware_{title}_avg_fps.csv"), index=False)

"""
#### Update V-Ray 5 render benchmark data from official page ####
#################################################################

URL = "https://benchmark.chaos.com/v5/vray-gpu-cuda"

df = read_r(DATA_DIR.joinpath("prices.rds"))[None]

unique_combinations = df.groupby(['ProductName','ProductFilters'])\
    .size().reset_index()

best_combinations = unique_combinations\
    .loc[unique_combinations['ProductFilters'].str.len()\
    .groupby(unique_combinations['ProductName']).idxmin()]

MODEL_NAMES = best_combinations["ProductName"]
MODEL_FILTERS = best_combinations["ProductFilters"]

chrome_options = webdriver.ChromeOptions()
chrome_options.add_argument("headless")

DRIVER = webdriver.Chrome(options=chrome_options)
DRIVER.maximize_window() # Ensure all elements are on screen

def get_vray5_render_pts():

    def filtered_result(name_to_filter: str, filters: list) -> bool:
        if not filters:
            return True
        checks_up = False
        for word in filters:
            if bool( re.search(word.lower(), name_to_filter.lower()) ): 
                checks_up = True
            else: 
                checks_up = False
            if not checks_up: 
                break
        return checks_up
    
    def wait_grid_update(driver):
        results_grid = "//div[@class='scores']"

        wait = WebDriverWait(driver, timeout=15)
        wait.until(
            EC.invisibility_of_element_located(
                (By.XPATH, results_grid)
            )
        )
        wait.until(
            EC.presence_of_element_located(
                (By.XPATH, results_grid)
            )
        )
    
    def grid_is_present(driver):
        try:
            driver.find_element(By.CLASS_NAME, "no-scores")
            return False
        except NoSuchElementException:
            return True
    
    def set_desired_sorting(driver, attempts=5) -> bool:
        #row_elems = "//li[contains(@class, 'row') and not(contains(@class, 'head'))]"
        count_elements = "//span[contains(@class, 'count')]"
        wait = WebDriverWait(driver, 10)

        for att in range(attempts):
            wait.until(
                EC.presence_of_element_located(
                    (By.XPATH, count_elements)
                )
            )
            
            counts_list = driver.find_elements(By.XPATH, count_elements)
            counts_values = [int(count.text.replace(" ", "")) for count in counts_list]
            
            if counts_values != sorted(counts_values, reverse=True):
                # Click the option to sort by benchmark
                wait.until(
                    EC.element_to_be_clickable(
                        (By.LINK_TEXT, "Benchmarks")
                    )
                )
                driver.find_element(By.LINK_TEXT, "Benchmarks").click()
                # Wait until the scores are updated
                wait_grid_update(driver)
                # Check if the scores are sorted by number of benchmarks (decreasing)
                #counts_list = driver.find_elements(By.XPATH, count_elements)
                #counts_values = [int(score.text.replace(",", "")) for score in counts_list]

            else:
                success = True
                break

        if not success:
            print(f"Model: {model}; Filters: {filters}")
            raise TimeoutError(
                f"Number of retries exceeded when interacting with sort\n{counts_values}")
        else:
            wait.until(
                EC.presence_of_element_located(
                    (By.XPATH, count_elements)
                )
            )

    results = {}
    for model, filters in zip(MODEL_NAMES, MODEL_FILTERS):
        results_size = len(results)
        filter_is_empty = len(filters) == 0
        
        if filter_is_empty:
            filters = None
        else:
            filters = filters.split(", ")
        
        DRIVER.get(URL)
        
        navigate = DRIVER.find_element(By.CLASS_NAME, "advanced")
        wait = WebDriverWait(DRIVER, 10)
        wait.until(
            EC.element_to_be_clickable(
                (By.CLASS_NAME, "advanced")
            )
        )
        navigate.click()
        
        d_name_box = "(//input)[2]"
        d_count_box = "(//input)[3]"
        
        DRIVER.find_element(By.XPATH, d_name_box).clear()
        DRIVER.find_element(By.XPATH, d_name_box).send_keys(model)
        
        DRIVER.find_element(By.XPATH, d_count_box).clear()
        DRIVER.find_element(By.XPATH, d_count_box).send_keys("1")
        
        search_btn = "//button[contains(., 'Search')]"
        navigate = DRIVER.find_element(By.XPATH, search_btn)
        navigate.click()
        
        wait_grid_update(DRIVER)
        if grid_is_present(DRIVER):
            set_desired_sorting(DRIVER)
        
            returned_scores = "//localised-number[contains(@number, True)]"
            returned_labels = "//span[@class='configuration']"
            wait.until(
                EC.visibility_of_element_located(
                    (By.XPATH, returned_scores)
                )
            )
            
            # _append_result_
            try:
                scores = [i.text for i in DRIVER.find_elements(By.XPATH, returned_scores)]
                labels = [i.text for i in DRIVER.find_elements(By.XPATH, returned_labels)]
                for score, label in zip(scores, labels):
                    if filtered_result(label, filters):
                        results[model] = float(score.replace(" ", ""))
                        break
            except Exception as grid_faliure:
                print(f"Exception at _append_result_\n{grid_faliure}")
                results[model] = grid_faliure
        else:
            results[model] = "NA"
        
    print(results)
    df = pd.DataFrame({"model" : results.keys(), "score":results.values()})
    df.to_csv("data/vray5_benchmarks.csv")
    DRIVER.quit()
"""
# run from command line: python3 ./routine/update_benchmarks.py
if __name__ == "__main__":
    try: 
        get_th_avg_fps()
    except Exception as expt: 
        print("Error trying to collect Avg. FPS from Tom's Hardware")
        print(expt)
    
    #if path.isfile(getcwd() + r"\\data\\prices.rds"):
    #    get_vray5_render_pts()
    #else: 
    #    print(r"Not able to collect Vray-5 benchmarks, '\\data\\prices.rds' not found in this folder")
    
    
