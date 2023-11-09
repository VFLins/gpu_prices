from bs4 import BeautifulSoup
from pyreadr import read_r
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from os import getcwd, path
import pandas as pd
import requests
import re

UA = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36 Edg/118.0.2088.57"

#### Update avg fps data from Tom's Hardware ####
#################################################
def get_th_avg_fps() :
    response = requests.get(
        "https://www.tomshardware.com/reviews/gpu-hierarchy,4388.html",
        headers={"User-Agent" : UA})
        
    webpage = BeautifulSoup(DRIVER.page_source, "lxml")
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
        re_match = re.search(r"\d+\.\d(?=[fps])", string)
        return float(re_match.group(0)) if re_match else None
    
    tbl_titles = ["raster", "rt"]
    tbl_header = [
        "model", "fhd_ultra", "fhd_medium", "qhd_ultra", "uhd_ultra", "specs"]
        
    for table, title in zip(webpage_tables, tbl_titles):
        tbl_body = table.find("tbody")
        body = build_table(tbl_body)
        
        df = pd.DataFrame(body, columns=tbl_header)
        for colname in tbl_header[1:5]:
            df[colname] = df[colname].apply(get_fps_val)
        
        df.to_csv(f"data/tomshardware_{title}_avg_fps.csv", index=False)


#### Update V-Ray 5 render benchmark data from official page ####
#################################################################

URL = "https://benchmark.chaos.com/v5/vray-gpu-cuda"

df = read_r(getcwd() + "\\data\\prices.rds")[None]

unique_combinations = df.groupby(['ProductName','ProductFilters'])\
    .size().reset_index()

best_combinations = unique_combinations\
    .loc[unique_combinations['ProductFilters'].str.len()\
    .groupby(unique_combinations['ProductName']).idxmin()]

MODEL_NAMES = best_combinations["ProductName"]
MODEL_FILTERS = best_combinations["ProductFilters"]

DRIVER = webdriver.Chrome()
DRIVER.maximize_window() # Required to ensure all elements are on screen

def get_vray5_render_pts(MODEL_NAMES, MODEL_FILTERS):

    def filtered_result(name_to_filter: str, filters: list) -> bool:
        checks_up = False        
        for word in filters:
            if not bool( re.search(word.lower(), name_to_filter.lower()) ): 
                checks_up = True
            else: 
                checks_up = False
            if not checks_up: 
                break
        return checks_up
    
    def wait_grid_update(DRIVER):
        results_grid = "//div[@class='scores']"

        wait = WebDriverWait(DRIVER, timeout=15)
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
    
    def set_desired_sorting(DRIVER, attempts=5):
        clickable_sort_by_sample = "//a[@href and contains(text(), 'Benchmarks')]"
        element_sort_by_sample = "//span[@class='benchmarks']"
        
        navigate = DRIVER.find_element(By.XPATH, clickable_sort_by_sample)
        inspect = DRIVER.find_element(By.XPATH, element_sort_by_sample)
        
        wait = WebDriverWait(DRIVER, 10)
        required_status = ["current", "desc"]
        success = False

        ## test this section ##
        for att in range(attempts):
            wait.until(
                EC.presence_of_element_located(
                    (By.XPATH, element_sort_by_sample)
                )
            )
            
            current_grid_sort_status = inspect.get_attribute("class")

            if not all(i in current_grid_sort_status for i in required_status):
                wait.until(
                    EC.element_to_be_clickable(
                        (By.XPATH, clickable_sort_by_sample)
                    )
                )
                navigate.click()
                wait_grid_update(DRIVER)

            else:
                success = True
                break
        ## ---- ##
        
        if not success:
            raise TimeoutError(
                f"Couldn't sort the list correctly, list status:\n {current_grid_sort_status}"
            )
       
    results = []
    for model, filters in zip(MODEL_NAMES, MODEL_FILTERS):
        
        filter_is_empty = len(filters) == 0
        
        if filter_is_empty:
            filters = None
        else:
            filters = filters.split(", ")
        
        DRIVER.get(URL)
        
        navigate = DRIVER.find_element(By.CLASS_NAME, "advanced")
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
        set_desired_sorting(DRIVER)
        
        desired_element = "(//localised-number)[1]"
        wait = WebDriverWait(DRIVER, 10)
        wait.until(
            EC.visibility_of_element_located(
                (By.XPATH, desired_element)
            )
        )
        
        try:
            best_score = DRIVER.find_element(By.XPATH, desired_element)\
                .get_attribute("number")
            best_score = float(best_score)
        except:
            best_score = None
        scores.append(best_score)
        
    df = pd.DataFrame({"model" : MODEL_NAMES, "score" : scores})
    print(df)
    DRIVER.quit()
    
# run from command line: python3 ./routine/update_benchmarks.py
if __name__ == "__main__":
    #try: 
    #    get_th_avg_fps()
    #except Exception as expt: 
    #    print("Error trying to collect Avg. FPS from Tom's Hardware")
    #    print(expt)
    
    if path.isfile(getcwd() + "\\data\\prices.rds"):
        get_vray5_render_pts(['Geforce Rtx 3060'], ['Ti'])
    else: 
        print("Not able to collect Vray-5 benchmarks, '\\data\\prices.rds' not found in this folder")
    
    
