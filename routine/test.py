from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.support import expected_conditions as EC

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

def grid_is_present(driver):
    try:
        driver.find_element(By.CLASS_NAME, "no-scores")
        return False
    except NoSuchElementException:
        return True
    

# Create a new instance of the Firefox driver
driver = webdriver.Chrome()
driver.maximize_window()

# Navigate to the website
driver.get("https://benchmark.chaos.com/v5/vray-gpu-cuda")

print(grid_is_present(driver))