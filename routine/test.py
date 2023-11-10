from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
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

# Create a new instance of the Firefox driver
driver = webdriver.Chrome()
driver.maximize_window()

# Navigate to the website
driver.get("https://benchmark.chaos.com/v5/vray-gpu-cuda")

# Wait until the scores are loaded
sample_size_elems = "//li[contains(@class, 'row') and not(contains(@class, 'head'))]"
WebDriverWait(driver, 10).until(
    EC.presence_of_element_located(
        (By.XPATH, score_items)
    )
)

# Check if the scores are sorted by number of benchmarks (decreasing)
scores_list = driver.find_elements(By.XPATH, "//span[contains(@class, 'count')]")
scores_values = [int(score.text.replace(",", "")) for score in scores_list]
if scores_values != sorted(scores_values, reverse=True):
    # Click the option to sort by benchmark
    driver.find_element(By.LINK_TEXT, "Benchmarks").click()

    # Wait until the scores are updated
    wait_grid_update(driver)

    # Check if the scores are sorted by number of benchmarks (decreasing)
    scores_list = driver.find_elements(By.XPATH, "//span[contains(@class, 'count')]")
    scores_values = [int(score.text.replace(",", "")) for score in scores_list]

    faliure_message = f"The scores are NOT sorted by number of benchmarks (decreasing):\n{scores_values}"
    success_message = f"The scores are sorted by number of benchmarks (decreasing):\n{scores_values}"

    if scores_values != sorted(scores_values, reverse=True):
        print(faliure_message)
    else:
        print(success_message)
else:
    print(success_message)
