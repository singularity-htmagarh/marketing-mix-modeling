# LIBRARIES ----

import pandas as pd
import numpy as np
import pytimetk as tk

from pymc_marketing.mmm.delayed_saturated_mmm import
DelayedSaturatedMMM

import arviz as az
import seaborn as sns
import matplotlib as plt

# DATA DEFINITION 

data = pd.read_csv("data/de_simulated_data.csv", parse_dates=['DATE'])

data.glimpse()

# Data Definition:
# - revenue = sales, target variable
# - ooh_S = Out of Home (Billboards, etc) Spend
# - tv_S = Television, Spend
# - print_S = Print Media (Newspapers, Magazines), Spend
# - facebook_I = Facebook Ads, Impressions
# - facebook_S = Facebook Ads, Spend
# - search_S = Google Search Ads, Search Spend
# - search_clicks_P = google Search Ads Performance, (Number of clicks)
# - competitor_sales_B = Competitor Sales Baseline

# * 1.0 Exploratory Data Analysis ---

df = data.copy()

df.columns = [col.lower() for col in df.columns]

df.columns.tolist()

# * Visualize Revenue and Spend by Marketing Channels

df \
    .melth(
            
    )