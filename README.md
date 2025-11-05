# Introduction to Machine Learning for the Life Sciences (R version)

This repository contains the R code and materials for the **Machine learning in the life sciences** workshop, presented by the Australian BioCommons and led by Dr. Benjamin Goudey.

The goal of this workshop is to provide a high-level, hands-on introduction to machine learning (ML). It covers what ML is, its advantages and disadvantages compared to traditional modelling, and scenarios where it is a useful tool. Using example datasets, we explore commonly used algorithms, their trade-offs, and common pitfalls in how ML is applied and evaluated, with a specific focus on life science applications.

## Repository Contents

* **`IntroToML2024_R.ipynb`**: This is the primary Jupyter notebook containing all the R code for the workshop's hands-on activities. It is designed to be run either in Google Colab or on your local machine.
* **Workshop Slides/References**: A PDF copy of the workshop slides and links to other useful resources.

---

## How to Run the Code

You can run the workshop notebook `IntroToML2024_R.ipynb` in two ways:

1.  **Google Colab (Recommended)**: This is the easiest way to get started, as it requires no local setup.
2.  **Locally**: You can run the notebook on your own computer if you have an R environment set up.

### 1. Using Google Colab

This method requires a Google account. We strongly recommend you complete the setup steps **before** the workshop, as some steps can take up to 20 minutes.

**A. Open the Notebook in Colab**

1.  Navigate to [https://colab.research.google.com/](https://colab.research.google.com/).
2.  **Sign in** to your Google account.
3.  Go to **File** → **Open notebook**.
4.  Select the **'GitHub'** tab.
5.  Paste this URL into the search box: `https://github.com/bwgoudey/IntroMLforLifeScienceWorkshopR/tree/main`.
6.  Click the search icon, and then click on `IntroToML2024_R.ipynb` to open it.
    
7.  **(Optional but recommended)** Save a copy to your own Google Drive by clicking **'Copy to Drive'**. This will allow you to save any changes you make.

**B. Set Up the Environment (Allow ~20 mins)**

Once the notebook is open, you must run two code cells to install the necessary R packages.

1.  Navigate to **'Section 0: Package loading and data set up'**.
2.  Run the **first code block** (starting with `# Google Colab set up`) by clicking the 'play' icon. This will take about 1 minute.
    
3.  Once it's finished, **skip the second block** and run the **third code block** (starting with `# Load packages`).
4.  This block can take **up to 20 minutes** to run.
    
5.  Once both cells have a green tick, your environment is ready.

### 2. Running Locally

You can also run the `IntroToML2024_R.ipynb` notebook on your local machine. This requires you to have R and Jupyter (e.g., via RStudio, VS Code, or JupyterLab) installed.

This workshop assumes familiarity with R and the `tidyverse` collection of packages.

You will need to install the following R package dependencies, which are listed in the notebook's setup cell:
* `tidyverse`
* `rpart.plot`
* `MASS`
* `scales`
* `remotes`
* `tidymodels`
* `DataExplorer`
* `pROC`
* `randomForest`
* `ranger`
* `glmnet`
* `devtools`

When running locally, you will need to adapt the code cells in **'Section 0'** to suit your local environment (i.e., do not run the Colab-specific setup code).

---

## Acknowledgements

This workshop is presented by the Australian BioCommons.

Thank you to Melissa Burke, Vicky Perreau, Christina Hall, and Giorgia Mori for their valuable input into this workshop material.
