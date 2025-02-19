#!/usr/bin/Rscript

# Code to use height-diameter models ----
#
# Aitor Vázquez Veloso
# 2024-11-25
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# How to cite this Python script:

#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# load libraries
import math
import pandas as pd



# Model equations

def elmamoun_2013_M18(a, b, dbh):
    """
    Calculates the estimated tree height based on the El Mamoun (2013) Model M18.

    Reference:
    El Mamoun, H. Osman, El Zein, A. Idris, & El Mugira, M. Ibrahim. (2013).
    Modelling height-diameter relationships of selected economically important natural forests species.
    Journal of Forest Products & Industries, 2, 34–42.
    URL: https://www.researchgate.net/profile/Elmamoun-Osman/publication/284581695_Modelling_height-diameter_relationships_of_selected_economically_important_natural_forests_species

    Parameters:
        a (float): First parameter of the equation
        b (float): Second parameter of the equation
        dbh (float): Tree diameter at breast height (cm)

    Returns:
        float: Estimated tree height (m)
    
    Created by Aitor Vázquez Veloso, 2024-11-25
    """
    # calculate the height using the provided formula
    h = 1.3 + a * (math.log(1 + dbh))**b
    return h



# Model parameters

def read_pars(path_to_pars):
    """
    Reads coefficient files from the specified directory and returns them as dataframes.

    Parameters:
        path_to_pars (str): Path to the directory containing the 'ab_coefs.csv' and 'fe_coefs.csv' files.

    Returns:
        dict: A dictionary containing two DataFrames:
            - 'ab': DataFrame with 'ab_coefs.csv' data
            - 'fe': DataFrame with 'fe_coefs.csv' data
    
    Example:
        pars = read_pars('/path/to/parameters/')
        ab_coefs = pars['ab']
        fe_coefs = pars['fe']
    """
    # Read the CSV files
    ab = pd.read_csv(f"{path_to_pars}/ab_coefs.csv")
    fe = pd.read_csv(f"{path_to_pars}/fe_coefs.csv")
    
    # Return the data as a dictionary
    return {'ab': ab, 'fe': fe}

def select_pars(tree, ab, fe, species_value='name', species_col='species_name', 
                clim_col='climate', mix_col='mixture', or_col='origin'):
    """
    Selects the specific parameters for a tree based on species, climate region, mixture type, and origin.

    Parameters:
        tree (dict): A dictionary with the tree values (just one row).
        ab (DataFrame): DataFrame with the parameters for the species-specific model.
        fe (DataFrame): DataFrame with the parameters for the fixed effects included in the model.
        species_value (str): 'name' or 'code' to select the species-specific parameters by the desired column ('name' by default).
        species_col (str): Column name with the species name or code to filter parameters ('species_name' by default).
        clim_col (str): Column name with the climate region ('climate' by default).
        mix_col (str): Column name with the mixture type ('mixture' by default).
        or_col (str): Column name with the origin type ('origin' by default).

    Returns:
        dict: A dictionary with parameters `a` and `b`, or an error message.
    """

    # Helper function to safely retrieve values from the dictionary
    def safe_get(dictionary, key, default=None):
        return dictionary[key] if key in dictionary else default

    # Select species-specific parameters
    species_value_in_tree = safe_get(tree, species_col)
    if species_value_in_tree is None:
        return "ERROR: Select the proper column name for the species name or code."

    if species_value == 'name':
        if species_value_in_tree in ab['Species name'].values:
            pars_ab = ab.loc[ab['Species name'] == species_value_in_tree].iloc[0]
        else:
            pars_ab = ab.loc[ab['Species name'] == 'All the species'].iloc[0]
    elif species_value == 'code':
        if species_value_in_tree in ab['Species code'].values:
            pars_ab = ab.loc[ab['Species code'] == species_value_in_tree].iloc[0]
        else:
            pars_ab = ab.loc[ab['Species code'] == 0].iloc[0]
    else:
        pars_ab = ab.loc[ab['Species name'] == 'All the species'].iloc[0]

    # Climate region parameters
    climate_value = safe_get(tree, clim_col)
    if climate_value == 'Atlantic':
        a_clim = fe['a ~ fe3 (Atlantic region)'].values[0]
        b_clim = fe['b ~ fe3 (Atlantic region)'].values[0]
    elif climate_value == 'Alpine':
        a_clim = fe['a ~ fe3 (Alpine region)'].values[0]
        b_clim = fe['b ~ fe3 (Alpine region)'].values[0]
    elif climate_value == 'Macaronesian':
        a_clim = fe['a ~ fe3 (Macaronesian region)'].values[0]
        b_clim = fe['b ~ fe3 (Macaronesian region)'].values[0]
    else:
        a_clim = b_clim = 0

    # Mixture parameters
    mix_value = safe_get(tree, mix_col)
    if mix_value == 'mix':
        a_mix = fe['a ~ fe1 (mix stand)'].values[0]
        b_mix = fe['b ~ fe1 (mix stand)'].values[0]
    else:
        a_mix = b_mix = 0

    # Origin parameters
    origin_value = safe_get(tree, or_col)
    if origin_value == 'plantation':
        a_or = fe['a ~ fe2 (artificial stand)'].values[0]
        b_or = fe['b ~ fe2 (artificial stand)'].values[0]
    else:
        a_or = b_or = 0

    # Calculate final parameters
    a = pars_ab['a'] + a_clim + a_mix + a_or
    b = pars_ab['b'] + b_clim + b_mix + b_or

    return {'a': a, 'b': b}



# Model application

def predict_height(df, path_to_pars='data/', dbh_col='dbh', species_value='name', species_col='species_name', 
                   clim_col='climate', mix_col='mixture', or_col='origin'):
    """
    Estimates tree height for each row in the DataFrame and appends the predicted height.

    Parameters:
        df (DataFrame): DataFrame with tree values.
        path_to_pars (str): Path to the directory containing parameter files.
        dbh_col (str): Column name with the tree diameter at breast height (cm) ('dbh' by default).
        species_value (str): 'name' or 'code' to select species-specific parameters by column ('name' by default).
        species_col (str): Column name with the species name or code to filter parameters ('species' by default).
        clim_col (str): Column name with the climate region ('climate_region' by default).
        mix_col (str): Column name with the mixture type ('pure' by default).
        or_col (str): Column name with the origin type ('natural' by default).

    Returns:
        DataFrame: Original DataFrame with the predicted tree height appended in a new column ('pred_h').
    """

    # Load parameters
    pars = read_pars(path_to_pars)
    ab = pars['ab']
    fe = pars['fe']

    # Initialize a list to store rows with predictions
    result_rows = []

    # Iterate over each row in the DataFrame
    for _, tree in df.iterrows():
      
        # Convert the current row to a dictionary
        tree_dict = tree.to_dict()

        # Get species-specific parameters
        params = select_pars(tree=tree_dict, ab=ab, fe=fe, species_value=species_value, 
                             species_col=species_col, clim_col=clim_col, mix_col=mix_col, or_col=or_col)

        # Check for errors in parameter selection
        if params == "ERROR":
            print("Error: Unable to select parameters for the tree.")
            break

        # Calculate predicted height using the height-diameter equation
        pred_h = elmamoun_2013_M18(a=params['a'], b=params['b'], dbh=tree_dict[dbh_col])

        # Add the predicted height to the row and append to results
        tree_dict['pred_h'] = pred_h
        result_rows.append(tree_dict)

    # Convert the list of dictionaries back to a DataFrame
    result_df = pd.DataFrame(result_rows)

    return result_df



# Sample data

def test_hd_df():
    """
    Generates a test dataset with the minimum required values for testing the height-diameter function.

    Parameters:
        None

    Returns:
        DataFrame: A DataFrame with sample tree data including species, climate regions, mixtures, and origins.
    """
    data = {
        'species_name': [
            'Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster',
            'Populus alba', 'Populus alba', 'Quercus robur', 'Quercus robur'
        ],
        'species_code': [26, 26, 26, 26, 51, 51, 41, 41],
        'dbh': [20, 20, 20, 20, 15, 15, 30, 30],
        'climate': [
            'Alpine', 'Atlantic', 'Macaronesian', 'Mediterranean',
            'Mediterranean', 'Mediterranean', 'Atlantic', 'Atlantic'
        ],
        'mixture': ['pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'mix'],
        'origin': ['natural', 'natural', 'natural', 'natural', 'natural', 'plantation', 'natural', 'natural']
    }

    df = pd.DataFrame(data)
    return df

def test_hd_df_2():
    """
    Generates a test dataset with the minimum required values for testing the height-diameter function.
    In that case, the column names were customized.

    Parameters:
        None

    Returns:
        DataFrame: A DataFrame with sample tree data including species, climate regions, mixtures, and origins.
    """
    data = {
        'species': [
            'Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster',
            'Populus alba', 'Populus alba', 'Quercus robur', 'Quercus robur'
        ],
        'codes': [26, 26, 26, 26, 51, 51, 41, 41],
        'dbh_cm': [20, 20, 20, 20, 15, 15, 30, 30],
        'region': [
            'Alpine', 'Atlantic', 'Macaronesian', 'Mediterranean',
            'Mediterranean', 'Mediterranean', 'Atlantic', 'Atlantic'
        ],
        'pure_mix': ['pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'mix'],
        'nat_plant': ['natural', 'natural', 'natural', 'natural', 'natural', 'plantation', 'natural', 'natural']
    }

    df = pd.DataFrame(data)
    return df



# Example of application with the test dataset

# # Generate the test dataset
# df = test_hd_df()
# 
# # Export dataset as CSV
# df.to_csv('test_hd_df.csv', index=False)
# print("Dataset exported to 'test_hd_df.csv'")
# 
# # Apply the function using detailed column names
# new_df = predict_height(
#     df=df, 
#     path_to_pars='data/', 
#     dbh_col='dbh', 
#     species_value='name', 
#     species_col='species_name', 
#     clim_col='climate_region', 
#     mix_col='mixture', 
#     or_col='origin'
# )
# 
# # Display the result
# print("Predicted heights with detailed column names:")
# print(new_df)
# 
# # Apply the function using default column names
# new_df_default = predict_height(df=df)
# 
# # Display the result
# print("Predicted heights with default column names:")
# print(new_df_default)
