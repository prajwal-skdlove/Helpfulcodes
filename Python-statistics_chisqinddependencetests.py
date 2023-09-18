# -*- coding: utf-8 -*-
#%%    
import numpy as np
from scipy.stats import chi2_contingency

#%%
def chi_squared_ouput(statistic, dof, pvalue, expectedcounts):
    dict_chi_squared = {}
    dict_chi_squared['statistic'] = statistic
    dict_chi_squared['dof'] = dof
    dict_chi_squared['pvalue'] = pvalue
    dict_chi_squared['expectedcounts'] = expectedcounts
    
    return dict_chi_squared

#%%
def chi_squared_independence_test(observed_counts, group_names, alpha=0.05):
    """
    Perform a chi-squared test for independence for more than 2 groups.
    
    Parameters:
    - observed_counts: A list of lists, where each inner list contains observed counts for a group.
    - alpha: The significance level for determining significance (default is 0.05).
    - group_names : name of the groups in a list that matches the number of groups

    Returns:
    - A dictionary where significant group pairs are keys, and their p-values are values.
    """

    chi2, p, dof, expected = chi2_contingency(observed_counts)
    
    if p < alpha:
        significantpairs=1
    else:
        significantpairs=0
    
    chisq_allgroups_results_dict = chi_squared_ouput(chi2, dof, p, expected)
    chisq_allgroups_results_dict["significantpairs"]=significantpairs
    
    print("Chi-Squared Test of Independence between all Groups")
    print(f"Chi-squared statistic: {chi2:.2f};\nDegrees of freedom: {dof:.0f};\nP-value: {p:.4f}")
    if p < alpha:
        print("There is a significant difference between the groups.")
    else:
        print("There is no significant difference between the groups.")
   
    chisq_tests_dict = {}
    key = "allgroups"
    chisq_tests_dict[key] = chisq_allgroups_results_dict

    # Perform pairwise chi-squared tests with Bonferroni correction
    num_groups = len(observed_counts)
    
    for i in range(num_groups):
        for j in range(i + 1, num_groups):
            group_pair = group_names[i], group_names[j]
            contingency_table = np.array([observed_counts[i], observed_counts[j]])
            chi2_groups, p_groups, dof_groups, expected_groups = chi2_contingency(contingency_table)
            p_adjusted = p_groups * (num_groups * (num_groups - 1)) / 2  # Bonferroni correction
            
            if p_adjusted < alpha:
                significantpairs_groups=1
            else:
                significantpairs_groups=0
                
            chisq_betweengroups_dict =  chi_squared_ouput(chi2_groups, dof_groups, p_adjusted, expected_groups)
            chisq_betweengroups_dict["significantpairs"]=significantpairs_groups
            
            key = group_pair
            chisq_tests_dict[key] = chisq_betweengroups_dict
            
            print("")
            print(f"Chi-Squared Test of Independence between {group_names[i]} & {group_names[j]}")
            print(f"Chi-squared statistic: {chi2_groups:.2f};\nDegrees of freedom: {dof_groups:.0f};\nP-value: {p_adjusted:.4f}")
            if p_adjusted < alpha:
                print("There is a significant difference between the groups.")
            else:
                print("There is no significant difference between the groups.")
            print("")
            
          
    return chisq_tests_dict
