# -*- coding: utf-8 -*-
"""
Created by AMRIT MADHAV

"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mlxtend.preprocessing import TransactionEncoder
from mlxtend.frequent_patterns import apriori
from mlxtend.frequent_patterns import association_rules

############## NOTE ######################################################################################
##Opening the file in the read Mode
## This count variable will check how many objects has been detected by the YOLO-OpenCV Detector model
##########################################################################################################

file_itemset_lines = open('C:/Users/Desktop/deep_learning/yolo-object-detection/yolo-object-detection/out_item.txt','r')
count =1
##file_itemset_item = file_itemset_lines.readlines()

######## NOTE #################################################################################################################################
## BELOW FOR LOOP ACTUALLY ITERATE OVER THIS FILE (out_item.txt) TILL IT FINDS THE ITEM NAME (IF MANY OBJECT DETECTED BY Object Detector then 
## multiple names would be in the file.)
################################################################################################################################################

for item_names in file_itemset_lines:
    print('\n')
    print(item_names)
    ## below item_val is the value which has been passed from Detector Model
    item_val=item_names.strip()
    if item_val in ('person','cat','dog','sheep','cow'):
        continue
    
    store_datamart = pd.read_csv('C:/Users/Desktop/deep_learning/yolo-object-detection/yolo-object-detection/retail_store_data.csv')
    #print(store_datamart.head())
    #store_datamart=store_datamart.fillna(method='ffill')
    
    ############ NOTE ###############################################################################
    ## Taking len of rows and columns in the dataset
    ## Iterating over the datasets('retail_store_data.csv') to tranform into correct format. 
    ## Tranforming datasets into correct format using TransactionEncoder 
    ## Setting Minimum Suppport and Confidence
    ##################################################################################################
    
    nrows,ncols = store_datamart.shape
    nrows = int(nrows)
    ncols = int(ncols)
    dataset = []
    for i in range(0, nrows):  
        dataset.append([str(store_datamart.values[i,j]) for j in range(0, ncols)])
    te = TransactionEncoder()
    te_ary = te.fit(dataset).transform(dataset)
    df = pd.DataFrame(te_ary, columns=te.columns_)

    frequent_itemsets = apriori(df, min_support=0.001, use_colnames=True)
    #frequent_itemsets = apriori(df, min_support=0.001,min_confidence=0.1, use_colnames=True)
    
    ############ NOTE  ####################################################
    ## PREPARING LENGTH OF ALL SET OF RULES USING LAMBDA FUNCTION.
    #######################################################################
    
    frequent_itemsets['length'] = frequent_itemsets['itemsets'].apply(lambda x: len(x))

    association_rules(frequent_itemsets, metric="confidence", min_threshold=0.4)
    #print('Check frequent_itemsets before filtering the items :- ',frequent_itemsets)    
    #print(item_val)
    #frequent_itemsets=frequent_itemsets[itemsets for item_val in set(frequent_itemsets[itemsets])]
    #frequent_itemsets['itemsets2'] = frequent_itemsets['itemsets'].apply(lambda x: item_val in for i in list(set(x[i])) )
    #frequent_itemsets['itemsets2'] = frequent_itemsets['itemsets'].apply(lambda x: item==item_val for x in list(set(x)))
    print(count,' Identified/Detected Product :- ',item_val)
    #item_val = item_val
    
    #### NOTE #################################################################################################################################
    ## Below method will add a boolean column which will tell what all are the RULES which has the object name detected by Detector Model
    ###########################################################################################################################################
    
    def item_val_func(data):
        #print('data as sets',data)
        data_sets =list(data)
        #print('datasets value ', data_sets)
        #print('item value input',item_val)
        if item_val in data_sets:
            #print(' Inside item_val: ',item_val,'data_sets: ', data_sets)
            return True
        else:
            #print('item not found')
            return False
    #frequent_itemsets['itemsets2'] = frequent_itemsets['itemsets'].apply(lambda x: item_val in set(x))
    
    ########### NOTE ####################################################################################################################################
    ## Note: Using Below LAMBDA function, i am selecting all the set of RULES having 'item_val'(Detected object) in the SET. 'item_val' 
    ## is the value which has been passed from YOLO-OpenCV Detector Model.So basically if item_val is present in the set i will mark 
    ## as True else False.
    #####################################################################################################################################################
    
    frequent_itemsets['itemsets_bool'] = frequent_itemsets['itemsets'].apply(lambda x: item_val_func(x))
    frequent_itemsets= frequent_itemsets[frequent_itemsets['itemsets_bool'] == True]
    
    ################## NOTE ##############################################################
    ## Note: Selecting only those RULES having length greater than or equals to 2
    ########################################################################################
    
    frequent_itemsets=frequent_itemsets[ (frequent_itemsets['length'] >= 2) &
                                        (frequent_itemsets['support'] >= 0.0057) ]
    print('Your Product :-> ',item_val)
    df2 = pd.DataFrame(frequent_itemsets)
    products = []
    final_product = []
    final_set = {}
    for row in df2['itemsets']:
        ## As this being a Frozen Set .So converting into a SET.So as to iterate over.
        sets=[row]
        #print([print('\nRecommendations ,You can also check other products',list(x)) for x in sets])
        
        ####### Note ##############################
        ## Below Iterating over each rules
        ###########################################
        
        for x in sets:
            products = list(x)
            #print('products list check here:-> ',products)
            
            ####### Note ###################################
            ## Below Iterating over each items in a SET.
            ################################################
            
            for item in products:
                ##item = list(item)
                ##if products[0] == item_val:
                
                ########## NOTE ########################################################################################################
                ##Below in the loop checking if item is not in the list then only appending to the list in each iteration of the RULES.
                ########################################################################################################################
                
                if item not in final_product:
                    
                    ##final_product.append(products[0:])
                    final_product.append(item)                  
                    #print('We Recommend you to check ',"'",products[0],"'",' ->',products[1:])
                
    final_set = set(list(final_product))
    final_set -={item_val}
    final_set -={'nan'}
    final_set -={'person'}
    
    ############ NOTE ############################################################################
    ## IF FINAL_SET is null then there is no recommendation for the product/object detected
    ## Giving Recommendation to the User
    ###############################################################################################
    
    if len(final_set) == 0:            
        print('\nNo Recommendation for this Product ',"'",item_val,"'", '::')
    else:
        if count ==1:
            print('************************************************')
            print('\nHere is your Recommendation for the',count,'st' ,'Product:',"'",item_val,"'",' -> ','\n\n',final_set)
            print('************************************************')
        elif count ==2:
            print('************************************************')
            print('\nHere is your Recommendation for the',count,'nd' ,'Product:',"'",item_val,"'",' -> ','\n\n',final_set)
            print('************************************************')
        elif count ==3:
            print('************************************************')
            print('\nHere is your Recommendation for the',count,'rd' ,'Product:',"'",item_val,"'",' -> ','\n\n',final_set)
            print('************************************************')
        elif count ==4:
            print('************************************************')
            print('\nHere is your Recommendation for the',count,'th' ,'Product:',"'",item_val,"'",' -> ','\n\n',final_set)
            print('************************************************')
        elif count ==5:
            print('************************************************')
            print('\nHere is your Recommendation for the',count,'th' ,'Product:',"'",item_val,"'",' -> ','\n\n',final_set)
            print('************************************************')
        elif count ==6:
            print('************************************************')
            print('\nHere is your Recommendation for the',count,'th' ,'Product:',"'",item_val,"'",' -> ','\n\n',final_set)
            print('************************************************')
        else:
            print('************************************************')
            print('\nHere is your Recommendation for the',count ,' Product:',"'",item_val,"'",' -> ','\n\n',final_set)
            print('************************************************')
    count +=1