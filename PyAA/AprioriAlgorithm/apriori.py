#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
High-level implementations of the apriori algorithm.
"""

import typing
from itemset import itemsets_from_transactions
from rules import generate_rules_apriori
import numpy as np
from sklearn.cluster import KMeans
from collections import defaultdict
from scipy.cluster.hierarchy import fclusterdata
from scipy.spatial import Voronoi, voronoi_plot_2d, cKDTree

def apriori(
    transactions: typing.Iterable[typing.Union[set, tuple, list]],
    min_support: float = 0.5,
    min_confidence: float = 0.5,
    max_length: int = 20,
    verbosity: int = 0,
    output_transaction_ids: bool = False,
    bins: int = 1,
):
    if bins == 1:
        transactions = [item[1:] for item in transactions]
        itemsets, num_trans = itemsets_from_transactions(
            transactions,
            min_support,
            max_length,
            verbosity,
            output_transaction_ids=True,
        )

        itemsets_raw = {
        length: {item: counter.itemset_count for (item, counter) in itemset_dic.items()}
         for (length, itemset_dic) in itemsets.items()
        }
        rules = generate_rules_apriori(itemsets_raw, min_confidence, num_trans, verbosity)

        if output_transaction_ids:
            return itemsets, list(rules)

        return itemsets_raw, list(rules)
    else:
        
        #the comments below are only for if you are using the voronoi algorithm and want to create the graph
        trans, vor, groups = dividebins(bins=bins, transactions=transactions)# trans, vor, groups

        #this is for splitting each transaction into a seporate group, not incorporating the binning into the transaction set
        '''itemset ={}
        rule = {}
        for group in trans:    
            itemsets, num_trans = itemsets_from_transactions(
                trans[group],
                2/len(trans[group]),
                max_length,
                verbosity,
                output_transaction_ids=True,
            )
            itemsets_raw = {
            length: {item: counter.itemset_count for (item, counter) in itemset_dict.items()}
            for (length, itemset_dict) in itemsets.items()
            }
            rules = list(generate_rules_apriori(itemsets_raw, min_confidence, num_trans, verbosity))
            if output_transaction_ids:
                itemset[group] = itemsets
            else:
                itemset[group] = itemsets_raw
            rule[group] = rules
        
        return itemset, rule #itemset, rule, vor, groups'''
        
        #this is for incorporating the grouping into each transaction (either method)
        itemsets, num_trans = itemsets_from_transactions(trans, min_support, max_length, verbosity,output_transaction_ids=True)
        itemsets_raw = {
        length: {item: counter.itemset_count for (item, counter) in itemset_dic.items()}
         for (length, itemset_dic) in itemsets.items()
        }
        rules = generate_rules_apriori(itemsets_raw, min_confidence, num_trans, verbosity)

        if output_transaction_ids:
            return itemsets, list(rules)

        return itemsets_raw, list(rules), vor, groups

#equal width binning
"""def dividebins(bins, transactions):
    first_numbers = [t[0] for t in transactions]
    min_val, max_val = min(first_numbers), max(first_numbers)
    bin_edges = np.linspace(min_val, max_val, bins + 1)

    #this is for splitting each transaction into a seporate group, not incorporating the binning into the transactionset
    '''result = {}
    for i in range(bins):
        lo = bin_edges[i]
        hi = bin_edges[i + 1]
        key = f'{lo:.4f}<=x<={hi:.4f}'
        result[key] = []
    for t in transactions:
        num = t[0]
        data = t[1:]
        for i in range(bins):
            lo = bin_edges[i]
            hi = bin_edges[i + 1]
            if (lo <= num <= hi) if i == bins - 1 else (lo <= num < hi):
                key = f'{lo:.4f}<=x<={hi:.4f}'
                result[key].append(data)
                break'''
    #this is for incorporating the grouping as the first item in each transaction {bin group, item1, item2,...}
    '''result = []
    for i in range(bins):
        lo = bin_edges[i]
        hi = bin_edges[i + 1]
        print(lo, hi)
        key = f'{lo:.4f}<=x<={hi:.4f}'
        for t in transactions:
            num = t[0]
            data = t[1:]
            if (lo <= num <= hi):
                result.append([key]+data)'''
    #this is for incorporating the grouping as an additional element of each item {bingroup item1, bingroup item2,...}
    result = []
    for i in range(bins):
        lo = bin_edges[i]
        hi = bin_edges[i + 1]
        print(lo, hi)
        key = f'{lo:.4f}<=x<={hi:.4f}'
        for t in transactions:
            num = t[0]
            data = t[1:]
            if (lo <= num <= hi):
                info = []
                for h in data:
                    info.append(key+': '+h)
                result.append(info)

    return result"""

#Equal frequency binning
"""def dividebins(bins, transactions):
    if not transactions or bins <= 0:
        return {}
    sorted_transactions = sorted(transactions, key=lambda x: x[0])
    n = len(transactions)
    size = n // bins
    remainder = n % bins
    #this is for splitting each transaction into a seporate group, not incorporating the binning into the transactionset
    '''result = {}
    start_idx = 0
    for i in range(bins):
        bin_size = size + (1 if i < remainder else 0)
        end_idx = start_idx + bin_size
        bin_slice = sorted_transactions[start_idx:end_idx]
        if bin_slice:
            lo = bin_slice[0][0]
            hi = bin_slice[-1][0]
        else:
            lo = hi = None
        key = f'{lo:.4f}<=x<={hi:.4f}' if lo is not None else f'empty_bin_{i}'
        result[key] = [t[1:] for t in bin_slice]

        start_idx = end_idx'''
    #this is for incorporating the grouping as the first item in each transaction {bin group, item1, item2,...}
    '''start_idx = 0
    result = []
    for i in range(bins):
        bin_size = size + (1 if i < remainder else 0)
        end_idx = start_idx + bin_size
        bin_slice = sorted_transactions[start_idx:end_idx]
        if bin_slice:
            lo = bin_slice[0][0]
            hi = bin_slice[-1][0]
        else:
            lo = hi = None
        key=([[f'{lo:.4f}<=x<={hi:.4f}' if lo is not None else f'empty_bin_{i}']+t[1:] for t in bin_slice])
        result.append(key)
        start_idx = end_idx
    result = [element for nestedlist in result for element in nestedlist]'''
    #this is for incorporating the grouping as an additional element of each item {bingroup item1, bingroup item2,...}
    start_idx = 0
    result = []
    for i in range(bins):
        bin_size = size + (1 if i < remainder else 0)
        end_idx = start_idx + bin_size
        bin_slice = sorted_transactions[start_idx:end_idx]
        if bin_slice:
            lo = bin_slice[0][0]
            hi = bin_slice[-1][0]
        else:
            lo = hi = None
        string = f'{lo:.4f}<=x<={hi:.4f}' if lo is not None else f'empty_bin_{i}'
        key=([[string+': '+ h  for h in t[1:]] for t in bin_slice])
        result.append(key)
        start_idx = end_idx
    result = [element for nestedlist in result for element in nestedlist]

    
    return result"""

#Kmean binning
"""def dividebins(bins, transactions):
    transactionsvalues = [[x[0]] for x in transactions]
    transactionsvalues = list(map(tuple, transactionsvalues))
    kmeans = KMeans(n_clusters=bins)
    kmeans.fit(transactionsvalues)
    first_numbers = [t[0] for t in transactions]
    min_val, max_val = min(first_numbers), max(first_numbers)
    binedge = [min_val]
    for i in range(kmeans.cluster_centers_.__len__()-1):
        low = kmeans.cluster_centers_[i-1]
        high = kmeans.cluster_centers_[i]
        middle = float(((high + low)/2)[0])
        binedge.append(middle)
    binedge.append(max_val)
    binedge = sorted(binedge)
    #this is for splitting each transaction into a seporate group, not incorporating the binning into the transactionset
    '''result = {}
    for i in range(bins):
        lo = binedge[i]
        hi = binedge[i + 1]
        key = f'{lo:.4f}<=x<={hi:.4f}'
        result[key] = []
    for t in transactions:
        num = t[0]
        data = t[1:]
        for i in range(bins):
            lo = binedge[i]
            hi = binedge[i + 1]
            if (lo <= num <= hi) if i == bins - 1 else (lo <= num < hi):
                key = f'{lo:.4f}<=x<={hi:.4f}'
                result[key].append(data)
                break'''
    #this is for incorporating the grouping as the first item in each transaction {bin group, item1, item2,...}
    '''result = []
    for i in range(bins):
        lo = binedge[i]
        hi = binedge[i + 1]
        print(lo, hi)
        key = f'{lo:.4f}<=x<={hi:.4f}'
        for t in transactions:
            num = t[0]
            data = t[1:]
            if (lo <= num <= hi):
                result.append([key]+data)'''
    #this is for incorporating the grouping as an additional element of each item {bingroup item1, bingroup item2,...}
    result = []
    for i in range(bins):
        lo = binedge[i]
        hi = binedge[i + 1]
        print(lo, hi)
        key = f'{lo:.4f}<=x<={hi:.4f}'
        for t in transactions:
            num = t[0]
            data = t[1:]
            if (lo <= num <= hi):
                info = []
                for h in data:
                    info.append(key+': '+h)
                result.append(info)
    return result"""

#Hierarchial Binning
#Usually 1 is best for the distance if using stellar eqt and metallicity
"""def dividebins(maxdist, transactions):
    values = np.array([[x[0], x[1]] for x in transactions])
    clusters = fclusterdata(values, t=maxdist, criterion='distance')
    num = 0
    #this is for splitting each transaction into a seporate group, not incorporating the binning into the transactionset
    '''result = {}
    for i in transactions:
        string = 'group ' + str(clusters[num])
        if (string in result):
            result[string].append(i[2:])
        else:
            result[string] = [i[2:]]
        num+=1 '''
    #this is for incorporating the grouping as the first item in each transaction {bin group, item1, item2,...}
    '''result = []
    for i in transactions:
        string = 'group ' + str(clusters[num])
        result.append([string]+i[2:])'''
    #this is for incorporating the grouping as an additional element of each item {bingroup item1, bingroup item2,...}
    result = []
    for i in transactions:
        string = 'group ' + str(clusters[num])
        itemset = []
        for h in i[2:]:
            itemset.append(string+': '+str(h))
        result.append(itemset)
    return result"""

#kmeans with two values
"""def dividebins(k, transactions):
    values = np.array([[x[0], x[1]] for x in transactions])
    kmeans = KMeans(n_clusters=k, random_state=0)
    labels = kmeans.fit_predict(values)

    #this is for splitting each transaction into a seporate group, not incorporating the binning into the transactionset
    '''result = defaultdict(list)
    for i, transaction in enumerate(transactions):
        result[f"group {labels[i]}"].append(transaction[2:])
    return dict(result)'''
    #this is for incorporating the grouping as the first item in each transaction {bin group, item1, item2,...}
    '''result = []
    for i, transaction in enumerate(transactions):
        result.append([f"group {labels[i]}"] + transaction[2:])
    return result'''
    #this is for incorporating the grouping as an additional element of each item {bingroup item1, bingroup item2,...}
    result = []
    for i, transaction in enumerate(transactions):
        itemset = []
        for h in transaction[2:]:
            itemset.append(f"group {labels[i]}"+': '+h)
        result.append(itemset)
    return result"""

#voronoi binning
def dividebins(bins, transactions):
    xvals = [x[0] for x in transactions]
    yvals = [x[1] for x in transactions]
    groups = {}
    xlow, xhigh = min(xvals), max(xvals)
    ylow, yhigh = min(yvals), max(yvals)
    points = np.array([[np.random.uniform(xlow, xhigh), np.random.uniform(ylow, yhigh)] for i in range(bins)])
    vor = Voronoi(points)
    tree = cKDTree(points)
    #this is for splitting each transaction into a seporate group, not incorporating the binning into the transactionset
    '''assignments = {}
    distances, indices = tree.query([[x[0], x[1]] for x in transactions])
    for idx, transaction in enumerate(transactions):
        string = 'group ' + str(indices[idx])
        if string not in assignments:
            assignments[string] = []
        assignments[string].append(transaction[2:])
        if not indices[idx] in groups:
            groups[indices[idx]] = transaction[:2]
    assignments = dict(sorted(assignments.items()))'''
    #this is for incorporating the grouping as the first item in each transaction {bin group, item1, item2,...}
    '''assignments = []
    distances, indices = tree.query([[x[0], x[1]] for x in transactions])
    for idx, transaction in enumerate(transactions):
        string = 'group ' + str(indices[idx])
        assignments.append([string]+transaction[2:])
        if not indices[idx] in groups:
            groups[indices[idx]] = transaction[:2]'''
    #this is for incorporating the grouping as an additional element of each item {bingroup item1, bingroup item2,...}
    assignments = []
    distances, indices = tree.query([[x[0], x[1]] for x in transactions])
    for idx, transaction in enumerate(transactions):
        string = 'group ' + str(indices[idx])
        itemset = []
        for h in transaction[2:]:
            itemset.append(string+': '+ h)
        assignments.append(itemset)
        if not indices[idx] in groups:
            groups[indices[idx]] = transaction[:2]


    return assignments, vor, groups


if __name__ == "__main__":
    import pytest

    pytest.main(args=[".", "--doctest-modules", "-v"])