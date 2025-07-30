#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
High-level implementations of the apriori algorithm.
"""

import random
from collections import defaultdict
from itertools import combinations

# THIS IS NOT FUNCTIONAL. IT DOES NOT WORK 100% OF THE TIME. USUALLY, IT GOES OF TO INFINITY. 
# DO NOT USE IF YOU DO NOT KNOW WHAT YOU ARE DOING.
# This is based off An Efficient Rigorous Approach for Identifying Statistically Significant Frequent Itemsets by Kirsch et al
# This is unfinished. I believed that I have almost replicated algorithm 1 from the paper. It, however, does not work for our needs. 
# One would also need to incorporate the tests in algorithm 2 for this to be complete.


def estimate_s_min(transactions, k, f):
    def I(X):
        return [Y for Y in W if set(X).intersection(Y)]
    def b1(s):
        return sum(px_s[s][X] * px_s[s][Y] for X in W for Y in I(X) if X != Y)
    def b2(s):
        seen = set()
        total = 0
        for X in W:
            for Y in I(X):
                if X != Y:
                    key = tuple(sorted((X, Y)))
                    if key not in seen:
                        total += pxy_s[s].get(key, 0)
                        seen.add(key)
        return total
    delta = 1000
    epsilon = .01
    t = len(transactions)
    f_list = sorted(f.items(), key=lambda item: item[1], reverse=True)
    top_k_freq = 1.0
    for i, freq in f_list[:k]:
        top_k_freq *= freq
    s_tilde = t * top_k_freq
    smax = 0
    while True:
        support_per_dataset = defaultdict(lambda: defaultdict(int))  #Itemset -> datasetIndex -> support
        items = list(f.keys())
        W = set()
        while len(W) == 0:
            print(len(W))
            for i in range(delta):
                D_hat = []
                for j in range(t):
                    D_hat.append([item for item in items if random.random() <= f[item]])
                
                #Count k-itemset supports
                support_hat = defaultdict(int)
                for transaction in D_hat:
                    sorted_trans = sorted(transaction)
                    for k_itemset in combinations(sorted_trans, k):
                        support_hat[k_itemset] += 1
                
                #Add frequent k-itemsets for this dataset to W
                for itemset, support in support_hat.items():
                    support_per_dataset[itemset][i] = support
                    if support >= s_tilde:
                        W.add(itemset)
        print(W)   
        if smax == 0:
            #max support of any X in W over all datasets + 1
            max_sup = max(max(support_per_dataset[itemset].values()) for itemset in W)
            smax = max_sup + 1

        px_s = {}
        pxy_s = {}
        for s in range(int(s_tilde), int(smax)+1):
            px = {}
            pxy = {}
            seenxy = set()
            for X in W:
                count = sum(1 for sim in support_per_dataset[X] if support_per_dataset[X][sim] >= s)
                px[X] = count / delta
            for X in W:
                for Y in W:
                    if X != Y and set(X).intersection(Y) and (tuple(sorted((X, Y))) not in seenxy):
                        # joint count
                        joint_count = 0
                        seenxy.add(tuple(sorted((X, Y))))
                        for sim in range(delta):
                            if (support_per_dataset[X].get(sim, 0) >= s and support_per_dataset[Y].get(sim, 0) >= s):
                                joint_count += 1
                        pxy[tuple(sorted((X, Y)))] = joint_count / delta
            
            px_s[s] = px
            pxy_s[s] = pxy
        if b1(int(s_tilde)) + b2(int(s_tilde)) <= epsilon / 4:
            smax = int(s_tilde)
            s_tilde /= 2
        else:
            break
    #Find minimal s > s_tilde such that condition holds
    for s in range(int(s_tilde), int(smax) + 1):
        if b1(s) + b2(s) <= epsilon / 4:
            return s
