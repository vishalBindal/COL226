Assignment 5
Readme

Note: Test cases are commented in the code at the bottom.

Representations:
1) variable: as string
2) symbol: as string
3) signature: as (symbol,arity) list, where arity is an integer
4) term: as V(variable) | Node(symbol, term list)
5) substitution: as (term,term) list. (t1,t2) means t1 is to be substituted by t2

Functions:
1) qsort
    - to sort the signature (i.e. (symbol*int) list) by sorting on basis of lexicographical order of string of symbol
    - e.g. for the given signature
        let s1 = [("Zero",0);("One",0);("Add",2);("Inc",1);("Some",5)];;
      The sorted signature will be
        [("Add", 2); ("Inc", 1); ("One", 0); ("Some", 5); ("Zero", 0)];;
    - The sorting is done to check for duplicate symbols in check_sig quickly
    - Time complexity = O(nlogn), where n is no of symbols in the signature

2) check_sig
    - It checks:
        (a) arity >= 0 for all symbols
        (b) no repetition of symbols
        (c) symbols <> ""
    - For checking (b), (qsort s) is passed to the tail recursive function check_sig_sorted. 
    - Given starting signature sign0 = [(s1,a1);(s2,a2);...(sn,an)], 
        then sign=[(s(k+1),a(k+1));...;(sn,an)] and prev_sym=sk at any stage in check_sig_sorted.
    - Since duplicate symbols will appear adjacent to each other in sorted signature, it is checked if s(k+1)=s(k) for all k. It is also checked if ak>=0 and sk <> "" for all k
    - Time complexity = O(nlogn + n) = O(nlogn), where n=no of symbols in the signature

3) wfterm
    - checks
        (a) all symbols belong to the given signature
        (b) no of elements in list of each symbol equals to its arity
    - For each variable, time = O(k) for checking if variable name matches with a symbol name in the signature.
      k = no of symbols in the signature
    - For each node, time = O(k + a), where a = no of elements in term list. It is checked if the symbol matches any symbol in the signature, and that length of term list is eqaul to the arity.
    - fold_left and map are used for Nodes.
    - Therefore total time = O(n*(k+a)), where n = no of nodes+variables in the tree (size of tree)

4) ht
    - ht(variable) = 0, ht(f(t1,t2,..tk)) = 1 + max(-1,ht(t1),ht(t2)..,ht(tk)), k>=0
    - fold_left and map are used for Nodes
    - Assumptions:
        (a) The term is well-formed
        (b) Height of leaves (variable/constant) is Zero
    - Time complexity = O(n), n is size of tree

5) size
    - size(variable) = 1, size(f(t1,t2,...tk)) = 1 + size(t1) + size(t2) + ... size(tk), k>=0
    - fold_left and map are used for Nodes
    - Assumption: The term is well-formed
    - Time complexity = O(n), n is size of tree

6) set
    - set(var) = [var], set(f(t1,t2,..tk)) = combinesets (set(t1) set(t2) .. set(tk)), k>=0
    - fold_left and map are used for Nodes
    - For nodes, set function is mapped to all subterms, and then all sets are combined.
    - For combining 2 sets, all elements of 2nd set are checked if they are present in 1st set, if not then the element is added. So time complexity for combinesets = O(n^2), n = no of elements in the set
    - Time complexity for set = O(n^2), n = size of tree

7) subst
    - For each variable, the substitution s is searched for that particular variable. If found, the variable is substituted
    - For nodes, map is used to apply subst to all terms in the list
    - ASSUMPTION: The term is well-formed
    - Time complexity = O(n*k), n=size of tree, k=no of substitutions in the list

8) compose
    - For composing two substitutions s1 and s2
    - For each (x,t) tuple in s2, it is checked if any term in s1 is of the form (y,x), it is replaced by (y,t).
    - Time complexity = O(n*m), where n, m are no of substitutions in s1 and s2

9) mgu
    - Implemented according to table studied in class
    - ASSUMPTION: t1 and t2 are well-formed
    - For cases where t1 = f(x1,x2,..xn) and t2 = f(y1,y2,...yn), the composition sigma1.sigma2....sigma(n) is found using findsigma function.
    
10) findsigma ls1 ls2 sprev
    - given sprev = sigma1.sigma2....sigma(k-1) and ls1 = [tk,t(k+1)...tn] and ls2 = [t'k,t'(k+1)...t'n], 1<=k<=n+1,
    find sigma1.sigma2...sigma(n)
    - The first terms in ls1 and ls2 are taken, sprev are applied to them, and their mgu is found out. The mgu is then composed with sprev to set sprev for next iteration. sprev is returned when ls1 and ls2 become empty.