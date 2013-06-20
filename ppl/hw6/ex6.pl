% Question 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1.1
% Signature: check_barcodes_product(Barcode)/1
% Purpose: report barcodes that appear in two different product records.
% 
check_barcodes_product(Barcode) :- 
    product(Barcode, ProductName1, Category, Refrigerated, VendorName),
    product(Barcode, ProductName2, Category, Refrigerated, VendorName),
    ProductName1 \= ProductName2.

check_barcodes_product(Barcode) :- 
    product(Barcode, ProductName, Category1, Refrigerated, VendorName),
    product(Barcode, ProductName, Category2, Refrigerated, VendorName),
    Category1 \= Category2.

check_barcodes_product(Barcode) :- 
    product(Barcode, ProductName, Category, Refrigerated1, VendorName),
    product(Barcode, ProductName, Category, Refrigerated2, VendorName),
    Refrigerated1 \= Refrigerated2.

check_barcodes_product(Barcode) :- 
    product(Barcode, ProductName, Category, Refrigerated, VendorName1),
    product(Barcode, ProductName, Category, Refrigerated, VendorName2),
    VendorName1 \= VendorName2.

% 1.2
% Signature: phone_product(ProductName, Phone)/2
% Purpose: the phone number of the vendor corresponding to the product name.
% Examples:
% ?- phone_product(pita, P).
% P = 8877665
%
% ?- phone_product(chips, P).
% P = 5456464 ;
% P = 2002001
%
% ?- phone_product(N, 4564564).
% N = fresh_milk ;
% N = preserved_milk 
%
% ?- phone_product(lego, P).
% false
%
% ?- phone_product(N, 1234567).
% false
%
%
%

phone_product(ProductName, Phone) :- 
    product(Barcode, ProductName, Category, Refrigerated, VendorName),
    vendor_info(VendorName, Phone, MonthlyContactDay).

% 1.3
% Signature: unrefrigerated_ordering(CategoryName, MonthlyContactDay)/2
% Purpose: CategoryName is a name of a category that contains unrefrigerated
%          products that should be ordered on day MonthlyContactDay.
% Examples:
% ?- unrefrigerated_ordering(dairy, MCD).
% MCD = 5
%
% ?- unrefrigerated_ordering(snack, MCD).
% MCD = 28 ;
% MCD = 28 ;
% MCD = 28
%
% ?- unrefrigerated_ordering(C, 28).
% C = bread ;
% C = bread ;
% C = snack ;
% C = snack ;
% C = snack
%
% ?- unrefrigerated_ordering(toys, MCD).
% false
%
% ?- unrefrigerated_ordering(C, 1).
% false
%

unrefrigerated_ordering(CategoryName, MonthlyContactDay) :-
    product_category(CategoryId, CategoryName),
    product(Barcode, ProductName, CategoryId, false, VendorName),
    vendor_info(VendorName, Phone, MonthlyContactDay).

% 1.4
% Signature: unrefrigerated_ordering_list(CategoryDayPairsList)/1
% Purpose: CategoryDayPairsList is a list of
%          all unique CategoryName-MonthlyContactDay pairs
%          such that CategoryName contains unrefrigerated products
%          that should be ordered on day MonthlyContactDay.
% Postcondition: The list must be without repetitions 
%                (any list order is acceptable as answer).
% Example:
% ?- unrefrigerated_ordering_list(CategoryDayPairsList).
% CategoryDayPairsList = [(misc, 10), (snack, 28), (bread, 10), (bread, 28), (dairy, 5)]
%

%unrefrigerated_ordering_list([]).
%unrefrigerated_ordering_list([H|T]) :- 
    %H = (CategoryName, MonthlyContactDay),
    %unrefrigerated_ordering(CategoryName, MonthlyContactDay),
    %not_member((CategoryName, MonthlyContactDay), T),
    %unrefrigerated_ordering_list(T).

% Question 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2.1
% Signature: deep_reverse(Lst, RevLst)/2
% Purpose: RevLst is a deep reversal of Lst.
% Precondition: Lst is fully instantiated.
% Examples:
% ?- deep_reverse([a, b, c], [c, b, a]).
% true
%
% ?- deep_reverse([a, b, c], X).
% X = [c, b, a]
%
% ?- deep_reverse([a, b, [c, [c, 4], [], 8], [11], 3], X).
% X = [3, [11], [8, [], [4, c], c], b, a]
%
% ?- deep_reverse([a, b, c], [b, c, a]).
% false
%

deep_reverse([], []) :- !.
deep_reverse([H|T], List) :-
    !,
    deep_reverse(T, RT),
    deep_reverse(H, RH),
    append(RT, [RH], List).

deep_reverse(X, X).

% 2.2
% Signature: sublist_perm(Full, PermSub)/2
% Purpose: PermSub is permutation of a sublist of Full.
% Precondition: Full is fully instantiated.
% Examples:
% ?- sublist_perm([a, b, c], [a]).
% true
%
% ?- sublist_perm([a, b, c], [c, a]).
% true
%
% ?- sublist_perm([a, a, b, c], [a, a, a]).
% false
%
% ?- sublist_perm([r, a, s, d, c, b], [a, b, c]).
% true
%
% ?- sublist_perm([r, a, s, d, c, b], [a, b, f]).
% false
%
% ?- sublist_perm([a, b, c], X). /* permutations of sublists of [a, b, c] */
% /* order of answers is not important */
% X = [] ;
% X = [c] ;
% X = [b] ;
% X = [b, c] ;
% X = [c, b] ;
% X = [a] ;
% X = [a, c] ;
% X = [c, a] ;
% X = [a, b] ;
% X = [b, a] ;
% X = [a, b, c] ;
% X = [b, a, c] ;
% X = [b, c, a] ;
% X = [a, c, b] ;
% X = [c, a, b] ;
% X = [c, b, a]
%

% Signature: delete(Needle, Haystack, Result)/3
% Purpose: Delete the first occurrence of Needle in Haystack.
delete(Needle, [], []).
delete(Needle, Haystack, Result) :-
    delete_help(Needle, Before, Haystack, Result).

delete_help(Needle, Before, [Needle|After], Result) :-
    append(Before, After, Result),!.

delete_help(Needle, Before, [X|After], Result) :-
    append(Before, [X], Z),
    delete_help(Needle, Z, After, Result),!.

sublist_perm(X, []).
sublist_perm([], X) :- !, false.
sublist_perm(Full, [H|T]) :-
    member(H, Full),
    delete(H, Full, Without),
    sublist_perm(Without, T).

% 2.3
% Signature: s(Z)/1
% Purpose: a context-free grammar for a subset of English.
%          The operator ';' is used for disjunction, so that
%          all derivation rules of a single left-hand variable
%          appear in a single rule, instead of specifying several axioms, 
%          one for each derivation rule.
% Examples:
% ?- s([the, cat, saw, a, dog]).
% true
%
% ?- s([the, cat, ate, the, mouse, in, the, house]).
% true
%
% ?- s([the, big, funny, mouse, ate, a, cat]).
% true
%
% ?- s([the, big, funny, mouse, ate, a, tall, cat]).
% true
%
% ?- s([the, funny, cat, saw, a, dog, in, the, tall, house]).
% true
%
% ?- s([a, funny, beautiful, cat, saw, a, dog, in, the, tall, house]).
% true
%
% ?- s([a, funny, beautiful, cat, saw, a, dog, in, the, tall, beautiful, house]).
% true
%
% ?- s([a, funny, beautiful, cat, in, the, tall, beautiful, house, saw, a, dog]).
% false
%
% ?- s([a, cat, ate, it]).
% false
%
s(Z)     :- append(X, Y, Z), np(X), vp(Y).
pp(Z)    :- Z=[] ; (append(X, Y, Z), p(X), np(Y)).
np(Z)    :- append(A, Y, Z), append(V, X, A), det(V), adjs(X), n(Y).
adjs(Z)  :- Z=[] ; (Z=[X|Xs], adj([X]), adjs(Xs)).
adj([Z]) :- member(Z, [beautiful, funny ,tall, big]).
vp(Z)    :- append(A, Y, Z), append(V, X, A), v(V), np(X), pp(Y).
det([Z]) :- member(Z, [a, the]).
n([Z])   :- member(Z, [cat, dog, mouse, house]).
v([Z])   :- member(Z, [saw, ate]).
p([Z])   :- member(Z, [in, from, on]).

% 2.3.a
% Add and modify grammar derivation rules for adverb support in verb phrases.
% Examples:
% ?- s([a, tall, cat, ate, the, mouse, gladly]).
% true
%
% ?- s([a, tall, cat, ate, the, mouse, in, the, house, hungrily]).
% true
%
% ?- s([the, cat, ate, the, mouse, in, the, funny, house, hungrily]).
% true
%
% ?- s([the, big, funny, mouse, ate, a, tall, cat, on, a, dog, gladly]).
% true
%
% ?- s([a, tall, cat, ate, gladly, the, mouse]).
% false
%
% ?- s([a, tall, cat, gladly, ate, the, mouse]).
% false
%

adv([Z]) :- Z=[] ; (member(Z, [hungrily, slowly, gladly])).
vp(Z)    :- append(F, T, Z), append(A, Y, F), append(V, X, A), v(V), np(X), pp(Y), adv(T).

% 2.3.b
% Signature: subCFG(Text, SubText)/2
% Purpose: SubText is sublist of a permutation of Text, 
%          both fit the 's' grammar.
% Precondition: The variables are fully instantiated.
% Examples:
% ?- subCFG([the, cat, saw, a, dog, in, a, funny, beautiful, house], 
%           [the, cat, saw, a, house]).
% true ;
% true
%
% ?- subCFG([the, cat, saw, a, dog, in, a, funny, beautiful, house], 
%	    [the, house, saw, a, beautiful, funny, dog]).
% true ;
% true
%
% ?- subCFG([the, big, funny, mouse, ate, a, tall, cat, on, a, dog], 
%           [a, tall, cat, ate, a, big, mouse, on, the, funny, dog]).
% true ;
% true
%
% ?- subCFG([the, big, funny, mouse, ate, a, tall, cat, on, a, dog], 
%           [a, tall, cat, ate, a, big, mouse, on, a, funny, dog]).
% false
%

subCFG(Text, SubText) :- 
    s(Text), 
    s(SubText),
    sublist_perm(Text, SubText).
