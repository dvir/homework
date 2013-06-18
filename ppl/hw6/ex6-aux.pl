% A relational database for supermarket grocery management.
% The database contains the tables: product, product_category and vendor_info.

% Signature: product(Barcode, ProductName, Category, Refrigerated, VendorName)/5
% Purpose: details on available products in the supermarket.
%
product(972000232542, fresh_milk,     01,  true, dairy_inc).
product(972000237599, low_fat_milk,   01,  true, more_milk).
product(972000232543, preserved_milk, 01, false, dairy_inc).
product(972000756575, white_bread,    02, false, angy).
product(972000756576, rye_bread,      02, false, angy).
product(972000756577, pita,           02, false, breadman).
product(972000556756, chips,          03, false, much_salt).
product(972000767847, chips,          03, false, tasty_products).
product(972000767848, crisps,         03, false, tasty_products).
product(972000684856, cola,           04,  true, cool_drinks).
product(972000456745, lighter,        05, false, fire_ltd).

% Signature: product_category(Id, CategoryName)/2
% Purpose: specify product categories.
%
product_category(01, dairy).
product_category(02, bread).
product_category(03, snack).
product_category(04, soft_drinks).
product_category(05, misc).

% Signature: vendor_info(VendorName, Phone, MonthlyContactDay)/3
% Purpose: specify vendor contact information.
%
vendor_info(dairy_inc,      4564564, 5).
vendor_info(more_milk,      4564567, 15).
vendor_info(angy,           4574746, 28).
vendor_info(breadman,       8877665, 10).
vendor_info(much_salt,      5456464, 28).
vendor_info(tasty_products, 2002001, 28).
vendor_info(cool_drinks,    6785785, 15).
vendor_info(fire_ltd,       7855775, 10).

% Signature: not_member(Element, List)/2
% Purpose: the relation in which Element is not a member of a List.
%
not_member(_, []).
not_member(X, [Y|Ys]) :- X \= Y, 
                         not_member(X, Ys).
