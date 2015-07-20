SELECT * FROM testdb.sales_month
where Customer_Identifier="000026"
and yr_month="201505"
order by familyID, item;

create table itemChange as
	SELECT Customer_Identifier, yr_month, familyID, itemName, qty, itemPkgSize,
	qty*itemPkgSize as vol, sum(revenue) as itemRevenue 
	FROM testdb.sales_month
	where yr_month in ("201505","201312")
	group by Customer_Identifier, familyID, yr_month
	order by Customer_Identifier, familyID, yr_month;
ALTER TABLE itemChange add index (customer_identifier), add index (familyID),add index (familyID),add index (Customer_Identifier, familyID, yr_month);