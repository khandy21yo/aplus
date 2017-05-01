1	!
	! Convert text file into polaris descriptions
	!
	option size = (integer long, real gfloat)

	%include "source:[pd.open]pd_product.hb"
	map (pd_product) pd_product_cdd pd_product

	dim politem$(20%)

100	open "polar.txt" for input as file 1%

110	%include "source:[pd.open]pd_product.cre"

1000	!*******************************************************************
	! Load in one line from the text file, and split it up
	!*******************************************************************

	input line #1%, polline$
	polline$ = edit$(polline$, 4%)
	goto 1000 if polline$ = ""

	item% = 1%

1100	!
	! Pop off one item
	!
	if left(polline$, 1%) = "'"
	then
		i% = instr(2%, polline$, "',")
		i% = len(polline$) + 1% if i% = 0%
		item$ = seg$(polline$, 2%, i% - 1%)
		polline$ = right(polline$, i% + 2%)
	else
		i% = instr(1%, polline$, ",")
		i% = len(polline$) + 1% if i% = 0%
		item$ = seg$(polline$, 1%, i% - 1%)
		polline$ = right(polline$, i% + 1%)
	end if

	politem$(item%) = item$
	item% = item% + 1%

	goto 1100 if polline$ <> ""

2000	!
	! Generate a product record
	!

 !	print
 !	print i%, politem$(i%) for i% = 1% to item%

	pd_product::PRODUCT_NUM = politem$(10%)
	pd_product::DESCRIPTION = politem$(8%)
	pd_product::PROD_TYPE = "P"
	pd_product::CATEGORY = politem$(3%)
	pd_product::UOM = "EA"
	pd_product::PACK = ""
	pd_product::LABEL = ""
	pd_product::METHOD = "STD"
	pd_product::BDATE = "19950101"
	pd_product::SSTATUS = "A"
	pd_product::EDATE = "19950101"
	pd_product::SECONDARY_CODE = politem$(11%)
	pd_product::WEIGHT = 0.0
	pd_product::BOMUOM = "EA"
	pd_product::PRODUCT_FACTOR = 1

	put #pd_product.ch%

	print pd_product::product_num; " "; pd_product::description

	goto 1000

32767	end
