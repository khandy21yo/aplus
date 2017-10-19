1	%TITLE "Change Product in Product Master Files"
	%SBTTL "PD_SPEC_CHANGEPRODUCT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Purge
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_SPEC_CHANGEPRODUCT/LINE
	!	$ LINK/EXE=PD_EXE: PD_SPEC_CHANGEPRODUCT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_SPEC_CHANGEPRODUCT.OBJ;*
	!
	! Author:
	!
	!	06/24/93 - Dan Perkins
	!
	! Modification history:
	!
	!	07/06/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Maps
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)		PC_PRICE_CDD		PC_PRICE

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP	(PC_COST)	PC_COST_CDD		PC_COST

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.HB"
	MAP (MO_MODEL)		MO_MODEL_CDD		MO_MODEL

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	ON ERROR GOTO 19000

	%PAGE

	!
	! Open product file and all related files
	!
300	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"

310	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"

320	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"

330	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.CRE"

340	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.CRE"

350	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.CRE"

360	%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.CRE"

370	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.CRE"

380	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.CRE"

390	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.CRE"

400	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"

410	YYYY$ = "1993"
	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.CRE"

420	YYYYPP$ = "199309"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.CRE"

430	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.CRE"

	!
	! Ask for old product number and new product number
	!
500	PRINT

	INPUT "Old Product Number"; OLD_PRODUCT$

	GOTO ExitProgram IF OLD_PRODUCT$ = ""

	OLD_PRODUCT$ = OLD_PRODUCT$ + SPACE$(14% - LEN(OLD_PRODUCT$))

	!
	! See if good product number was entered
	!
510	GET #PD_PRODUCT.CH%, KEY#0% EQ OLD_PRODUCT$

	TEXT$ = TRM$(PD_PRODUCT::PRODUCT_NUM) + "     " + &
		TRM$(PD_PRODUCT::DESCRIPTION)

	PRINT TEXT$

600	PRINT

	INPUT "New Product Number"; NEW_PRODUCT$

	IF NEW_PRODUCT$ = ""
	THEN
		PRINT "Can't enter a blank product."
		GOTO 600
	END IF

	NEW_PRODUCT$ = NEW_PRODUCT$ + SPACE$(14% - LEN(NEW_PRODUCT$))

	!
	! See if the new product already exists in the product file
	!
610	GET #PD_PRODUCT.CH%, KEY#0% EQ NEW_PRODUCT$

	TEXT$ = "New Product Number Already Exists:" + "  "    + &
		TRM$(PD_PRODUCT::PRODUCT_NUM)        + "     " + &
		TRM$(PD_PRODUCT::DESCRIPTION)

	PRINT TEXT$

	GOTO 600

	!
	! Do the price file
	!
1000	GET #PC_PRICE.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #PC_PRICE.CH%

	PC_PRICE::PRODUCT_NUM = NEW_PRODUCT$

1100	PUT #PC_PRICE.CH%

	GOTO 1000

	!
	! Do the cost file
	!
2000	GET #PC_COST.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #PC_COST.CH%

	PC_COST::PRODUCT = NEW_PRODUCT$

2100	PUT #PC_COST.CH%

	GOTO 2000

	!
	! Do the binmap file
	!
3000	GET #IC_BINMAP.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #IC_BINMAP.CH%

	IC_BINMAP::PRODUCT = NEW_PRODUCT$

3100	PUT #IC_BINMAP.CH%

	GOTO 3000

	!
	! Do the prodoper file
	!
4000	GET #BM_PRODOPER.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #BM_PRODOPER.CH%

	BM_PRODOPER::PRODUCT = NEW_PRODUCT$

4100	PUT #BM_PRODOPER.CH%

	GOTO 4000

	!
	! Do the relation product
	!
5000	GET #BM_RELATION.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #BM_RELATION.CH%

	BM_RELATION::PRODUCT = NEW_PRODUCT$

5100	PUT #BM_RELATION.CH%

	GOTO 5000

	!
	! Do the relation component
	!
6000	GET #BM_RELATION.CH%, KEY#1% EQ OLD_PRODUCT$

	DELETE #BM_RELATION.CH%

	BM_RELATION::COMPONENT = NEW_PRODUCT$

6100	PUT #BM_RELATION.CH%

	GOTO 6000

	!
	! Do the model file
	!
7000	GET #MO_MODEL.CH%, KEY#1% EQ OLD_PRODUCT$

	DELETE #MO_MODEL.CH%

	MO_MODEL::PRODUCT = NEW_PRODUCT$

7100	PUT #MO_MODEL.CH%

	GOTO 7000

	!
	! Do the option file
	!
8000	GET #MO_OPTION.CH%, KEY#1% EQ OLD_PRODUCT$

	DELETE #MO_OPTION.CH%

	MO_OPTION::PRODUCT = NEW_PRODUCT$

8100	PUT #MO_OPTION.CH%

	GOTO 8000

	!
	! Do the partcross file
	!
9000	GET #PO_PARTCROSS.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #PO_PARTCROSS.CH%

	PO_PARTCROSS::PRODUCT = NEW_PRODUCT$

9100	PUT #PO_PARTCROSS.CH%

	GOTO 9000

	!
	! Do the po regline file
	!
10000	GET #PO_REG_LINE.CH%, KEY#4% EQ OLD_PRODUCT$

	DELETE #PO_REG_LINE.CH%

	PO_REG_LINE::PRODUCT = NEW_PRODUCT$

10100	PUT #PO_REG_LINE.CH%

	GOTO 10000

	!
	! Do the ic balance file
	!
11000	GET #IC_35BALANCE.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #IC_35BALANCE.CH%

	IC_35BALANCE::PRODUCT = NEW_PRODUCT$

11100	PUT #IC_35BALANCE.CH%

	GOTO 11000

	!
	! Do the ic history file
	!
12000	GET #IC_35HISTORY.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #IC_35HISTORY.CH%

	IC_35HISTORY::PRODUCT = NEW_PRODUCT$

12100	PUT #IC_35HISTORY.CH%

	GOTO 12000

	!
	! Do the ic transaction file
	!
13000	GET #IC_TRANSACTION.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #IC_TRANSACTION.CH%

	IC_TRANSACTION::PRODUCT = NEW_PRODUCT$

13100	PUT #IC_TRANSACTION.CH%

	GOTO 13000

	!
	! Do the oe regline file
	!
14000	GET #PO_REG_LINE.CH%, KEY#1% EQ OLD_PRODUCT$

	DELETE #OE_REGLINE.CH%

	OE_REGLINE::PRODUCT = NEW_PRODUCT$

14100	PUT #OE_REGLINE.CH%

	GOTO 14000

	!
	! Do the product file
	!
15000	GET #PD_PRODUCT.CH%, KEY#0% EQ OLD_PRODUCT$

	DELETE #PD_PRODUCT.CH%

	PD_PRODUCT::PRODUCT_NUM = NEW_PRODUCT$

15100	PUT #PD_PRODUCT.CH%

	GOTO 500

 EntryError:
	PRINT "This product does not exist."

	GOTO 500

 ExitProgram:
	!
	! Exit to next program or menu
	!
	!CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	GOTO 32767

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	SELECT ERL

	CASE 500%
		RESUME ExitProgram IF ERR = 11%

	CASE 510%
		RESUME EntryError IF ERR = 155%

	CASE 600%
		RESUME ExitProgram IF ERR = 11%

	CASE 610
		RESUME 1000 IF ERR = 155%

	CASE 1000%
		RESUME 2000 IF ERR = 155%

	CASE 1100%
		RESUME 1000 IF ERR = 134%

	CASE 2000%
		RESUME 3000 IF ERR = 155%

	CASE 2100%
		RESUME 2000 IF ERR = 134%

	CASE 3000%
		RESUME 4000 IF ERR = 155%

	CASE 3100%
		RESUME 3000 IF ERR = 134%

	CASE 4000%
		RESUME 5000 IF ERR = 155%

	CASE 4100%
		RESUME 4000 IF ERR = 134%

	CASE 5000%
		RESUME 6000 IF ERR = 155%

	CASE 5100%
		RESUME 5000 IF ERR = 134%

	CASE 6000%
		RESUME 7000 IF ERR = 155%

	CASE 6100%
		RESUME 6000 IF ERR = 134%

	CASE 7000%
		RESUME 8000 IF ERR = 155%

	CASE 7100%
		RESUME 7000 IF ERR = 134%

	CASE 8000%
		RESUME 9000 IF ERR = 155%

	CASE 8100%
		RESUME 8000 IF ERR = 134%

	CASE 9000%
		RESUME 10000 IF ERR = 155%

	CASE 9100%
		RESUME 9000 IF ERR = 134%

	CASE 10000%
		RESUME 11000 IF ERR = 155%

	CASE 10100%
		RESUME 10000 IF ERR = 134%

	CASE 11000%
		RESUME 12000 IF ERR = 155%

	CASE 11100%
		RESUME 11000 IF ERR = 134%

	CASE 12000%
		RESUME 13000 IF ERR = 155%

	CASE 12100%
		RESUME 12000 IF ERR = 134%

	CASE 13000%
		RESUME 14000 IF ERR = 155%

	CASE 13100%
		RESUME 13000 IF ERR = 134%

	CASE 14000%
		RESUME 15000 IF ERR = 155%

	CASE 14100%
		RESUME 14000 IF ERR = 134%

	CASE 15000%
		RESUME 500 IF ERR = 155%

	CASE 15100%
		RESUME 15000 IF ERR = 134%

	END SELECT

	ON ERROR GOTO 0

32767	END