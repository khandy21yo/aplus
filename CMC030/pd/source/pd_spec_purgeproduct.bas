1	%TITLE "Purge Product Master Files"
	%SBTTL "PD_SPEC_PURGEPRODUCT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:PD008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Purge Product Description File\* option
	!	purges selected products from
	!	the Product Description file. The purge selection can be accomplished
	!	by selecting specific product numbers, product types, product
	!	categories, product label codes or any combination of the above.
	!	.b
	!	The products will NOT be purged if they are statused as "A"ctive.
	!	Products will ONLY be purged if the "Status Date" in the
	!	product description file is older than the termination date entered
	!	prior to running this purge process.
	!	.lm -5
	!
	! Index:
	!	.x Purge
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_SPEC_PURGEPRODUCT/LINE
	!	$ LINK/EXE=PD_EXE: PD_SPEC_PURGEPRODUCT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_SPEC_PURGEPRODUCT.OBJ;*
	!
	! Author:
	!
	!	03/14/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	10/11/88 - Frank F. Starman
	!		Add all master files to be purged
	!
	!	01/15/91 - Val James Allen
	!		Add report of deleted items and set status
	!		flag to "D" for the purge process after the
	!		report is printed.
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Updated to version 3.6 source standards.
	!		Modified to use .MOD unstead of .UPD in opens.
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP	(PC_PRICE)	PC_PRICE_CDD		PC_PRICE

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP	(PC_COST)	PC_COST_CDD		PC_COST

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP	(IC_BINMAP)	IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[RM.OPEN]RM_TRANSFLAG.HB"
	MAP	(RM_TRANSFLAG)	RM_TRANSFLAG_CDD	RM_TRANSFLAG

	%INCLUDE "SOURCE:[RI.OPEN]RI_RELATION.HB"
	MAP	(RI_RELATION)	RI_RELATION_CDD		RI_RELATION

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRODUCT_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD001
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field
	!	selects products to be purged
	!	from the Product Description file by entering an individual product
	!	number, a string of product numbers separated by commas or by entering
	!	a wildcard mask.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Purge
	!	.x Purge>Product Number
	!	.x Number>Product
	!
	!--

	TYPE_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD002
	!	^*(02) Product Type\*
	!	.b
	!	.lm +5
	!	The ^*Product Type\* field
	!	selects products to be
	!	purged from the Product Description file by entering an individual
	!	product type code, a string of type codes separated by commas, or
	!	by entering a wildcard mask.
	!	.lm -5
	!
	! Index:
	!	.x Product Type>Purge
	!	.x Purge>Product Type
	!	.x Type>Product
	!
	!--

	CAT_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD003
	!	^*(03) Product Category\*
	!	.b
	!	.lm +5
	!	The ^*Product Category\* field
	!	selects products to be
	!	purged from the Product Description file by entering individual
	!	product category codes, strings of category codes or by entering
	!	a wildcard mask.
	!	.lm -5
	!
	! Index:
	!	.x Product Category>Purge
	!	.x Purge>Product Category
	!	.x Category>Product
	!
	!--

	LABEL_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD004
	!	^*(04) Label\*
	!	.b
	!	.lm +5
	!	The ^*Label\* field
	!	selects products to be purged
	!	from the Product Description file by entering a label code, a string
	!	of label codes separated by commas or by entering a wildcard mask.
	!	.lm -5
	!
	! Index:
	!	.x Label>Purge
	!	.x Purge>Label
	!
	!--


	TERM_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD005
	!	^*(05) Termination Date\*
	!	.b
	!	.lm +5
	!	The ^*Termination Date\* field
	!	selects products to be purged
	!	from the Product Description file by entering the effective
	!	termination date. All inactive/obsolete items with a termination
	!	date older than the date entered will be removed if they meet the selection
	!	criteria.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Termination Date>Purge
	!	.x Purge>Termination Date
	!
	!--


300	!
	! Open product file and all related files
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.MOD"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.MOD"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.MOD"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[RM.OPEN]RM_TRANSFLAG.MOD"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[RI.OPEN]RI_RELATION.MOD"
	USE
		CONTINUE 500 IF ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

500	ADD_TITLE$ = ""

	ADD_TITLE$ = "Product: " + PRODUCT_ITEM$ IF PRODUCT_ITEM$ <> ""
	ADD_TITLE$ = ADD_TITLE$ + ", Type: " + TYPE_ITEM$ IF TYPE_ITEM$ <> ""
	ADD_TITLE$ = ADD_TITLE$ + ", Catg: " + CAT_ITEM$ IF CAT_ITEM$ <> ""
	ADD_TITLE$ = ADD_TITLE$ + ", Label: " + LABEL_ITEM$ IF LABEL_ITEM$ <> ""

	GOTO ExitProgram IF ADD_TITLE$ = ""

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PURGE PRODUCT MASTER FILE - DATE " + &
		PRNT_DATE(TERM_DATE$, 8%)
	TITLE$(2%) = "Product Description System"
	TITLE$(3%) = ADD_TITLE$
	TITLE$(4%) = ""
	!
	! Heading
	!
	TITLE$(5%) = "Product#        Description                   " + &
		"            Type  UOM Pack  Categ  Label  Method  " + &
		"SecondaryCode  TermDate"
	TITLE$(6%) = "                     Location  Pctype       " + &
		"Price        Cost  EffectDate"
	TITLE$(7%) = "."

	%PAGE

	!
	! Print
	!
	RESET #PD_PRODUCT.CH%

1000	!
	! Main loop starts here
	!
	!
 GetNextRecPrint:

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%
	USE
		CONTINUE 1900 IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRecPrint &
		IF COMP_STRING(PD_PRODUCT::PRODUCT_NUM, PRODUCT_ITEM$) = 0%
	GOTO GetNextRecPrint &
		IF COMP_STRING(PD_PRODUCT::PROD_TYPE, TYPE_ITEM$) = 0%
	GOTO GetNextRecPrint &
		IF COMP_STRING(PD_PRODUCT::CATEGORY, CAT_ITEM$) = 0%
	GOTO GetNextRecPrint &
		IF COMP_STRING(PD_PRODUCT::LABEL, LABEL_ITEM$) = 0%

	!
	! Check status flag - only process if status is not equal "a" or "d"
	!
	GOTO GetNextRecPrint IF PD_PRODUCT::SSTATUS = "A"
	GOTO GetNextRecPrint IF PD_PRODUCT::SSTATUS = "D"
	GOTO GetNextRecPrint IF PD_PRODUCT::EDATE = ""

	!
	! Check termination date of product and if future - leave it in
	!
	GOTO GetNextRecPrint IF PD_PRODUCT::EDATE > TERM_DATE$

	!
	! Print out the description information
	!
	TEXT$ = PD_PRODUCT::PRODUCT_NUM	+ "  " + &
		PD_PRODUCT::DESCRIPTION	+ "  " + &
		PD_PRODUCT::PROD_TYPE + "    " + &
		PD_PRODUCT::UOM + "  " + &
		PD_PRODUCT::PACK + "  " + &
		PD_PRODUCT::CATEGORY + "   " + &
		PD_PRODUCT::LABEL + "   " + &
		PD_PRODUCT::METHOD + "    " + &
		PD_PRODUCT::SECONDARY_CODE + "     " + &
		PRNT_DATE(PD_PRODUCT::EDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

1010	PD_PRODUCT::SSTATUS = "D"

	WHEN ERROR IN
		UPDATE #PD_PRODUCT.CH%
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

1020	!
	! Print out price records now
	!
	WHEN ERROR IN
		FIND #PC_PRICE.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 1030 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN


 PrintPrice:
	WHEN ERROR IN
		GET #PC_PRICE.CH%
	USE
		CONTINUE 1030 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 1030 IF PC_PRICE::PRODUCT_NUM <> PD_PRODUCT::PRODUCT_NUM

	TEXT$ = PD_PRODUCT::PRODUCT_NUM	+ "       " + &
		PC_PRICE::LOCATION + "      " + &
		PC_PRICE::PCTYPE + "      " + &
		FORMAT$(PC_PRICE::PRICECOST, "#######.##") + "              " + &
		PRNT_DATE(PC_PRICE::XDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO PrintPrice

1030	!
	! Print record from the product cost file
	!
	WHEN ERROR IN
		FIND #PC_COST.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 1040 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

 PrintCost:
	WHEN ERROR IN
		GET #PC_COST.CH%
	USE
		CONTINUE 1040 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO 1040 IF PC_COST::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	TEXT$ = PD_PRODUCT::PRODUCT_NUM	+ "       " + &
		PC_COST::LOCATION + "                          " + &
		FORMAT$(PC_COST::COST, "#######.##") + "  " + &
		PRNT_DATE(PC_COST::EFFDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO PrintCost

1040	TEXT$ = ""
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO GetNextRecPrint

1900	!
	! Purge
	!
	RESET #PD_PRODUCT.CH%

2000	!
	! Main loop starts here
	!
	!
 GetNextRec:

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check for delete flag then remove all file information
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "D"

2010	!
	! Remove record from the product price file
	!
	WHEN ERROR IN
		FIND #PC_PRICE.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 2020 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

 Price:
	WHEN ERROR IN
		GET #PC_PRICE.CH%
	USE
		CONTINUE 2020 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 2020 IF PC_PRICE::PRODUCT_NUM <> PD_PRODUCT::PRODUCT_NUM

	WHEN ERROR IN
		DELETE #PC_PRICE.CH%
	USE
		CONTINUE 2020 IF ERR = 9%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO Price

2020	!
	! Remove record from the product cost file
	!
	WHEN ERROR IN
		FIND #PC_COST.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 2030 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

 Cost:
	WHEN ERROR IN
		GET #PC_COST.CH%
	USE
		CONTINUE 2030 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO 2030 IF PC_COST::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	WHEN ERROR IN
		DELETE #PC_COST.CH%
	USE
		CONTINUE 2030 IF ERR = 9%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO Cost

2030	!
	! Remove record from the product binmap file
	!
	WHEN ERROR IN
		FIND #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 2040 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

 Binmap:
	WHEN ERROR IN
		GET #IC_BINMAP.CH%
	USE
		CONTINUE 2040 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	GOTO 2040 IF IC_BINMAP::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	WHEN ERROR IN
		DELETE #IC_BINMAP.CH%
	USE
		CONTINUE 2040 IF ERR=9%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	Goto Binmap

2040	!
	! Remove record from the product transflag file
	!
	WHEN ERROR IN
		FIND #RM_TRANSFLAG.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 2050 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "RM_TRANSFLAG"
		CONTINUE HelpError
	END WHEN

 Transflag:
	WHEN ERROR IN
		GET #RM_TRANSFLAG.CH%
	USE
		CONTINUE 2050 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "RM_TRANSFLAG"
		CONTINUE HelpError
	END WHEN

	GOTO 2050 IF RM_TRANSFLAG::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	WHEN ERROR IN
		DELETE #RM_TRANSFLAG.CH%
	USE
		CONTINUE 2050 IF ERR=9%
		FILENAME$ = "RM_TRANSFLAG"
		CONTINUE HelpError
	END WHEN

	Goto Transflag

2050	!
	! Remove record from the product relation file
	!
	WHEN ERROR IN
		FIND #RI_RELATION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 Relation:
	WHEN ERROR IN
		GET #RI_RELATION.CH%
	USE
		CONTINUE 2100 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO 2100 IF RI_RELATION::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	WHEN ERROR IN
		DELETE #RI_RELATION.CH%
	USE
		CONTINUE 2100 IF ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	Goto Relation

2100	!
	! Remove record from the product description
	!
	WHEN ERROR IN
		DELETE #PD_PRODUCT.CH%
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
