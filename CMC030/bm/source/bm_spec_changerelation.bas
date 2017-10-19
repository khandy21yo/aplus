1	!
	!	This program is used to change all of the bloody part
	!	numbers in the BOM, or at least as many of them as
	!	possible
	!
	!++
	! History:
	!
	!	12/23/97 - Kevin Handy
	!		Initial Version
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION) BM_RELATION_CDD BM_RELATION

	DECLARE INTEGER CONSTANT MAXPARTS = 300

	DIM OLD_PART$(MAXPARTS)
	DIM NEW_PART$(MAXPARTS)

	ON ERROR GOTO 19000

1000	!
	! Load in conversion table
	!
	ITEM_COUNT% = 1%

	CALL ASSG_CHANNEL(RENAME.CH%, STAT%)
	OPEN "RENAMEBOM.TXT" FOR INPUT AS FILE RENAME.CH%

1100	INPUT #RENAME.CH%, OLD_PART$(ITEM_COUNT%), NEW_PART$(ITEM_COUNT%)
 !	PRINT OLD_PART$(ITEM_COUNT%), NEW_PART$(ITEM_COUNT%)

	ITEM_COUNT% = ITEM_COUNT% + 1%

	GOTO 1100

1900	CLOSE RENAME.CH%
	CALL ASSG_FREECHANNEL(RENAME.CH%)

	ITEM_COUNT% = ITEM_COUNT% - 1%

2000	!
	! Now, go through the bom relation file and make all of these changes
	!
	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.MOD"

2100	FOR ITEM_LOOP% = 1% TO ITEM_COUNT%

		LAST_PRODUCT$ = SPACE$(LEN(BM_RELATION::PRODUCT))
		COMP_PRODUCT$ = SPACE$(LEN(BM_RELATION::PRODUCT))
		LSET COMP_PRODUCT$ = OLD_PART$(ITEM_LOOP%)

2110		GET #BM_RELATION.CH%, &
			KEY #1% GT COMP_PRODUCT$ + LAST_PRODUCT$

2120		WHILE (BM_RELATION::COMPONENT == COMP_PRODUCT$)

			PRINT "Updating: "; BM_RELATION::PRODUCT; &
				BM_RELATION::ITEMNUM; "  "; &
				BM_RELATION::COMPONENT

			OLD_COMPONENT$ = BM_RELATION::COMPONENT
			BM_RELATION::COMPONENT = NEW_PART$(ITEM_LOOP%)

			DELETE #BM_RELATION.CH%

2125			PUT #BM_RELATION.CH%
			GOTO 2130

2127			PRINT "AAARRRGGGHHH, I HAVE TO LEAVE IT ALONE"
			BM_RELATION::COMPONENT = OLD_COMPONENT$
			PUT #BM_RELATION.CH%

2130			LAST_PRODUCT$ = BM_RELATION::PRODUCT

			GET #BM_RELATION.CH%, &
				KEY #1% GE COMP_PRODUCT$ + LAST_PRODUCT$

2140		NEXT

2900	NEXT ITEM_LOOP%

3000	GOTO 32767

19000	!
	! Error trap
	!
	SELECT ERL
	CASE 1100%
		RESUME 1900 IF ERR = 11%

	CASE 2110%
		RESUME 2900 IF ERR = 155%

	CASE 2130%
		RESUME 2900 IF ERR = 155%

	END SELECT

	PRINT ERR, ERL, ERT$(ERR)

	ON ERROR GOTO 0

32767	END