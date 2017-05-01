DEFINE RECORD CDD$TOP.OE.OE_CONTROL

        DESCRIPTION IS /*Sales Order Controlling File*/.

        OE_CONTROL_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Control Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Last Purge Date (YYYYMMDD) */
        PURGDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Activity status */
        STATUS_FLAG             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = For the memo form to communicate with */
        LAST_MEMO               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = For the invoice form to communicate with */
        LAST_INV                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Aging Days in the Period */
        AGEPER                  ARRAY 0:4 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Name of Backorder Period */
        AGENAM                  ARRAY 0:4 DATATYPE IS TEXT SIZE IS 16.

        /* Element = PRICETYPE
        Description = Price type for Miscellaneous Charges */
        MISCTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = FLAG
        Description = Misc Charges Flag */
        MISCFLAG                DATATYPE IS TEXT SIZE IS 1.

        /* Element = FLAG
        Description = Display Price (Y/N) */
        DSPLPRICE               DATATYPE IS TEXT SIZE IS 1.

        /* Element = FLAG
        Description = Display Balances (Y/N) */
        DSPLQTY                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = PRICETYPE
        Description = List Price Type Code */
        LISTCODE                DATATYPE IS TEXT SIZE IS 2.

        END OE_CONTROL_CDD STRUCTURE.

END OE_CONTROL.
