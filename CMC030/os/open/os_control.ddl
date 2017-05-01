DEFINE RECORD CDD$TOP.OS.OS_CONTROL

        DESCRIPTION IS /*OS Control File*/.

        OS_CONTROL_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Last Ticket Number */
        LAST_TICKET             DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Last Purge Date (YYYYMMDD) */
        PURGDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Activity Status */
        STATUS_FLAG             DATATYPE IS TEXT SIZE IS 1.

        /* Element = PRICETYPE
        Description = Price type for miscellaneous charges */
        MISCTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = FLAG
        Description = Display Price */
        DSPLPRICE               DATATYPE IS TEXT SIZE IS 1.

        /* Element = FLAG
        Description = Display Balance */
        DSPLQTY                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = PRICETYPE
        Description = List Price type */
        LISTCODE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Wildcard for cust type without balances */
        CUSBAL                  DATATYPE IS TEXT SIZE IS 20.

        /* Element = YESNO
        Description = Is this item taxable? */
        MISCTAXABLE             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = What customer tax types are exempt */
        MISCEXEMPT              DATATYPE IS TEXT SIZE IS 6.

        /* Element = PRICETYPE
        Description = Misc. (2) Price type */
        MISC2TYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = YESNO
        Description = Is this item taxable? */
        MISC2TAXABLE            DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = What tax types are exempt */
        MISC2EXEMPT             DATATYPE IS TEXT SIZE IS 6.

        END OS_CONTROL_CDD STRUCTURE.

END OS_CONTROL.
