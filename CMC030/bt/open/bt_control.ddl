DEFINE RECORD CDD$TOP.BT.BT_CONTROL

        DESCRIPTION IS /*Billing Tuition Control File*/.

        BT_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = Last period closed */
        LASTPERCLOSE            DATATYPE IS SIGNED WORD.

        /* Element = YEAR
        Description = Physical year (YYYY) */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = INVOICE
        Description = Invoice number */
        INV_NUM                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = FLAG
        Description = Flag */
        FLAG                    DATATYPE IS TEXT SIZE IS 1.

        END BT_CONTROL_CDD STRUCTURE.

END BT_CONTROL.
