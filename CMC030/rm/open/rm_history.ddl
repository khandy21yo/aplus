DEFINE RECORD CDD$TOP.RM.RM_HISTORY

        DESCRIPTION IS /*Restaurant Sales and Labor History*/.

        RM_HISTORY_CDD STRUCTURE.

        /* Element =
        Description = Record type */
        CATEGORY                DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Date (MMDDYYYY) */
        ACTION_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        TIME_FROM               DATATYPE IS TEXT SIZE IS 6.

        /* Element = TIME
        Description = Time (HHMMSS) */
        TIME_TO                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Rate */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Store quantity/amount flag */
        REC_TYPE                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Amount/Quantity */
        AMOUNT_QTY              ARRAY 0:47 DATATYPE IS SIGNED WORD.

        END RM_HISTORY_CDD STRUCTURE.

END RM_HISTORY.
