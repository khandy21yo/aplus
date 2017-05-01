DEFINE RECORD CDD$TOP.BT.BT_TUITION

        DESCRIPTION IS /*Tuition Table*/.

        BT_TUITION_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Guardian */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Child */
        CHILD                   DATATYPE IS TEXT SIZE IS 40.

        /* Element = DATE
        Description = Effective from date */
        FROMDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Effective to date */
        TODATE                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Monthly Rate */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Monthly Rate */
        DIAPER_RATE             DATATYPE IS G_FLOATING.

        END BT_TUITION_CDD STRUCTURE.

END BT_TUITION.
