DEFINE RECORD CDD$TOP.AP.AP_37CLOSE

        DESCRIPTION IS /*AP Closed Information*/.

        AP_37CLOSE_CDD STRUCTURE.

        /* Element =
        Description = */
        VENNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        TRANKEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        TRANKEY_DATE            DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        INVNUM                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = */
        INVDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        INVAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        CODE_1099               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        AMT_1099                DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        USE_JOB_NUM             DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        USE_AMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        DISCDAT                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        DISAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        DUEDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        PONUM                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        AP_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        CASH_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        CKNUM                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        CKDAT                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        CKDESC                  DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = */
        CKAMT                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = (MMYYYY) */
        UPDATED                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = (MMYYYY) */
        CLOSEDATE               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Selected for payment (Y/N) */
        SELECTED                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END AP_37CLOSE_CDD STRUCTURE.

END AP_37CLOSE.
