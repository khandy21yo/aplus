DEFINE RECORD CDD$TOP.WP.WP_BUYOFFLINE

        DESCRIPTION IS /*Manufacturing WIP Buyoff line file*/.

        WP_BUYOFFLINE_CDD STRUCTURE.

        /* Element =
        Description = WIP Job Number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Line Number */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Completed Quantity */
        COMPQTY                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Cancelled Quantity */
        CANCELQTY               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Unit Cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Date of buyoff(YYYYMMDD) */
        BDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = Buyoff General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Finished Goods Id No. */
        FG_ID_NO                DATATYPE IS TEXT SIZE IS 10.

        END WP_BUYOFFLINE_CDD STRUCTURE.

END WP_BUYOFFLINE.
