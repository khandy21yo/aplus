DEFINE RECORD CDD$TOP.AP.AP_CDJ

        DESCRIPTION IS /*Cash Disbursements Journal*/.

        AP_CDJ_CDD STRUCTURE.

        /* Element =
        Description = Vendor number */
        VENNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Transaction key */
        TRANKEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Invoice date */
        INVDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Invoice amount */
        INVAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Check amount */
        CKAMT                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Check number */
        CKNUM                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Check date */
        CKDAT                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Check description */
        CKDESC                  DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Discount date */
        DISCDAT                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Discount amount */
        DISAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount discount lost */
        DISC_LOST_AMT           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Discount lost to account */
        DISCLOST_ACCT           DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Due date */
        DUEDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Purchase order number */
        PONUM                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Accounts Payable Account */
        AP_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Cash Account */
        CASH_ACCT               DATATYPE IS TEXT SIZE IS 18.

        END AP_CDJ_CDD STRUCTURE.

END AP_CDJ.
