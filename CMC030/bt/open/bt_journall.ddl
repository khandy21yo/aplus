DEFINE RECORD CDD$TOP.BT.BT_JOURNALL

        DESCRIPTION IS /*Billing to Agency Journal Lines*/.

        BT_JOURNALL_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Guardian */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Child */
        CHILD                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Rate */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        END BT_JOURNALL_CDD STRUCTURE.

END BT_JOURNALL.
