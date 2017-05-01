DEFINE RECORD CDD$TOP.BA.BA_JOURNALL

        DESCRIPTION IS /*Agency Journal*/.

        BA_JOURNALL_CDD STRUCTURE.

        /* Element =
        Description = Billing Number */
        BILLNUM                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = EMPLOYEE
        Description = Client Number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Days worked */
        DAYS                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Hours Worked */
        HOURS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Units Worked */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Wages Worked */
        WAGES                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Rate billed */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Fee Billed */
        FEE                     DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Billing Method */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        END BA_JOURNALL_CDD STRUCTURE.

END BA_JOURNALL.
