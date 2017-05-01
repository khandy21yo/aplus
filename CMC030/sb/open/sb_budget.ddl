DEFINE RECORD CDD$TOP.SB.SB_BUDGET

        DESCRIPTION IS /*Subaccount Budget File*/.

        SB_BUDGET_CDD STRUCTURE.

        /* Element =
        Description = System name */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = SUBACCT
        Description = Sub account (job number) */
        SUBACCOUNT              DATATYPE IS TEXT SIZE IS 10.

        /* Element = OPERATION
        Description = Operation */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Units */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Hours */
        HOURS                   DATATYPE IS G_FLOATING.

        END SB_BUDGET_CDD STRUCTURE.

END SB_BUDGET.
