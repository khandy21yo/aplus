DEFINE RECORD CDD$TOP.SB.SB_ACCOUNT

        DESCRIPTION IS /*GL Accounts for Subsidiary Systems*/.

        SB_ACCOUNT_CDD STRUCTURE.

        /* Element = SYSTEM
        Description = Software System code */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Account Group */
        ACCTGROUP               DATATYPE IS TEXT SIZE IS 4.

        END SB_ACCOUNT_CDD STRUCTURE.

END SB_ACCOUNT.
