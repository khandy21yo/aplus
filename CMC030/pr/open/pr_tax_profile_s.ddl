DEFINE RECORD CDD$TOP.PR.PR_TAX_PROFILE_S

        DESCRIPTION IS /*Payroll Tax Profile - State*/.

        PR_TAX_PROFILE_S_CDD STRUCTURE.

        /* Element =
        Description = 'S' for state */
        AUTH                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = User defined (ID, FL, ...) */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        REPNO                   DATATYPE IS TEXT SIZE IS 20.

        /* Element = ACCOUNT
        Description = WH Account */
        WH_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = OST Liability Account */
        OST_LIA_ACCT            DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = SUI Expense Account */
        SUI_EX_ACCT             DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = SUI Liability Account */
        SUI_LIA_ACCT            DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = SUI Percentage */
        SUI_PCT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = SUI Maximum */
        SUI_MAX                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Minimum wage */
        MIN_WAGE                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = OST Expense account */
        OST_EX_ACCT             DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = OST Percentage */
        OST_PCT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = OST Maximum */
        OST_MAX                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = OST Ded maximum */
        OST_DEDMAX              DATATYPE IS G_FLOATING.

        /* Element =
        Description = SUI Account Number */
        SUTANO                  DATATYPE IS TEXT SIZE IS 20.

        END PR_TAX_PROFILE_S_CDD STRUCTURE.

END PR_TAX_PROFILE_S.
