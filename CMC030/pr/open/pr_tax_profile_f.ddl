DEFINE RECORD CDD$TOP.PR.PR_TAX_PROFILE_F

        DESCRIPTION IS /*Payroll Tax Profile - Federal*/.

        PR_TAX_PROFILE_F_CDD STRUCTURE.

        /* Element =
        Description = 'F' for federal records */
        AUTH                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = ' ' for federal */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        REPNO                   DATATYPE IS TEXT SIZE IS 20.

        /* Element = ACCOUNT
        Description = WH Account */
        WH_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = FICA Expense Account */
        FICA_EX_ACCT            DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = FICA Liability Account */
        FICA_LIA_ACCT_EMPR      DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = FICA Employee Liability */
        FICA_LIA_ACCT_EMPE      DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = FUI Expense Account */
        FUI_EX_ACCT             DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = FUI Liability Account */
        FUI_LIA_ACCT            DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = FUI Percentage */
        FUI_PCT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = FUI Maximum */
        FUI_MAX                 DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = Cash Account */
        CASH_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = PR Accrual Account */
        PR_ACCRUAL_ACCT         DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Minimum wage */
        MIN_WAGE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Direct Deposit Code */
        DIRECT                  DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Extra Space 1 */
        EXTRA1                  DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Extra Space 2 */
        EXTRA2                  DATATYPE IS TEXT SIZE IS 20.

        END PR_TAX_PROFILE_F_CDD STRUCTURE.

END PR_TAX_PROFILE_F.
