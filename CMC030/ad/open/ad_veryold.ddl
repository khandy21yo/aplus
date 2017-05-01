DEFINE RECORD CDD$TOP.AD.AD_VERYOLD

        DESCRIPTION IS /*Old (RSTS) asset format*/.

        AD_VERYOLD_CDD STRUCTURE.

        /* Element = ASSET
        Description = Asset number */
        ASSET                   DATATYPE IS TEXT SIZE IS 10.

        /* Element = DESCRIPTION
        Description = Description 1 */
        DESCR1                  DATATYPE IS TEXT SIZE IS 40.

        /* Element = DESCRIPTION
        Description = Description 2 */
        DESCR2                  DATATYPE IS TEXT SIZE IS 40.

        /* Element = DESCRIPTION
        Description = Description 3 */
        DESCR3                  DATATYPE IS TEXT SIZE IS 40.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = DATE
        Description = Pirchase Date (YYYYMMDD) */
        PURCHASE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Disposal Date (YYYYMMDD) */
        DISPOSAL                DATATYPE IS TEXT SIZE IS 8.

        /* Element = METHOD
        Description = Method */
        METHOD                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Lifetime */
        LIFE                    DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Cost/Basis */
        COSTBASIS               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Salvage */
        SALVAGE                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Invest Tax Credit */
        INVEST                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = First Year Allowance */
        FIRSTYEAR               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Begining Depreciation */
        BEGIN                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = this Period Depreciation */
        THISPER                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Accountants Dep. */
        ACCOUNTANTS             DATATYPE IS G_FLOATING.

        /* Element =
        Description = ACRS Life */
        ACRSLIFE                DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = ACRS Method */
        ACRSMETHOD              DATATYPE IS TEXT SIZE IS 2.

        END AD_VERYOLD_CDD STRUCTURE.

END AD_VERYOLD.
