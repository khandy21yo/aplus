DEFINE RECORD CDD$TOP.CK.CK_CKMNT

        DESCRIPTION IS /*Check Maintainance*/.

        CK_CKMNT_CDD STRUCTURE.

        /* Element =
        Description = Bank Account */
        BANK_ACCT               DATATYPE IS TEXT SIZE IS 6.

        /* Element = CHECK
        Description = Check number */
        CKNUM                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Entry Flag */
        ETYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = From GL (G), or Bank (B) */
        STYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Check Date */
        CKDAT                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Check Amount */
        CKAMT                   DATATYPE IS G_FLOATING.

        /* Element = PERIOD
        Description = GL Fiscal year (YYYY) and Cycle (PP) */
        GLDATE                  DATATYPE IS TEXT SIZE IS 6.

        END CK_CKMNT_CDD STRUCTURE.

END CK_CKMNT.
