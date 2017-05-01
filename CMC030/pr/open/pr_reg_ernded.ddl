DEFINE RECORD CDD$TOP.PR.PR_REG_ERNDED

        DESCRIPTION IS /*Payroll Earnings and Deduction Register*/.

        PR_REG_ERNDED_CDD STRUCTURE.

        /* Element =
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Deduction, noncompensaTion, Memo */
        ETYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Ernded code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Quarter ernded dollars */
        QTR_DOLL                ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Regular hours */
        REG_HRS                 ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Premium hours */
        PRE_HRS                 ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Units */
        UNITS                   ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Update counter */
        UPDATE_COUNTER          DATATYPE IS SIGNED WORD.

        END PR_REG_ERNDED_CDD STRUCTURE.

END PR_REG_ERNDED.
