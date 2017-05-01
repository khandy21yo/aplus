DEFINE RECORD CDD$TOP.PR.PR_REG_TAXWH

        DESCRIPTION IS /*Payroll Tax Register*/.

        PR_REG_TAXWH_CDD STRUCTURE.

        /* Element =
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = FW, FI, SW, SX, CW, DW, EX */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Used if type = SW,SX,CW,DW,EX */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Quarterly wages */
        QTRWAG                  ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quarterly taxes witheld */
        QTRTAX                  ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Weeks worked during quarter */
        WKWRK                   ARRAY 0:3 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Batch number from update */
        UPDATE_COUNTER          DATATYPE IS SIGNED WORD.

        END PR_REG_TAXWH_CDD STRUCTURE.

END PR_REG_TAXWH.
