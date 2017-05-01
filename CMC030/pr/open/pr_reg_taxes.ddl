DEFINE RECORD CDD$TOP.PR.PR_REG_TAXES

        DESCRIPTION IS /*Payroll Tax Register*/.

        PR_REG_TAXES_CDD STRUCTURE.

        /* Element = EMPLOYEE
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = FW,FI,SW,SX,SU,DW,EW,etc. */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Tax code (Used if type=SW,SX,CW,DW,EX) */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Quarterly Taxes Witheld */
        TAX                     ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quarterly Reportable Wages */
        REPORTABLE              ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quarterly Taxable Wages */
        TAXABLE                 ARRAY 0:3 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Weeks Worked during Quarter */
        WKWRK                   ARRAY 0:3 DATATYPE IS SIGNED WORD.

        /* Element = UPCOUNT
        Description = Update counter */
        UPDATE_COUNTER          DATATYPE IS SIGNED WORD.

        END PR_REG_TAXES_CDD STRUCTURE.

END PR_REG_TAXES.
