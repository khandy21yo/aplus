DEFINE RECORD CDD$TOP.PR.PR_TRN_DED

        DESCRIPTION IS /*Payroll Deduction Journal*/.

        PR_TRN_DED_CDD STRUCTURE.

        /* Element = EMPNUM
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Payroll Date */
        PR_END_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = C-calculated tax, D-deduction */
        DTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Tax or deduction code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element = AMOUNT
        Description = Deduction amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Tax code */
        TAX_CODE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Withholding status */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = EXEMPT
        Description = Number of exemptions */
        EXEMPT                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Update flag */
        UPDATE_FLAG             DATATYPE IS SIGNED WORD.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Taxable Basis */
        TAXABLE                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Reportable Basis */
        REPORTABLE              DATATYPE IS G_FLOATING.

        /* Element = EXEMPT
        Description = Number of exemptions */
        ADDEXEMPT               DATATYPE IS SIGNED WORD.

        END PR_TRN_DED_CDD STRUCTURE.

END PR_TRN_DED.
