DEFINE RECORD CDD$TOP.PR.PR_HIS_CHECK

        DESCRIPTION IS /*Payroll Check History*/.

        PR_HIS_CHECK_CDD STRUCTURE.

        /* Element = EMPNUM
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Payroll end Date */
        PR_END_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element = CHKNUM
        Description = Check number */
        CHECK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Date */
        CHECK_DATE              DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Pay frequency */
        PAYFREQ                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Update flag */
        UPDATE_FLAG             DATATYPE IS SIGNED WORD.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END PR_HIS_CHECK_CDD STRUCTURE.

END PR_HIS_CHECK.
