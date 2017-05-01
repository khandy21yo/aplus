DEFINE RECORD CDD$TOP.PR.PR_CONTROL

        DESCRIPTION IS /*Payroll Control File*/.

        PR_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = Year of last purge */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Last folder Date Posted */
        POST_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Last update/reverse Date */
        UR_DATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Update/reverse counter */
        UR_COUNT                DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Close flag */
        CLOSEFLAG               DATATYPE IS TEXT SIZE IS 1.

        /* Element = FLAG
        Description = Apply oh to Department or Subacct (D/S) */
        OH_APPLY_FLAG           DATATYPE IS TEXT SIZE IS 1.

        END PR_CONTROL_CDD STRUCTURE.

END PR_CONTROL.
