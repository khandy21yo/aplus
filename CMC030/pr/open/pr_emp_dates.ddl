DEFINE RECORD CDD$TOP.PR.PR_EMP_DATES

        DESCRIPTION IS /*Payroll Employee Date History*/.

        PR_EMP_DATES_CDD STRUCTURE.

        /* Element = EMPLOYEE
        Description = Employee number */
        EMPLOYEE                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Date Code */
        DATECD                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Beginning Date */
        DATEBEGIN               DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Ending Date */
        DATEEND                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END PR_EMP_DATES_CDD STRUCTURE.

END PR_EMP_DATES.
