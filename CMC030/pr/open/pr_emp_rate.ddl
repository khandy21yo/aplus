DEFINE RECORD CDD$TOP.PR.PR_EMP_RATE

        DESCRIPTION IS /*Payroll Employee Rate File*/.

        PR_EMP_RATE_CDD STRUCTURE.

        /* Element =
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = OPERATION
        Description = Operation */
        OPER                    DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Effective date */
        EFFDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Hourly,Salary,Piece,Mile */
        RATE_TYPE               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Rate code */
        RATE_CDE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Hourly Rate */
        HOUR_RATE               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Piece Salary */
        PIECE_RATE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Overtime Percentage */
        FACTOR                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Standard efficiency rating */
        STDEFF                  DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Evaluation Date */
        EVAL_DATE               DATATYPE IS TEXT SIZE IS 8.

        END PR_EMP_RATE_CDD STRUCTURE.

END PR_EMP_RATE.
