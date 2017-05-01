DEFINE RECORD CDD$TOP.PR.PR_EMP_STATUS

        DESCRIPTION IS /*Payroll Employee Status File*/.

        PR_EMP_STATUS_CDD STRUCTURE.

        /* Element =
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Datatype. FW=Fed WH, SW=State WH, etc. */
        STTYPE                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = State, city, county, school code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Withholding status */
        STSTATUS                DATATYPE IS TEXT SIZE IS 1.

        /* Element = EXEMPT
        Description = Number of exemptions */
        EXEMPT                  DATATYPE IS SIGNED WORD.

        /* Element = EXEMPT
        Description = Additional Number of exemptions */
        ADDEXEMPT               DATATYPE IS SIGNED WORD.

        END PR_EMP_STATUS_CDD STRUCTURE.

END PR_EMP_STATUS.
