DEFINE RECORD CDD$TOP.PR.PR_TAX_PKG

        DESCRIPTION IS /*Payroll Tax Package Table*/.

        PR_TAX_PKG_CDD STRUCTURE.

        /* Element =
        Description = Tax package number */
        TAX_PKG                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Datatype. FW=Fed, SW=state, etc. */
        STTYPE                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = State,city, county, school code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        END PR_TAX_PKG_CDD STRUCTURE.

END PR_TAX_PKG.
