DEFINE RECORD CDD$TOP.PR.PR_SALES

        DESCRIPTION IS /*Sales File*/.

        PR_SALES_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DEPARTMENT
        Description = Department number */
        DEPARTMENT              DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Date of sale (YYYYMMDD) */
        SALEDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Sales amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        END PR_SALES_CDD STRUCTURE.

END PR_SALES.
