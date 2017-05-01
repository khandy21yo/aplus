DEFINE RECORD CDD$TOP.PR.PR_EMP_W2

        DESCRIPTION IS /*W2 EMPLOYEE GENERATION*/.

        PR_EMP_W2_CDD STRUCTURE.

        /* Element = EMPLOYEE
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = NAME
        Description = Name of a Person */
        EMPNAME                 DATATYPE IS TEXT SIZE IS 30.

        /* Element = ADDRESS
        Description = Address Line */
        ADD1                    DATATYPE IS TEXT SIZE IS 20.

        /* Element = ADDRESS
        Description = Address Line */
        ADD2                    DATATYPE IS TEXT SIZE IS 20.

        /* Element = CITY
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 16.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Zip code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = SSN
        Description = Social Security Number */
        SSN                     DATATYPE IS TEXT SIZE IS 12.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Array of earnings */
        EARNINGS                ARRAY 0:10 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Array of earnings codes */
        EARNINGS_CODE           ARRAY 0:10 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Array of Taxes witheld */
        TAXES                   ARRAY 0:10 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Array of taxables */
        TAXABLE                 ARRAY 0:10 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Array of tax codes */
        TAXES_CODE              ARRAY 0:10 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Array of state codes for taxes */
        TAXES_STATE             ARRAY 0:10 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Array of tax id numbers */
        TAXES_ID                ARRAY 0:10 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Array of deductions */
        DEDUCTIONS              ARRAY 0:10 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Array of deduction codes */
        DEDUCTIONS_CODE         ARRAY 0:10 DATATYPE IS TEXT SIZE IS 2.

        END PR_EMP_W2_CDD STRUCTURE.

END PR_EMP_W2.
