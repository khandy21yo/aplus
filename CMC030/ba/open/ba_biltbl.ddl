DEFINE RECORD CDD$TOP.BA.BA_BILTBL

        DESCRIPTION IS /*Employee Billing*/.

        BA_BILTBL_CDD STRUCTURE.

        /* Element = EMPLOYEE
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Contract number */
        CONTRACT                DATATYPE IS TEXT SIZE IS 20.

        /* Element = DATE
        Description = From Date (YYYYMMDD) */
        FROMDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = To Date (YYYYMMDD) */
        TODATE                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Method */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Rate */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount Billable */
        BILLABLE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount Billed To date */
        BILTODAT                DATATYPE IS G_FLOATING.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END BA_BILTBL_CDD STRUCTURE.

END BA_BILTBL.
