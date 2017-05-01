DEFINE RECORD CDD$TOP.SA.SA_SALEGROUP

        DESCRIPTION IS /*Salesman Group File*/.

        SA_SALEGROUP_CDD STRUCTURE.

        /* Element = SALESMAN
        Description = Salesperson number */
        SALGROUP                DATATYPE IS TEXT SIZE IS 10.

        /* Element = SALESMAN
        Description = Salesperson number */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        END SA_SALEGROUP_CDD STRUCTURE.

END SA_SALEGROUP.
