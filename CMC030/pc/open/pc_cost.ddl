DEFINE RECORD CDD$TOP.PC.PC_COST

        DESCRIPTION IS /*Product Standard Cost*/.

        PC_COST_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Effective date (MMDDYYYY) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = AMOUNT
        Description = Product standard cost */
        COST                    DATATYPE IS G_FLOATING.

        END PC_COST_CDD STRUCTURE.

END PC_COST.
