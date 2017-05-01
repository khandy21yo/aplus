DEFINE RECORD CDD$TOP.PC.PC_DEAL

        DESCRIPTION IS /*Special deals to customers*/.

        PC_DEAL_CDD STRUCTURE.

        /* Element =
        Description = Deal number */
        DEAL                    DATATYPE IS TEXT SIZE IS 20.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSTOMER                DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Start Date (YYYYMMDD) */
        STARTD                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = End Date (YYYYMMDD) */
        ENDD                    DATATYPE IS TEXT SIZE IS 8.

        /* Element = DESCRIPTION
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END PC_DEAL_CDD STRUCTURE.

END PC_DEAL.
