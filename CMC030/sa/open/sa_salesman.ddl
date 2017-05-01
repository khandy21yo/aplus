DEFINE RECORD CDD$TOP.SA.SA_SALESMAN

        DESCRIPTION IS /*Salesman Address File*/.

        SA_SALESMAN_CDD STRUCTURE.

        /* Element = SUBJECT
        Description = Subject type for salesman "S" */
        SUBJECT                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Salesperson number */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Salesman or Broker Name */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Job Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Job Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Onset Date */
        BDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Current Job Status */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Termination Date */
        EDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = ADDRESS
        Description = Address Line */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element = ADDRESS
        Description = Address Line */
        ADD2                    DATATYPE IS TEXT SIZE IS 25.

        /* Element = CITY
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Zip code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element = INITIALS
        Description = Inititals */
        INITIALS                DATATYPE IS TEXT SIZE IS 3.

        /* Element = REGION
        Description = Region number */
        REGION                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Commission Percentage */
        COMMPER                 DATATYPE IS G_FLOATING.

        END SA_SALESMAN_CDD STRUCTURE.

END SA_SALESMAN.
