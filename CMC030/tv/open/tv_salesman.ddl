DEFINE RECORD CDD$TOP.TV.TV_SALESMAN

        DESCRIPTION IS /*TV Salesman Table*/.

        TV_SALESMAN_CDD STRUCTURE.

        /* Element = TV_SALNUM
        Description = Salesperson number */
        SALNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = NAME
        Description = Name */
        SNAME                   DATATYPE IS TEXT SIZE IS 30.

        /* Element = ADD1
        Description = Address line 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element = ADD2
        Description = Address line 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 21.

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
        COUNTRY                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element = ALPSRT
        Description = Alpha sort key */
        ALPSRT                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Rate */
        COMM                    DATATYPE IS G_FLOATING.

        END TV_SALESMAN_CDD STRUCTURE.

END TV_SALESMAN.
