DEFINE RECORD CDD$TOP.UTL.UTL_LOCATION

        DESCRIPTION IS /*Location Profile*/.

        UTL_LOCATION_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Location name */
        LOCNAME                 DATATYPE IS TEXT SIZE IS 40.

        /* Element = REGION
        Description = Region number */
        REGION                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCGROUP
        Description = Location group number */
        LOCGROUP                DATATYPE IS TEXT SIZE IS 2.

        /* Element = STREET
        Description = Address (street) */
        ADDRESS1                DATATYPE IS TEXT SIZE IS 25.

        /* Element = POBOX
        Description = Address (P.O.Box) */
        ADDRESS2                DATATYPE IS TEXT SIZE IS 21.

        /* Element = CITY
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Zip code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = COUNTY
        Description = County */
        COUNTY                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element = ADDRESS1
        Description = Ship to address, line 1 */
        SHPADDRESS1             DATATYPE IS TEXT SIZE IS 25.

        /* Element = ADDRESS2
        Description = Ship to address, line 2 */
        SHPADDRESS2             DATATYPE IS TEXT SIZE IS 21.

        /* Element = CITY
        Description = Ship to City */
        SHPCITY                 DATATYPE IS TEXT SIZE IS 15.

        /* Element = STATE
        Description = Ship to State */
        SHPSTATE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Ship to Zip Code */
        SHPZIP                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = COUNTY
        Description = Ship to County */
        SHPCOUNTY               DATATYPE IS TEXT SIZE IS 2.

        /* Element = COUNTRY
        Description = Ship to Country */
        SHPCOUNTRY              DATATYPE IS TEXT SIZE IS 2.

        /* Element = PHONE
        Description = Ship to Phone number */
        SHPPHONE                DATATYPE IS TEXT SIZE IS 10.

        END UTL_LOCATION_CDD STRUCTURE.

END UTL_LOCATION.
