DEFINE RECORD CDD$TOP.UTL.UTL_PROFILE

        DESCRIPTION IS /*Company Profile*/.

        UTL_PROFILE_CDD STRUCTURE.

        /* Element =
        Description = Company name for menu */
        MENU_NAME               DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Company name for report */
        REP_NAME                DATATYPE IS TEXT SIZE IS 30.

        /* Element = LOCATION
        Description = Main Office Location number */
        MAINLOCATION            DATATYPE IS TEXT SIZE IS 4.

        /* Element = LOCATION
        Description = Default Location number */
        DEFLOCATION             DATATYPE IS TEXT SIZE IS 4.

        END UTL_PROFILE_CDD STRUCTURE.

END UTL_PROFILE.
