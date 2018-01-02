/*
 * %TITLE "Create Print String to Be Run Through WRIT_STRING"
 * %SBTTL "OUTP_CREATESTR"
 * %IDENT "V3.6 Calico"
 */

/*
 * Include files
 */
#include <cstdlib>
#include <cstring>
#include <string.h>
#include <string>

/*
 * Abstract:HELP
 *	.p
 *	Create a device control string.
 *
 * Parameters:
 *
 *	SEQ$
 *		The passed string that holds the print string to the run
 *		through WRIT_STRING.
 *
 *	ITEM$
 *		The passed string that holds the item to be run through.
 *
 *
 *	Returned value
 *		Creates a print string to be run through
 *		WRIT_STRING.
 *
 * Author:
 *
 *	01/01/86 - Kevin Handy
 *
 *--
 */


/*
 * Main function
 */
std::string  outp_createstr(
	const std::string &Source,
	const std::string &Item)
{
	std::string BuildString;	/* Build area for new string */
	std::string NullItem;		/* Null terminated Item */
	int NullLength;
	int SourceLoop;
	int Character;
	int Value;

	/*
	 * Make a version of Item that is NULL terminated so all the
	 * Standard C Library functions will work correctly.
	 */
	NullItem = Item;

	/*
	 * Trim off trailing spaces
	 */
	while ((NullItem.size() > 0) && (NullItem[NullItem.size() - 1] == ' '))
	{
		NullItem.erase(NullItem.size() - 1);
	}

	/*
	 * Scan through source string
	 */
	for (SourceLoop = 0; SourceLoop < Source.size(); SourceLoop++)
	{
		switch(Character = Source[SourceLoop])
		{
		case '*':
			switch(Source[SourceLoop + 1])
			{
			case '0':
				/*
				 * Copy item in as-is
				 */
				BuildString += NullItem;
				SourceLoop++;
				break;

			case '1':
				/*
				 * Write out single character + 31
				 */
				Value = stoi(NullItem) + 31;
				BuildString += '/';
				BuildString +=
					(Value / 100) % 10 + '0';
				BuildString +=
					(Value / 10) % 10 + '0';
				BuildString +=
					(Value) % 10 + '0';
				SourceLoop++;
				break;

			case '2':
				/*
				 * Write out single character
				 */
				Value = stoi(NullItem);
				BuildString += '/';
				BuildString +=
					(Value / 100) % 10 + '0';
				BuildString +=
					(Value / 10) % 10 + '0';
				BuildString +=
					(Value) % 10 + '0';
				SourceLoop++;
				break;

			default:
				/*
				 * Process a normal character
				 */
				BuildString += '*';
				break;
			}
			break;

		default:
			/*
			 * Process a normal character
			 */
			BuildString += Character;
			break;
		}
	}

	/*
	 * Trim off trailing spaces
	 */
	while ((BuildString.size() > 0) &&
		(BuildString[BuildString.size() - 1] == ' '))
	{
		BuildString.erase(BuildString.size() - 1);
	}

	/*
	 * Return string
	 */
	return BuildString;
}
