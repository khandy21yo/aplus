/*	%TITLE "Handle Broadcast Trapping."
 */
#pragma module read_3broadcast "V3.6 Calico"

/*
 *++
 *
 * Abstract:HELP
 *	.p
 * Parameters:
 *
 *	The input is five parameters passed through by the
 *	AST trapping routine.
 *
 * Example:
 *
 *	DO NOT CALL AS A NORMAL SUB.  This function is
 *	special and called by AST's.
 *
 * Author:
 *
 *	09/01/87 - Kevin Handy
 *
 *--
 */

/*
 * Include files
 */
#include <string>
#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"

/*
 * Main function
 */
void read_3broadcast(
	void *scopex,
	void *a2,
	void *a3,
	void *a4)
{
	long status;

	std::string text;
	scope_struct *scope = static_cast<scope_struct*>(scopex);

	/*
	 * Clear out buffering
	 */
	smg$flush_buffer(scope->smg_pbid);

	/*
	 * Read broadcast messages
	 */
	status = smg$get_broadcast_message(scope->smg_pbid, text);

	while (status == 1)
	{
		/*
		 * Write broadcast messages
		 */
		smg$put_chars(scope->smg_message, text, 1, 1);
		status = smg$get_broadcast_message(scope->smg_pbid, text);
	}
}
