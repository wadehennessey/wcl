/*
 * $XConsortium: Xos.h,v 1.24 89/12/18 16:14:23 rws Exp $
 * 
 * Copyright 1987 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 */

/* This is a collection of things to try and minimize system dependencies
 * in a "signficant" number of source files.
 */

#ifndef _XOS_H_
#define _XOS_H_

/*
 * Get major data types (esp. caddr_t)
 */

#ifdef USG
#ifndef __TYPES__
#ifdef CRAY
#define word word_t
#endif
#include <sys/types.h>			/* forgot to protect it... */
#define __TYPES__
#ifdef CRAY
#undef word
#endif
#endif /* __TYPES__ */
#else
#include <sys/types.h>
#endif /* USG */


/*
 * Just about everyone needs the strings routines.  For uniformity, we use
 * the BSD-style index() and rindex() in application code, so any systems that
 * don't provide them need to have #defines here.  Unfortunately, we can't
 * use #if defined() here since makedepend will get confused.
 *
 * The list of systems that currently needs System V stings includes:
 *
 *	hpux
 * 	macII
 *	CRAY
 *	stellar
 *	USG
 * 
 * all of which happen to define SYSV as well.
 */

#ifdef SYSV
#include <string.h>
#define index strchr
#define rindex strrchr
#else
#include <strings.h>
#endif /* SYSV */


/*
 * Get open(2) constants
 */
#ifdef SYSV
#ifndef macII
#include <fcntl.h>
#endif
#endif /* SYSV */
#include <sys/file.h>
#ifdef USG
#include <unistd.h>
#endif


/*
 * Get struct timeval
 */

#ifdef SYSV
#if defined(sgi) || defined(CRAY2) || defined(stellar)
#include <sys/time.h>				/* SYSV sys/time.h */
#ifdef CRAY2
#include <time.h>
#define __TIMEVAL__
#endif
#else
#include <time.h>				/* else SYSV time.h */
#endif

#ifdef USG
#ifndef __TIMEVAL__
#define __TIMEVAL__
struct timeval {
    long tv_sec;
    long tv_usec;
};
struct timezone {
    int tz_minuteswest;
    int tz_dsttime;
};
#endif /* __TIMEVAL__ */
#endif /* USG */

#ifdef macII
#include <sys/time.h>				/* SYSV && macII */
#endif

#else
#include <sys/time.h>				/* else bsd */
#endif

/*
 * More BSDisms
 */

#ifdef SYSV
#ifndef macII
#ifndef ibm
#define SIGCHLD SIGCLD
#endif
#endif
#endif


/*
 * Put system-specific definitions here
 */

#ifdef hpux
#define sigvec sigvector
#endif

#endif /* _XOS_H_ */

